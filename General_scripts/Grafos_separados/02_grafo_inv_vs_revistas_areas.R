library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(echarts4r)

# 1. Cargar datos
areas_df <- read.csv("es_parte_de_limpio_listas.csv", stringsAsFactors = FALSE)
investigadores <- read.csv2("Investigadores_internos.csv", sep = ";", fileEncoding = "latin1")
revistas <- read.csv("Revistas.csv", stringsAsFactors = FALSE)

# 2. Procesar áreas
split_name_and_areas <- function(nombre, areas) {
  if (is.na(areas) && grepl(",", nombre)) {
    parts <- unlist(strsplit(nombre, ",", fixed = TRUE))
    nombre <- trimws(parts[1])
    areas <- trimws(parts[2])
  }
  list(Nombre = nombre, Areas = areas)
}

cleaned <- mapply(split_name_and_areas, areas_df$Nombre, areas_df$Areas, SIMPLIFY = FALSE)
areas_df <- do.call(rbind, lapply(cleaned, as.data.frame))
areas_df$Areas <- gsub('"', '', areas_df$Areas)
areas_df$Lista_areas <- strsplit(gsub("/", ",", areas_df$Areas), ",\\s*")

# 3. Leer publicaciones individuales
investigadores_con_csv <- investigadores %>%
  filter(!is.na(Acortacion))

publicaciones_list <- list()
for (i in 1:nrow(investigadores_con_csv)) {
  nombre <- investigadores_con_csv$Nombre[i]
  acort <- investigadores_con_csv$Acortacion[i]
  archivo <- paste0("1ha_publicado_en_", acort, "_editoriales.csv")
  
  if (file.exists(archivo)) {
    df <- read_csv(archivo, locale = locale(encoding = "Latin1"), show_col_types = FALSE)
    df$investigador <- nombre
    publicaciones_list[[length(publicaciones_list) + 1]] <- df
  }
}

# 4. Unificar publicaciones
publicaciones_df <- bind_rows(publicaciones_list)
relaciones <- publicaciones_df %>%
  select(journal, investigador) %>%
  filter(!is.na(journal), journal != "")

# 5. Unir investigadores con áreas
investigadores <- investigadores %>%
  left_join(areas_df[, c("Nombre", "Lista_areas")], by = c("Nombre" = "Nombre"))

# 6. Crear nodos con categorías
nodos_invest <- investigadores %>%
  select(name = Nombre, Lista_areas, category = Tipo.de.empleado) %>%
  mutate(category = ifelse(is.na(category), "Desconocido", category))

nodos_revistas <- revistas %>%
  filter(!is.na(Revista), Revista != "") %>%
  filter(Revista %in% relaciones$journal) %>%
  select(name = Revista) %>%
  mutate(category = "revista", Lista_areas = NA)

nodos <- bind_rows(nodos_invest, nodos_revistas) %>%
  mutate(value = 10, size = 20)

edges <- relaciones %>%
  rename(source = investigador, target = journal)

nombres_ordenados <- investigadores %>%
  arrange(Nombre) %>%
  pull(Nombre)

todas_las_areas <- sort(unique(unlist(areas_df$Lista_areas)))

# 7. Shiny UI
ui <- fluidPage(
  titlePanel("Investigadores y Revistas"),
  fluidRow(
    column(
      width = 4,
      selectInput("investigador_filtro", "Selecciona un investigador:",
                  choices = c("-" = "-", nombres_ordenados),
                  selected = "-"),
      selectInput("area_filtro", "Selecciona un área:",
                  choices = c("-" = "-", todas_las_areas),
                  selected = "-")
    ),
    column(
      width = 8,
      echarts4rOutput("grafo", height = "800px")
    ),
    column(
      width = 4,
      h4("Información del nodo seleccionado"),
      verbatimTextOutput("info_nodo")
    )
  )
)

# 8. Shiny Server
server <- function(input, output, session) {
  output$grafo <- renderEcharts4r({
    # Filtro por área
    filtrar_por_area <- function(nodo) {
      if (is.null(nodo$Lista_areas)) return(FALSE)
      input$area_filtro %in% nodo$Lista_areas[[1]]
    }
    
    if (input$investigador_filtro == "-" && input$area_filtro == "-") {
      nodos_mostrar <- nodos
      edges_mostrar <- edges
    } else if (input$area_filtro != "-" && input$investigador_filtro == "-") {
      nodos_filtrados <- nodos %>%
        filter(category != "revista") %>%
        filter(sapply(split(., 1:nrow(.)), filtrar_por_area))
      
      edges_mostrar <- edges %>%
        filter(source %in% nodos_filtrados$name)
      
      revistas_conectadas <- unique(edges_mostrar$target)
      nodos_mostrar <- bind_rows(nodos_filtrados,
                                 nodos %>% filter(name %in% revistas_conectadas))
      
    } else if (input$investigador_filtro != "-" && input$area_filtro == "-") {
      edges_mostrar <- edges %>% filter(source == input$investigador_filtro)
      revistas_conectadas <- unique(edges_mostrar$target)
      nodos_mostrar <- nodos %>%
        filter(name == input$investigador_filtro | name %in% revistas_conectadas)
    } else {
      if (input$area_filtro %in% unlist(nodos$Lista_areas[nodos$name == input$investigador_filtro])) {
        edges_mostrar <- edges %>% filter(source == input$investigador_filtro)
        revistas_conectadas <- unique(edges_mostrar$target)
        nodos_mostrar <- nodos %>%
          filter(name == input$investigador_filtro | name %in% revistas_conectadas)
      } else {
        nodos_mostrar <- nodos[0, ]
        edges_mostrar <- edges[0, ]
      }
    }
    
    nodos_mostrar %>%
      e_charts(name) %>%
      e_graph(layout = "force") %>%
      e_graph_nodes(nodos_mostrar, name, value, size, category) %>%
      e_graph_edges(edges_mostrar, source, target) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return '<strong>' + params.name + '</strong>';
        }
      ")) %>%
      e_legend(top = "20") %>%
      e_on(
        query = list(dataType = "node"),
        handler = htmlwidgets::JS("
          function(params) {
            Shiny.setInputValue('clicked_node', params.data.name, {priority: 'event'});
          }
        ")
      )
  })
  
  output$info_nodo <- renderText({
    node_to_show <- input$clicked_node
    if (is.null(node_to_show)) {
      if (input$investigador_filtro == "-") {
        return("Seleccione o clique un nodo para ver su información.")
      } else {
        node_to_show <- input$investigador_filtro
      }
    }
    
    tipo_nodo <- nodos %>% filter(name == node_to_show) %>% pull(category)
    if (length(tipo_nodo) == 0) return("Nodo no reconocido.")
    
    if (tipo_nodo != "revista") {
      articulos <- publicaciones_df %>%
        filter(investigador == node_to_show) %>%
        pull(title)
      
      if (length(articulos) == 0) {
        return(paste0("Investigador/a: ", node_to_show, "\n\nNo se han encontrado publicaciones registradas."))
      }
      
      return(paste0("Investigador/a: ", node_to_show, "\n\nArtículos publicados:\n", paste0("- ", articulos, collapse = "\n")))
    } else {
      articulos <- publicaciones_df %>%
        filter(journal == node_to_show) %>%
        select(title, investigador)
      
      if (nrow(articulos) == 0) {
        return(paste0("Revista: ", node_to_show, "\n\nNo se han encontrado artículos publicados en esta revista."))
      }
      
      textos <- paste0("- ", articulos$title, " (", articulos$investigador, ")")
      return(paste0("Revista: ", node_to_show, "\n\nArtículos publicados:\n", paste(textos, collapse = "\n")))
    }
  })
}

# 9. Ejecutar app
shinyApp(ui, server)
