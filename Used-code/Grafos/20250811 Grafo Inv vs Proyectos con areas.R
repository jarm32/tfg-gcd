library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(echarts4r)

# 1. Cargar datos
areas_df <- read.csv("es_parte_de_limpio_listas.csv", stringsAsFactors = FALSE)
investigadores <- read.csv2("Investigadores_internos.csv", sep = ";", fileEncoding = "latin1")

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

# 3. Leer proyectos individuales
investigadores_con_proyectos <- investigadores %>%
  filter(!is.na(Acortacion))

proyectos_list <- list()
for (i in 1:nrow(investigadores_con_proyectos)) {
  nombre <- investigadores_con_proyectos$Nombre[i]
  acort <- investigadores_con_proyectos$Acortacion[i]
  archivo <- paste0("ha_participado_en_", acort, "_FINAL.xlsx")
  
  if (file.exists(archivo)) {
    df <- read_excel(archivo)
    df$investigador <- nombre
    proyectos_list[[length(proyectos_list) + 1]] <- df
  }
}

# 4. Unificar proyectos
proyectos_df <- bind_rows(proyectos_list)
relaciones <- proyectos_df %>%
  select(Proyecto = TÍTULO, investigador) %>%
  filter(!is.na(Proyecto), Proyecto != "")

# 5. Unir investigadores con áreas
investigadores <- investigadores %>%
  left_join(areas_df[, c("Nombre", "Lista_areas")], by = c("Nombre" = "Nombre"))

# 6. Crear nodos con categorías
nodos_invest <- investigadores %>%
  select(name = Nombre, Lista_areas, category = Tipo.de.empleado) %>%
  mutate(category = ifelse(is.na(category), "Desconocido", category))

proyectos_unicos <- unique(relaciones$Proyecto)
nodos_proyectos <- data.frame(name = proyectos_unicos, 
                              category = "proyecto", 
                              Lista_areas = NA,
                              stringsAsFactors = FALSE)

nodos <- bind_rows(nodos_invest, nodos_proyectos) %>%
  mutate(value = 10, size = 20)

edges <- relaciones %>%
  rename(source = investigador, target = Proyecto)

nombres_ordenados <- investigadores %>%
  arrange(Nombre) %>%
  pull(Nombre)

todas_las_areas <- sort(unique(unlist(areas_df$Lista_areas)))

# 7. Shiny UI
ui <- fluidPage(
  titlePanel("Investigadores y Proyectos"),
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
        filter(category != "proyecto") %>%
        filter(sapply(split(., 1:nrow(.)), filtrar_por_area))
      
      edges_mostrar <- edges %>%
        filter(source %in% nodos_filtrados$name)
      
      proyectos_conectados <- unique(edges_mostrar$target)
      nodos_mostrar <- bind_rows(nodos_filtrados,
                                 nodos %>% filter(name %in% proyectos_conectados))
      
    } else if (input$investigador_filtro != "-" && input$area_filtro == "-") {
      edges_mostrar <- edges %>% filter(source == input$investigador_filtro)
      proyectos_conectados <- unique(edges_mostrar$target)
      nodos_mostrar <- nodos %>%
        filter(name == input$investigador_filtro | name %in% proyectos_conectados)
    } else {
      if (input$area_filtro %in% unlist(nodos$Lista_areas[nodos$name == input$investigador_filtro])) {
        edges_mostrar <- edges %>% filter(source == input$investigador_filtro)
        proyectos_conectados <- unique(edges_mostrar$target)
        nodos_mostrar <- nodos %>%
          filter(name == input$investigador_filtro | name %in% proyectos_conectados)
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
            if (params.data && params.data.name && params.data.category) {
              Shiny.setInputValue('clicked_node', { name: params.data.name, category: params.data.category }, {priority: 'event'});
            }
          }
        ")
      )
  })
  
  output$info_nodo <- renderText({
    node_data <- input$clicked_node
    if (is.null(node_data)) {
      if (input$investigador_filtro == "-") {
        return("Seleccione o clique un nodo para ver su información.")
      } else {
        node_data <- list(name = input$investigador_filtro, 
                          category = nodos %>% filter(name == input$investigador_filtro) %>% pull(category))
      }
    }
    
    node_to_show <- node_data$name
    tipo_nodo <- node_data$category
    
    if (length(tipo_nodo) == 0) return("Nodo no reconocido.")
    
    if (tipo_nodo != "proyecto") {
      proyectos <- proyectos_df %>%
        filter(investigador == node_to_show) %>%
        pull(TÍTULO)
      
      if (length(proyectos) == 0) {
        return(paste0("Investigador/a: ", node_to_show, "\n\nNo se han encontrado proyectos registrados."))
      }
      
      return(paste0("Investigador/a: ", node_to_show, "\n\nProyectos en los que participa:\n", paste0("- ", proyectos, collapse = "\n")))
    } else {
      investigadores_en_proyecto <- proyectos_df %>%
        filter(TÍTULO == node_to_show) %>%
        pull(investigador)
      
      if (length(investigadores_en_proyecto) == 0) {
        return(paste0("Proyecto: ", node_to_show, "\n\nNo se han encontrado investigadores en este proyecto."))
      }
      
      return(paste0("Proyecto: ", node_to_show, "\n\nInvestigadores que participan:\n", paste0("- ", investigadores_en_proyecto, collapse = "\n")))
    }
  })
}

# 9. Ejecutar app
shinyApp(ui, server)