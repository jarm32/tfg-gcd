library(shiny)
library(echarts4r)
library(dplyr)
library(readr)

setwd("C:/Users/josep/Downloads/ZonaT")

# 1. Cargar datos principales
revistas <- read_csv("Revistas.csv", locale = locale(encoding = "Latin1"))
investigadores <- read_csv2("Investigadores_internos.csv", locale = locale(encoding = "Latin1"))

# 2. Leer las áreas desde 'es_parte_de_limpio.csv'
lineas_areas <- readLines("es_parte_de_limpio.csv", encoding = "UTF-8")[-1]
datos_areas <- strsplit(lineas_areas, ',"')
datos_areas <- lapply(datos_areas, function(x) c(gsub('"', '', x[1]), gsub('"', '', x[2])))
df_areas <- as.data.frame(do.call(rbind, datos_areas), stringsAsFactors = FALSE)
colnames(df_areas) <- c("nombre", "areas")
df_areas$area_principal <- sapply(strsplit(df_areas$areas, ",|/"), function(x) trimws(x[1]))

# 3. Leer CSV individuales por investigador
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

# 4. Unir publicaciones
publicaciones_df <- bind_rows(publicaciones_list)

# 5. Relaciones entre investigadores y revistas
relaciones <- publicaciones_df %>%
  select(journal, investigador) %>%
  filter(!is.na(journal), journal != "")

# 6. Crear nodos

# a) Nodos de investigadores con área
nodos_invest <- investigadores %>%
  select(name = Nombre) %>%
  left_join(df_areas, by = c("name" = "nombre")) %>%
  mutate(
    area_principal = ifelse(is.na(area_principal), "Sin área", area_principal),
    category = area_principal
  )

# b) Nodos de revistas
nodos_revistas <- revistas %>%
  filter(!is.na(Revista), Revista != "") %>%
  filter(Revista %in% relaciones$journal) %>%
  select(name = Revista) %>%
  mutate(category = "Revista")

# Unimos nodos
nodos_invest <- nodos_invest %>%
  mutate(area_categoria = area_principal)

nodos_revistas <- nodos_revistas %>%
  mutate(area_categoria = "revista")

nodos <- bind_rows(nodos_invest, nodos_revistas) %>%
  mutate(value = 10, size = 20)

# 7. Aristas
edges <- relaciones %>%
  rename(source = investigador, target = journal)

# 8. Vector de colores fijos por área
area_colores <- c(
  "Analisis" = "#E41A1C",
  "Algebra" = "#377EB8",
  "Topologia" = "#4DAF4A",
  "Fisica" = "#984EA3",
  "Matematica aplicada" = "#FF7F00",
  "Ciencia Datos" = "#A65628",
  "Becarios" = "#F781BF",
  "Gestora" = "#999999",
  "Sin área" = "#CCCCCC"
)


# Asegurar orden de categorías
nodos$category <- factor(nodos$category, levels = names(area_colores))

# 9. Nombres ordenados
nombres_ordenados <- investigadores %>%
  arrange(Acortacion) %>%
  pull(Nombre)

# UI
ui <- fluidPage(
  titlePanel("Investigadores y Revistas"),
  fluidRow(
    column(
      width = 8,
      selectInput("investigador_filtro", "Selecciona un investigador:",
                  choices = c("-" = "-", nombres_ordenados),
                  selected = "-"),
      echarts4rOutput("grafo", height = "800px")
    ),
    column(
      width = 4,
      h4("Información del nodo seleccionado"),
      verbatimTextOutput("info_nodo")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  output$grafo <- renderEcharts4r({
    if (input$investigador_filtro == "-") {
      nodos_mostrar <- nodos
      edges_mostrar <- edges
    } else {
      edges_mostrar <- edges %>%
        filter(source == input$investigador_filtro)
      
      revistas_conectadas <- unique(edges_mostrar$target)
      
      nodos_mostrar <- nodos %>%
        filter(name == input$investigador_filtro | name %in% revistas_conectadas)
    }
    
    categorias_lista <- lapply(names(area_colores), function(cat) list(name = cat))
    
    nodos_mostrar %>%
      e_charts(name) %>%
      e_graph(
        layout = "force",
        categories = categorias_lista,
        roam = TRUE,
        legend = TRUE
      ) %>%
      e_graph_nodes(nodos_mostrar, name, value, size, category = area_categoria)  %>%
      e_graph_edges(edges_mostrar, source, target) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return '<strong>' + params.name + '</strong>';
        }
      ")) %>%
      e_color(unname(area_colores)) %>%
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
    
    tipo_nodo <- nodos %>%
      filter(name == node_to_show) %>%
      pull(category)
    
    if (length(tipo_nodo) == 0) {
      return("Nodo no reconocido.")
    }
    
    if (tipo_nodo != "Revista") {
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

# Lanzar la app
shinyApp(ui, server)
