library(shiny)
library(echarts4r)
library(dplyr)
library(readr)

setwd("C:/Users/josep/Downloads/ZonaT")

# 1. Cargar los datos principales
revistas <- read_csv("Revistas.csv", locale = locale(encoding = "Latin1"))
investigadores <- read_csv2("Investigadores_internos.csv", locale = locale(encoding = "Latin1"))

# 2. Leer los CSV individuales (solo los que tienen valor en 'Acortacion')
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

# 3. Unificar en un solo dataframe
publicaciones_df <- bind_rows(publicaciones_list)

# 4. Filtrar columnas necesarias
relaciones <- publicaciones_df %>%
  select(journal, investigador) %>%
  filter(!is.na(journal), journal != "")

# 5. Preparar nodos y aristas
nodos_invest <- investigadores %>%
  select(name = Nombre) %>%
  mutate(category = "investigador")

nodos_revistas <- revistas %>%
  filter(!is.na(Revista), Revista != "") %>%
  filter(Revista %in% relaciones$journal) %>%
  select(name = Revista) %>%
  mutate(category = "revista")

nodos <- bind_rows(nodos_invest, nodos_revistas) %>%
  mutate(value = 10, size = 20)

edges <- relaciones %>%
  rename(source = investigador, target = journal)

nombres_ordenados <- investigadores %>%
  mutate(apellidos = sub("^(\\S+)\\s+(.*)$", "\\2 \\1", Nombre)) %>%
  arrange(apellidos) %>%
  pull(Nombre)

nombres_ordenados <- investigadores$Nombre

# 6. Shiny App
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
    
    nodos_mostrar %>%
      e_charts(name) %>%
      e_graph(
        layout = "force",
      ) %>%
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
    
    # Determinar tipo de nodo (investigador o revista)
    tipo_nodo <- nodos %>%
      filter(name == node_to_show) %>%
      pull(category)
    
    if (length(tipo_nodo) == 0) {
      return("Nodo no reconocido.")
    }
    
    if (tipo_nodo == "investigador") {
      articulos <- publicaciones_df %>%
        filter(investigador == node_to_show) %>%
        pull(title)
      
      if (length(articulos) == 0) {
        return(paste0("Investigador/a: ", node_to_show, "\n\nNo se han encontrado publicaciones registradas."))
      }
      
      return(paste0("Investigador/a: ", node_to_show, "\n\nArtículos publicados:\n", paste0("- ", articulos, collapse = "\n")))
      
    } else if (tipo_nodo == "revista") {
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

shinyApp(ui, server)
