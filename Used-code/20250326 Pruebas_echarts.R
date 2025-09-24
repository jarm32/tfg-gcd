library(echarts4r)


################################################################################
#Grafo básico
################################################################################

nodes_base <- data.frame(
  name = c("A", "B", "C", "D"),
  category = c("Grupo 1", "Grupo 1", "Grupo 2", "Grupo 2"),
  size = c(10,15,20,4),
  stringsAsFactors = FALSE
)

edges_base <- data.frame(
  source = c("A", "A", "B", "C"),
  target = c("D", "C", "D", "D"),
  stringsAsFactors = FALSE
)

e_charts() |> 
  e_graph() |> 
  e_graph_nodes(nodes_base, name, category, size) |> 
  e_graph_edges(edges_base, source, target) |> 
  e_tooltip()

################################################################################
#Grafo coloreado + añadir símbolos
################################################################################

value <- c(rnorm(8, 10, 2))

nodes_2 <- data.frame(
  name = c("A", "B", "C", "D", "E","F","G","H"),
  size = c(10,15,20,4,10,12,12,12),
  value = value,
  grp = c("Grupo 1", "Grupo 1", "Grupo 2", "Grupo 2","Grupo 1","Grupo 1","Grupo 1","Grupo 2"),
  symbol = c(rep("circle",2),rep("triangle",2),rep("circle",3), "triangle"),
  stringsAsFactors = FALSE
)

edges_2 <- data.frame(
  source = c("A", "A", "B", "C","F","E","H","H"),
  target = c("D", "C", "D", "D","G","A","E","F"),
  stringsAsFactors = FALSE
)

e_charts() |> 
  e_graph() |> 
  e_graph_nodes(nodes_2, name, size, value, grp, symbol) |>
  e_graph_edges(edges_2, source, target) |>
  e_tooltip()

#Grafo GL funciona MAL

################################################################################
#Interacción al clicar un nodo
################################################################################

library(echarts4r)

value <- rnorm(8, 10, 2)

nodes_click <- data.frame(
  name = c("A", "B", "C", "D", "E", "F", "G", "H"),
  size = c(10, 15, 20, 4, 10, 12, 12, 12),
  value = value,
  grp = c("Grupo 1", "Grupo 1", "Grupo 2", "Grupo 2", "Grupo 1", "Grupo 1", "Grupo 1", "Grupo 2"),
  symbol = c(rep("circle", 2), rep("triangle", 2), rep("circle", 3), "triangle"),
  url = c("https://www.upv.es", "https://www.google.es", "https://www.r-project.org/", "https://eu.bic.com/es-es", 
          "https://es.wikipedia.org/wiki/Vo", "https://ads.google.com/intl/es_es/home/", 
          "https://es.wikipedia.org/wiki/Doblaje", "https://es.wikipedia.org/wiki/Videojuego"),
  stringsAsFactors = FALSE
)

edges_click <- data.frame(
  source = c("A", "A", "B", "C", "F", "E", "H", "H"),
  target = c("D", "C", "D", "D", "G", "A", "E", "F"),
  stringsAsFactors = FALSE
)

e <- e_charts() |> 
  e_graph() |> 
  e_graph_nodes(nodes_click, name, size, value, grp, symbol) |> 
  e_graph_edges(edges_click, source, target) |> 
  e_tooltip()

e |> e_on(
  query = list(dataType = "node"),
  handler = htmlwidgets::JS(
    "function(params) {
      var urlMap = {
        'A': 'https://www.upv.es',
        'B': 'https://www.google.es',
        'C': 'https://www.r-project.org/',
        'D': 'https://eu.bic.com/es-es',
        'E': 'https://es.wikipedia.org/wiki/Vo',
        'F': 'https://ads.google.com/intl/es_es/home/',
        'G': 'https://es.wikipedia.org/wiki/Doblaje',
        'H': 'https://es.wikipedia.org/wiki/Videojuego'
      };
      
      var url = urlMap[params.data.name];
      if (url) {
        window.open(url, '_blank');
      }
    }"
  )
)

################################################################################
#Elegir nodos
################################################################################


library(echarts4r)
library(shiny)

value <- rnorm(8, 10, 2)

nodes_click <- data.frame(
  name = c("A", "B", "C", "D", "E", "F", "G", "H"),
  size = c(10, 15, 20, 4, 10, 12, 12, 12),
  value = value,
  grp = c("Grupo 1", "Grupo 1", "Grupo 2", "Grupo 2", "Grupo 1", "Grupo 1", "Grupo 1", "Grupo 2"),
  symbol = c(rep("circle", 2), rep("triangle", 2), rep("circle", 3), "triangle"),
  url = c("https://www.upv.es", "https://www.google.es", "https://www.r-project.org/", "https://eu.bic.com/es-es", 
          "https://es.wikipedia.org/wiki/Vo", "https://ads.google.com/intl/es_es/home/", 
          "https://es.wikipedia.org/wiki/Doblaje", "https://es.wikipedia.org/wiki/Videojuego"),
  stringsAsFactors = FALSE
)

edges_click <- data.frame(
  source = c("A", "A", "B", "C", "F", "E", "H", "H"),
  target = c("D", "C", "D", "D", "G", "A", "E", "F"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  selectInput("selected_node", "Selecciona un nodo:", choices = c("-", nodes_click$name)),
  echarts4rOutput("graph")
)

server <- function(input, output, session) {
  output$graph <- renderEcharts4r({
    if (input$selected_node == "-") {
      selected_nodes <- nodes_click
      selected_edges <- edges_click
    } else {
      selected_nodes <- nodes_click[nodes_click$name == input$selected_node, ]
      selected_edges <- edges_click[edges_click$source %in% selected_nodes$name | edges_click$target %in% selected_nodes$name, ]
    }
    
    e_charts() |> 
      e_graph() |> 
      e_graph_nodes(selected_nodes, name, size, value, grp, symbol) |> 
      e_graph_edges(selected_edges, source, target) |> 
      e_tooltip() |> 
      e_on(
        query = list(dataType = "node"),
        handler = htmlwidgets::JS(
          "function(params) {
            var urlMap = {
              'A': 'https://www.upv.es',
              'B': 'https://www.google.es',
              'C': 'https://www.r-project.org/',
              'D': 'https://eu.bic.com/es-es',
              'E': 'https://es.wikipedia.org/wiki/Vo',
              'F': 'https://ads.google.com/intl/es_es/home/',
              'G': 'https://es.wikipedia.org/wiki/Doblaje',
              'H': 'https://es.wikipedia.org/wiki/Videojuego'
            };
            
            var url = urlMap[params.data.name];
            if (url) {
              window.open(url, '_blank');
            }
          }"
        )
      )
  })
}

shinyApp(ui, server)

################################################################################
#Diferentes páginas
################################################################################

library(echarts4r)
library(shiny)

value <- rnorm(8, 10, 2)

nodes_click <- data.frame(
  name = c("A", "B", "C", "D", "E", "F", "G", "H"),
  size = c(10, 15, 20, 4, 10, 12, 12, 12),
  value = value,
  grp = c("Grupo 1", "Grupo 1", "Grupo 2", "Grupo 2", "Grupo 1", "Grupo 1", "Grupo 1", "Grupo 2"),
  symbol = c(rep("circle", 2), rep("triangle", 2), rep("circle", 3), "triangle"),
  url = c("https://www.upv.es", "https://www.google.es", "https://www.r-project.org/", "https://eu.bic.com/es-es", 
          "https://es.wikipedia.org/wiki/Vo", "https://ads.google.com/intl/es_es/home/", 
          "https://es.wikipedia.org/wiki/Doblaje", "https://es.wikipedia.org/wiki/Videojuego"),
  stringsAsFactors = FALSE
)

edges_click <- data.frame(
  source = c("A", "A", "B", "C", "F", "E", "H", "H"),
  target = c("D", "C", "D", "D", "G", "A", "E", "F"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  tabsetPanel(id = "tabs",
              tabPanel("Grafo",
                       selectInput("selected_node", "Selecciona un nodo:", choices = c("-", nodes_click$name)),
                       echarts4rOutput("graph")
              ),
              tabPanel("Detalle nodo",
                       h3("Información del nodo"),
                       verbatimTextOutput("node_info"),
                       actionButton("go_back", "Volver al grafo")
              )
  )
)

server <- function(input, output, session) {
  
  # Reactive value para guardar el nodo clicado
  clicked_node <- reactiveVal(NULL)
  
  observeEvent(input$go_back, {
    updateTabsetPanel(session, "tabs", selected = "Grafo")
  })
  
  output$graph <- renderEcharts4r({
    if (input$selected_node == "-") {
      selected_nodes <- nodes_click
      selected_edges <- edges_click
    } else {
      selected_nodes <- nodes_click[nodes_click$name == input$selected_node, ]
      selected_edges <- edges_click[edges_click$source %in% selected_nodes$name | edges_click$target %in% selected_nodes$name, ]
    }
    
    e_charts() |> 
      e_graph() |> 
      e_graph_nodes(selected_nodes, name, size, value, grp, symbol) |> 
      e_graph_edges(selected_edges, source, target) |> 
      e_tooltip() |> 
      e_on(
        query = list(dataType = "node"),
        handler = htmlwidgets::JS(
          "function(params) {
            Shiny.setInputValue('node_click', params.data.name, {priority: 'event'});
          }"
        )
      )
  })
  
  observeEvent(input$node_click, {
    node_name <- input$node_click
    node_data <- nodes_click[nodes_click$name == node_name, ]
    
    if (nrow(node_data) > 0) {
      clicked_node(paste0("Nombre del nodo: ", node_data$name, "\nURL asociada: ", node_data$url))
      updateTabsetPanel(session, "tabs", selected = "Detalle nodo")
    }
  })
  
  output$node_info <- renderText({
    clicked_node()
  })
}

shinyApp(ui, server)

################################################################################
#Otras librerías
################################################################################

library(igraph)
library(visNetwork)

# Cargar datos desde los CSVs
nodos <- read.csv("C:/Users/josep/Downloads/nodos.csv", stringsAsFactors = FALSE)
relaciones <- read.csv("C:/Users/josep/Downloads/relaciones.csv", stringsAsFactors = FALSE)

# Crear objeto grafo
g <- graph_from_data_frame(relaciones, directed = FALSE, vertices = nodos)

# Convertir los datos para visNetwork
nodes <- data.frame(
  id = nodos$id,
  label = nodos$label,
  title = nodos$url,  # Muestra la URL al pasar el mouse
  url = nodos$url     # Guarda la URL para abrir en clic
)

edges <- data.frame(
  from = relaciones$from,
  to = relaciones$to
)

# Crear visualización interactiva
visNetwork(nodes, edges) %>%
  visNodes(shape = "dot", size = 15) %>%
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visEvents(
    click = "function(nodes) { 
              if (nodes.nodes.length > 0) { 
                var url = this.body.data.nodes.get(nodes.nodes[0]).url;
                if (url) { window.open(url, '_blank'); }
              } 
            }"
  )


