################################################################################
#Información por nodo
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
  titlePanel("Grafo interactivo con URLs"),
  fluidRow(
    column(
      width = 8,
      echarts4rOutput("graph", height = "600px")
    ),
    column(
      width = 4,
      h4("Información del nodo seleccionado"),
      verbatimTextOutput("node_url")
    )
  )
)

server <- function(input, output, session) {
  
  output$graph <- renderEcharts4r({
    e_charts() |> 
      e_graph() |> 
      e_graph_nodes(nodes_click, name, size, value, grp, symbol) |> 
      e_graph_edges(edges_click, source, target) |> 
      e_tooltip() |> 
      e_on(
        query = list(dataType = "node"),
        handler = htmlwidgets::JS(
          "function(params) {
            Shiny.setInputValue('clicked_node', params.data.name, {priority: 'event'});
          }"
        )
      )
  })
  
  output$node_url <- renderText({
    req(input$clicked_node)
    node_data <- nodes_click[nodes_click$name == input$clicked_node, ]
    paste0("Nodo: ", node_data$name, "\nURL: ", node_data$url)
  })
}

shinyApp(ui, server)

################################################################################
# Prueba con datos
################################################################################
# 1º Leer los datos

setwd("C:/Users/josep/Downloads")
n_invest <- read.csv2("Investigadores_internos.csv", sep=";")
pub_con <- read.csv("ha_publicado_con.csv")

# 2º Formatearlos para que cueste menos de leer a la hora de generar el grafo --> indicar con quién trabaja del IUMPA

dic_acron <- setNames(
  lapply(strsplit(n_invest$Acronimo, ",\\s*"), trimws),
  n_invest$Nombre
)

#print(dic_acron[["Carmen Alegre Gil"]])

dic_otros_inv <- setNames(
  lapply(strsplit(pub_con$otros_inv, ",\\s*"), trimws), 
  pub_con$nombre
)

#print(dic_otros_inv[["Carmen Alegre Gil"]])

acronimo_a_nombre <- list()

for (nombre in names(dic_acron)) {
  for (acronimo in dic_acron[[nombre]]) {
    acronimo_a_nombre[[acronimo]] <- nombre
  }
}


solo_inv_iumpa <- list()

for (investigador in names(dic_acron)) {
  if (investigador %in% names(dic_otros_inv)) {
    for (otr in dic_otros_inv[[investigador]]) {
      if (otr %in% names(acronimo_a_nombre)) {
        nombre_completo <- acronimo_a_nombre[[otr]]
        if (!is.null(solo_inv_iumpa[[investigador]])) {
          solo_inv_iumpa[[investigador]] <- unique(c(solo_inv_iumpa[[investigador]], nombre_completo))
        } else {
          solo_inv_iumpa[[investigador]] <- nombre_completo
        }
      }
    }
  }
}

# 3º Crear el grafo

library(echarts4r)
library(shiny)

edges_click <- do.call(rbind, lapply(names(solo_inv_iumpa), function(source) {
  targets <- solo_inv_iumpa[[source]]
  if (!is.null(targets) && length(targets) > 0) {
    data.frame(
      source = rep(source, length(targets)),
      target = targets,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}))

all_edges <- rbind(
  data.frame(name = edges_click$source)
  #,data.frame(name = edges_click$target)
)

grado_nodos <- as.data.frame(table(all_edges$name))
colnames(grado_nodos) <- c("name", "connections")

nombres_investigadores <- names(solo_inv_iumpa)

nodes_click <- merge(
  data.frame(name = nombres_investigadores, stringsAsFactors = FALSE),
  grado_nodos,
  by = "name",
  all.x = TRUE
)

nodes_click$connections[is.na(nodes_click$connections)] <- 0
nodes_click$size <- 10 + nodes_click$connections*2
nodes_click$value <- nodes_click$size

ui <- fluidPage(
  titlePanel("Grafo investigadores IUMPA"),
  fluidRow(
    column(
      width = 8,
      selectInput("selected_node", "Selecciona un nodo para filtrar:", 
                  choices = c("-" = "-", nombres_investigadores),
                  selected = "-"),
      echarts4rOutput("graph", height = "800px")
    ),
    column(
      width = 4,
      h4("Información del nodo seleccionado o clicado"),
      verbatimTextOutput("node_info")
    )
  )
)

server <- function(input, output, session) {
  
  output$graph <- renderEcharts4r({
    if (input$selected_node == "-") {
      selected_nodes <- nodes_click
      selected_edges <- edges_click
    } else {
      selected_nodes <- nodes_click[nodes_click$name == input$selected_node |
                                      nodes_click$name %in% edges_click$target[edges_click$source == input$selected_node], ]
      
      selected_edges <- edges_click[edges_click$source %in% selected_nodes$name |
                                      edges_click$target %in% selected_nodes$name, ]
    }
    
    selected_nodes |>
      e_charts(name) |>
      e_graph(
        layout = "circular"
      ) |>
      e_graph_nodes(selected_nodes, name, size, value) |>
      e_graph_edges(selected_edges, source, target) |>
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){return(
                                        '<strong>'+params.name+'<br/></strong>'+
                                        'Número colaboradores: ' + params.value)}"))|>
      e_on(
        query = list(dataType = "node"),
        handler = htmlwidgets::JS(
          "function(params) {
            Shiny.setInputValue('clicked_node', params.data.name, {priority: 'event'});
          }"
        )
      )
  })
  
  output$node_info <- renderText({
    node_to_show <- input$clicked_node
    
    if (is.null(node_to_show)) {
      if (input$selected_node == "-") {
        return("Seleccione o clique un nodo para ver su información.")
      } else {
        node_to_show <- input$selected_node
      }
    }
    
    conexiones <- edges_click[edges_click$source == node_to_show, "target"]
    
    conexiones_sin_mismo <- conexiones[conexiones != node_to_show]
    
    if (length(conexiones_sin_mismo) == 0) {
      return(paste0("Nodo: ", node_to_show, "\n\nNo ha trabajado con nadie del IUMPA"))
    }
    
    paste0(
      "Investigador: ", node_to_show, "\n\n",
      "Ha trabajado con:\n",
      paste0("- ", conexiones_sin_mismo, collapse = "\n")
    )
  })
}

shinyApp(ui, server)




