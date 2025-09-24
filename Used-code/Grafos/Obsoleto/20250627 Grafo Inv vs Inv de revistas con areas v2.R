################################################################################
# Librerías y carga de datos
################################################################################
library(shiny)
library(echarts4r)

setwd("C:/Users/josep/Downloads/ZonaT")

n_invest <- read.csv2("Investigadores_internos.csv", sep = ";", fileEncoding = "UTF-8")
pub_con <- read.csv("ha_publicado_con.csv")
areas_df <- read.csv("es_parte_de_limpio_listas.csv", stringsAsFactors = FALSE)

################################################################################
# Limpieza y separación de áreas
################################################################################
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

################################################################################
# Construcción del grafo
################################################################################
dic_acron <- setNames(lapply(strsplit(n_invest$Acronimo, ",\\s*"), trimws), n_invest$Nombre)
dic_otros_inv <- setNames(lapply(strsplit(pub_con$otros_inv, ",\\s*"), trimws), pub_con$nombre)

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

edges_click <- do.call(rbind, lapply(names(solo_inv_iumpa), function(source) {
  targets <- solo_inv_iumpa[[source]]
  if (!is.null(targets) && length(targets) > 0) {
    df <- data.frame(source = rep(source, length(targets)), target = targets, stringsAsFactors = FALSE)
    df_reverse <- data.frame(source = df$target, target = df$source, stringsAsFactors = FALSE)
    rbind(df, df_reverse)
  } else {
    NULL
  }
}))
edges_click <- unique(edges_click)
edges_click <- edges_click[edges_click$source != edges_click$target, ]

all_edges <- data.frame(name = edges_click$source)
grado_nodos <- as.data.frame(table(all_edges$name))
colnames(grado_nodos) <- c("name", "connections")

nombres_investigadores <- n_invest$Nombre

nodes_click <- merge(
  data.frame(name = nombres_investigadores, stringsAsFactors = FALSE),
  grado_nodos,
  by = "name",
  all.x = TRUE
)
nodes_click$connections[is.na(nodes_click$connections)] <- 0
nodes_click$size <- 10 + nodes_click$connections * 2
nodes_click$value <- nodes_click$connections

# Añadir áreas
nodes_click <- merge(nodes_click, areas_df[, c("Nombre", "Lista_areas")],
                     by.x = "name", by.y = "Nombre", all.x = TRUE)

################################################################################
# Interfaz Shiny
################################################################################
ui <- fluidPage(
  titlePanel("Grafo investigadores IUMPA"),
  fluidRow(
    column(
      width = 4,
      selectInput("selected_node", "Selecciona un nodo para filtrar:",
                  choices = c("-" = "-", sort(nombres_investigadores)),
                  selected = "-"),
      selectInput("selected_area", "Selecciona un área para filtrar:",
                  choices = c("-" = "-", sort(unique(unlist(areas_df$Lista_areas)))),
                  selected = "-")
    ),
    column(width = 8, echarts4rOutput("graph", height = "800px")),
    column(width = 4,
           h4("Información del nodo seleccionado o clicado"),
           verbatimTextOutput("node_info"))
  )
)

################################################################################
# Servidor Shiny
################################################################################
server <- function(input, output, session) {
  output$graph <- renderEcharts4r({
    
    filtrar_por_area <- function(nodo) {
      if (is.null(nodo$Lista_areas)) return(FALSE)
      input$selected_area %in% nodo$Lista_areas[[1]]
    }
    
    if (input$selected_node == "-" && input$selected_area == "-") {
      selected_nodes <- nodes_click
    } else if (input$selected_area != "-" && input$selected_node == "-") {
      selected_nodes <- nodes_click[sapply(split(nodes_click, 1:nrow(nodes_click)), filtrar_por_area), ]
    } else if (input$selected_node != "-" && input$selected_area == "-") {
      selected_nodes <- nodes_click[nodes_click$name == input$selected_node |
                                      nodes_click$name %in% edges_click$target[edges_click$source == input$selected_node], ]
    } else {
      subset_nodes <- nodes_click[nodes_click$name == input$selected_node |
                                    nodes_click$name %in% edges_click$target[edges_click$source == input$selected_node], ]
      selected_nodes <- subset_nodes[sapply(split(subset_nodes, 1:nrow(subset_nodes)), filtrar_por_area), ]
    }
    
    selected_edges <- edges_click[edges_click$source %in% selected_nodes$name &
                                    edges_click$target %in% selected_nodes$name, ]
    
    selected_nodes |>
      e_charts(name) |>
      e_graph(layout = "circular") |>
      e_graph_nodes(selected_nodes, name, value = connections, size) |>
      e_graph_edges(selected_edges, source, target) |>
      e_tooltip(formatter = htmlwidgets::JS("
        function(params){
          return('<strong>'+params.name+'<br/></strong>'+
                 'Número colaboradores totales: ' + params.value)
        }
      ")) |>
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
    paste0("Investigador: ", node_to_show, "\n\nHa trabajado con:\n",
           paste0("- ", conexiones_sin_mismo, collapse = "\n"))
  })
}

################################################################################
# Lanzar app
################################################################################
shinyApp(ui, server)
