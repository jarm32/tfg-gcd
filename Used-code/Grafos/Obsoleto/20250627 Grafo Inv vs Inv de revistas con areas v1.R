################################################################################
# Prueba con datos
################################################################################
# 1º Leer los datos

setwd("C:/Users/josep/Downloads/ZonaT")
n_invest <- read.csv2("Investigadores_internos.csv", sep = ";", fileEncoding = "UTF-8")
pub_con <- read.csv("ha_publicado_con.csv")

lines <- readLines("es_parte_de_limpio.csv", encoding = "UTF-8")
lines <- lines[-1]  # quitar encabezado
data <- strsplit(lines, ',"')
data <- lapply(data, function(x) c(gsub('"', '', x[1]), gsub('"', '', x[2])))
df_areas <- as.data.frame(do.call(rbind, data), stringsAsFactors = FALSE)
colnames(df_areas) <- c("nombre", "areas")
df_areas$area_principal <- sapply(strsplit(df_areas$areas, ",|/"), function(x) trimws(x[1]))

# 2º Formateo para el grafo

dic_acron <- setNames(
  lapply(strsplit(n_invest$Acronimo, ",\\s*"), trimws),
  n_invest$Nombre
)

dic_otros_inv <- setNames(
  lapply(strsplit(pub_con$otros_inv, ",\\s*"), trimws), 
  pub_con$nombre
)

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
    df <- data.frame(
      source = rep(source, length(targets)),
      target = targets,
      stringsAsFactors = FALSE
    )
    df_reverse <- data.frame(
      source = df$target,
      target = df$source,
      stringsAsFactors = FALSE
    )
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

nodes_click <- merge(nodes_click, df_areas[, c("nombre", "area_principal")],
                     by.x = "name", by.y = "nombre", all.x = TRUE)


area_colores <- c(
  "Algebra" = "#E41A1C",             
  "Analisis" = "#377EB8",            
  "Topologia" = "#4DAF4A",           
  "Fisica" = "#984EA3",              
  "Matematica aplicada" = "#FF7F00", 
  "Ciencia Datos" = "#A65628",       
  "Becarios" = "#F781BF",            
  "Gestora" = "#999999",             
  "Sin área" = "#CCCCCC"             
)
nodes_click$category <- nodes_click$area_principal
nodes_click$category[is.na(nodes_click$category)] <- "Sin área"
area_colores <- c(area_colores, "Sin área" = "#999999")

################################################################################
# UI
################################################################################

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

################################################################################
# SERVER
################################################################################

server <- function(input, output, session) {
  
  output$graph <- renderEcharts4r({
    # Si no hay nodo seleccionado, usar todos
    if (input$selected_node == "-") {
      filtered_nodes <- nodes_click
    } else {
      vecinos <- edges_click$target[edges_click$source == input$selected_node]
      filtered_nodes <- nodes_click[nodes_click$name == input$selected_node |
                                      nodes_click$name %in% vecinos, ]
    }
    
    # Solo usamos edges que conectan nodos visibles
    filtered_edges <- edges_click[edges_click$source %in% filtered_nodes$name &
                                    edges_click$target %in% filtered_nodes$name, ]
    
    # Creamos leyenda completa con todas las áreas definidas desde el principio
    categorias_lista <- lapply(names(area_colores), function(cat) list(name = cat))
    
    filtered_nodes |>
      e_charts(name) |>
      e_graph(
        layout = "circular",  # o "force"
        categories = categorias_lista,
        roam = TRUE,
        legend = TRUE
      ) |>
      e_graph_nodes(filtered_nodes, name, value = connections, size, category = category) |>
      e_graph_edges(filtered_edges, source, target) |>
      e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        return('<strong>'+params.name+'<br/></strong>'+
        'Número colaboradores: ' + params.value)}
    ")) |>
      e_color(unname(area_colores)) |>
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
    
    area <- nodes_click$area_principal[nodes_click$name == node_to_show]
    
    if (length(conexiones_sin_mismo) == 0) {
      return(paste0("Investigador: ", node_to_show, "\nÁrea principal: ", area, "\n\nNo ha trabajado con nadie del IUMPA"))
    }
    
    paste0(
      "Investigador: ", node_to_show, "\nÁrea principal: ", area, "\n\n",
      "Ha trabajado con:\n",
      paste0("- ", conexiones_sin_mismo, collapse = "\n")
    )
  })
}

################################################################################
# RUN APP
################################################################################

shinyApp(ui, server)
