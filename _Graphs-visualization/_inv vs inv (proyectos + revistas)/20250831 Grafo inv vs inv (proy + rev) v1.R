################################################################################
# 1. Librerías y carga de datos
################################################################################
library(shiny)
library(echarts4r)

setwd("C:/Users/josep/Downloads/Zona de trabajo/_inv vs inv (proyectos + revistas)")

# --- Revistas ---
n_invest_rev <- read.csv2("Investigadores_internos.csv", sep = ";", fileEncoding = "latin1")
pub_con <- read.csv("ha_publicado_con.csv", fileEncoding = "latin1")
areas_rev <- read.csv("es_parte_de_limpio_listas.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")

# --- Proyectos ---
n_invest_proy <- n_invest_rev
parti_con <- read.csv2("ha_participado_con.csv", fileEncoding = "latin1")
areas_proy <- areas_rev

################################################################################
# 2. Limpieza y separación de áreas
################################################################################
split_name_and_areas <- function(nombre, areas) {
  if (is.na(areas) && grepl(",", nombre)) {
    parts <- unlist(strsplit(nombre, ",", fixed = TRUE))
    nombre <- trimws(parts[1])
    areas <- trimws(parts[2])
  }
  list(Nombre = nombre, Areas = areas)
}

# --- Revistas ---
cleaned_rev <- mapply(split_name_and_areas, areas_rev$Nombre, areas_rev$Areas, SIMPLIFY = FALSE)
areas_rev <- do.call(rbind, lapply(cleaned_rev, as.data.frame))
areas_rev$Areas <- gsub('"', '', areas_rev$Areas)
areas_rev$Lista_areas <- strsplit(gsub("/", ",", areas_rev$Areas), ",\\s*")

# --- Proyectos ---
cleaned_proy <- mapply(split_name_and_areas, areas_proy$Nombre, areas_proy$Areas, SIMPLIFY = FALSE)
areas_proy <- do.call(rbind, lapply(cleaned_proy, as.data.frame))
areas_proy$Areas <- gsub('"', '', areas_proy$Areas)
areas_proy$Lista_areas <- strsplit(gsub("/", ",", areas_proy$Areas), ",\\s*")

################################################################################
# 3. Construcción de grafos
################################################################################

# ==============================
# --- GRAFO REVISTAS ---
# ==============================
dic_acron_rev <- setNames(lapply(strsplit(n_invest_rev$Acronimo, ",\\s*"), trimws), n_invest_rev$Nombre)
dic_otros_inv_rev <- setNames(lapply(strsplit(pub_con$otros_inv, ",\\s*"), trimws), pub_con$nombre)

acronimo_a_nombre_rev <- list()
for (nombre in names(dic_acron_rev)) {
  for (acronimo in dic_acron_rev[[nombre]]) {
    acronimo_a_nombre_rev[[acronimo]] <- nombre
  }
}

solo_inv_rev <- list()
for (investigador in names(dic_acron_rev)) {
  if (investigador %in% names(dic_otros_inv_rev)) {
    for (otr in dic_otros_inv_rev[[investigador]]) {
      if (otr %in% names(acronimo_a_nombre_rev)) {
        nombre_completo <- acronimo_a_nombre_rev[[otr]]
        solo_inv_rev[[investigador]] <- unique(c(solo_inv_rev[[investigador]], nombre_completo))
      }
    }
  }
}

edges_rev <- do.call(rbind, lapply(names(solo_inv_rev), function(source) {
  targets <- solo_inv_rev[[source]]
  if (!is.null(targets) && length(targets) > 0) {
    df <- data.frame(source = rep(source, length(targets)), target = targets, stringsAsFactors = FALSE)
    df_reverse <- data.frame(source = df$target, target = df$source, stringsAsFactors = FALSE)
    rbind(df, df_reverse)
  } else NULL
}))
edges_rev <- unique(edges_rev)
edges_rev <- edges_rev[edges_rev$source != edges_rev$target, ]

grado_rev <- as.data.frame(table(edges_rev$source))
colnames(grado_rev) <- c("name", "connections")

nodes_rev <- merge(data.frame(name = n_invest_rev$Nombre, stringsAsFactors = FALSE),
                   grado_rev, by = "name", all.x = TRUE)
nodes_rev$connections[is.na(nodes_rev$connections)] <- 0
nodes_rev$size <- 10 + nodes_rev$connections * 2
nodes_rev$value <- nodes_rev$connections
nodes_rev <- merge(nodes_rev, areas_rev[, c("Nombre", "Lista_areas")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_rev <- merge(nodes_rev, n_invest_rev[, c("Nombre", "Tipo.de.empleado")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_rev$category <- nodes_rev$Tipo.de.empleado


# ==============================
# --- GRAFO PROYECTOS ---
# ==============================
dic_acron_proy <- setNames(n_invest_proy$Girado, n_invest_proy$Nombre)
dic_otros_inv_proy <- setNames(lapply(strsplit(parti_con$otros_inv, ";\\s*"), trimws), parti_con$nombre)

acronimo_a_nombre_proy <- list()
for (nombre in names(dic_acron_proy)) {
  for (acronimo in dic_acron_proy[[nombre]]) {
    acronimo_a_nombre_proy[[acronimo]] <- nombre
  }
}

solo_inv_proy <- list()
for (investigador in names(dic_acron_proy)) {
  if (investigador %in% names(dic_otros_inv_proy)) {
    for (otr in dic_otros_inv_proy[[investigador]]) {
      if (otr %in% names(acronimo_a_nombre_proy)) {
        nombre_completo <- acronimo_a_nombre_proy[[otr]]
        solo_inv_proy[[investigador]] <- unique(c(solo_inv_proy[[investigador]], nombre_completo))
      }
    }
  }
}

edges_proy <- do.call(rbind, lapply(names(solo_inv_proy), function(source) {
  targets <- solo_inv_proy[[source]]
  if (!is.null(targets) && length(targets) > 0) {
    df <- data.frame(source = rep(source, length(targets)), target = targets, stringsAsFactors = FALSE)
    df_reverse <- data.frame(source = df$target, target = df$source, stringsAsFactors = FALSE)
    rbind(df, df_reverse)
  } else NULL
}))
edges_proy <- unique(edges_proy)
edges_proy <- edges_proy[edges_proy$source != edges_proy$target, ]

grado_proy <- as.data.frame(table(edges_proy$source))
colnames(grado_proy) <- c("name", "connections")

nodes_proy <- merge(data.frame(name = n_invest_proy$Nombre, stringsAsFactors = FALSE),
                    grado_proy, by = "name", all.x = TRUE)
nodes_proy$connections[is.na(nodes_proy$connections)] <- 0
nodes_proy$size <- 10 + nodes_proy$connections * 2
nodes_proy$value <- nodes_proy$connections
nodes_proy <- merge(nodes_proy, areas_proy[, c("Nombre", "Lista_areas")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_proy <- merge(nodes_proy, n_invest_proy[, c("Nombre", "Tipo.de.empleado")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_proy$category <- nodes_proy$Tipo.de.empleado

################################################################################
# 4. Interfaz Shiny (ui)
################################################################################
ui <- fluidPage(
  titlePanel("Grafo investigadores IUMPA"),
  tabsetPanel(
    tabPanel("Revistas",
             fluidRow(
               column(
                 width = 4,
                 selectInput("selected_node_rev", "Selecciona un nodo para filtrar (revistas):",
                             choices = c("-" = "-", sort(nodes_rev$name)), selected = "-"),
                 selectInput("selected_area_rev", "Selecciona un área para filtrar (revistas):",
                             choices = c("-" = "-", sort(unique(unlist(areas_rev$Lista_areas)))), selected = "-")
               ),
               column(width = 8, echarts4rOutput("graph_rev", height = "800px")),
               column(width = 4,
                      h4("Información del nodo seleccionado o clicado"),
                      verbatimTextOutput("node_info_rev"))
             )
    ),
    tabPanel("Proyectos",
             fluidRow(
               column(
                 width = 4,
                 selectInput("selected_node_proy", "Selecciona un nodo para filtrar (proyectos):",
                             choices = c("-" = "-", sort(nodes_proy$name)), selected = "-"),
                 selectInput("selected_area_proy", "Selecciona un área para filtrar (proyectos):",
                             choices = c("-" = "-", sort(unique(unlist(areas_proy$Lista_areas)))), selected = "-")
               ),
               column(width = 8, echarts4rOutput("graph_proy", height = "800px")),
               column(width = 4,
                      h4("Información del nodo seleccionado o clicado"),
                      verbatimTextOutput("node_info_proy"))
             )
    )
  )
)

################################################################################
# 5. Servidor Shiny (server)
################################################################################
server <- function(input, output, session) {
  
  # ----------------- GRAFO REVISTAS -----------------
  output$graph_rev <- renderEcharts4r({
    selected_nodes <- nodes_rev
    
    # Filtrar por área
    if (input$selected_area_rev != "-") {
      selected_nodes <- selected_nodes[sapply(selected_nodes$Lista_areas, function(x) input$selected_area_rev %in% x), ]
    }
    
    # Filtrar por nodo
    if (input$selected_node_rev != "-") {
      selected_nodes <- selected_nodes[selected_nodes$name == input$selected_node_rev |
                                         selected_nodes$name %in% edges_rev$target[edges_rev$source == input$selected_node_rev], ]
    }
    
    selected_edges <- edges_rev[edges_rev$source %in% selected_nodes$name &
                                  edges_rev$target %in% selected_nodes$name, ]
    
    selected_nodes |>
      e_charts(name) |>
      e_graph(layout = "circular") |>
      e_graph_nodes(selected_nodes, name, value = connections, size, category = category) |>
      e_graph_edges(selected_edges, source, target) |>
      e_tooltip(formatter = htmlwidgets::JS("
        function(params){
          return('<strong>'+params.name+'<br/></strong>'+
                 'Número colaboradores totales: ' + params.value)
        }
      ")) |>
      e_on(query = list(dataType = "node"),
           handler = htmlwidgets::JS("function(params) {
              Shiny.setInputValue('clicked_node_rev', params.data.name, {priority: 'event'});
            }"))
  })
  
  output$node_info_rev <- renderText({
    node <- if (!is.null(input$clicked_node_rev)) input$clicked_node_rev else if (input$selected_node_rev != "-") input$selected_node_rev else NULL
    if (is.null(node)) return("Seleccione o clique un nodo para ver su información.")
    conexiones <- edges_rev[edges_rev$source == node, "target"]
    if (length(conexiones) == 0) return(paste0("Nodo: ", node, "\n\nNo ha trabajado con nadie del IUMPA"))
    paste0("Investigador: ", node, "\n\nHa trabajado con:\n", paste0("- ", conexiones, collapse = "\n"))
  })
  
  
  # ----------------- GRAFO PROYECTOS -----------------
  output$graph_proy <- renderEcharts4r({
    selected_nodes <- nodes_proy
    
    # Filtrar por área
    if (input$selected_area_proy != "-") {
      selected_nodes <- selected_nodes[sapply(selected_nodes$Lista_areas, function(x) input$selected_area_proy %in% x), ]
    }
    
    # Filtrar por nodo
    if (input$selected_node_proy != "-") {
      selected_nodes <- selected_nodes[selected_nodes$name == input$selected_node_proy |
                                         selected_nodes$name %in% edges_proy$target[edges_proy$source == input$selected_node_proy], ]
    }
    
    selected_edges <- edges_proy[edges_proy$source %in% selected_nodes$name &
                                   edges_proy$target %in% selected_nodes$name, ]
    
    selected_nodes |>
      e_charts(name) |>
      e_graph(layout = "circular") |>
      e_graph_nodes(selected_nodes, name, value = connections, size, category = category) |>
      e_graph_edges(selected_edges, source, target) |>
      e_tooltip(formatter = htmlwidgets::JS("
        function(params){
          return('<strong>'+params.name+'<br/></strong>'+
                 'Número colaboradores totales: ' + params.value)
        }
      ")) |>
      e_on(query = list(dataType = "node"),
           handler = htmlwidgets::JS("function(params) {
              Shiny.setInputValue('clicked_node_proy', params.data.name, {priority: 'event'});
            }"))
  })
  
  output$node_info_proy <- renderText({
    node <- if (!is.null(input$clicked_node_proy)) input$clicked_node_proy else if (input$selected_node_proy != "-") input$selected_node_proy else NULL
    if (is.null(node)) return("Seleccione o clique un nodo para ver su información.")
    conexiones <- edges_proy[edges_proy$source == node, "target"]
    if (length(conexiones) == 0) return(paste0("Nodo: ", node, "\n\nNo ha trabajado con nadie del IUMPA"))
    paste0("Investigador: ", node, "\n\nHa trabajado con:\n", paste0("- ", conexiones, collapse = "\n"))
  })
}

################################################################################
# 6. Lanzar la aplicación
################################################################################
shinyApp(ui, server)

