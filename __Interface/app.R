# 1. Cargar librerías necesarias
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(echarts4r)

# 2. Carga y procesamiento de datos (consolidado de los 3 scripts)
#setwd("C:/Code/tfg-gcd/__Interface")

# --- Formato de los nodos ---

normalizar_categoria <- function(x) {
  x <- trimws(as.character(x))
  x_simple <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  
  dplyr::case_when(
    is.na(x) | x == "" ~ "Desconocido",
    x_simple == "investigador" ~ "Investigador",
    x_simple == "becario" ~ "Becario",
    x_simple == "becario de investigacion" ~ "Becario de investigación",
    x_simple == "revista" ~ "Revista",
    x_simple == "proyecto" ~ "Proyecto",
    TRUE ~ x
  )
}

aplicar_colores_nodos <- "
function(el, x) {
  var chart = echarts.getInstanceByDom(el);
  if (!chart) return;

  var option = chart.getOption();
  if (!option.series || !option.series[0] || !option.series[0].data) return;

  var ordenCategorias = [
    'Investigador',
    'Becario',
    'Becario de investigación',
    'Revista',
    'Proyecto'
  ];

  function normalizarCategoria(cat) {
    if (cat === null || cat === undefined) return 'Desconocido';

    cat = String(cat).trim();
    var simple = cat.normalize('NFD').replace(/[\\u0300-\\u036f]/g, '').toLowerCase();

    if (simple === 'investigador') return 'Investigador';
    if (simple === 'becario') return 'Becario';
    if (simple === 'becario de investigacion') return 'Becario de investigación';
    if (simple === 'revista') return 'Revista';
    if (simple === 'proyecto') return 'Proyecto';

    return cat;
  }

  function estiloCategoria(cat) {
    if (cat === 'Investigador') {
      return { color: '#5470C6' };
    }
    if (cat === 'Revista' || cat === 'Proyecto') {
      return { color: '#EE6666' };
    }
    if (cat === 'Becario de investigación') {
      return { color: '#FAC858' };
    }
    if (cat === 'Becario') {
      return { color: '#91CC75' };
    }
    return { color: '#AAAAAA' };
  }

  var categoriasPresentes = {};

  option.series[0].data.forEach(function(node) {
    var cat = normalizarCategoria(node.category);
    node.category = cat;
    node.itemStyle = estiloCategoria(cat);
    categoriasPresentes[cat] = true;
  });

  option.series[0].categories = ordenCategorias
    .filter(function(cat) {
      return categoriasPresentes[cat];
    })
    .map(function(cat) {
      return {
        name: cat,
        itemStyle: estiloCategoria(cat)
      };
    });

  chart.setOption(option);
}
"

# --- Datos compartidos ---
n_invest_rev <- read.csv2("Investigadores_internos.csv", sep = ";", fileEncoding = "latin1")
n_invest_proy <- n_invest_rev
areas_rev <- read.csv("es_parte_de_limpio_listas.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
areas_proy <- areas_rev
revistas <- read.csv("Revistas.csv", stringsAsFactors = FALSE, fileEncoding = "latin1")
pub_con <- read.csv("ha_publicado_con.csv", fileEncoding = "latin1")
parti_con <- read.csv2("ha_participado_con.csv", fileEncoding = "latin1")

# --- Funciones de limpieza (compartida) ---
split_name_and_areas <- function(nombre, areas) {
  if (is.na(areas) && grepl(",", nombre)) {
    parts <- unlist(strsplit(nombre, ",", fixed = TRUE))
    nombre <- trimws(parts[1])
    areas <- trimws(parts[2])
  }
  list(Nombre = nombre, Areas = areas)
}

# --- Limpieza de áreas (revistas y proyectos) ---
cleaned_rev <- mapply(split_name_and_areas, areas_rev$Nombre, areas_rev$Areas, SIMPLIFY = FALSE)
areas_rev <- do.call(rbind, lapply(cleaned_rev, as.data.frame))
areas_rev$Areas <- gsub('"', '', areas_rev$Areas)
areas_rev$Lista_areas <- strsplit(gsub("/", ",", areas_rev$Areas), ",\\s*")

cleaned_proy <- mapply(split_name_and_areas, areas_proy$Nombre, areas_proy$Areas, SIMPLIFY = FALSE)
areas_proy <- do.call(rbind, lapply(cleaned_proy, as.data.frame))
areas_proy$Areas <- gsub('"', '', areas_proy$Areas)
areas_proy$Lista_areas <- strsplit(gsub("/", ",", areas_proy$Areas), ",\\s*")

# --- GRAFO 1: Revistas y Artículos (script 20250627) ---
investigadores_con_csv <- n_invest_rev %>% filter(!is.na(Acortacion))
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
publicaciones_df <- bind_rows(publicaciones_list)
relaciones_revistas <- publicaciones_df %>% select(journal, investigador) %>% filter(!is.na(journal), journal != "")
investigadores_rev <- n_invest_rev %>% left_join(areas_rev[, c("Nombre", "Lista_areas")], by = "Nombre")

nodos_invest_rev <- investigadores_rev %>%
  select(name = Nombre, Lista_areas, category = Tipo.de.empleado) %>%
  mutate(category = normalizar_categoria(category))

nodos_revistas <- revistas %>%
  filter(!is.na(Revista), Revista != "") %>%
  filter(Revista %in% relaciones_revistas$journal) %>%
  select(name = Revista) %>%
  mutate(category = "Revista", Lista_areas = NA)

nodos_rev_completo <- bind_rows(nodos_invest_rev, nodos_revistas) %>%
  mutate(value = 10, size = 20)

edges_rev_completo <- relaciones_revistas %>% rename(source = investigador, target = journal)

metricas_investigadores_rev <- publicaciones_df %>%
  filter(!is.na(journal), journal != "") %>%
  group_by(investigador) %>%
  summarise(
    n_publicaciones = n(),
    n_revistas = n_distinct(journal),
    .groups = "drop"
  )

metricas_revistas <- publicaciones_df %>%
  filter(!is.na(journal), journal != "") %>%
  group_by(journal) %>%
  summarise(
    n_investigadores = n_distinct(investigador),
    .groups = "drop"
  )

stats_investigadores_rev <- setNames(
  lapply(seq_len(nrow(metricas_investigadores_rev)), function(i) {
    list(
      n_publicaciones = metricas_investigadores_rev$n_publicaciones[i],
      n_revistas = metricas_investigadores_rev$n_revistas[i]
    )
  }),
  metricas_investigadores_rev$investigador
)

stats_revistas <- setNames(
  lapply(seq_len(nrow(metricas_revistas)), function(i) {
    list(
      n_investigadores = metricas_revistas$n_investigadores[i]
    )
  }),
  metricas_revistas$journal
)

nombres_ordenados_rev <- investigadores_rev %>% arrange(Nombre) %>% pull(Nombre)
todas_las_areas_rev <- sort(unique(unlist(areas_rev$Lista_areas)))

# --- GRAFO 2: Proyectos y Participantes (script 20250811) ---
investigadores_con_proyectos <- n_invest_proy %>% filter(!is.na(Acortacion))
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
proyectos_df <- bind_rows(proyectos_list)
relaciones_proy <- proyectos_df %>% select(Proyecto = TÍTULO, investigador) %>% filter(!is.na(Proyecto), Proyecto != "")
investigadores_proy <- n_invest_proy %>% left_join(areas_proy[, c("Nombre", "Lista_areas")], by = "Nombre")

nodos_invest_proy <- investigadores_proy %>%
  select(name = Nombre, Lista_areas, category = Tipo.de.empleado) %>%
  mutate(category = normalizar_categoria(category))

proyectos_unicos <- unique(relaciones_proy$Proyecto)

nodos_proyectos <- data.frame(
  name = proyectos_unicos,
  category = "Proyecto",
  Lista_areas = NA,
  stringsAsFactors = FALSE
)

nodos_proy_completo <- bind_rows(nodos_invest_proy, nodos_proyectos) %>%
  mutate(value = 10, size = 20)

edges_proy_completo <- relaciones_proy %>% rename(source = investigador, target = Proyecto)

metricas_investigadores_proy <- relaciones_proy %>%
  group_by(investigador) %>%
  summarise(
    n_proyectos = n_distinct(Proyecto),
    .groups = "drop"
  )

metricas_proyectos <- relaciones_proy %>%
  group_by(Proyecto) %>%
  summarise(
    n_investigadores = n_distinct(investigador),
    .groups = "drop"
  )

stats_investigadores_proy <- setNames(
  lapply(seq_len(nrow(metricas_investigadores_proy)), function(i) {
    list(
      n_proyectos = metricas_investigadores_proy$n_proyectos[i]
    )
  }),
  metricas_investigadores_proy$investigador
)

stats_proyectos <- setNames(
  lapply(seq_len(nrow(metricas_proyectos)), function(i) {
    list(
      n_investigadores = metricas_proyectos$n_investigadores[i]
    )
  }),
  metricas_proyectos$Proyecto
)

nombres_ordenados_proy <- investigadores_proy %>% arrange(Nombre) %>% pull(Nombre)
todas_las_areas_proy <- sort(unique(unlist(areas_proy$Lista_areas)))

normalizar_ip <- function(x) {
  if (is.logical(x)) {
    return(ifelse(is.na(x), FALSE, x))
  }
  
  x <- tolower(trimws(as.character(x)))
  x %in% c("true", "verdadero", "sí", "si", "1")
}

proyectos_df$Es_IP_Principal <- normalizar_ip(proyectos_df$Es_IP_Principal)

# --- GRAFO 3: Colaboración entre Investigadores (script 20250831) ---
# REVISTAS
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
edges_inv_rev <- do.call(rbind, lapply(names(solo_inv_rev), function(source) {
  targets <- solo_inv_rev[[source]]
  if (!is.null(targets) && length(targets) > 0) {
    df <- data.frame(source = rep(source, length(targets)), target = targets, stringsAsFactors = FALSE)
    df_reverse <- data.frame(source = df$target, target = df$source, stringsAsFactors = FALSE)
    rbind(df, df_reverse)
  } else NULL
}))
edges_inv_rev <- unique(edges_inv_rev)
edges_inv_rev <- edges_inv_rev[edges_inv_rev$source != edges_inv_rev$target, ]
grado_inv_rev <- as.data.frame(table(edges_inv_rev$source))
colnames(grado_inv_rev) <- c("name", "connections")
nodes_inv_rev <- merge(data.frame(name = n_invest_rev$Nombre, stringsAsFactors = FALSE), grado_inv_rev, by = "name", all.x = TRUE)
nodes_inv_rev$connections[is.na(nodes_inv_rev$connections)] <- 0
nodes_inv_rev$size <- 10 + nodes_inv_rev$connections * 2
nodes_inv_rev$value <- nodes_inv_rev$connections
nodes_inv_rev <- merge(nodes_inv_rev, areas_rev[, c("Nombre", "Lista_areas")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_inv_rev <- merge(nodes_inv_rev, n_invest_rev[, c("Nombre", "Tipo.de.empleado")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_inv_rev$category <- normalizar_categoria(nodes_inv_rev$Tipo.de.empleado)

# PROYECTOS
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
edges_inv_proy <- do.call(rbind, lapply(names(solo_inv_proy), function(source) {
  targets <- solo_inv_proy[[source]]
  if (!is.null(targets) && length(targets) > 0) {
    df <- data.frame(source = rep(source, length(targets)), target = targets, stringsAsFactors = FALSE)
    df_reverse <- data.frame(source = df$target, target = df$source, stringsAsFactors = FALSE)
    rbind(df, df_reverse)
  } else NULL
}))
edges_inv_proy <- unique(edges_inv_proy)
edges_inv_proy <- edges_inv_proy[edges_inv_proy$source != edges_inv_proy$target, ]
grado_inv_proy <- as.data.frame(table(edges_inv_proy$source))
colnames(grado_inv_proy) <- c("name", "connections")
nodes_inv_proy <- merge(data.frame(name = n_invest_proy$Nombre, stringsAsFactors = FALSE), grado_inv_proy, by = "name", all.x = TRUE)
nodes_inv_proy$connections[is.na(nodes_inv_proy$connections)] <- 0
nodes_inv_proy$size <- 10 + nodes_inv_proy$connections * 2
nodes_inv_proy$value <- nodes_inv_proy$connections
nodes_inv_proy <- merge(nodes_inv_proy, areas_proy[, c("Nombre", "Lista_areas")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_inv_proy <- merge(nodes_inv_proy, n_invest_proy[, c("Nombre", "Tipo.de.empleado")], by.x = "name", by.y = "Nombre", all.x = TRUE)
nodes_inv_proy$category <- normalizar_categoria(nodes_inv_proy$Tipo.de.empleado)

  
# 3. Interfaz de usuario (UI) con pestañas
ui <- navbarPage(
  "Grafo de Investigadores IUMPA",
  
  # Pestaña 1: Producción académica
  tabPanel("Producción académica",
           tabsetPanel(
             tabPanel("Revistas",
                      fluidRow(
                        column(
                          width = 4,
                          selectInput("investigador_filtro_rev", "Selecciona un investigador:",
                                      choices = c("-" = "-", nombres_ordenados_rev),
                                      selected = "-"),
                          selectInput("area_filtro_rev", "Selecciona un área:",
                                      choices = c("-" = "-", todas_las_areas_rev),
                                      selected = "-")
                        ),
                        column(
                          width = 8,
                          echarts4rOutput("grafo_rev", height = "800px")
                        ),
                        column(
                          width = 4,
                          h4("Información del nodo seleccionado"),
                          verbatimTextOutput("info_nodo_rev")
                        )
                      )
             ),
             tabPanel("Proyectos",
                      fluidRow(
                        column(
                          width = 4,
                          selectInput("investigador_filtro_proy", "Selecciona un investigador:",
                                      choices = c("-" = "-", nombres_ordenados_proy),
                                      selected = "-"),
                          selectInput("area_filtro_proy", "Selecciona un área:",
                                      choices = c("-" = "-", todas_las_areas_proy),
                                      selected = "-")
                        ),
                        column(
                          width = 8,
                          echarts4rOutput("grafo_proy", height = "800px")
                        ),
                        column(
                          width = 4,
                          h4("Información del nodo seleccionado"),
                          verbatimTextOutput("info_nodo_proy")
                        )
                      )
             )
           )
  ),
  
  # Pestaña 2: Colaboración entre Investigadores (sin cambios)
  tabPanel("Colaboración entre Investigadores",
           tabsetPanel(
             tabPanel("Revistas",
                      fluidRow(
                        column(
                          width = 4,
                          selectInput("selected_node_rev", "Selecciona un investigador:",
                                      choices = c("-" = "-", sort(nodes_inv_rev$name)), selected = "-"),
                          selectInput("selected_area_rev", "Selecciona un área:",
                                      choices = c("-" = "-", sort(unique(unlist(areas_rev$Lista_areas)))), selected = "-")
                        ),
                        column(width = 8, echarts4rOutput("graph_inv_rev", height = "800px")),
                        column(width = 4,
                               h4("Información del nodo seleccionado o clicado"),
                               verbatimTextOutput("node_info_inv_rev"))
                      )
             ),
             tabPanel("Proyectos",
                      fluidRow(
                        column(
                          width = 4,
                          selectInput("selected_node_proy", "Selecciona un investigador:",
                                      choices = c("-" = "-", sort(nodes_inv_proy$name)), selected = "-"),
                          selectInput("selected_area_proy", "Selecciona un área:",
                                      choices = c("-" = "-", sort(unique(unlist(areas_proy$Lista_areas)))), selected = "-")
                        ),
                        column(width = 8, echarts4rOutput("graph_inv_proy", height = "800px")),
                        column(width = 4,
                               h4("Información del nodo seleccionado o clicado"),
                               verbatimTextOutput("node_info_inv_proy"))
                      )
             )
           )
  )
)
  
# 4. Lógica del servidor (server)
server <- function(input, output, session) {
    
    # Lógica para la pestaña "Revistas y Artículos"
    output$grafo_rev <- renderEcharts4r({
      filtrar_por_area <- function(nodo) {
        if (is.null(nodo$Lista_areas)) return(FALSE)
        input$area_filtro_rev %in% nodo$Lista_areas[[1]]
      }
      
      nodos_mostrar <- nodos_rev_completo
      edges_mostrar <- edges_rev_completo
      
      if (input$investigador_filtro_rev != "-" || input$area_filtro_rev != "-") {
        
        if (input$area_filtro_rev != "-") {
          nodos_filtrados <- nodos_rev_completo %>%
            filter(category != "Revista") %>%
            filter(sapply(split(., 1:nrow(.)), filtrar_por_area))
          
          edges_mostrar <- edges_rev_completo %>%
            filter(source %in% nodos_filtrados$name)
          
          revistas_conectadas <- unique(edges_mostrar$target)
          nodos_mostrar <- bind_rows(nodos_filtrados, nodos_rev_completo %>% filter(name %in% revistas_conectadas))
        }
        
        if (input$investigador_filtro_rev != "-") {
          edges_mostrar <- edges_rev_completo %>% filter(source == input$investigador_filtro_rev)
          revistas_conectadas <- unique(edges_mostrar$target)
          nodos_mostrar <- nodos_rev_completo %>%
            filter(name == input$investigador_filtro_rev | name %in% revistas_conectadas)
        }
        
        if (input$investigador_filtro_rev != "-" && input$area_filtro_rev != "-") {
          if (input$area_filtro_rev %in% unlist(nodos_rev_completo$Lista_areas[nodos_rev_completo$name == input$investigador_filtro_rev])) {
            edges_mostrar <- edges_rev_completo %>% filter(source == input$investigador_filtro_rev)
            revistas_conectadas <- unique(edges_mostrar$target)
            nodos_mostrar <- nodos_rev_completo %>%
              filter(name == input$investigador_filtro_rev | name %in% revistas_conectadas)
          } else {
            nodos_mostrar <- nodos_rev_completo[0, ]
            edges_mostrar <- edges_rev_completo[0, ]
          }
        }
      }
      
      nodos_mostrar %>%
        e_charts(name) %>%
        e_graph(layout = "force") %>%
        e_graph_nodes(nodos_mostrar, name, value, size, category) %>%
        e_graph_edges(edges_mostrar, source, target) %>%
        e_tooltip(formatter = htmlwidgets::JS(
          paste0(
            "function(params) {
          if (params.dataType === 'edge') {
            return '<strong>' + params.data.source + ' > ' + params.data.target + '</strong>';
          }
    
          var statsInvestigadores = ", jsonlite::toJSON(stats_investigadores_rev, auto_unbox = TRUE), ";
          var statsRevistas = ", jsonlite::toJSON(stats_revistas, auto_unbox = TRUE), ";
    
          var nombre = params.name;
          var categoria = params.data.category;
    
          if (categoria === 'Revista') {
            var sRev = statsRevistas[nombre] || {};
            return '<strong>Revista: ' + nombre + '</strong><br/>' +
                   'Investigadores asociados: ' + (sRev.n_investigadores || 0);
          } else {
            var sInv = statsInvestigadores[nombre] || {};
            return '<strong>Investigador/a: ' + nombre + '</strong><br/>' +
                   'Revistas asociadas: ' + (sInv.n_revistas || 0) + '<br/>' +
                   'Artículos publicados: ' + (sInv.n_publicaciones || 0);
          }
        }"
              )
            )) %>%
        e_legend(top = "20") %>%
        e_on(query = list(dataType = "node"),
             handler = htmlwidgets::JS("function(params) {
              Shiny.setInputValue('clicked_node_rev_main', { name: params.data.name, category: params.data.category }, {priority: 'event'});
           }")) %>%
      htmlwidgets::onRender(aplicar_colores_nodos)
    })
    
    output$info_nodo_rev <- renderText({
      node_data <- input$clicked_node_rev_main
      if (is.null(node_data) && input$investigador_filtro_rev != "-") {
        node_data <- list(name = input$investigador_filtro_rev, category = "investigador")
      }
      if (is.null(node_data)) return("Seleccione o clique un nodo para ver su información.")
      
      node_to_show <- node_data$name
      tipo_nodo <- tolower(node_data$category)
      
      if (tipo_nodo != "revista") {
        articulos <- publicaciones_df %>% filter(investigador == node_to_show) %>% pull(title)
        if (length(articulos) == 0) return(paste0("Investigador/a: ", node_to_show, "\n\nNo se han encontrado publicaciones registradas."))
        return(paste0("Investigador/a: ", node_to_show, "\n\nArtículos publicados:\n", paste0("- ", articulos, collapse = "\n")))
      } else {
        articulos <- publicaciones_df %>%
          filter(journal == node_to_show) %>%
          select(title, investigador)
        
        editorial <- revistas %>%
          filter(Revista == node_to_show) %>%
          pull(Editorial) %>%
          unique()
        
        editorial <- editorial[!is.na(editorial) & editorial != "" & editorial != "None"]
        
        texto_editorial <- if (length(editorial) > 0) {
          paste(editorial, collapse = ", ")
        } else {
          "No disponible"
        }
        
        if (nrow(articulos) == 0) {
          return(paste0(
            "Revista: ", node_to_show,
            "\nEditorial: ", texto_editorial,
            "\n\nNo se han encontrado artículos publicados en esta revista."
          ))
        }
        
        textos <- paste0("- ", articulos$title, " (", articulos$investigador, ")")
        
        return(paste0(
          "Revista: ", node_to_show,
          "\nEditorial: ", texto_editorial,
          "\n\nArtículos publicados por algún investigador del instituto:\n",
          paste(textos, collapse = "\n")
        ))
      }
    })
    
    # Lógica para la pestaña "Proyectos y Participantes"
    output$grafo_proy <- renderEcharts4r({
      filtrar_por_area <- function(nodo) {
        if (is.null(nodo$Lista_areas)) return(FALSE)
        input$area_filtro_proy %in% nodo$Lista_areas[[1]]
      }
      
      nodos_mostrar <- nodos_proy_completo
      edges_mostrar <- edges_proy_completo
      
      if (input$investigador_filtro_proy != "-" || input$area_filtro_proy != "-") {
        if (input$area_filtro_proy != "-") {
          nodos_filtrados <- nodos_proy_completo %>%
            filter(category != "Proyecto") %>%
            filter(sapply(split(., 1:nrow(.)), filtrar_por_area))
          
          edges_mostrar <- edges_proy_completo %>%
            filter(source %in% nodos_filtrados$name)
          
          proyectos_conectados <- unique(edges_mostrar$target)
          nodos_mostrar <- bind_rows(nodos_filtrados, nodos_proy_completo %>% filter(name %in% proyectos_conectados))
        }
        
        if (input$investigador_filtro_proy != "-") {
          edges_mostrar <- edges_proy_completo %>% filter(source == input$investigador_filtro_proy)
          proyectos_conectados <- unique(edges_mostrar$target)
          nodos_mostrar <- nodos_proy_completo %>%
            filter(name == input$investigador_filtro_proy | name %in% proyectos_conectados)
        }
        
        if (input$investigador_filtro_proy != "-" && input$area_filtro_proy != "-") {
          if (input$area_filtro_proy %in% unlist(nodos_proy_completo$Lista_areas[nodos_proy_completo$name == input$investigador_filtro_proy])) {
            edges_mostrar <- edges_proy_completo %>% filter(source == input$investigador_filtro_proy)
            proyectos_conectados <- unique(edges_mostrar$target)
            nodos_mostrar <- nodos_proy_completo %>%
              filter(name == input$investigador_filtro_proy | name %in% proyectos_conectados)
          } else {
            nodos_mostrar <- nodos_proy_completo[0, ]
            edges_mostrar <- edges_proy_completo[0, ]
          }
        }
      }
      
      nodos_mostrar %>%
        e_charts(name) %>%
        e_graph(layout = "force") %>%
        e_graph_nodes(nodos_mostrar, name, value, size, category) %>%
        e_graph_edges(edges_mostrar, source, target) %>%
        e_tooltip(formatter = htmlwidgets::JS(
          paste0(
            "function(params) {
          if (params.dataType === 'edge') {
            return '<strong>' + params.data.source + ' > ' + params.data.target + '</strong>';
          }
    
          var statsInvestigadores = ", jsonlite::toJSON(stats_investigadores_proy, auto_unbox = TRUE), ";
          var statsProyectos = ", jsonlite::toJSON(stats_proyectos, auto_unbox = TRUE), ";
    
          var nombre = params.name;
          var categoria = params.data.category;
    
          if (categoria === 'Proyecto') {
            var sProy = statsProyectos[nombre] || {};
            return '<strong>Proyecto: ' + nombre + '</strong><br/>' +
                   'Investigadores asociados: ' + (sProy.n_investigadores || 0);
          } else {
            var sInv = statsInvestigadores[nombre] || {};
            return '<strong>Investigador/a: ' + nombre + '</strong><br/>' +
                   'Proyectos asociados: ' + (sInv.n_proyectos || 0);
          }
        }"
              )
            )) %>%
        e_legend(top = "20") %>%
        e_on(query = list(dataType = "node"),
             handler = htmlwidgets::JS("function(params) {
              Shiny.setInputValue('clicked_node_proy_main', { name: params.data.name, category: params.data.category }, {priority: 'event'});
           }")) %>%
      htmlwidgets::onRender(aplicar_colores_nodos)
    })
    
    output$info_nodo_proy <- renderText({
      node_data <- input$clicked_node_proy_main
      
      if (is.null(node_data) && input$investigador_filtro_proy != "-") {
        node_data <- list(name = input$investigador_filtro_proy, category = "investigador")
      }
      
      if (is.null(node_data)) {
        return("Seleccione o clique un nodo para ver su información.")
      }
      
      node_to_show <- node_data$name
      tipo_nodo <- tolower(node_data$category)
      
      if (tipo_nodo != "proyecto") {
        
        proyectos_investigador <- proyectos_df %>%
          filter(investigador == node_to_show) %>%
          select(Titulo = `TÍTULO`, Es_IP_Principal) %>%
          filter(!is.na(Titulo), Titulo != "") %>%
          group_by(Titulo) %>%
          summarise(Es_IP_Principal = any(Es_IP_Principal), .groups = "drop")
        
        if (nrow(proyectos_investigador) == 0) {
          return(paste0(
            "Investigador/a: ", node_to_show,
            "\n\nNo se han encontrado proyectos registrados."
          ))
        }
        
        proyectos_principal <- proyectos_investigador %>%
          filter(Es_IP_Principal) %>%
          pull(Titulo)
        
        proyectos_no_principal <- proyectos_investigador %>%
          filter(!Es_IP_Principal) %>%
          pull(Titulo)
        
        texto_principal <- if (length(proyectos_principal) > 0) {
          paste0(
            "Proyectos en los que participa como investigador/a principal:\n",
            paste0("- ", proyectos_principal, collapse = "\n")
          )
        } else {
          "Proyectos en los que participa como investigador/a principal:\nNo figura como investigador/a principal en ningún proyecto."
        }
        
        texto_no_principal <- if (length(proyectos_no_principal) > 0) {
          paste0(
            "Proyectos en los que participa como investigador/a no principal:\n",
            paste0("- ", proyectos_no_principal, collapse = "\n")
          )
        } else {
          "Proyectos en los que participa como investigador/a no principal:\nNo figura como investigador/a no principal en ningún proyecto."
        }
        
        return(paste0(
          "Investigador/a: ", node_to_show,
          "\n\n", texto_principal,
          "\n\n", texto_no_principal
        ))
        
      } else {
        
        investigadores_proyecto <- proyectos_df %>%
          filter(`TÍTULO` == node_to_show) %>%
          select(investigador, Es_IP_Principal) %>%
          filter(!is.na(investigador), investigador != "") %>%
          group_by(investigador) %>%
          summarise(Es_IP_Principal = any(Es_IP_Principal), .groups = "drop")
        
        if (nrow(investigadores_proyecto) == 0) {
          return(paste0(
            "Proyecto: ", node_to_show,
            "\n\nNo se han encontrado investigadores en este proyecto."
          ))
        }
        
        investigadores_principales <- investigadores_proyecto %>%
          filter(Es_IP_Principal) %>%
          pull(investigador)
        
        investigadores_no_principales <- investigadores_proyecto %>%
          filter(!Es_IP_Principal) %>%
          pull(investigador)
        
        texto_principales <- if (length(investigadores_principales) > 0) {
          paste0(
            "Investigadores/as principales del instituto:\n",
            paste0("- ", investigadores_principales, collapse = "\n")
          )
        } else {
          "Investigadores/as principales del instituto:\nNo se ha identificado ningún investigador/a del IUMPA como principal en este proyecto."
        }
        
        texto_no_principales <- if (length(investigadores_no_principales) > 0) {
          paste0(
            "Investigadores/as no principales del instituto:\n",
            paste0("- ", investigadores_no_principales, collapse = "\n")
          )
        } else {
          "Investigadores/as no principales del instituto:\nNo se han identificado investigadores/as no principales en este proyecto."
        }
        
        return(paste0(
          "Proyecto: ", node_to_show,
          "\n\n", texto_principales,
          "\n\n", texto_no_principales
        ))
      }
    })
    
    # Lógica para la pestaña "Colaboración entre Investigadores" - subpestaña Revistas
    output$graph_inv_rev <- renderEcharts4r({
      selected_nodes <- nodes_inv_rev
      if (input$selected_area_rev != "-") {
        selected_nodes <- selected_nodes[sapply(selected_nodes$Lista_areas, function(x) input$selected_area_rev %in% x), ]
      }
      if (input$selected_node_rev != "-") {
        selected_nodes <- selected_nodes[selected_nodes$name == input$selected_node_rev | selected_nodes$name %in% edges_inv_rev$target[edges_inv_rev$source == input$selected_node_rev], ]
      }
      selected_edges <- edges_inv_rev[edges_inv_rev$source %in% selected_nodes$name & edges_inv_rev$target %in% selected_nodes$name, ]
      
      selected_nodes |>
        e_charts(name) |>
        e_graph(layout = "circular") |>
        e_graph_nodes(selected_nodes, name, value = connections, size, category = category) |>
        e_graph_edges(selected_edges, source, target) |>
        e_tooltip(formatter = htmlwidgets::JS("
      function(params) {
        if (params.dataType === 'edge') {
          return '<strong>' + params.data.source + ' > ' + params.data.target + '</strong>';
        }
    
        return '<strong>' + params.name + '</strong><br/>' +
               'Número de colaboradores: ' + params.value;
      }
    ")) |>
        e_on(query = list(dataType = "node"),
             handler = htmlwidgets::JS("function(params) { Shiny.setInputValue('clicked_node_inv_rev', params.data.name, {priority: 'event'}); }")) %>%
      htmlwidgets::onRender(aplicar_colores_nodos)
    })
    
    output$node_info_inv_rev <- renderText({
      node <- if (!is.null(input$clicked_node_inv_rev)) input$clicked_node_inv_rev else if (input$selected_node_rev != "-") input$selected_node_rev else NULL
      if (is.null(node)) return("Seleccione o clique un nodo para ver su información.")
      conexiones <- edges_inv_rev[edges_inv_rev$source == node, "target"]
      if (length(conexiones) == 0) return(paste0("Nodo: ", node, "\n\nNo ha trabajado con nadie del IUMPA"))
      paste0("Investigador: ", node, "\n\nHa trabajado con:\n", paste0("- ", conexiones, collapse = "\n"))
    })
    
    # Lógica para la pestaña "Colaboración entre Investigadores" - subpestaña Proyectos
    output$graph_inv_proy <- renderEcharts4r({
      selected_nodes <- nodes_inv_proy
      if (input$selected_area_proy != "-") {
        selected_nodes <- selected_nodes[sapply(selected_nodes$Lista_areas, function(x) input$selected_area_proy %in% x), ]
      }
      if (input$selected_node_proy != "-") {
        selected_nodes <- selected_nodes[selected_nodes$name == input$selected_node_proy | selected_nodes$name %in% edges_inv_proy$target[edges_inv_proy$source == input$selected_node_proy], ]
      }
      selected_edges <- edges_inv_proy[edges_inv_proy$source %in% selected_nodes$name & edges_inv_proy$target %in% selected_nodes$name, ]
      
      selected_nodes |>
        e_charts(name) |>
        e_graph(layout = "circular") |>
        e_graph_nodes(selected_nodes, name, value = connections, size, category = category) |>
        e_graph_edges(selected_edges, source, target) |>
        e_tooltip(formatter = htmlwidgets::JS("
      function(params) {
        if (params.dataType === 'edge') {
          return '<strong>' + params.data.source + ' > ' + params.data.target + '</strong>';
        }
    
        return '<strong>' + params.name + '</strong><br/>' +
               'Número de colaboradores: ' + params.value;
      }
    ")) |>
        e_on(query = list(dataType = "node"),
             handler = htmlwidgets::JS("function(params) { Shiny.setInputValue('clicked_node_inv_proy', params.data.name, {priority: 'event'}); }")) %>%
      htmlwidgets::onRender(aplicar_colores_nodos)
    })
    
    output$node_info_inv_proy <- renderText({
      node <- if (!is.null(input$clicked_node_inv_proy)) input$clicked_node_inv_proy else if (input$selected_node_proy != "-") input$selected_node_proy else NULL
      if (is.null(node)) return("Seleccione o clique un nodo para ver su información.")
      conexiones <- edges_inv_proy[edges_inv_proy$source == node, "target"]
      if (length(conexiones) == 0) return(paste0("Nodo: ", node, "\n\nNo ha trabajado con nadie del IUMPA"))
      paste0("Investigador: ", node, "\n\nHa trabajado con:\n", paste0("- ", conexiones, collapse = "\n"))
    })
  }

# 5. Ejecutar la aplicación
shinyApp(ui, server)
