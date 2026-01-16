###############################################
# CARGAR DATOS
###############################################

library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(networkD3)

setwd("C:/Code/tfg-gcd/_Clusterization/Conclusiones")

# Cargar ambos excels como listas de dataframes
jerMan <- readxl::read_excel("Clusters_resultado_JerMan.xlsx", sheet = NULL)
jerUMAP <- readxl::read_excel("Clusters_resultado_JerCosUMAP.xlsx", sheet = NULL)

###############################################
# EXTRAER LISTAS DE INVESTIGADORES POR CLUSTER
###############################################

get_investigadores <- function(list_df) {
  lapply(list_df, function(df) df$Investigador[df$Investigador != "" & !is.na(df$Investigadores)])
}

man_inv <- get_investigadores(jerMan)
umap_inv <- get_investigadores(jerUMAP)

###############################################
# CORRESPONDENCIA ENTRE CLUSTERS (TABLA CRUZADA)
###############################################

pairs <- data.frame()

for (m in names(man_inv)) {
  for (u in names(umap_inv)) {
    inter <- length(intersect(man_inv[[m]], umap_inv[[u]]))
    pairs <- rbind(pairs, data.frame(
      Cluster_Man = m,
      Cluster_UMAP = u,
      Coincidencias = inter
    ))
  }
}

###############################################
# 3️⃣ MATRIZ DE SIMILITUD JACCARD ENTRE CLUSTERS
###############################################

jaccard <- function(a, b) {
  length(intersect(a, b)) / length(union(a, b))
}

jac_matrix <- matrix(0, nrow = length(man_inv), ncol = length(umap_inv))
rownames(jac_matrix) <- names(man_inv)
colnames(jac_matrix) <- names(umap_inv)

for (i in 1:length(man_inv)) {
  for (j in 1:length(umap_inv)) {
    jac_matrix[i, j] <- jaccard(man_inv[[i]], umap_inv[[j]])
  }
}

###############################################
# 4️⃣ HEATMAP DE SIMILITUD (ggplot2)
###############################################

jac_df <- as.data.frame(jac_matrix)
jac_df$Cluster_Man <- rownames(jac_df)
jac_long <- melt(jac_df, id.vars = "Cluster_Man",
                 variable.name = "Cluster_UMAP",
                 value.name = "Jaccard")

ggplot(jac_long, aes(x = Cluster_UMAP, y = Cluster_Man, fill = Jaccard)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal(base_size = 14) +
  labs(title = "Similitud Jaccard entre Clusters (Manhattan vs UMAP)",
       x = "Clusters UMAP",
       y = "Clusters Manhattan") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###############################################
# 5️⃣ GRÁFICO SANKEY – CORRESPONDENCIA
###############################################

sankey_data <- pairs %>% filter(Coincidencias > 0)

nodes <- data.frame(
  name = c(as.character(unique(sankey_data$Cluster_Man)),
           as.character(unique(sankey_data$Cluster_UMAP)))
)

sankey_data$IDsource <- match(sankey_data$Cluster_Man, nodes$name) - 1
sankey_data$IDtarget <- match(sankey_data$Cluster_UMAP, nodes$name) - 1

sankeyNetwork(
  Links = sankey_data,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "Coincidencias",
  NodeID = "name",
  fontSize = 12
)

###############################################
# 6️⃣ TEXTO AUTOMÁTICO DE COMPARACIÓN CLUSTER A CLUSTER
###############################################

cat("\n==== COMPARACIÓN CLUSTER A CLUSTER ====\n\n")

for (m in names(man_inv)) {
  cat(paste0("\n--- Cluster ", m, " (Manhattan) ---\n"))
  cat("Investigadores: ", paste(man_inv[[m]], collapse = ", "), "\n")
  
  sims <- sapply(umap_inv, function(x) jaccard(man_inv[[m]], x))
  best <- names(which.max(sims))
  
  cat("Cluster UMAP más similar: ", best,
      "   (Jaccard = ", round(max(sims), 3), ")\n")
  
  comunes <- intersect(man_inv[[m]], umap_inv[[best]])
  cat("Investigadores en común: ",
      ifelse(length(comunes) > 0,
             paste(comunes, collapse = ", "),
             "Ninguno"),
      "\n")
}

cat("\n===========================================\n")
