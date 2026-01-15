#########################################
# REDUCCIÓN NO LINEAL CON UMAP + CLUSTERING (k = 4 FINAL)
#########################################

library("umap")
library("cluster")
library("factoextra")
library("ggplot2")
library("dplyr")

set.seed(123)

#################
# Parámetros
#################

usar_pca <- TRUE
varianza_objetivo <- 0.9
metodo_clustering <- "hclust"
linkage_hclust <- "average"
k_min <- 3
k_max <- 10

k_final <- 4   # <-- CLUSTERING FINAL A USAR

#################
# Cargar datos
#################

setwd("C:/Code/tfg-gcd/_Clusterization/Sin_Locl/Investigadores y keywords")

emb <- read.csv("Matriz_Investigadores_Embeddings_sinlocl.csv", row.names = 1)

# Asegurar nombres válidos
rownames(emb) <- make.names(rownames(emb), unique = TRUE)

X <- as.data.frame(lapply(emb, as.numeric))
X[is.na(X)] <- 0

# Nombres reales de investigadores
nombres_reales <- rownames(X)

#################
# PCA previa (opcional)
#################

if (usar_pca) {
  var_ok <- apply(X, 2, sd, na.rm = TRUE) > 0
  X_pca_input <- X[, var_ok, drop = FALSE]
  
  pca <- prcomp(X_pca_input, center = TRUE, scale. = TRUE)
  var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k_comp <- which(var_exp >= varianza_objetivo)[1]
  
  message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza)",
                  k_comp, 100 * var_exp[k_comp]))
  
  X_for_umap <- pca$x[, 1:k_comp, drop = FALSE]
  
} else {
  X_for_umap <- X
}

#################
# UMAP
#################

umap_config <- umap.defaults
umap_config$n_neighbors <- 10
umap_config$min_dist <- 0.2
umap_config$metric <- "cosine"

umap_res <- umap(X_for_umap, config = umap_config)

umap_df <- as.data.frame(umap_res$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
rownames(umap_df) <- nombres_reales  # <-- nombres reales

#################
# Clustering sobre UMAP (hclust)
#################

D_umap <- dist(umap_df, method = "euclidean")
fit <- hclust(D_umap, method = linkage_hclust)

#### 1) Calcular silhouette para info (NO SE USA COMO FINAL)
sil_scores <- sapply(k_min:k_max, function(k) {
  gr <- cutree(fit, k = k)
  sil_tmp <- silhouette(gr, D_umap)
  mean(sil_tmp[, "sil_width"])
})

k_opt <- which.max(sil_scores) + (k_min - 1)
message(sprintf("Mejor número de clusters (Silhouette): %d (NO SE USA)", k_opt))

#### 2) FORZAR k = 4 PARA EL TFG
grupos <- cutree(fit, k = k_final)

#################
# Silhouette final para k = 4
#################

sil <- silhouette(grupos, D_umap)
mean_sil <- mean(sil[, "sil_width"])
cat(sprintf("Silhouette medio (k = %d): %.3f\n", k_final, mean_sil))

#################
# Tabla final con ID + nombres reales
#################

# Crear ID numérico 1..n
tabla_final <- data.frame(
  InvestigadorID = seq_len(nrow(umap_df)),
  Cluster = grupos,
  UMAP1 = umap_df$UMAP1,
  UMAP2 = umap_df$UMAP2,
  Silhouette = sil[, "sil_width"],
  stringsAsFactors = FALSE
)

# Tabla de mapeo ID -> nombre real
investigadores_lista <- data.frame(
  InvestigadorID = seq_len(nrow(emb)),
  Investigador = rownames(emb),
  stringsAsFactors = FALSE
)

# Añadir nombres reales
tabla_final <- merge(tabla_final, investigadores_lista,
                     by = "InvestigadorID", all.x = TRUE)

# Ordenar y dejar solo nombre + resto
tabla_final <- tabla_final[order(tabla_final$Cluster, -tabla_final$Silhouette),
                           c("Investigador", "Cluster", "UMAP1", "UMAP2", "Silhouette")]

#################
# Visualización
#################

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = as.factor(grupos))) +
  geom_point(size = 3) +
  theme_minimal(base_size = 14) +
  labs(title = sprintf("UMAP + hclust (k=%d)", k_final),
       color = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))

fviz_silhouette(sil)

#################
# GUARDAR RESULTADOS UMAP k = 4
#################

dir.create("UMAP_Resultados_k4", showWarnings = FALSE)

write.csv(tabla_final,
          "UMAP_Resultados_k4/UMAP_Clusters_k4.csv",
          row.names = FALSE)

write.csv(umap_df,
          "UMAP_Resultados_k4/UMAP_Coordenadas.csv")

saveRDS(umap_res,
        "UMAP_Resultados_k4/UMAP_Modelo.rds")

saveRDS(list(
  n_neighbors = umap_config$n_neighbors,
  min_dist = umap_config$min_dist,
  metric = umap_config$metric,
  k_final = k_final,
  silhouette = mean_sil
), file = "UMAP_Resultados_k4/UMAP_Parametros.rds")

cat("\n=== UMAP k = 4 GUARDADO CORRECTAMENTE ===\n")

