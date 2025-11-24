#########################################
# REDUCCIÓN NO LINEAL CON UMAP + CLUSTERING
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

#################
# Cargar datos
#################

setwd("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords")

ruta_excel <- "Matriz_Investigadores_Embeddings.csv"
raw <- read.csv(ruta_excel, row.names = 1)
X <- as.data.frame(lapply(raw, as.numeric))
X[is.na(X)] <- 0

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
rownames(umap_df) <- rownames(X_for_umap)

#################
# Clustering sobre UMAP
#################

if (metodo_clustering == "hclust") {
  D_umap <- dist(umap_df, method = "euclidean")
  fit <- hclust(D_umap, method = linkage_hclust)
  
  sil_scores <- sapply(k_min:k_max, function(k) {
    gr <- cutree(fit, k = k)
    sil <- silhouette(gr, D_umap)
    mean(sil[, "sil_width"])
  })
  k_opt <- which.max(sil_scores) + (k_min - 1)
  message(sprintf("Mejor número de clusters (Silhouette): %d", k_opt))
  
  grupos <- cutree(fit, k = k_opt)
  
} else if (metodo_clustering == "kmeans") {
  set.seed(123)
  sil_scores <- c()
  for (k in k_min:k_max) {
    km <- kmeans(umap_df, centers = k, nstart = 25)
    sil <- silhouette(km$cluster, dist(umap_df))
    sil_scores[k - k_min + 1] <- mean(sil[, "sil_width"])
  }
  k_opt <- which.max(sil_scores) + (k_min - 1)
  message(sprintf("Mejor número de clusters (Silhouette): %d", k_opt))
  km_final <- kmeans(umap_df, centers = k_opt, nstart = 25)
  grupos <- km_final$cluster
}

#################
# Visualización
#################

ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = as.factor(grupos))) +
  geom_point(size = 3) +
  theme_minimal(base_size = 14) +
  labs(title = sprintf("UMAP + %s clustering (k=%d)", metodo_clustering, k_opt),
       color = "Cluster") +
  theme(plot.title = element_text(hjust = 0.5))

#################
# Silhouette
#################

D_umap <- dist(umap_df, method = "euclidean")
sil <- silhouette(grupos, D_umap)
mean_sil <- mean(sil[, "sil_width"])
cat(sprintf("Silhouette medio (UMAP + hclust, k=%d): %.3f\n", k_opt, mean_sil))
fviz_silhouette(sil)

#################
# Tabla final completa
#################

# Crear tabla base
resultado <- data.frame(
  Investigador = rownames(umap_df),
  Cluster = grupos,
  UMAP1 = umap_df$UMAP1,
  UMAP2 = umap_df$UMAP2,
  stringsAsFactors = FALSE
)

# Añadir silhouette individual
sil_df <- data.frame(
  Investigador = rownames(umap_df),
  Silhouette = sil[, "sil_width"]
)

# Combinar todo
tabla_final <- merge(resultado, sil_df, by = "Investigador", all.x = TRUE)

# Ordenar por cluster (y silhouette dentro de cada grupo)
tabla_final <- tabla_final[order(tabla_final$Cluster, -tabla_final$Silhouette), ]

#################
# Mostrar y guardar resultados
#################

cat(sprintf("\nSilhouette medio: %.3f\n", mean_sil))
cat("Listado completo de investigadores con cluster y silhouette:\n")

print(tabla_final[, c("Investigador", "Cluster", "Silhouette", "UMAP1", "UMAP2")],
      row.names = FALSE)

emb <- read.csv("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords/Matriz_Investigadores_Embeddings.csv",
                row.names = 1)

# Crear una lista numerada con los nombres de los investigadores
investigadores_lista <- data.frame(
  Investigador = seq_len(nrow(emb)),
  Nombre = rownames(emb)
)

# Mostrar la lista en consola
print(investigadores_lista, row.names = FALSE)

tabla_final$Investigador = as.numeric(as.character(tabla_final$Investigador))

n = left_join(tabla_final, investigadores_lista, by="Investigador")


###############################################
# GUARDAR MODELO UMAP Y RESULTADOS
###############################################

# Crear carpeta si no existe
dir.create("UMAP_Resultados", showWarnings = FALSE)

# 1. Guardar la tabla final completa (investigadores + cluster + UMAP + silhouette)
write.csv(
  tabla_final,
  "UMAP_Resultados/UMAP_Clusters_Investigadores.csv",
  row.names = FALSE
)

# 2. Guardar coordenadas UMAP
write.csv(
  umap_df,
  "UMAP_Resultados/UMAP_Coordenadas.csv",
  row.names = TRUE
)

# 3. Guardar configuración del UMAP
umap_config_list <- list(
  n_neighbors = umap_config$n_neighbors,
  min_dist = umap_config$min_dist,
  metric = umap_config$metric,
  k_opt = k_opt,
  metodo_clustering = metodo_clustering,
  linkage = linkage_hclust,
  silhouette = mean_sil
)

saveRDS(
  umap_config_list,
  file = "UMAP_Resultados/UMAP_Parametrizacion.rds"
)

# 4. Guardar el objeto del modelo UMAP completo
saveRDS(
  umap_res,
  file = "UMAP_Resultados/UMAP_Modelo.rds"
)

#################
# Probar manualmente k
#################

k_test <- 4   # número de clusters a probar
grupos_k5 <- cutree(fit, k = k_test)

# Calcular Silhouette para k = 5
D_umap <- dist(umap_df, method = "euclidean")
sil_k5 <- silhouette(grupos_k5, D_umap)
mean_sil_k5 <- mean(sil_k5[, "sil_width"])

cat(sprintf("Silhouette medio para k = %d: %.3f\n", k_test, mean_sil_k5))

# Visualizar Silhouette
fviz_silhouette(sil_k5)

# Crear tabla con investigadores y nuevo clustering
resultado_k5 <- data.frame(
  Investigador = rownames(umap_df),
  Cluster = grupos_k5,
  UMAP1 = umap_df$UMAP1,
  UMAP2 = umap_df$UMAP2,
  Silhouette = sil_k5[, "sil_width"]
)

# Mostrar y guardar resultados
print(resultado_k5[order(resultado_k5$Cluster, -resultado_k5$Silhouette), ], row.names = FALSE)

resultado_k5$Investigador = as.numeric(as.character(resultado_k5$Investigador))

n = left_join(resultado_k5, investigadores_lista, by="Investigador")

