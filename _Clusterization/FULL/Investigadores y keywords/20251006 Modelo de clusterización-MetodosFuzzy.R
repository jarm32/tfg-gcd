##############################################
# CLUSTERING AVANZADO SIN PCA (versión robusta)
##############################################

# Librerías necesarias
library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(ppclust)
library(kernlab)
library(dbscan)
library(ggplot2)

##############################################
# 1. CARGA Y LIMPIEZA DE DATOS
##############################################

ruta_excel <- "Matriz_Investigadores_Keywords.xlsx"
raw <- read_excel(ruta_excel)
df <- as.data.frame(raw)

# Eliminar columna de nombres si la hay
X <- df %>% select(-1)

# Asegurar formato numérico
X <- as.data.frame(sapply(X, as.numeric))
X[!is.finite(as.matrix(X))] <- 0
X[is.na(X)] <- 0

# Eliminar columnas constantes (varianza 0)
X <- X[, apply(X, 2, var) > 0, drop = FALSE]

# Escalar los datos con control de NAs
X_for_clust <- scale(X)
X_for_clust[!is.finite(X_for_clust)] <- 0
X_for_clust[is.na(X_for_clust)] <- 0

cat("Tamaño final de la matriz para clustering:", 
    dim(X_for_clust)[1], "filas y", dim(X_for_clust)[2], "columnas\n")

##############################################
# 2. MÉTODO 1: FUZZY C-MEANS
##############################################

k_fcm <- 6  # número de clusters
fcm_model <- fcm(X_for_clust, centers = k_fcm, m = 2)
fcm_clusters <- fcm_model$cluster

sil_fcm <- silhouette(fcm_clusters, dist(X_for_clust))
mean_sil_fcm <- mean(sil_fcm[, 3])

cat(sprintf("\nFuzzy C-means: silhouette medio = %.3f\n", mean_sil_fcm))
fviz_cluster(list(data = X_for_clust, cluster = fcm_clusters)) + 
  ggtitle("Clustering Fuzzy C-means")

##############################################
# 3. MÉTODO 2: SPECTRAL CLUSTERING
##############################################

k_spec <- 6
spec_model <- specc(as.matrix(X_for_clust), centers = k_spec)
spec_clusters <- as.numeric(spec_model)

sil_spec <- silhouette(spec_clusters, dist(X_for_clust))
mean_sil_spec <- mean(sil_spec[, 3])

cat(sprintf("Spectral Clustering: silhouette medio = %.3f\n", mean_sil_spec))
fviz_cluster(list(data = X_for_clust, cluster = spec_clusters)) + 
  ggtitle("Clustering Espectral")

##############################################
# 4. MÉTODO 3: DBSCAN
##############################################

# Usa kNNdistplot para estimar eps
kNNdistplot(X_for_clust, k = 5)
abline(h = 0.5, col = "red", lty = 2)  # ajusta manualmente

eps_value <- 0.5
minPts_value <- 5

db_model <- dbscan(X_for_clust, eps = eps_value, minPts = minPts_value)
db_clusters <- db_model$cluster

if (length(unique(db_clusters)) > 1) {
  sil_db <- silhouette(db_clusters, dist(X_for_clust))
  mean_sil_db <- mean(sil_db[, 3], na.rm = TRUE)
} else {
  mean_sil_db <- NA
}

cat(sprintf("DBSCAN: silhouette medio = %.3f\n", mean_sil_db))
fviz_cluster(list(data = X_for_clust, cluster = db_clusters)) + 
  ggtitle("Clustering DBSCAN")

##############################################
# 5. MÉTODO 4: HDBSCAN
##############################################

hdb_model <- hdbscan(X_for_clust, minPts = 5)
hdb_clusters <- hdb_model$cluster

if (length(unique(hdb_clusters)) > 1) {
  sil_hdb <- silhouette(hdb_clusters, dist(X_for_clust))
  mean_sil_hdb <- mean(sil_hdb[, 3], na.rm = TRUE)
} else {
  mean_sil_hdb <- NA
}

cat(sprintf("HDBSCAN: silhouette medio = %.3f\n", mean_sil_hdb))
fviz_cluster(list(data = X_for_clust, cluster = hdb_clusters)) + 
  ggtitle("Clustering HDBSCAN")

##############################################
# 6. COMPARATIVA FINAL
##############################################

resumen <- data.frame(
  Metodo = c("Fuzzy C-means", "Spectral", "DBSCAN", "HDBSCAN"),
  Silhouette = c(mean_sil_fcm, mean_sil_spec, mean_sil_db, mean_sil_hdb)
)

print(resumen)

