library("readxl")
library("dplyr")
library("proxy")
library("cluster")
library("factoextra")
library("kohonen")
library("ggplot2")
library("dbscan")

#################
# Parámetros
#################

datos_binarios <- FALSE

metrica_distancia <- "cosine"
metodo_clustering <- "dbscan"

eps <- 0.45    # distancia máxima de vecindad
minPts <- 3   # número mínimo de puntos por cluster
eps_values <- seq(0.25, 0.5, by = 0.05)

usar_pca <- TRUE
varianza_objetivo <- 0.9

usar_som <- FALSE
som_grid_x <- 10
som_grid_y <- 10   
som_rlen <- 100                      
som_k_clusters <- 6  

#################
# Cargar archivo y modificarlo
#################

setwd("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords")

ruta_excel <- "Matriz_Investigadores_Embeddings.csv"
raw <- read.csv(ruta_excel, row.names = 1)
df <- as.data.frame(raw)

# Aseguramos que las columnas sean numéricas
X <- df
X[] <- lapply(X, as.numeric)
X[is.na(X)] <- 0

#################
# PCA (robusto)
#################
if (usar_pca) {
  var_ok <- apply(X, 2, sd, na.rm = TRUE) > 0
  if (!all(var_ok)) {
    message(sprintf("PCA: eliminadas %d columnas de varianza cero.", sum(!var_ok)))
  }
  X_pca_input <- X[, var_ok, drop = FALSE]
  
  if (ncol(X_pca_input) == 0) {
    warning("PCA: no hay columnas con varianza > 0. Se continúa sin PCA.")
    X_for_dist <- X
  } else {
    pca <- prcomp(X_pca_input, center = TRUE, scale. = TRUE)
    var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
    k_comp <- which(var_exp >= varianza_objetivo)[1]
    if (is.na(k_comp)) k_comp <- length(var_exp)
    message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza)",
                    k_comp, 100 * var_exp[k_comp]))
    X_for_dist <- pca$x[, 1:k_comp, drop = FALSE]
  }
} else {
  X_for_dist <- X
}

#################
# Distancia (solo para visualización)
#################

if (metrica_distancia %in% c("euclidean", "manhattan", "cosine")) {
  D <- proxy::dist(X_for_dist, method = metrica_distancia)
} else {
  D <- dist(X_for_dist)
}

#################
# Modelo DBSCAN
#################

# Normalizar cada fila a norma 1 si se usa cosine (recomendado)
if (metrica_distancia == "cosine") {
  normalize <- function(x) x / sqrt(sum(x^2))
  X_norm <- t(apply(X_for_dist, 1, normalize))
} else {
  X_norm <- X_for_dist
}

db <- dbscan::dbscan(as.matrix(X_norm), eps = eps, minPts = minPts)
grupos <- db$cluster

message(sprintf("DBSCAN detectó %d clusters + %d puntos ruido (cluster 0)",
                length(unique(grupos[grupos != 0])), sum(grupos == 0)))

#################
# Visualizaciones
#################

resultado <- data.frame(Investigador = rownames(X_for_dist), Cluster = grupos)
head(resultado)

# Plot MDS o PCA para visualizar
if (ncol(X_for_dist) > 2) {
  mds <- cmdscale(dist(X_norm), k = 2, eig = TRUE)
  plot(mds$points, col = ifelse(grupos == 0, "grey", grupos + 1),
       pch = 19, main = sprintf("DBSCAN eps=%.2f, minPts=%d", eps, minPts),
       xlab = "Dim 1", ylab = "Dim 2")
  legend("topright", legend = c("ruido", paste("Cluster", 1:max(grupos))),
         col = c("grey", 2:(max(grupos)+1)), pch = 19)
} else {
  plot(X_norm[,1], X_norm[,2], col = ifelse(grupos == 0, "grey", grupos + 1), pch = 19)
}

for (eps_test in seq(0.25, 0.5, by = 0.05)) {
  db <- dbscan::dbscan(as.matrix(X_norm), eps = eps_test, minPts = 3)
  n_clusters <- length(unique(db$cluster[db$cluster != 0]))
  pct_noise <- mean(db$cluster == 0) * 100
  message(sprintf("eps = %.2f -> %d clusters, %.1f%% ruido",
                  eps_test, n_clusters, pct_noise))
}

write.csv(resultado, "Clusters_resultado_DBSCAN.csv", row.names = FALSE

          