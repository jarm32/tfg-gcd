#########################################
# CLUSTERING HDBSCAN
#########################################

library("dbscan")
library("proxy")
library("ggplot2")

#################
# Parámetros
#################

minPts <- 3                 # mínimo de puntos por cluster
usar_pca <- TRUE             # recomendable para embeddings
varianza_objetivo <- 0.9
metrica_distancia <- "cosine"

#################
# Cargar archivo
#################

setwd("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords")

ruta_excel <- "Matriz_Investigadores_Embeddings.csv"
raw <- read.csv(ruta_excel, row.names = 1)
df <- as.data.frame(raw)

# Aseguramos que todo es numérico
X <- df
X[] <- lapply(X, as.numeric)
X[is.na(X)] <- 0

#################
# PCA opcional
#################
if (usar_pca) {
  var_ok <- apply(X, 2, sd, na.rm = TRUE) > 0
  X_pca_input <- X[, var_ok, drop = FALSE]
  pca <- prcomp(X_pca_input, center = TRUE, scale. = TRUE)
  var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k_comp <- which(var_exp >= varianza_objetivo)[1]
  message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza)",
                  k_comp, 100 * var_exp[k_comp]))
  X_for_clust <- pca$x[, 1:k_comp, drop = FALSE]
} else {
  X_for_clust <- X
}

#################
# Normalizar (si usas cosine)
#################
if (metrica_distancia == "cosine") {
  normalize <- function(x) x / sqrt(sum(x^2))
  X_norm <- t(apply(X_for_clust, 1, normalize))
} else {
  X_norm <- X_for_clust
}

#################
# HDBSCAN
#################

hdb <- hdbscan(X_norm, minPts = minPts)

message(sprintf("HDBSCAN detectó %d clusters (+ ruido = 0)",
                length(unique(hdb$cluster[hdb$cluster != 0]))))

#################
# Resultados
#################

resultado <- data.frame(
  Investigador = rownames(X_norm),
  Cluster = hdb$cluster,
  Probabilidad = hdb$membership_prob
)
head(resultado)

#################
# Visualización
#################

# MDS para visualizar clusters
mds <- cmdscale(dist(X_norm), k = 2, eig = TRUE)
plot(mds$points,
     col = ifelse(hdb$cluster == 0, "grey", hdb$cluster + 1),
     pch = 19,
     main = sprintf("HDBSCAN (minPts=%d)", minPts),
     xlab = "Dim 1", ylab = "Dim 2")
legend("topright",
       legend = c("ruido", paste("Cluster", sort(unique(hdb$cluster[hdb$cluster != 0])))),
       col = c("grey", 2:(length(unique(hdb$cluster)) + 1)),
       pch = 19)

#################
# Diagnóstico adicional
#################

# Densidad de pertenencia: muestra qué tan firmemente está cada punto en su cluster
hist(hdb$membership_prob, breaks = 20, col = "skyblue",
     main = "Confianza de pertenencia a cluster (HDBSCAN)",
     xlab = "Probabilidad de pertenencia")

