###############################
# Librerías
###############################
library(readxl)
library(dplyr)
library(factoextra)
library(cluster)
library(mclust)
library(ggplot2)

###############################
# Parámetros
###############################

usar_pca <- TRUE
varianza_objetivo <- 0.9

k_min <- 2
k_max <- 12

###############################
# Cargar archivo
###############################

setwd("C:/Code/tfg-gcd/_Clusterization/Investigadores y keywords")

ruta_excel <- "Matriz_Investigadores_Keywords.xlsx"
raw <- read_excel(ruta_excel)
df <- as.data.frame(raw)

# Identificadores si la primera columna no es numérica
tiene_id <- !is.numeric(df[[1]])
if (tiene_id) {
  ids <- df[[1]]
  X <- df[, -1, drop = FALSE]
  rownames(X) <- make.names(ids, unique = TRUE)
} else {
  X <- df
}

# Asegurar que todas las columnas sean numéricas
X[] <- lapply(X, function(x) as.numeric(as.character(x)))
X[is.na(X)] <- 0

# Eliminar columnas con varianza cero
var_ok <- apply(X, 2, sd, na.rm = TRUE) > 0
if (!all(var_ok)) {
  message(sprintf("Eliminadas %d columnas con varianza cero.", sum(!var_ok)))
  X <- X[, var_ok, drop = FALSE]
}

###############################
# PCA opcional
###############################
if (usar_pca) {
  pca <- prcomp(X, center = TRUE, scale. = TRUE)
  var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k_comp <- which(var_exp >= varianza_objetivo)[1]
  if (is.na(k_comp)) k_comp <- length(var_exp)
  message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza explicada)", 
                  k_comp, 100 * var_exp[k_comp]))
  X_for_clust <- pca$x[, 1:k_comp, drop = FALSE]
} else {
  X_for_clust <- scale(X)
}

# Sustituir posibles valores no finitos
X_for_clust[!is.finite(as.matrix(X_for_clust))] <- 0

###############################
# Modelo GMM (Mclust)
###############################

set.seed(123)

# Ajustar el modelo con número óptimo de clusters (BIC)
gmm_fit <- Mclust(X_for_clust, G = k_min:k_max)

cat(sprintf("\nNúmero óptimo de clusters (según BIC): %d\n", gmm_fit$G))
print(summary(gmm_fit))

###############################
# Calcular Silhouette
###############################

clusters <- gmm_fit$classification
D <- dist(X_for_clust, method = "euclidean")

# Silhouette general
sil <- silhouette(clusters, D)
mean_sil <- mean(sil[, "sil_width"])
cat(sprintf("\nÍndice de Silhouette promedio: %.3f\n", mean_sil))

# Mostrar gráfico de silhouette
fviz_silhouette(sil) +
  ggtitle(sprintf("Silhouette - GMM (k = %d, índice medio = %.3f)", gmm_fit$G, mean_sil))

###############################
# Visualizaciones de GMM
###############################

# Gráfico BIC (elección de número de clusters)
fviz_mclust_bic(gmm_fit) + 
  ggtitle("Selección de número óptimo de clusters (BIC)")

# Visualización de clasificación
fviz_mclust(gmm_fit, "classification") +
  ggtitle(sprintf("GMM - Clasificación (k = %d)", gmm_fit$G))

# Densidades en componentes principales
fviz_mclust(gmm_fit, "density") +
  ggtitle("Densidad por cluster (GMM)")

###############################
# Guardar resultados
###############################

resultado <- data.frame(Objeto = rownames(X_for_clust), Cluster = clusters, row.names = NULL)
write.csv(resultado, "Clusters_resultado_GMM.csv", row.names = FALSE)

cat("\nResumen de asignaciones:\n")
print(table(resultado$Cluster))

head(resultado)
