library("readxl")
library("dplyr")
library("proxy")
library("cluster")
library("factoextra")
library("kohonen")
library("ggplot2")

#################
# Parámetros
#################

datos_binarios <- TRUE

metrica_distancia <- "euclidean"   # K-means usa euclídea
metodo_clustering <- "kmeans"

k_min <- 15
k_max <- 25

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

ruta_excel <- "Matriz_Investigadores_Keywords.xlsx"
raw <- readxl::read_excel(ruta_excel)
df <- as.data.frame(raw)

# Primera columna como ID si no es numérica
tiene_id <- !is.numeric(df[[1]])
if (tiene_id) {
  ids <- df[[1]]
  X <- df[, -1, drop = FALSE]
  rownames(X) <- make.names(ids, unique = TRUE)
} else {
  X <- df
}

# Aseguramos que todas las columnas sean numéricas
X[] <- lapply(X, function(x) as.numeric(as.character(x)))
X[is.na(X)] <- 0

#################
# Limpieza robusta antes del clustering
#################

# Eliminar columnas con varianza 0
var_ok <- apply(X, 2, sd, na.rm = TRUE) > 0
if (!all(var_ok)) {
  message(sprintf("Eliminadas %d columnas con varianza cero.", sum(!var_ok)))
  X <- X[, var_ok, drop = FALSE]
}

# Sustituir posibles NA, NaN o Inf por 0
X[!is.finite(as.matrix(X))] <- 0

#################
# PCA (opcional y robusto)
#################

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

# Reemplazar cualquier NA/NaN/Inf resultante de PCA o scale
X_for_clust[!is.finite(as.matrix(X_for_clust))] <- 0

#################
# Distancia (para silhouette)
#################

D <- dist(X_for_clust, method = metrica_distancia)

#################
# Evaluación de k (por silhouette)
#################

evaluar_k_por_silhouette <- function(X_input, D, k_min = 3, k_max = 10) {
  res <- data.frame(k = integer(), silhouette = numeric(), stringsAsFactors = FALSE)
  for (k in k_min:k_max) {
    set.seed(123)
    km <- kmeans(X_input, centers = k, nstart = 25)
    sil <- cluster::silhouette(km$cluster, D)
    mean_s <- mean(sil[, "sil_width"])
    res <- rbind(res, data.frame(k = k, silhouette = mean_s))
  }
  return(res)
}

eval <- evaluar_k_por_silhouette(X_for_clust, D, k_min, k_max)
print(eval)

k_opt <- eval$k[which.max(eval$silhouette)]
k_opt

#################
# Modelo final
#################

set.seed(123)
km_final <- kmeans(X_for_clust, centers = k_opt, nstart = 25)
grupos <- km_final$cluster

#################
# Visualizaciones
#################

# Silhouette plot
sil <- silhouette(grupos, D)
fviz_silhouette(sil) + ggtitle(sprintf("Silhouette - K-means (k = %d)", k_opt))

# Visualización PCA o MDS
if (usar_pca) {
  fviz_cluster(km_final, data = X_for_clust,
               geom = "point", ellipse.type = "norm") +
    ggtitle(sprintf("K-means (k = %d) sobre PCA", k_opt))
} else {
  mds <- cmdscale(D, k = 2)
  plot(mds, col = grupos, pch = 19,
       main = sprintf("K-means (k = %d) - MDS 2D", k_opt),
       xlab = "Dim 1", ylab = "Dim 2")
  legend("topright", legend = sort(unique(grupos)), 
         col = sort(unique(grupos)), pch = 19, title = "Cluster")
}

#################
# Guardar resultados
#################

resultado <- data.frame(Objeto = rownames(X_for_clust), Cluster = grupos, row.names = NULL)
write.csv(resultado, "Clusters_resultado_kmeans.csv", row.names = FALSE)

head(resultado)
