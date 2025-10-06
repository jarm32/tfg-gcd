library("readxl")
library("dplyr")
library("cluster")
library("factoextra")
library("ggplot2")

#################
# Parámetros
#################

datos_binarios <- TRUE

metrica_distancia <- "euclidean"   # Usaremos euclídea
metodo_clustering <- "kmeans"      # K-means
k_min <- 3
k_max <- 12

usar_pca <- TRUE
varianza_objetivo <- 0.9

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

# Aseguramos que las columnas sean numéricas
X[] <- lapply(X, function(x) as.numeric(as.character(x)))
X[is.na(X)] <- 0

#################
# PCA (opcional)
#################

if (usar_pca) {
  X_scale <- scale(X)
  pca <- prcomp(X_scale, center = TRUE, scale. = TRUE)
  var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k_comp <- which(var_exp >= varianza_objetivo)[1]
  message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza)", 
                  k_comp, 100 * var_exp[k_comp]))
  X_for_dist <- pca$x[, 1:k_comp, drop = FALSE]
} else {
  X_for_dist <- X
}

#################
# Distancia euclídea
#################

D <- dist(X_for_dist, method = "euclidean")

#################
# Evaluación de k (Silhouette)
#################

evaluar_k_por_silhouette <- function(X_input, D, k_min = 3, k_max = 10) {
  res <- data.frame(k = integer(), silhouette = numeric(), stringsAsFactors = FALSE)
  for (k in k_min:k_max) {
    set.seed(123)
    km <- kmeans(X_input, centers = k, nstart = 50, iter.max = 100)
    sil <- silhouette(km$cluster, D)
    mean_s <- mean(sil[, "sil_width"])
    res <- rbind(res, data.frame(k = k, silhouette = mean_s))
  }
  return(res)
}

eval <- evaluar_k_por_silhouette(X_for_dist, D, k_min, k_max)
print(eval)

k_opt <- eval$k[which.max(eval$silhouette)]
cat("Mejor número de clusters:", k_opt, "\n")

#################
# Modelo final K-means
#################

set.seed(123)
km <- km <- kmeans(X_for_dist, centers = k_opt, nstart = 50, iter.max = 100)
grupos <- km$cluster

resultado <- data.frame(Objeto = rownames(X_for_dist), Cluster = grupos)
write.csv(resultado, "Clusters_resultado.csv", row.names = FALSE)

#################
# Visualizaciones
#################

# Gráfico silhouette
sil <- silhouette(grupos, D)
fviz_silhouette(sil) + 
  ggtitle(sprintf("Silhouette (K-means, %s)", metrica_distancia))

# Visualización MDS
mds <- cmdscale(D, k = 2, eig = TRUE)
plot(mds$points, col = grupos, pch = 19,
     main = sprintf("MDS 2D - K-means (%s)", metrica_distancia),
     xlab = "Dim 1", ylab = "Dim 2")
legend("topright", legend = sort(unique(grupos)), col = sort(unique(grupos)), 
       pch = 19, title = "Cluster")
