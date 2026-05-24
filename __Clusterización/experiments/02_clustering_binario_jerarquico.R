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

metrica_distancia <- "manhattan"

metodo_clustering <- "hclust"

linkage_hclust <- "average"

k_min <- 3
k_max <- 12

usar_pca <- FALSE
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

# Primera fila a ID
tiene_id <- !is.numeric(df[[1]])
if (tiene_id) {
  ids <- df[[1]]
  X <- df[, -1, drop = FALSE]
  rownames(X) <- make.names(ids, unique = TRUE)
} else {
  X <- df
}

# Aseguramos que las columnas sean int y no str
X[] <- lapply(X, function(x) as.numeric(as.character(x)))
X[is.na(X)] <- 0

#################
# PCA (robusto)
#################
if (usar_pca) {
  # 1) Quitar columnas con varianza 0 (imprescindible en binario)
  var_ok <- apply(X, 2, sd, na.rm = TRUE) > 0
  if (!all(var_ok)) {
    message(sprintf("PCA: eliminadas %d columnas de varianza cero.", sum(!var_ok)))
  }
  X_pca_input <- X[, var_ok, drop = FALSE]
  
  # 2) Si tras filtrar no queda ninguna columna, aborta el PCA con gracia
  if (ncol(X_pca_input) == 0) {
    warning("PCA: no hay columnas con varianza > 0. Se continúa sin PCA.")
    X_for_dist <- X
  } else {
    # 3) PCA sin doble escalado: NO hagas scale(X) + scale.=TRUE a la vez
    pca <- prcomp(X_pca_input, center = TRUE, scale. = TRUE)
    
    # 4) Elegir nº de componentes por varianza acumulada
    var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
    k_comp <- which(var_exp >= varianza_objetivo)[1]
    if (is.na(k_comp)) k_comp <- length(var_exp)
    
    message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza)",
                    k_comp, 100 * var_exp[k_comp]))
    
    # 5) Coordenadas en espacio PCA
    X_for_dist <- pca$x[, 1:k_comp, drop = FALSE]
  }
} else {
  X_for_dist <- X
}


#################
# Distancia
#################

distancia_custom <- function(M, metodo = "jaccard", binario = TRUE) {
  metodo <- tolower(metodo)
  if (metodo %in% c("euclidean", "manhattan")) {
    return(dist(M, method = metodo))
  } else if (metodo == "jaccard") {
    
    return(proxy::dist(M, method = "Jaccard", diag = FALSE, upper = FALSE, by_rows = TRUE, 
                       
    ))
  } else if (metodo == "hamming") {
    # Hamming normalizada: proporción de posiciones distintas (0..1)
    ham_fun <- function(x, y) mean(x != y)
    return(proxy::dist(M, method = ham_fun))
  } else if (metodo == "cosine") {
    return(proxy::dist(M, method = "cosine"))
  } else if (metodo == "correlation") {
    
    C <- stats::cor(t(M), method = "pearson")
    D <- as.dist(1 - C)
    return(D)
  } else if (metodo == "mahalanobis") {
    
    S <- cov(M)
    S_inv <- tryCatch(solve(S), error = function(e) MASS::ginv(S))
    M_whiten <- as.matrix(scale(M, center = TRUE, scale = FALSE)) %*% chol(S_inv)
    return(dist(M_whiten, method = "euclidean"))
  } else {
    stop("Métrica de distancia no reconocida.")
  }
}

D <- distancia_custom(X_for_dist, metodo = metrica_distancia, binario = datos_binarios)

#################
# Modelo
#################

evaluar_k_por_silhouette <- function(D, metodo = "hclust", linkage = "ward.D2", k_min = 3, k_max = 10, X_input = NULL) {
  res <- data.frame(k = integer(), silhouette = numeric(), stringsAsFactors = FALSE)
  for (k in k_min:k_max) {
    if (metodo == "hclust") {
      fit <- hclust(D, method = linkage)
      grupos <- cutree(fit, k = k)
      sil <- cluster::silhouette(grupos, D)
      mean_s <- mean(sil[, "sil_width"])
    } else if (metodo == "kmeans") {
      if (is.null(X_input)) stop("Para kmeans, necesitas X_input (no D).")
      set.seed(123)
      km <- kmeans(X_input, centers = k, nstart = 25)
      sil <- cluster::silhouette(km$cluster, D)
      mean_s <- mean(sil[, "sil_width"])
    } else if (metodo == "pam") {
      pam_fit <- cluster::pam(D, k = k, diss = TRUE)
      sil <- cluster::silhouette(pam_fit$clustering, D)
      mean_s <- mean(sil[, "sil_width"])
    } else {
      stop("Método de clustering no reconocido.")
    }
    res <- rbind(res, data.frame(k = k, silhouette = mean_s))
  }
  return(res)
}

# Para k-means necesitamos X_for_dist; para hclust/pam basta con D

eval <- evaluar_k_por_silhouette(D,
                                 metodo = metodo_clustering,
                                 linkage = linkage_hclust,
                                 k_min = k_min, k_max = k_max,
                                 X_input = if (metodo_clustering == "kmeans") X_for_dist else NULL)

print(eval)

k_opt <- eval$k[which.max(eval$silhouette)]
(k_opt)

#################
# Visualizaciones
#################

# Ajustar con k_opt y dendograma
if (metodo_clustering == "hclust") {
  fit <- hclust(D, method = linkage_hclust)
  grupos <- cutree(fit, k = k_opt)
  plot(fit, cex = 0.6, main = sprintf("Dendrograma - %s (%s)", metrica_distancia, linkage_hclust))
  rect.hclust(fit, k = k_opt, border = 2:6)
} else if (metodo_clustering == "kmeans") {
  set.seed(123)
  km <- kmeans(X_for_dist, centers = k_opt, nstart = 25)
  grupos <- km$cluster
} else if (metodo_clustering == "pam") {
  pam_fit <- cluster::pam(D, k = k_opt, diss = TRUE)
  grupos <- pam_fit$clustering
}

resultado <- data.frame(Objeto = rownames(X_for_dist), Cluster = grupos, row.names = NULL)
head(resultado)

# Silhouette plot
sil <- silhouette(grupos, D)
fviz_silhouette(sil) + ggtitle(sprintf("Silhouette (%s, %s)", metodo_clustering, metrica_distancia))

resultado <- data.frame(Objeto = rownames(X_for_dist), Cluster = grupos, row.names = NULL)
head(resultado)

# MDS clásico sobre D para dibujar en 2D (independiente de PCA)
mds <- cmdscale(D, k = 2, eig = TRUE)
plot(mds$points, col = grupos, pch = 19,
     main = sprintf("MDS 2D sobre %s + %s", metrica_distancia, metodo_clustering),
     xlab = "Dim 1", ylab = "Dim 2")
legend("topright", legend = sort(unique(grupos)), col = sort(unique(grupos)), pch = 19, title = "Cluster")

if (usar_som) {
  X_som <- scale(X)
  grid <- somgrid(xdim = som_grid_x, ydim = som_grid_y, topo = "hexagonal")
  set.seed(123)
  som_fit <- som(as.matrix(X_som), grid = grid, rlen = som_rlen, keep.data = TRUE)
  plot(som_fit, type = "changes", main = "SOM - cambios de entrenamiento")
  plot(som_fit, type = "dist.neighbours", main = "SOM - U-Matrix")
  plot(som_fit, type = "count", main = "SOM - densidad por nodo")
  
  codebooks <- som_fit$codes[[1]]
  set.seed(123)
  km_cb <- kmeans(codebooks, centers = som_k_clusters, nstart = 25)
  clusters_nodos <- km_cb$cluster
  clusters_som <- clusters_nodos[som_fit$unit.classif]  # cluster por individuo
  # Comparar con clustering previo si quieres
  tabla_comp <- table(Previo = grupos, SOM = clusters_som)
  print(tabla_comp)
  
  # Mapa con clusters de nodos
  plot(som_fit, type = "mapping", bgcol = clusters_nodos[som_fit$unit.classif],
       main = "SOM - mapping (clusters de nodos)")
}

write.csv(resultado, "Clusters_resultado.csv", row.names = FALSE)

