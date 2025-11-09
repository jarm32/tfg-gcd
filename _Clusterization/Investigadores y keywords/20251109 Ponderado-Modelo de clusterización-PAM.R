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

# MÉTRICAS A PROBAR -->  "manhattan", "jaccard", "cosine", "hamming", "gower"
metrica_distancia <- "gower"

metodo_clustering <- "pam"

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

X[] <- lapply(X, function(x) as.numeric(as.character(x)))
X[is.na(X)] <- 0

freq <- rowSums(X)                     
mu <- mean(freq)                       
sigma <- sd(freq) * 1                  

# Ponderación gaussiana
weights <- exp(-((freq - mu)^2) / (2 * sigma^2))

weights <- (weights - min(weights)) / (max(weights) - min(weights))
X_weighted <- X * weights
X <- X_weighted

message("Distribución de pesos aplicada:")
print(summary(weights))

#################
# PCA (opcional)
#################
if (usar_pca) {
  var_ok <- apply(X, 2, sd, na.rm = TRUE) > 0
  X_pca_input <- X[, var_ok, drop = FALSE]
  
  if (ncol(X_pca_input) > 0) {
    pca <- prcomp(X_pca_input, center = TRUE, scale. = TRUE)
    var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
    k_comp <- which(var_exp >= varianza_objetivo)[1]
    X_for_dist <- pca$x[, 1:k_comp, drop = FALSE]
    message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza)",
                    k_comp, 100 * var_exp[k_comp]))
  } else {
    X_for_dist <- X
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
    return(proxy::dist(M, method = "Jaccard", diag = FALSE, upper = FALSE))
  } else if (metodo == "hamming") {
    ham_fun <- function(x, y) mean(x != y)
    return(proxy::dist(M, method = ham_fun))
  } else if (metodo == "cosine") {
    return(proxy::dist(M, method = "cosine"))
  } else if (metodo == "gower") {
    return(cluster::daisy(M, metric = "gower"))
  } else {
    stop("Métrica no reconocida.")
  }
}

D <- distancia_custom(X_for_dist, metodo = metrica_distancia, binario = datos_binarios)

#################
# Evaluar número de clusters (PAM)
#################
evaluar_k_por_silhouette <- function(D, metodo = "pam", k_min = 3, k_max = 10) {
  res <- data.frame(k = integer(), silhouette = numeric(), stringsAsFactors = FALSE)
  for (k in k_min:k_max) {
    if (metodo == "pam") {
      pam_fit <- cluster::pam(D, k = k, diss = TRUE)
      sil <- cluster::silhouette(pam_fit$clustering, D)
      mean_s <- mean(sil[, "sil_width"])
    }
    res <- rbind(res, data.frame(k = k, silhouette = mean_s))
  }
  return(res)
}

eval <- evaluar_k_por_silhouette(D, metodo = "pam", k_min = k_min, k_max = k_max)
print(eval)

k_opt <- eval$k[which.max(eval$silhouette)]
k_opt

#################
# Modelo final PAM
#################
pam_fit <- cluster::pam(D, k = k_opt, diss = TRUE)
grupos <- pam_fit$clustering

resultado <- data.frame(Objeto = rownames(X_for_dist), Cluster = grupos, row.names = NULL)
write.csv(resultado, "Clusters_resultado_PAM.csv", row.names = FALSE)

#################
# Visualizaciones
#################
sil <- silhouette(grupos, D)
fviz_silhouette(sil) + ggtitle(sprintf("PAM - %s", metrica_distancia))

# MDS clásico para visualizar en 2D
mds <- cmdscale(D, k = 2, eig = TRUE)
plot(mds$points, col = grupos, pch = 19,
     main = sprintf("PAM - %s (k=%d)", metrica_distancia, k_opt),
     xlab = "Dim 1", ylab = "Dim 2")
legend("topright", legend = sort(unique(grupos)), col = sort(unique(grupos)),
       pch = 19, title = "Cluster")

plot(freq, weights, pch = 19, col = "steelblue",
     main = "Ponderación gaussiana simétrica de investigadores",
     xlab = "Frecuencia (keywords por investigador)", ylab = "Peso aplicado")
abline(v = mu, col = "red", lwd = 2, lty = 2)

