# =========================
# 0) Paquetes
# =========================
libs <- c("readxl", "dplyr", "proxy", "cluster", "factoextra", "kohonen", "ggplot2")
to_install <- libs[!libs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(libs, library, character.only = TRUE))

# =========================
# 1) Parámetros (ajústalos)
# =========================
ruta_excel <- "Matriz_Investigadores_Keywords.xlsx"  # nombre del archivo
hoja <- 1                                            # o el nombre de la hoja

# ¿Datos binarios (0/1) o conteos? (sirve para decisiones de distancia)
datos_binarios <- TRUE

# Distancias disponibles: "euclidean","manhattan","jaccard","hamming",
#                         "cosine","correlation","mahalanobis"
metrica_distancia <- "jaccard"

# Clustering: "hclust", "kmeans", "pam"
metodo_clustering <- "hclust"
linkage_hclust <- "ward.D2"   # "single","complete","average","ward.D2", etc.

# Rango de k a evaluar
k_min <- 2; k_max <- 12

# PCA previo (TRUE/FALSE) y porcentaje varianza a retener
usar_pca <- FALSE
varianza_objetivo <- 0.9  # 90%

# SOM (opcional)
usar_som <- FALSE
som_grid_x <- 10; som_grid_y <- 10   # tamaño del mapa
som_rlen <- 100                      # iteraciones de entrenamiento
som_k_clusters <- 6                  # nº de clusters sobre codebooks SOM

# =========================
# 2) Carga y preparación
# =========================
# Se asume que cada fila es un investigador y cada columna una keyword (0/1 o conteos)
raw <- readxl::read_excel(ruta_excel, sheet = hoja)
df <- as.data.frame(raw)

# Detectar columna de identificadores si existe (ej. nombre del investigador)
# Si tu excel tiene una primera columna con nombres/IDs, sepárala aquí:
tiene_id <- !is.numeric(df[[1]])
if (tiene_id) {
  ids <- df[[1]]
  X <- df[, -1, drop = FALSE]
  rownames(X) <- make.names(ids, unique = TRUE)
} else {
  X <- df
}

# Aseguramos numérico
X[] <- lapply(X, function(x) as.numeric(as.character(x)))
X[is.na(X)] <- 0

# =========================
# 3) (Opcional) PCA previo
# =========================
if (usar_pca) {
  # Para PCA conviene estandarizar si las escalas difieren
  X_scale <- scale(X)
  pca <- prcomp(X_scale, center = TRUE, scale. = TRUE)
  var_exp <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k_comp <- which(var_exp >= varianza_objetivo)[1]
  message(sprintf("PCA: reteniendo %d componentes (%.1f%% varianza)", k_comp, 100*var_exp[k_comp]))
  X_pca <- pca$x[, 1:k_comp, drop = FALSE]
  X_for_dist <- X_pca
} else {
  X_for_dist <- X
}

# =========================
# 4) Funciones de distancia
# =========================
distancia_custom <- function(M, metodo = "jaccard", binario = TRUE) {
  metodo <- tolower(metodo)
  if (metodo %in% c("euclidean", "manhattan")) {
    return(dist(M, method = metodo))
  } else if (metodo == "jaccard") {
    # proxy::dist maneja Jaccard; para binario suele ser lo correcto
    return(proxy::dist(M, method = "Jaccard", diag = FALSE, upper = FALSE, by_rows = TRUE, 
                       # para binario estricto:
                       # Nota: proxy::dist(Jaccard) asume binarización internamente
    ))
  } else if (metodo == "hamming") {
    return(proxy::dist(M, method = "Hamming"))
  } else if (metodo == "cosine") {
    return(proxy::dist(M, method = "cosine"))
  } else if (metodo == "correlation") {
    # Distancia = 1 - correlación de Pearson entre filas
    # cor espera variables en columnas → correlación entre filas => transponer
    C <- stats::cor(t(M), method = "pearson")
    D <- as.dist(1 - C)
    return(D)
  } else if (metodo == "mahalanobis") {
    # Mahalanobis por pares: blanqueo + distancia euclídea
    S <- cov(M)
    S_inv <- tryCatch(solve(S), error = function(e) MASS::ginv(S))
    M_whiten <- as.matrix(scale(M, center = TRUE, scale = FALSE)) %*% chol(S_inv)
    return(dist(M_whiten, method = "euclidean"))
  } else {
    stop("Métrica de distancia no reconocida.")
  }
}

D <- distancia_custom(X_for_dist, metodo = metrica_distancia, binario = datos_binarios)

# =========================
# 5) Clustering + evaluación
# =========================
evaluar_k_por_silhouette <- function(D, metodo = "hclust", linkage = "ward.D2", k_min = 2, k_max = 10, X_input = NULL) {
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

# Para k-means necesitamos X_for_dist (coordenadas); para hclust/pam basta con D
eval <- evaluar_k_por_silhouette(D,
                                 metodo = metodo_clustering,
                                 linkage = linkage_hclust,
                                 k_min = k_min, k_max = k_max,
                                 X_input = if (metodo_clustering == "kmeans") X_for_dist else NULL)

print(eval)
k_opt <- eval$k[which.max(eval$silhouette)]
message(sprintf("k óptimo por silhouette ≈ %d (valor=%.3f)", k_opt, max(eval$silhouette)))

# Ajustar modelo final con k_opt
if (metodo_clustering == "hclust") {
  fit <- hclust(D, method = linkage_hclust)
  grupos <- cutree(fit, k = k_opt)
  # Dendrograma
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

# Silhouette plot
sil <- silhouette(grupos, D)
fviz_silhouette(sil) + ggtitle(sprintf("Silhouette (%s, %s)", metodo_clustering, metrica_distancia))

# Resultados
resultado <- data.frame(Objeto = rownames(X_for_dist), Cluster = grupos, row.names = NULL)
head(resultado)

# =========================
# 6) Visualizaciones 2D
# =========================
# MDS clásico sobre D para dibujar en 2D (independiente de PCA)
mds <- cmdscale(D, k = 2, eig = TRUE)
plot(mds$points, col = grupos, pch = 19,
     main = sprintf("MDS 2D sobre %s + %s", metrica_distancia, metodo_clustering),
     xlab = "Dim 1", ylab = "Dim 2")
legend("topright", legend = sort(unique(grupos)), col = sort(unique(grupos)), pch = 19, title = "Cluster")

# =========================
# 7) (Opcional) SOM
# =========================
if (usar_som) {
  # Escalado recomendado para SOM
  X_som <- scale(X)
  grid <- somgrid(xdim = som_grid_x, ydim = som_grid_y, topo = "hexagonal")
  set.seed(123)
  som_fit <- som(as.matrix(X_som), grid = grid, rlen = som_rlen, keep.data = TRUE)
  plot(som_fit, type = "changes", main = "SOM - cambios de entrenamiento")
  plot(som_fit, type = "dist.neighbours", main = "SOM - U-Matrix")
  plot(som_fit, type = "count", main = "SOM - densidad por nodo")
  
  # Clusterizar los codebooks del SOM
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

# =========================
# 8) Guardar resultados
# =========================
write.csv(resultado, "clusters_resultado.csv", row.names = FALSE)
message("Resultados guardados en clusters_resultado.csv")
