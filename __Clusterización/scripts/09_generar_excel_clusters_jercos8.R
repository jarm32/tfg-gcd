library("openxlsx")

###############################################
# Cargar datos
###############################################

# Matriz OHE Investigadores × Keywords
ohe <- readxl::read_excel("../data/Matriz_Investigadores_Keywords_sinlocl.xlsx")
ohe <- as.data.frame(ohe)

# Convertir primera columna en rownames
rownames(ohe) <- ohe[[1]]
ohe <- ohe[, -1, drop = FALSE]

# Asegurar que todas las columnas son numéricas 0/1
ohe[] <- lapply(ohe, function(x) as.numeric(as.character(x)))
ohe[is.na(ohe)] <- 0

# Cargar la asignación de clusters del modelo sin UMAP
resultado <- read.csv("../results/Clusters_resultado_JerCos8.csv")
colnames(resultado) <- c("Investigador", "Cluster")

###############################################
# Obtener investigadores por cluster
###############################################

clusters_investigadores <- split(resultado$Investigador, resultado$Cluster)

###############################################
# Obtener keywords representativas por cluster
###############################################

keywords_por_cluster <- list()

for (k in names(clusters_investigadores)) {
  
  inv <- clusters_investigadores[[k]]
  submatriz <- ohe[inv, , drop = FALSE]
  
  # Suma de keywords dentro del cluster
  frec <- colSums(submatriz)
  frec <- sort(frec, decreasing = TRUE)
  
  # Guardar en lista
  keywords_por_cluster[[k]] <- frec[frec > 0]
}

# Crear workbook
wb <- createWorkbook()

################################################
# Exportar INVESTIGADORES por cluster
################################################
for (k in names(clusters_investigadores)) {
  
  addWorksheet(wb, paste0("Cluster_", k, "_investigadores"))
  
  df_inv <- data.frame(Investigador = clusters_investigadores[[k]])
  writeData(wb, paste0("Cluster_", k, "_investigadores"), df_inv)
}

################################################
# Exportar KEYWORDS por cluster
################################################
for (k in names(keywords_por_cluster)) {
  
  addWorksheet(wb, paste0("Cluster_", k, "_keywords"))
  
  frec <- keywords_por_cluster[[k]]
  df_kw <- data.frame(
    Keyword = names(frec),
    Frecuencia = as.numeric(frec)
  )
  
  writeData(wb, paste0("Cluster_", k, "_keywords"), df_kw)
}

################################################
# Guardar Excel
################################################
saveWorkbook(wb, "../results/Clusters_resultado_JerCos8.xlsx", overwrite = TRUE)


