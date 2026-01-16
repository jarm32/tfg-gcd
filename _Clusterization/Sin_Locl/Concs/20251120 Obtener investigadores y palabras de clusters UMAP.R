library(readxl)
library(openxlsx)
library(dplyr)

###############################################
# 1. Cargar clusters UMAP k = 4
###############################################

setwd("C:/Code/tfg-gcd/_Clusterization/Sin_Locl/Concs")

clusters <- read.csv(
  "UMAP_Resultados_k4/UMAP_Clusters_k4.csv",
  stringsAsFactors = FALSE
)

###############################################
# 2. Cargar matriz OHE
###############################################

ohe <- read_excel("Matriz_Investigadores_Keywords_sinlocl.xlsx")
ohe <- as.data.frame(ohe)

rownames(ohe) <- ohe[[1]]
ohe <- ohe[, -1, drop = FALSE]

ohe[] <- lapply(ohe, function(x) as.numeric(as.character(x)))
ohe[is.na(ohe)] <- 0

###############################################
# 3. Crear Excel igual al ejemplo
###############################################

wb <- createWorkbook()

clusters_ids <- sort(unique(clusters$Cluster))

for (k in clusters_ids) {
  
  addWorksheet(wb, paste0("Cluster ", k))
  
  # ---- INVESTIGADORES ----
  investigadores <- clusters$Investigador[clusters$Cluster == k]
  df_invest <- data.frame(Investigador = investigadores)
  
  # ---- KEYWORDS ----
  submat <- ohe[investigadores, , drop = FALSE]
  frec <- sort(colSums(submat), decreasing = TRUE)
  frec <- frec[frec > 0]
  
  df_keywords <- data.frame(
    Keyword = names(frec),
    Frecuencia = as.numeric(frec),
    stringsAsFactors = FALSE
  )
  
  # ---- ESCRIBIR EN LA HOJA ----
  
  writeData(wb, sheet = paste0("Cluster ", k),
            x = data.frame("Investigadores" = ""), startRow = 1, colNames = FALSE)
  writeData(wb, sheet = paste0("Cluster ", k),
            x = df_invest, startRow = 2, colNames = TRUE)
  
  fila_keywords <- nrow(df_invest) + 4
  writeData(wb, sheet = paste0("Cluster ", k),
            x = data.frame("Keywords" = ""), startRow = fila_keywords, colNames = FALSE)
  
  writeData(wb, sheet = paste0("Cluster ", k),
            x = df_keywords, startRow = fila_keywords + 1, colNames = TRUE)
}

###############################################
# 4. Guardar Excel
###############################################

saveWorkbook(wb, "UMAP_Clusters_Resultados.xlsx", overwrite = TRUE)

cat("\n=== Excel generado correctamente: UMAP_Clusters_Resultados.xlsx ===\n")

