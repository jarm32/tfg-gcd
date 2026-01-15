# Junta los embeddings de las keywords con los investigadores
# Solo coge las comunes entre ambos archivos --> por si acaso se han quitado las no
# comunes con antelación

library(readxl)
library(dplyr)
library(proxy)
library(cluster)
library(factoextra)

#setwd("C:/Code/tfg-gcd/_Clusterization/Sin_Locl/Investigadores y keywords")

M <- as.data.frame(readxl::read_excel("Matriz_Investigadores_Keywords_sinlocl.xlsx"))
rownames(M) <- M[[1]]
M <- M[,-1]
M[is.na(M)] <- 0

E <- read.csv("Embeddings_keywords_sinlocl.csv", row.names = 1)
E <- as.matrix(E)

common_keywords <- intersect(colnames(M), rownames(E))
message(sprintf("Coinciden %d keywords entre embeddings y matriz original.", length(common_keywords)))

M <- M[, common_keywords]
E <- E[common_keywords, ]

X <- as.matrix(M)
investigadores_vec <- X %*% E   # (investigadores × keywords) × (keywords × dims)
investigadores_vec <- scale(investigadores_vec)  # opcional

write.csv(investigadores_vec, "Matriz_Investigadores_Embeddings_sinlocl.csv", row.names = TRUE)
message("Matriz de embeddings de investigadores guardada correctamente.")

