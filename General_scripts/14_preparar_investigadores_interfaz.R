library(readr)
library(dplyr)

invest <- read.csv2("D:/Code/tfg-gcd/Used-code/Investigadores_internos.csv", fileEncoding = "latin1")

invest_publico <- invest %>%
  select(Nombre, Acortacion, Acronimo, Girado, Tipo.de.empleado)

write.csv2(
  invest_publico,
  "D:/Code/tfg-gcd/Used-code/Investigadores_internos.csv",
  row.names = FALSE,
  fileEncoding = "latin1"
)

