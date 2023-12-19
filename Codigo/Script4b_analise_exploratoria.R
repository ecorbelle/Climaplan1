## Código para cruzar os grupos de municipios con variables explicativas
## Eduardo Corbelle, 19 decembro 2023

library(sf)

## Carga de datos ----
# Resultados da análise de conglomerados
datos0 <- st_read("Resultados/concellos_clasif.gpkg") |> st_drop_geometry() |> 
  unique() |> 
  subset(select = c("codigoine", "grupo"))
# Variables explicativas 
datos1 <- read.csv("Temp/Variables_explicativas.csv", skip = 7, sep = ";")
# Unión 
datos <- merge(datos0, datos1, by = "codigoine")
rm(datos0, datos1)


## Diagramas de caixas ----
pdf("Temp/Exploracion.pdf")
for (i in 4:ncol(datos)) {
  boxplot(datos[[i]] ~ datos$grupo,
          xlab = "Grupo de municipios",
          ylab = names(datos)[i],
          main = names(datos)[i])
  }
dev.off()
