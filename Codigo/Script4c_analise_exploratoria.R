# Guión para avaliar os grupos de municipios en función da súa aptitude produtiva para diferentes cultivos
# Eduardo Corbelle, 29 xaneiro 2024

library(sf)
library(data.table)
library(ggplot2)
library(grid)

# Carga de datos ----
# Resultados da análise de conglomerados
datos0 <- st_read("Resultados/concellos_clasif.gpkg") |> st_drop_geometry() |> 
  unique() |> 
  subset(select = c("codigoine", "grupo")) |> data.table()

# Variables de aptitude
apmillo <- read.csv("Datos/Aux/ap_millo.txt", sep = "|", header = FALSE,
                    col.names = c("codigoine", "nome", "clase", "label", "area")) |> data.table()
apprado <- read.csv("Datos/Aux/ap_prado.txt", sep = "|", header = FALSE,
                    col.names = c("codigoine", "nome", "clase", "label", "area")) |> data.table()
appinas <- read.csv("Datos/Aux/ap_pinaster.txt", sep = "|", header = FALSE,
                    col.names = c("codigoine", "nome", "clase", "label", "area")) |> data.table()
apglobu <- read.csv("Datos/Aux/ap_globulus.txt", sep = "|", header = FALSE,
                    col.names = c("codigoine", "nome", "clase", "label", "area")) |> data.table()

# Resumo das variables de aptitude
resumo <- function(datos, casos, nome) {
  datos2 = merge(datos[ clase %in% casos, .(apta  = sum(area)), .(codigoine)],
                 datos[                  , .(total = sum(area)), .(codigoine)])
  datos3 = datos2[ , .(codigoine, prop = 100*apta/total)]
  colnames(datos3) = c("codigoine", nome)
  return(datos3)
}

apmillo2 <- resumo(apmillo, 1:3, "prop.millo")
apprado2 <- resumo(apprado, 1:3, "prop.prado")  
appinas2 <- resumo(appinas, 1:3, "prop.pinas")
apglobu2 <- resumo(apglobu, 1:4, "prop.globu")

temp1 <- merge(apmillo2, apprado2, all = TRUE)
temp2 <- merge(appinas2, apglobu2, all = TRUE)
datos1 <- merge(temp1, temp2, all = TRUE)
write.csv(datos1, "Temp/Apt_biofisica.csv")

# Unión 
datos <- merge(datos0, datos1, by = "codigoine", all = TRUE)
datos$grupo <- factor(datos$grupo)
rm(apmillo, apmillo2, apprado, apprado2, appinas, appinas2, apglobu, apglobu2, temp1, temp2, datos0, datos1)


# Exploración

for (i in 3:6) {
boxplot(datos[[i]] ~ datos$grupo, main = colnames(datos)[i])
}

# Aptitude para a produción de millo
datos[, .(prop.millo = median(prop.millo, na.rm = TRUE),
          prop.prado = median(prop.prado, na.rm = TRUE),
          prop.pinas = median(prop.pinas, na.rm = TRUE),
          prop.globu = median(prop.globu, na.rm = TRUE)),
      keyby = grupo ]
datos[, .(prop.millo = mad(prop.millo, na.rm = TRUE),
          prop.prado = mad(prop.prado, na.rm = TRUE),
          prop.pinas = mad(prop.pinas, na.rm = TRUE),
          prop.globu = mad(prop.globu, na.rm = TRUE)),
      keyby = grupo ]

# kruskal-wallis ----

for (i in 3:6) {
  print("######################################")
  print(names(datos)[i])
  print(kruskal.test(datos[[i]], datos$grupo))
}
