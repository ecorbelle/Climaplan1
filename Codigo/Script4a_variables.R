## Código para producir unha base de datos con variables socioeconómicas
## Pensada para ter valores municipais que producir en gráficos
## Eduardo Corbelle, 18 decembro 2023

library(readxl)

# A) Carga de datos ----
# (Na maioría da importación, asumimos que a orde de presentación é sempre a mesma)

## 1. Variables demográficas ----

### Densidade de poboación 2022 ----
datos1 <- read_excel("Datos/1 Demográficas.xls", 
                     sheet = "Densidade poboación 2022",
                     range = "a6:c318",
                     col_names = c("codigoine", "concello", "dens.pob.2022")) |> 
  subset(select = c("dens.pob.2022", "concello"))

### Variación da poboación 2000-2022 ----
datos2 <- read_excel("Datos/1 Demográficas.xls",
                     sheet = "Variación poboación 2000-2022",
                     range = "a5:f317",
                     col_names = c("codigoine", "concello", 
                                   "pob.2000", "pob.2010", "pob.2020", "pob.2022")) 
datos2$var.pob.2000.2022 <- (datos2$pob.2022 - datos2$pob.2000) / datos2$pob.2000
datos2 <- subset(datos2, select = c("pob.2022", "var.pob.2000.2022"))

### Envellecemento ----
datos3 <- read_excel("Datos/1 Demográficas.xls",
                     sheet = "Índices envellecemento 2022",
                     range = "a5:d317",
                     col_names = c("codigoine", "concello", "porcent.mais.65", "ind.envell")) |> 
  subset(select = c("porcent.mais.65", "ind.envell"))

## 2. Variables socioeconómicas ----
### Emprego ----
datos4 <- read_excel("Datos/2 Socioeconómicas (emprego, PIB, renda).xls",
                     sheet = "Emprego por sectores 2022-agric",
                     range = "a7:h319",
                     col_names = c("codigoine", "concello", "emprego.total", 
                                   "emprego.agric", "emprego.pesca", "emprego.indus",
                                   "emprego.const", "emprego.servi"))[,-c(1:2)]

### PIB por habitante ----
datos5 <- read_excel("Datos/2 Socioeconómicas (emprego, PIB, renda).xls",
                     sheet = "PIB por habitante 2020",
                     range = "a7:d319",
                     col_names = c("codigoine", "concello", "PIB", "pib.hab")) |> 
  subset(select = c("pib.hab"))


### PIB por km² ----
datos6 <- read_excel("Datos/2 Socioeconómicas (emprego, PIB, renda).xls",
                     sheet = "PIB por Km2 2020",
                     range = "a7:e319",
                     col_names = c("codigoine", "concello", "PIB", "area.km2", "pib.km2")) |> 
  subset(select = c("pib.km2"))

### Renda dispoñible ----
datos7 <- read_excel("Datos/2 Socioeconómicas (emprego, PIB, renda).xls",
                     sheet = "Renda dispoñible por hab 2020",
                     range = "a8:d320",
                     col_names = c("codigoine", "concello", "renda.bruta.hab", "renda.bruta")) |> 
  subset(select = c("renda.bruta.hab"))

## 3. Agrarias e forestais ----

### Emprego agrario ----
datos8 <- read_excel("Datos/3 Agrarias e forestais.xlsx",
                     sheet = "Emprego agrario por Km2",
                     range = "a6:k318", 
                     col_names = c("codigoine", "concello", "afiliacions.ss.agric",
                                   "area.km2", "emprego.agrario.km2", "nada",
                                   "codigoine2", "concello2", "uta.2009", "area.km2.2", "uta.km2")) |> 
  subset(select = c("emprego.agrario.km2", "uta.km2"))


### Densidade explotacións e SAU ----
datos9 <- read_excel("Datos/3 Agrarias e forestais.xlsx",
                     sheet = "Densidade e SAU explot 2020",
                     range = "a4:h316",
                     col_names = c("codigoine", "concello", "num.explot", "sau_ha",
                                   "area.km2", "nada", "dens.explot", "sau.media.explot")) |> 
  subset(select = c("dens.explot", "sau.media.explot"))

### Unidades gandeiras ----
datos10 <- read_excel("Datos/3 Agrarias e forestais.xlsx",
                      sheet = "Unidades Gandeiras 2020",
                      range = "a6:l318",
                      col_names = c("codigoine", "concello", "bovino", "ovino.caprino",
                                    "porcino", "aves.curral", "ugt.total", "ugt.bovino.ovino.caprino",
                                    "nada", "area.km2", "ugt.km2", "ugt.bovino.ovino.caprino.km2")) |> 
  subset(select = c("ugt.km2", "ugt.bovino.ovino.caprino.km2"))
  
### Explotacións de bovino ----
datos11 <- read_excel("Datos/3 Agrarias e forestais.xlsx",
                      sheet = "Explot e cabezas bovino 2022",
                      range = "a7:h319",
                      col_names = c("codigoine", "concello", "num.explot",
                                    "num.bovinos", "nada", "area.km2",
                                    "explot.bovino.km2", "bovinos.km2")) |> 
  subset(select = c("explot.bovino.km2", "bovinos.km2"))

### Variación emprego agrario ----
datos12 <- read_excel("Datos/3 Agrarias e forestais.xlsx",
                      sheet = "Var emprego agrario 2011-2022",
                      range = "a7:e319",
                      col_names = c("codigoine", "concello", 
                                    "emprego.2011", "emprego.2022",
                                    "var.emprego.agrario.2011.2022")) |> 
  subset(select = "var.emprego.agrario.2011.2022")

### Variación número explotacións 2009-2020 ----
datos13 <- read_excel("Datos/3 Agrarias e forestais.xlsx",
                      sheet = "Var nº explotacións 2009-2020",
                      range = "a6:e318",
                      col_names = c("codigoine", "concello", "explot.2009",
                                    "explot.2020", "var.explot.2009.2020")) |> 
  subset(select = "var.explot.2009.2020")

### Volume de cortas de madeira ----
datos14 <- read_excel("Datos/3 Agrarias e forestais.xlsx",
                      sheet = "Cortas de madeira 2019-2021",
                      range = "a6:i318",
                      col_names = c("codigoine", "concello", "cortas.2019",
                                    "cortas.2020", "cortas.2021", "cortas.2019.2021",
                                    "nada", "area.km2", "cortas.km2.2019.2022")) |> 
  subset(select = "cortas.km2.2019.2022")

## 5. Propiedade e mobilidade da terra ----
### Catastro ----
datos15 <- read_excel("Datos/5 Propiedade e mobilidade da terra.xlsx",
                      sheet = "Datos Catastro Rústico 2022",
                      range = "a5:h317",
                      col_names = c("codigoine", "concello", "titulares.cat",
                                    "sup.rustica", "parcelas",
                                    "sup.media.propietario", 
                                    "sup.media.parcela",
                                    "parcelas.propietario")) |> 
  subset(select = c("titulares.cat", "sup.media.propietario",
                    "sup.media.parcela", "parcelas.propietario"))

### MVMC ----
datos16 <- read_excel("Datos/5 Propiedade e mobilidade da terra.xlsx",
                      sheet = "MVMC 2022",
                      range = "a5:e317",
                      col_names = c("codigoine", "concello", "sup.mvmc",
                                    "area.km2", "prop.mvmc")) |> 
  subset(select = c("codigoine", "area.km2", "prop.mvmc"))

## 6. Incendios ----
# (Estes precisan de utilizar o código INE para unirse aos demais)
datos17 <- read_excel("Datos/frq_incendios1996_2005_dd-web_tcm30-199964.xlsx",
                      sheet = "Datos1996_2005 Galicia",
                      range = "a2:k311",
                      col_names = c("concello", "codigoine", "Nut2", 
                                    "proine", "pronine", "n.conatos.1996.2005",
                                    "n.incendios.1996.2005", "incendios.arborado.1996.2005.ha",
                                    "incendios.non.arborado.1996.2005.ha", "incendios.total.1996.2005.ha", "n.incendios.total.1996.2005")) |> 
  subset(select = c("codigoine", "n.conatos.1996.2005", "n.incendios.1996.2005", "n.incendios.total.1996.2005",
                    "incendios.arborado.1996.2005.ha", "incendios.non.arborado.1996.2005.ha",
                    "incendios.total.1996.2005.ha"))

datos18 <- read_excel("Datos/frq_incendios2006_2015_dd-web_tcm30-525841.xlsx",
                      sheet = "Datos2006_2015 Galicia",
                      range = "a2:k311",
                      col_names = c("concello", "codigoine", "Nut2", 
                                    "proine", "pronine", "n.conatos.2006.2015",
                                    "n.incendios.2006.2015", "incendios.arborado.2006.2015.ha",
                                    "incendios.non.arborado.2006.2015.ha", "incendios.total.2006.2015.ha", "n.incendios.total.2006.2015")) |> 
  subset(select = c("codigoine", "n.conatos.2006.2015", "n.incendios.2006.2015", "n.incendios.total.2006.2015",
                    "incendios.arborado.2006.2015.ha", "incendios.non.arborado.2006.2015.ha",
                    "incendios.total.2006.2015.ha"))
         

# B) Preparación da saída ----
## 1. Unión das táboas importadas ----
datos_a <- cbind(datos1,  datos2,  datos3,  datos4,  datos5,  datos6,  
                 datos7,  datos8,  datos9,  datos10, datos11, datos12, 
                 datos13, datos14, datos15, datos16)

datos_b <- merge(datos_a, datos17, by = "codigoine", all.x = TRUE)
datos_b <- merge(datos_b, datos18, by = "codigoine", all.x = TRUE)

## 2. Creación e selección de campos ----
datos_b$titulares.hab <- datos_b$titulares.cat / datos_b$pob.2022
datos_b$dens.conatos.1996.2005 <- datos_b$n.conatos.1996.2005 / datos_b$area.km2
datos_b$dens.incendios.1996.2005 <- datos_b$n.incendios.1996.2005 / datos_b$area.km2
datos_b$dens.incendios.total.1996.2005 <- datos_b$n.incendios.total.1996.2005 / datos_b$area.km2
datos_b$dens.conatos.2006.2015 <- datos_b$n.conatos.2006.2015 / datos_b$area.km2
datos_b$dens.incendios.2006.2015 <- datos_b$n.incendios.2006.2015 / datos_b$area.km2
datos_b$dens.incendios.total.2006.2015 <- datos_b$n.incendios.total.2006.2015 / datos_b$area.km2
datos_b$prop.incendios.arb.1996.2005 <- datos_b$incendios.arborado.1996.2005.ha  / datos_b$area.km2
datos_b$prop.incendios.non.arb.1996.2005 <- datos_b$incendios.non.arborado.1996.2005.ha / datos_b$area.km2
datos_b$prop.incendios.total.1996.2005 <- datos_b$incendios.total.1996.2005.ha / datos_b$area.km2
datos_b$prop.incendios.arb.2006.2015 <- datos_b$incendios.arborado.2006.2015.ha / datos_b$area.km2
datos_b$prop.incendios.non.arb.2006.2015 <- datos_b$incendios.non.arborado.2006.2015.ha / datos_b$area.km2
datos_b$prop.incendios.total.2006.2015 <- datos_b$incendios.total.2006.2015.ha / datos_b$area.km2


datos <- subset(datos_b,
                select = c("codigoine", "concello",
                           "dens.pob.2022", "var.pob.2000.2022",
                           "porcent.mais.65", "ind.envell", 
                           "emprego.agric", "emprego.pesca",
                           "emprego.indus", "emprego.const", "emprego.servi",
                           "pib.hab", "pib.km2", "renda.bruta.hab", 
                           "emprego.agrario.km2", "uta.km2",
                           "dens.explot", "sau.media.explot",
                           "ugt.km2", "ugt.bovino.ovino.caprino.km2",
                           "explot.bovino.km2", "bovinos.km2",
                           "var.emprego.agrario.2011.2022",
                           "var.explot.2009.2020",
                           "cortas.km2.2019.2022",
                           "titulares.hab", "sup.media.propietario",
                           "sup.media.parcela", "parcelas.propietario",
                           "prop.mvmc", 
                           "dens.conatos.1996.2005", "dens.incendios.1996.2005",
                           "dens.incendios.total.1996.2005",
                           "dens.conatos.2006.2015", "dens.incendios.2006.2015",
                           "dens.incendios.total.2006.2015",
                           "prop.incendios.arb.1996.2005",
                           "prop.incendios.non.arb.1996.2005",
                           "prop.incendios.total.1996.2005",
                           "prop.incendios.arb.2006.2015",
                           "prop.incendios.non.arb.2006.2015",
                           "prop.incendios.total.2006.2015"))

## 3. Exportación ----
library(R.utils)

writeDataFrame(datos,
               "Temp/Variables_explicativas.csv",
               row.names = FALSE,
               createdBy = "Eduardo Corbelle (eduardo.corbelle@usc.es)",
               createdOn = format(Sys.time(), format = "%Y-%m-%d %H:%M:%S %Z"),
               header = list(content = "Variables potencialmente explicativas compiladas a partir da selección feita por Edelmiro."),
               sep    = ";",
               dec    = ".",
               overwrite = TRUE)
