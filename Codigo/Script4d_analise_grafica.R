## Código para cruzar os grupos de municipios con variables explicativas (saída gráfica)
## Eduardo Corbelle, 20 agosto 2024

library(sf)
library(data.table)
library(ggplot2)
library(grid)

# Carga de datos ----
# Resultados da análise de conglomerados
datos0 <- st_read("Resultados/concellos_clasif.gpkg") |> st_drop_geometry() |> 
  unique() |> 
  subset(select = c("codigoine", "grupo"))
# Variables explicativas 
datos1 <- read.csv("Temp/Variables_explicativas.csv", skip = 7, sep = ";")
datos2 <- read.csv("Temp/Apt_biofisica.csv")
# Unión 
datos <- merge(datos0, datos1, by = "codigoine") |> merge(datos2, by = "codigoine") |> data.table()
datos$grupo <- factor(datos$grupo)
rm(datos0, datos1, datos2)


# Diagramas de caixas ----
cores <- RColorBrewer::brewer.pal(4, "Set1")
comun <- list(geom_boxplot(alpha = .8),
              xlab(""),
              ylab(""),
              scale_fill_manual(values = cores),
              theme_minimal(),
              theme(legend.position = "none",
                    plot.title = element_text(size = 10),
                    axis.title = element_text(size =  9)))

## Variables demográficas ----
g.demo1 <- ggplot(datos, aes(x = grupo, y = dens.pob.2022, fill = grupo)) + 
  ggtitle("Population density, 2022 (log scale)") +
  scale_y_log10() +
  comun + ylab("inhab/km²")
g.demo2 <- ggplot(datos, aes(x = grupo, y = 100*var.pob.2000.2022, fill = grupo)) + 
  ggtitle("Population change, 2000-2022") + 
  comun + ylab("%")
g.demo3 <- ggplot(datos, aes(x = grupo, y = ind.envell, fill = grupo)) + 
  ggtitle("Ageing index, 2022") + 
  comun + ylab("%")
g.demo4 <- ggplot(datos, aes(x = grupo, y = porcent.mais.65, fill = grupo)) + 
  ggtitle("Population over 65") + 
  comun + ylab("%")

## Variables de emprego ----
g.empl1 <- ggplot(datos, aes(x = grupo, y = emprego.agric, fill = grupo)) + 
  ggtitle("Employment in agriculture, 2022") +
  comun + ylab("%")
g.empl2 <- ggplot(datos, aes(x = grupo, y = emprego.indus, fill = grupo)) + 
  ggtitle("Employment in industry, 2022") +
  comun + ylab("%")
g.empl3 <- ggplot(datos, aes(x = grupo, y = emprego.const, fill = grupo)) + 
  ggtitle("Employment in construction, 2022") +
  comun + ylab("%")
g.empl4 <- ggplot(datos, aes(x = grupo, y = emprego.servi, fill = grupo)) + 
  ggtitle("Employment in services, 2022") +
  comun + ylab("%")

## Variables económicas ----
g.econ1 <- ggplot(datos, aes(x = grupo, y = pib.km2/1e3, fill = grupo)) + 
  ggtitle("GDP per area, 2020 (log scale)") +
  scale_y_log10() +
  comun + ylab("thousand euro/km²") + xlab("Cluster")
g.econ2 <- ggplot(datos, aes(x = grupo, y = pib.hab/1e3, fill = grupo)) + 
  ggtitle("GDI per inhabitant, 2020 (log scale)") +
  scale_y_log10() +
  comun + ylab("thousand euro/inhab") + xlab("Cluster")

## Variables de sector agrario ----
g.agr1 <- ggplot(datos, aes(x = grupo, y = emprego.agrario.km2, fill = grupo)) + 
  ggtitle("Density of farm labor, 2020") +
  comun + ylab("workers/km²")
g.agr2 <- ggplot(datos, aes(x = grupo, y = dens.explot, fill = grupo)) + 
  ggtitle("Density of farms, 2020") +
  comun + ylab("farms/km²")
g.agr3 <- ggplot(datos, aes(x = grupo, y = sau.media.explot, fill = grupo)) + 
  ggtitle("Farm size, 2020") +
  comun + ylab("ha")
g.agr4 <- ggplot(datos, aes(x = grupo, y = ugt.km2, fill = grupo)) + 
  ggtitle("Stocking density, 2020") +
  comun + ylab("LU/km²")
g.agr5 <- ggplot(datos, aes(x = grupo, y = bovinos.km2, fill = grupo)) + 
  ggtitle("Bovine stocking density, 2020") +
  comun + ylab("LU/km²")
g.agr6 <- ggplot(datos, aes(x = grupo, y = cortas.km2.2019.2022, fill = grupo)) + 
  ggtitle("Wood extraction, 2019-2021") +
  comun + ylab("m³/km²-year")
## Variables de propiedade ----
g.lown1 <- ggplot(datos, aes(x = grupo, y = titulares.hab, fill = grupo)) + 
  ggtitle("landowners/inhabitants, 2022") +
  comun + ylab("")
g.lown2 <- ggplot(datos, aes(x = grupo, y = sup.media.propietario, fill = grupo)) + 
  ggtitle("Average property size, 2022") +
  comun + ylab("ha/owner")
g.lown3 <- ggplot(datos, aes(x = grupo, y = sup.media.parcela, fill = grupo)) + 
  ggtitle("Average plot size, 2022") +
  comun + ylab("ha/plot") + xlab("Cluster")
g.lown4 <- ggplot(datos, aes(x = grupo, y = parcelas.propietario, fill = grupo)) + 
  ggtitle("Plots per owner, 2022") +
  comun + ylab("plots/owner") + xlab("Cluster")
g.lown5 <- ggplot(datos, aes(x = grupo, y = 100*prop.mvmc, fill = grupo)) + 
  ggtitle("Area of common land, 2022") +
  comun + ylab("%")

## Incendios ----
g.inc1 <- ggplot(datos, aes(x = grupo, y = dens.conatos.2006.2015, fill = grupo)) + 
  ggtitle("Wildfires smaller than 1 ha, 2006-2015") +
  scale_y_log10() +
  comun + ylab("events/km² (log scale)")
g.inc2 <- ggplot(datos, aes(x = grupo, y = dens.incendios.2006.2015, fill = grupo)) + 
  ggtitle("Wildfires larger than 1 ha, 2006-2015") +
  scale_y_log10() +
  comun + ylab("events/km² (log scale)")
g.inc3 <- ggplot(datos, aes(x = grupo, y = dens.incendios.2006.2015, fill = grupo)) + 
  ggtitle("Wildfires, total, 2006-2015") +
  scale_y_log10() +
  comun + ylab("events/km² (log scale)")
g.inc4 <- ggplot(datos, aes(x = grupo, y = prop.incendios.arb.2006.2015, fill = grupo)) + 
  ggtitle("Burnt area - forest, 2006-2015") +
  comun + ylab("%")
g.inc5 <- ggplot(datos, aes(x = grupo, y = prop.incendios.non.arb.2006.2015, fill = grupo)) + 
  ggtitle("Burnt area - shrubland, 2006-2015") +
  comun + ylab("%")
g.inc6 <- ggplot(datos, aes(x = grupo, y = prop.incendios.total.2006.2015, fill = grupo)) + 
  ggtitle("Burnt area - total, 2006-2015") +
  comun + ylab("%")

## Aptitude biofísica ----
g.apt1 <- ggplot(datos, aes(x = grupo, y = prop.millo, fill = grupo)) + 
  ggtitle("Area suitable for maize") +
  comun + ylab("%") + xlab("Cluster")
g.apt2 <- ggplot(datos, aes(x = grupo, y = prop.globu, fill = grupo)) + 
  ggtitle("Area suitable for eucalyptus") +
  comun + ylab("%") + xlab("Cluster")


# Saída en pdf ----
## Gráfico 1 ----
pdf("Resultados/gr_boxplot1.pdf", width = 155/25.4, height = 220/25.4)
  print(g.demo1, vp = viewport( 0, 1, width = .5, height = .2, 
                                just = c("left", "top")))
  print(g.demo2, vp = viewport(.5, 1, width = .5, height = .2, 
                               just = c("left", "top")))
  print(g.demo3, vp = viewport( 0,.8, width = .5, height = .2, 
                                just = c("left", "top")))
  print(g.demo4, vp = viewport(.5,.8, width = .5, height = .2, 
                               just = c("left", "top")))
  print(g.empl1, vp = viewport( 0,.6, width = .5, height = .2, 
                                just = c("left", "top")))
  print(g.empl2, vp = viewport(.5,.6, width = .5, height = .2, 
                               just = c("left", "top")))
  print(g.empl3, vp = viewport( 0,.4, width = .5, height = .2, 
                                just = c("left", "top")))
  print(g.empl4, vp = viewport(.5,.4, width = .5, height = .2, 
                               just = c("left", "top")))
  print(g.econ1, vp = viewport( 0,.2, width = .5, height = .2, 
                                just = c("left", "top")))
  print(g.econ2, vp = viewport(.5,.2, width = .5, height = .2, 
                               just = c("left", "top")))
dev.off()

## Gráfico 2 ----
pdf("Resultados/gr_boxplot2.pdf", width = 155/25.4, height = 220/25.4)
print(g.agr1, vp = viewport( 0, 1, width = .5, height = .2, 
                              just = c("left", "top")))
print(g.agr2, vp = viewport(.5, 1, width = .5, height = .2, 
                             just = c("left", "top")))
print(g.agr3, vp = viewport( 0,.8, width = .5, height = .2, 
                              just = c("left", "top")))
print(g.agr4, vp = viewport(.5,.8, width = .5, height = .2, 
                             just = c("left", "top")))
print(g.agr5, vp = viewport( 0,.6, width = .5, height = .2, 
                              just = c("left", "top")))
print(g.agr6, vp = viewport(.5,.6, width = .5, height = .2, 
                             just = c("left", "top")))
print(g.lown1, vp = viewport( 0,.4, width = .5, height = .2,
                             just = c("left", "top")))
print(g.lown2, vp = viewport(.5,.4, width = .5, height = .2,
                             just = c("left", "top")))
print(g.lown3, vp = viewport( 0,.2, width = .5, height = .2,
                              just = c("left", "top")))
print(g.lown4, vp = viewport(.5,.2, width = .5, height = .2,
                             just = c("left", "top")))
dev.off()

## Gráfico 3 ----
pdf("Resultados/gr_boxplot3.pdf", width = 155/25.4, height = 220/25.4)
print(g.lown5, vp = viewport( 0, 1, width = .5, height = .2, 
                             just = c("left", "top")))
print(g.inc1, vp = viewport(.5, 1, width = .5, height = .2, 
                            just = c("left", "top")))
print(g.inc2, vp = viewport( 0,.8, width = .5, height = .2, 
                             just = c("left", "top")))
print(g.inc3, vp = viewport(.5,.8, width = .5, height = .2, 
                            just = c("left", "top")))
print(g.inc4, vp = viewport( 0,.6, width = .5, height = .2, 
                             just = c("left", "top")))
print(g.inc5, vp = viewport(.5,.6, width = .5, height = .2, 
                            just = c("left", "top")))
print(g.inc6, vp = viewport( 0,.4, width = .5, height = .2,
                              just = c("left", "top")))
print(g.apt1, vp = viewport(.5,.4, width = .5, height = .2,
                             just = c("left", "top")))
print(g.apt2, vp = viewport( 0,.2, width = .5, height = .2,
                              just = c("left", "top")))
dev.off()
