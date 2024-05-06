## Código para cruzar os grupos de municipios con variables explicativas
## Eduardo Corbelle, 19 decembro 2023

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
# Unión 
datos <- merge(datos0, datos1, by = "codigoine") |>  data.table()
datos$grupo <- factor(datos$grupo)
rm(datos0, datos1)


# Diagramas de caixas ----

## Exploratorios ----
pdf("Temp/Exploracion.pdf")
for (i in 4:ncol(datos)) {
  boxplot(datos[[i]] ~ datos$grupo,
          xlab = "Grupo de municipios",
          ylab = names(datos)[i],
          main = names(datos)[i])
  }
dev.off()

## Para presentar ----

cores <- RColorBrewer::brewer.pal(4, "Set1")
comun <- list(geom_boxplot(),
              xlab("Cluster"),
              ylab(""),
              scale_fill_manual(values = cores),
              theme_light(),
              theme(legend.position = "none",
                    plot.title = element_text(size = 10),
                    axis.title = element_text(size =  9)))

g.demo1 <- ggplot(datos, aes(x = grupo, y = dens.pob.2022, fill = grupo)) + 
  ggtitle("Population density, 2022 (inhab/km², log scale)") +
  scale_y_log10() +
  comun
g.demo2 <- ggplot(datos, aes(x = grupo, y = 100*var.pob.2000.2022, fill = grupo)) + 
  ggtitle("Population change, 2000-2022 (%)") +
  comun
g.demo3 <- ggplot(datos, aes(x = grupo, y = ind.envell, fill = grupo)) + 
  ggtitle("Ageing index, 2022 (%)") +
  comun
g.demo4 <- ggplot(datos, aes(x = grupo, y = porcent.mais.65, fill = grupo)) + 
  ggtitle("Population over 65 (%)") +
  comun


png("Resultados/gr_demograficas.png", width = 190, height = 120, units = "mm", res = 300)
print(g.demo1, vp = viewport(0, 1, width = .5, height = .5, 
                             just = c("left", "top")))
print(g.demo2, vp = viewport(0.5, 1, width = .5, height = .5, 
                             just = c("left", "top")))
print(g.demo3, vp = viewport(0, .5, width = .5, height = .5, 
                             just = c("left", "top")))
print(g.demo4, vp = viewport(0.5, .5, width = .5, height = .5, 
                             just = c("left", "top")))

dev.off()


# Táboas ----

# Variables demográficas
datos[,
      .(dens.pob = median(dens.pob.2022),
        var.pob  = median(100 * var.pob.2000.2022),
        ind.env  = median(ind.envell),
        pop.65   = median(porcent.mais.65)),
      keyby = grupo]
datos[,
      .(dens.pob = mad(dens.pob.2022),
        var.pob  = mad(100 * var.pob.2000.2022),
        ind.env  = mad(ind.envell),
        pop.65   = mad(porcent.mais.65)),
      keyby = grupo]

# Variables de emprego
datos[,
      .(agric = median(emprego.agric),
        indus = median(emprego.indus),
        const = median(emprego.const),
        serv  = median(emprego.servi)),
      keyby = grupo]
datos[,
      .(agric = mad(emprego.agric),
        indus = mad(emprego.indus),
        const = mad(emprego.const),
        serv  = mad(emprego.servi)),
      keyby = grupo]

# Variables económicas
datos[,
      .(pib.hab = median(pib.hab),
        pib.km2 = median(pib.km2),
        ren.hab = median(renda.bruta.hab)),
      keyby = grupo]
datos[,
      .(pib.hab = mad(pib.hab),
        pib.km2 = mad(pib.km2),
        ren.hab = mad(renda.bruta.hab)),
      keyby = grupo]

# Variables censo agrario
datos[,
      .(emprego = median(emprego.agrario.km2),
        uta.km2 = median(uta.km2),
        den.exp = median(dens.explot, na.rm = TRUE),
        sau.med = median(sau.media.explot, na.rm = TRUE),
        ugt.km2 = median(ugt.km2),
        bovinos.km2 = median(bovinos.km2),
        cortas.km2 = median(cortas.km2.2019.2022)),
      keyby = grupo]
datos[,
      .(emprego = mad(emprego.agrario.km2),
        uta.km2 = mad(uta.km2),
        den.exp = mad(dens.explot, na.rm = TRUE),
        sau.med = mad(sau.media.explot, na.rm = TRUE),
        ugt.km2 = mad(ugt.km2),
        bovinos.km2 = mad(bovinos.km2),
        cortas.km2 = mad(cortas.km2.2019.2022)),
      keyby = grupo]

# Variables estrutura da propiedade
datos[,
      .(tit.hab = median(titulares.hab),
        sup.tit = median(sup.media.propietario),
        sup.par = median(sup.media.parcela),
        par.tit = median(parcelas.propietario),
        prop.mvmc = median(100*prop.mvmc)),
      keyby = grupo]
datos[,
      .(tit.hab = mad(titulares.hab),
        sup.tit = mad(sup.media.propietario),
        sup.par = mad(sup.media.parcela),
        par.tit = mad(parcelas.propietario),
        prop.mvmc = mad(100*prop.mvmc)),
      keyby = grupo]


# Variables sobre incendios
datos[,
      .(dens.conatos = median(dens.conatos.2006.2015, na.rm = TRUE),
        dens.incendios = median(dens.incendios.2006.2015, na.rm = TRUE),
        dens.total  = median(dens.incendios.total.2006.2015, na.rm = TRUE),
        burnt.forest = median(prop.incendios.arb.2006.2015, na.rm = TRUE),
        burnt.shrub  = median(prop.incendios.non.arb.2006.2015, na.rm = TRUE),
        burnt.total  = median(prop.incendios.total.2006.2015, na.rm = TRUE)),
      keyby = grupo]
datos[,
      .(dens.conatos = mad(dens.conatos.2006.2015, na.rm = TRUE),
        dens.incendios = mad(dens.incendios.2006.2015, na.rm = TRUE),
        dens.total  = mad(dens.incendios.total.2006.2015, na.rm = TRUE),
        burnt.forest = mad(prop.incendios.arb.2006.2015, na.rm = TRUE),
        burnt.shrub  = mad(prop.incendios.non.arb.2006.2015, na.rm = TRUE),
        burnt.total  = mad(prop.incendios.total.2006.2015, na.rm = TRUE)),
      keyby = grupo]

# kruskal-wallis ----

for (i in 4:43) {
  print("######################################")
  print(names(datos)[i])
  print(kruskal.test(datos[[i]], datos$grupo))
  }

