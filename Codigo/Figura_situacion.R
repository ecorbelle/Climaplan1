## Guión para producir un mapa de situación
## Eduardo Corbelle, 18 decembro 2023

library(sf)
library(terra)
library(rmapshaper)
library(tmap)
library(grid)

# Entrada de datos ----
## Carga de datos ----
sombra <- rast("Datos/Aux/Relief200.tif")
relevo <- rast("~/Traballo/Recursos/Datos_Cartografia/mdt200.ncd")
relevo[relevo<=0] <- NA

sombra <- rast("Datos/Aux/Relief200.tif")
ccaa <- st_read("~/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/IGN/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/")
pt <- st_read("~/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/Portugal/PT_Continental.gpkg",
           layer = "PT_Continental")
fr <- st_read("~/Traballo/Recursos/Datos_Cartografia/LimitesAdmin/Francia/FRA_adm0_simp00005.shp")

## Simplificación ----
ccaa <- ms_simplify(ccaa, 
                  keep = 0.1)
pt <- ms_simplify(pt,
                  keep = 0.1)
fr <- ms_simplify(fr,
                  keep = 0.1)

## Reproxección ----
ccaa <- st_transform(ccaa, 
                   crs = 25830)
pt <- st_transform(pt, 
                   crs = 25830)
fr <- st_transform(fr, 
                   crs = 25830)

# Mapa 1: Galicia en España ----
## Selección de cores ----
numero <- which(ccaa$CODNUT2 == "ES11")
ccaa$color.ccaa <- c(rep("grey90", numero-1),
                   "grey40",
                   rep("grey90", nrow(ccaa) - numero))

## Bounding box: ES + PT ----
map.bb <- st_bbox(st_union(ccaa,pt))

## Mapa ----
mapa1 <- tm_shape(ccaa, 
                  bbox = map.bb) + 
  tm_fill(col = "color.ccaa") + 
  tm_borders(lwd = .5, col = "grey") +
  tm_shape(fr) + tm_borders(lwd = .5, col = "grey") +
  tm_shape(pt) + tm_borders(lwd = .5, col = "grey") +
  # tm_graticules(n.x = 6, n.y = 4, lwd = .2) 
  tm_graticules(x = seq(-9,3,3), 
                y = seq(36, 42, 2),
                lwd = .25) 

# Mapa 2: Modelo de elevacións ----
## Bounding box: Galicia ----
bbox <- st_bbox(ccaa[numero,])
bbox[1] <- bbox[1] - 100e3 # xmin

mapa2 <- tm_shape(relevo, bb = bbox) +
  tm_raster(palette = "Greys", 
            title = "Elevation (m asl)",
            breaks = c(0, 500, 1000, 1500, 2100)) +
  tm_shape(ccaa[numero,]) +
  tm_borders(lwd = 1) +
  tm_shape(sombra) +
  tm_raster(palette = "-Greys", alpha = .2, legend.show = FALSE) +
  tm_scale_bar(position = c(0,-.02),
               breaks = c(0,25,50),
               lwd = .2) +
  tm_compass(position = c(0,.85),
             size = 1) +
  tm_layout(legend.position = c(0, .15),
            legend.text.size = .7,
            legend.title.size = .85)

# Exportación ----
## Tamaño: 90 mm ancho (https://www.elsevier.com/about/policies-and-standards/author/artwork-and-media-instructions/artwork-sizing)
png("Resultados/Situacion.png",
    width = 190, height = 75, units = "mm", res = 300)
print(mapa1, vp = viewport(-.02, 1, width = .5, height = 1, 
                           just = c("left", "top")))
print(mapa2, vp = viewport(.5, .962, width = .5, height = .872,
                           just = c("left", "top")))
dev.off()

