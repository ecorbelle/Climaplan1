library(data.table)
library(sf)

valores_2005 <- as.data.table(
  st_read("~/Traballo/Recursos/Datos_Cartografia/Usos&Cubertas/SIOSE/SIOSE_2005/SIOSE_Galicia_2005_H29.gpkg",
          layer = "T_VALORES"))

valores_2009 <- as.data.table(
  st_read("~/Traballo/Recursos/Datos_Cartografia/Usos&Cubertas/SIOSE/SIOSE_2009/SIOSE_Galicia_2009_H29.gpkg",
          layer = "T_VALORES"))

valores_2014 <- as.data.table(
  st_read("~/Traballo/Recursos/Datos_Cartografia/Usos&Cubertas/SIOSE/SIOSE_2014/SIOSE_Galicia_2014_H29.gpkg",
          layer = "T_VALORES"))



area_2005 <- valores_2005[order(ID_COBERTURAS) , .(area_km2_2005 = round(sum(SUPERF_HA/100), 2)), .(ID_COBERTURAS)]
area_2009 <- valores_2009[order(ID_COBERTURAS) , .(area_km2_2009 = round(sum(SUPERF_HA/100), 2)), .(ID_COBERTURAS)]
area_2014 <- valores_2014[order(ID_COBERTURAS) , .(area_km2_2014 = round(sum(SUPERF_HA/100), 2)), .(ID_COBERTURAS)]

areas <- merge(merge(area_2005, area_2009), area_2014)
