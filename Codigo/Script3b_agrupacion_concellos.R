# Guión para procesar os valores municipais derivados de GRASS
# Eduardo Corbelle, 17 novembro 2022 (reelaborado para valores municipais o 4 setembro 2023)

library(sf)
library(data.table)
library(NbClust)
library(ggplot2)
library(tmap)
library(RColorBrewer)
library(terra)
library(grid)

# Carga de datos
datos0 <- st_read("Temp/Areas_siose.gpkg", layer = "concellos")
datos1 <- st_drop_geometry(datos0) |> as.data.table() |> unique()

# (Para a comprobación da área de SIOSE 2017 segundo IDmax)
datos1b <- fread("Temp/siose2017_concellos.csv", 
                col.names = c("codigoine", "v2", "v3", "cuberta", "area_m2"))

# Datos globais para Galicia (Valores en hectáreas)
datos1.galicia <- datos1[ , .("coni_2005" = sum(p_coni_2005_sum) / 1600,
            "coni_2009" = sum(p_coni_2009_sum) / 1600,
            "coni_2014" = sum(p_coni_2014_sum) / 1600,
            "coni_2017" = sum(p_coni_2017_sum) / 1600,
            "fper_2005" = sum(p_fper_2005_sum) / 1600,
            "fper_2009" = sum(p_fper_2009_sum) / 1600,
            "fper_2014" = sum(p_fper_2014_sum) / 1600,
            "fper_2017" = sum(p_fper_2017_sum) / 1600,
            "fcad_2005" = sum(p_fcad_2005_sum) / 1600,
            "fcad_2009" = sum(p_fcad_2009_sum) / 1600,
            "fcad_2014" = sum(p_fcad_2014_sum) / 1600,
            "fcad_2017" = sum(p_fcad_2017_sum) / 1600,
            "mato_2005" = sum(p_mato_2005_sum) / 1600,
            "mato_2009" = sum(p_mato_2009_sum) / 1600,
            "mato_2014" = sum(p_mato_2014_sum) / 1600,
            "mato_2017" = sum(p_mato_2017_sum) / 1600,
            "cult_2005" = sum(p_cult_2005_sum) / 1600,
            "cult_2009" = sum(p_cult_2009_sum) / 1600,
            "cult_2014" = sum(p_cult_2014_sum) / 1600,
            "cult_2017" = sum(p_cult_2017_sum) / 1600,
            "edif_2005" = sum(p_edif_2005_sum) / 1600,
            "edif_2009" = sum(p_edif_2009_sum) / 1600,
            "edif_2014" = sum(p_edif_2014_sum) / 1600,
            "edif_2017" = sum(p_edif_2017_sum) / 1600,
            "plan_2017" = sum(p_plan_2017_sum) / 1600),] |> 
  melt(measure.vars = 1:25)
datos1.galicia$cuberta <- substr(datos1.galicia$variable, 1, 4)
datos1.galicia$ano     <- substr(datos1.galicia$variable, 6, 9) |> as.integer()
datos1.galicia$fonte <- "porcent"

datos1b.galicia <- datos1b[, .(value = sum(area_m2/1e4)), .(cuberta)]
datos1b.galicia$ano <- 2017L
datos1b.galicia$fonte <- "idmax"

datos1c.galicia <- rbind(datos1.galicia[,-1], datos1b.galicia)


ggplot(datos1c.galicia, aes(x = ano, y = value, col = fonte)) +
  geom_line(linewidth = 1)  + 
  geom_point(size = 2, pch = 21, bg = "black", stroke = 1) +
  facet_wrap(~cuberta, scales = "free") 
  
## Conclusión: obtemos dúas estimacións a partir de SIOSE 2017 (calculando a porcentaxe 
## de cada mancha ocupada por cada cuberta, fronte a utilizar a cuberta maioritaria). 
## Ambas estimacións son, en xeral, parecidas. A salvedade está na estimación da área de coníferas 
## e a área de mato. Creo que a área obtida tendo en conta as porcentaxes sería preferible.


# Resumo de áreas para a táboa 1 (valores en km²)
datos1.galicia[cuberta %in% c("edif"),         .(sum(value)/1e2), .(ano)] # Built up
datos1.galicia[cuberta %in% c("cult"),         .(sum(value)/1e2), .(ano)] # Farmland
datos1.galicia[cuberta %in% c("mato"),         .(sum(value)/1e2), .(ano)] # Shrublands
datos1.galicia[cuberta %in% c("fper", "coni", "plan"), .(sum(value)/1e2), .(ano)] # Plantation forest
datos1.galicia[cuberta %in% c("fcad"),         .(sum(value)/1e2), .(ano)] # Native forest
datos1.galicia[, .(29576.74 - sum(value)/1e2), .(ano)] # Other areas

# Borramos todo o que sae da área de cuberta maioritaria e dos cálculos para Galicia
rm(datos1b, datos1b.galicia, datos1c.galicia)
rm(datos1.galicia)

# Resumo: unificar datos por concello (orixinalmente multipolígono), converter en proporción de área
datos2  <- datos1[ , .("plfor_2005" = (p_coni_2005_sum + p_fper_2005_sum) / (area_ha*1600),
                       "spfor_2005" = p_fcad_2005_sum / (area_ha*1600),
                       "shrub_2005" = p_mato_2005_sum / (area_ha*1600),
                       "agric_2005" = p_cult_2005_sum / (area_ha*1600),
                       "built_2005" = p_edif_2005_sum / (area_ha*1600),
                       "plfor_2017" = (p_coni_2017_sum + p_fper_2017_sum + p_plan_2017_sum) / (area_ha*1600),
                       "spfor_2017" = p_fcad_2017_sum / (area_ha*1600),
                       "shrub_2017" = p_mato_2017_sum / (area_ha*1600),
                       "agric_2017" = p_cult_2017_sum / (area_ha*1600),
                       "built_2017" = p_edif_2017_sum / (area_ha*1600)),
                   .(codigoine)]

# Para incorporar "outros"
datos2$outros_2005 <- datos2[ , .( 1 - (plfor_2005 + spfor_2005 + shrub_2005 + agric_2005 + built_2005) )]
datos2$outros_2017 <- datos2[ , .( 1 - (plfor_2017 + spfor_2017 + shrub_2017 + agric_2017 + built_2017) )]

# Datos para a análise de conglomerados
datos3 <- as.data.frame(scale(datos2[,-1]))
rownames(datos3) <- datos2$codigoine

# Análise de conglomerados (proba 1: análise xerárquica)
 # analise <- NbClust(data = datos3, distance = "euclidean", method = "ward.D2",
 #                    min.nc = 2, max.nc = 7) # Falla ao ter unha matriz "completa" despois de engadir "outros"

clust1 <- hclust(dist(datos3, method = "euclidean"),
                 method = "ward.D2")
dev.off()
plot(clust1)
ng = 4
agrup1 <- data.table(codigoine = as.integer(rownames(datos3)),
                     grupo.inic  = cutree(clust1, k = ng))

# Interpretación dos grupos
datos1b <- merge(datos1, agrup1) # data.table (concellos, formato ancho)
elev <- datos1b[ , .(elev = mean(mdt25_average)), grupo.inic ] # Estimación de elevación media
print(elev)
replace <- data.frame(grupo.inic = elev[order(elev), grupo.inic],        # Reordena por elevación media
                      grupo = 1:ng)
agrup1 <- merge(agrup1, replace, by = "grupo.inic")
datos1b <- merge(datos1b, replace, by = "grupo.inic")



# Interpretación numérica: porcentaxe de área ocupada en cada grupo (alimenta a táboa 2)
datos1b[order(grupo) ,
        .("number"     = .N,
          "area_km2"   = sum(area_ha) / 100,
          "area_por"   = sum(area_ha) / 29576.74,
          "avg_elev"   = mean(mdt25_average),
          
          "plfor_2005" = 100 * sum(p_coni_2005_sum + p_fper_2005_sum) / sum(area_ha * 1600),
          "spfor_2005" = 100 * sum(p_fcad_2005_sum) / sum(area_ha * 1600),
          "shrub_2005" = 100 * sum(p_mato_2005_sum) / sum(area_ha * 1600),
          "agric_2005" = 100 * sum(p_cult_2005_sum) / sum(area_ha * 1600),
          "built_2005" = 100 * sum(p_edif_2005_sum) / sum(area_ha * 1600),
          "other_2005" = 100 *( 1 - sum(p_coni_2005_sum + p_fper_2005_sum + p_fcad_2005_sum + p_mato_2005_sum + p_cult_2005_sum + p_edif_2005_sum) / sum(area_ha * 1600)),
          
          "plfor_2017" = 100 * sum(p_coni_2017_sum + p_fper_2017_sum + p_plan_2017_sum) / sum(area_ha * 1600),
          "spfor_2017" = 100 * sum(p_fcad_2017_sum) / sum(area_ha * 1600),
          "shrub_2017" = 100 * sum(p_mato_2017_sum) / sum(area_ha * 1600),
          "agric_2017" = 100 * sum(p_cult_2017_sum) / sum(area_ha * 1600),
          "built_2017" = 100 * sum(p_edif_2017_sum) / sum(area_ha * 1600),
          "other_2017" = 100 * (1 - sum(p_coni_2017_sum + p_fper_2017_sum + p_fcad_2017_sum + p_mato_2017_sum + p_cult_2017_sum + p_edif_2017_sum + p_plan_2017_sum) / sum(area_ha * 1600))),
        .(grupo)]



# Datos para a produción de gráficos
datos4 <- melt(datos2, id.vars = 1)
datos4$ano <- substr(datos4$variable, 7, 10)
datos4$cub <- substr(datos4$variable, 1, 5)

datos0b <- merge(datos0, agrup1) # sf
datos4b <- merge(datos4, agrup1) # data.table (concellos, formato longo)




# Produción de gráficos
sombra <- rast("Datos/Aux/Relief200.tif")

grupo.labels <- paste("Cluster", levels(factor(datos4b$grupo)))
names(grupo.labels) <- levels(factor(datos4b$grupo))
cub.labels <- c("Farmland", "Built-up", "Plantation forest", "Native forest", "Shrubland", "Other")
names(cub.labels) <- levels(factor(datos4b$cub))

cores <- brewer.pal(ng, "Set1")
g1 <- ggplot(datos4b, aes(x = ano, y = value, fill = factor(grupo))) +
  geom_boxplot() +
  # stat_smooth(aes(group = grupo), col = "black", lwd = 1, method = "lm") +
  scale_fill_manual(values = cores) +

  facet_grid(cub ~ grupo, labeller = labeller(cub = cub.labels, grupo = grupo.labels),
             scales = "free_y") +
  ylab("Proportion of municipality area") +
  xlab("Year") +
  # coord_cartesian(ylim = c(0, 0.75)) +
  theme_minimal() +
  theme(legend.position = "none") 
  

bbox <- st_bbox(datos0b)
bbox[1] <- bbox[1] - 10e3 # xmin
bbox[2] <- bbox[2] - 10e3 # ymin

m1 <- tm_shape(datos0b, bb = bbox) + 
  tm_fill(col = "grupo", style = "cat", palette = cores, title = "Cluster") +
  tm_shape(sombra, raster.downsample = FALSE) + 
  tm_raster(palette = "-Greys", alpha = .375, legend.show = FALSE) +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "left") +
  tm_scale_bar(position = c(0,-.02),
               breaks = c(0,25,50),
               lwd = .2) +
  tm_compass(position = c(0,.85),
             size = 1)



# pdf("Resultados/agrupacion_concellos.pdf", width = 7*16/9, height = 7)
# print(g1, vp = viewport( 0, 0, width = .5, height = .95, just = c("left", "bottom")))
# print(mapa1, vp = viewport(.5, 0, width = .5, height = 1, just = c("left", "bottom")))
# dev.off()

png("Resultados/agrupacion_concellos.png", 
    width = 90, height = 70, units = "mm", res = 300)
print(m1)
dev.off()




# Exportación para análises posteriores
st_write(datos0b, "Resultados/concellos_clasif.gpkg", append = FALSE)
