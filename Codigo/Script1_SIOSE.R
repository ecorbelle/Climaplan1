# Script para producir capas de porcentaxe de ocupación a partir de SIOSE
# Eduardo Corbelle, 15 novembro 2022

library(sf)
library(data.table)

for(i in c(2005, 2009, 2014)) {

valores <- as.data.table(
  st_read(paste("~/Traballo/Recursos/Datos_Cartografia/Usos&Cubertas/SIOSE/SIOSE_",
                i,
                "/SIOSE_Galicia_",
                i,
                "_H29.gpkg", sep = ""),
                    layer = "T_VALORES")
  )

p_edif <- valores[ID_COBERTURAS >= 101 & ID_COBERTURAS <= 131, .(p_edif = sum(SUPERF_POR)), .(ID_POLYGON)]
p_cult <- valores[ID_COBERTURAS %in% c(211,212,222,223,231,232,241,290), .(p_cult = sum(SUPERF_POR)), .(ID_POLYGON)]
p_mato <- valores[ID_COBERTURAS == 300 | ID_COBERTURAS == 320, .(p_mato = sum(SUPERF_POR)), .(ID_POLYGON)]
p_fcad <- valores[ID_COBERTURAS == 312                       , .(p_fcad = sum(SUPERF_POR)), .(ID_POLYGON)]
p_fper <- valores[ID_COBERTURAS == 313                       , .(p_fper = sum(SUPERF_POR)), .(ID_POLYGON)]
p_coni <- valores[ID_COBERTURAS == 316                       , .(p_coni = sum(SUPERF_POR)), .(ID_POLYGON)]

p_todo <- merge(p_edif, p_cult, by = "ID_POLYGON", all = TRUE)
p_todo <- merge(p_todo, p_mato, by = "ID_POLYGON", all = TRUE)
p_todo <- merge(p_todo, p_fcad, by = "ID_POLYGON", all = TRUE)
p_todo <- merge(p_todo, p_fper, by = "ID_POLYGON", all = TRUE)
p_todo <- merge(p_todo, p_coni, by = "ID_POLYGON", all = TRUE)

p_todo <- setnafill(p_todo, fill = 0, cols = 2:7)

write.csv(p_todo,
          paste("~/Traballo/Recursos/Datos_Cartografia/Usos&Cubertas/SIOSE/Derivados/p_todo_",
                i,
                ".csv", sep = ""),
          row.names = FALSE)
}


## Siose 2017
## Novos valores de coberturas en: https://www.siose.es/SIOSEtheme-theme/documentos/pdf/Estructura_base_datos_SIOSE_AR_v3.2.pdf

for(i in c("15_CORUÑA", "27_LUGO", "32_OURENSE", "36_PONTEVEDRA")) {

  valores <- fread(paste("~/Traballo/Recursos/Datos_Cartografia/Usos&Cubertas/SIOSE/SAR2017_",
                         i, "_GPKG/t_valores_selec.csv", sep = "")  )
  
  p_edif <- valores[ID_COBERTURA >= 101 & ID_COBERTURA <= 145, .(p_edif = round(sum(SUPERF_POR), 0)), .(ID_POLYGON)]
  p_cult <- valores[ID_COBERTURA >= 210 & ID_COBERTURA <= 290, .(p_cult = round(sum(SUPERF_POR), 0)), .(ID_POLYGON)]
  p_mato <- valores[ID_COBERTURA == 300 | ID_COBERTURA == 301 | ID_COBERTURA == 320 | 
                      (ID_COBERTURA == 310 & FF_ESPECIE_NOM == ""), .(p_mato = round(sum(SUPERF_POR), 0)), .(ID_POLYGON)]
  p_fcad <- valores[ID_COBERTURA == 312                      , .(p_fcad = round(sum(SUPERF_POR), 0)), .(ID_POLYGON)]
  p_fper <- valores[ID_COBERTURA == 313                      , .(p_fper = round(sum(SUPERF_POR), 0)), .(ID_POLYGON)]
  p_coni <- valores[ID_COBERTURA == 316                      , .(p_coni = round(sum(SUPERF_POR), 0)), .(ID_POLYGON)]
  p_plan <- valores[ID_COBERTURA == 310 & FF_ESPECIE_NOM != "", .(p.plan = round(sum(SUPERF_POR), 0)), .(ID_POLYGON)]
  
  p_todo <- merge(p_edif, p_cult, by = "ID_POLYGON", all = TRUE)
  p_todo <- merge(p_todo, p_mato, by = "ID_POLYGON", all = TRUE)
  p_todo <- merge(p_todo, p_fcad, by = "ID_POLYGON", all = TRUE)
  p_todo <- merge(p_todo, p_fper, by = "ID_POLYGON", all = TRUE)
  p_todo <- merge(p_todo, p_coni, by = "ID_POLYGON", all = TRUE)
  p_todo <- merge(p_todo, p_plan, by = "ID_POLYGON", all = TRUE)
  
  p_todo <- setnafill(p_todo, fill = 0, cols = 2:7)
  
  write.csv(p_todo,
            paste("~/Traballo/Recursos/Datos_Cartografia/Usos&Cubertas/SIOSE/Derivados/p_todo_2017_",
                  i,
                  ".csv", sep = ""),
            row.names = FALSE)
  
}
