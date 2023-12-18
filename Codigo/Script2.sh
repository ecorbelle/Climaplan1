## En GRASS: 
## 1. Importar as sete capas de polígonos (as de 2017 con snap=1 e min_area=100) 
## 2. Importar as sete táboas resultantes de Script1_SIOSE.R
## 3. Crear un índice para cada unha con db.execute sql="create index porcent2005_index on porcent2005(ID_POLYGON)"
## 4. Unir con v.db.join    ;o)

## SIOSE 2017 (Alternativa 1)

v.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/SAR2017_15_CORUÑA_GPKG/15_CORUÑA.gpkg         layer=SAR_15_T_POLIGONOS out=poligonos_coruna     snap=1 min_area=100
v.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/SAR2017_27_LUGO_GPKG/27_LUGO.gpkg             layer=SAR_27_T_POLIGONOS out=poligonos_lugo       snap=1 min_area=100
v.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/SAR2017_32_OURENSE_GPKG/32_OURENSE.gpkg       layer=SAR_32_T_POLIGONOS out=poligonos_ourense    snap=0.5 min_area=100
v.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/SAR2017_36_PONTEVEDRA_GPKG/36_PONTEVEDRA.gpkg layer=SAR_36_T_POLIGONOS out=poligonos_pontevedra snap=0.5 min_area=100

db.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/Derivados/p_todo_2017_15_CORUÑA.csv     out=porcent_coruna
db.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/Derivados/p_todo_2017_27_LUGO.csv       out=porcent_lugo
db.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/Derivados/p_todo_2017_32_OURENSE.csv    out=porcent_ourense
db.in.ogr in=Traballo/Recursos/Datos_Cartografia/Usos\&Cubertas/SIOSE/Derivados/p_todo_2017_36_PONTEVEDRA.csv out=porcent_pontevedra

db.execute sql="create index porcent_coruna_index     on porcent_coruna(ID_POLYGON)"
db.execute sql="create index porcent_lugo_index       on porcent_lugo(ID_POLYGON)"
db.execute sql="create index porcent_ourense_index    on porcent_ourense(ID_POLYGON)"
db.execute sql="create index porcent_pontevedra_index on porcent_pontevedra(ID_POLYGON)"

v.db.join map=poligonos_coruna     column="ID_POLYGON" other_table=porcent_coruna     other_column="ID_POLYGON"
v.db.join map=poligonos_lugo       column="ID_POLYGON" other_table=porcent_lugo       other_column="ID_POLYGON"
v.db.join map=poligonos_ourense    column="ID_POLYGON" other_table=porcent_ourense    other_column="ID_POLYGON"
v.db.join map=poligonos_pontevedra column="ID_POLYGON" other_table=porcent_pontevedra other_column="ID_POLYGON"


# g.mapset Prox_Climaplan2

g.mapsets SIOSE,siose_2017

g.region vect=poligonos_siose_2014 res=25 -ap
r.mask   vect=Parroquias

# Paso a ráster dos valores de porcentaxe de área de diferentes cubertas
for i in 2005 2009 2014

do
for j in p_edif p_cult p_mato p_fcad p_fper p_coni

do

v.to.rast in=poligonos_siose_${i} out=${j}_${i} use=attr attr="cast($j as integer)"

done
done

# Paso a ráster dos valores de 2017

for i in  p_plan # p_edif p_cult p_mato p_fcad p_fper p_coni
do 
for j in coruna lugo ourense pontevedra
do
g.region vect=poligonos_${j} res=25 -ap
v.to.rast in=poligonos_${j} out=${i}_${j} use=attr attribute_column="cast($i as integer)" --o
done
g.region rast=p_cult_2005
r.patch in=${i}_coruna,${i}_lugo,${i}_ourense,${i}_pontevedra out=${i}_2017 --o
done

g.remove type=rast pattern=*coruna -f
g.remove type=rast pattern=*lugo -f
g.remove type=rast pattern=*ourense -f
g.remove type=rast pattern=*pontevedra -f




# SIOSE 2017 (Alternativa 2)

g.region vect=poligonos_coruna res=25
v.to.rast in=poligonos_coruna out=idmax1 use=attr attr=ID_COBERTURA_MAX
g.region vect=poligonos_lugo res=25
v.to.rast in=poligonos_lugo out=idmax2 use=attr attr=ID_COBERTURA_MAX
g.region vect=poligonos_ourense res=25
v.to.rast in=poligonos_ourense out=idmax3 use=attr attr=ID_COBERTURA_MAX
g.region vect=poligonos_pontevedra res=25
v.to.rast in=poligonos_pontevedra out=idmax4 use=attr attr=ID_COBERTURA_MAX

r.patch in=idmax1,idmax2,idmax3,idmax4 out=idMax
g.remove type=rast name=idmax1,idmax2,idmax3,idmax4 -f

r.reclass in=idMax out=idMax_r rules=Scripts/Reclas_2017.txt

g.region rast=idMax -ap
v.to.rast in=concellos out=concellos use=attr attr=codigoine
r.stats -aln in=concellos,idMax_r out=Tmp/siose2017_concellos.csv sep=","


# Resumo estatístico por parroquias e concellos
g.copy vect=Parroquias,parroquias --o
g.copy vect=concellos_GL,concellos --o
r.mask -r
g.region rast=p_cult_2005


for i in `g.list type=rast pattern=p_*`

do
echo $i
v.rast.stats -c map=parroquias raster=$i column_prefix=$i method=sum
v.rast.stats -c map=concellos  raster=$i column_prefix=$i method=sum
done

v.to.db map=parroquias option=area columns=area_ha units=hectares
v.to.db map=concellos  option=area columns=area_ha units=hectares



# Variables do terreo (elevación media, pendente media por parroquia, distancia ao mar)
r.mask -r
g.region rast=mdt25

r.slope.aspect elev=mdt25 slope=pend25 format=percent --o
r.mapcalc expression="mar=if(mdt25=0, 1, null())" --o
r.grow.distance -m input=mar distance=distanciamar metric=euclidean

for i in mdt25 pend25 distanciamar
do
v.rast.stats map=parroquias raster=$i column_prefix=$i method=average
v.rast.stats map=concellos  raster=$i column_prefix=$i method=average
done


# Exportación do resultado
v.out.ogr    in=parroquias out="Tmp/Areas_siose.gpkg"  output_layer="parroquias" --o
v.out.ogr -a in=concellos  out="Tmp/Areas_siose.gpkg"  output_layer="concellos"


# Resumo estatístico, por parroquias, do MCA 
#g.mapsets MCA
#g.region rast=mca1989reclas
#v.to.rast in=parroquias out=parroquias use=attr attribute="cast(CodPARRO as integer)" label=Parroquia --o
#r.stats -aln in=parroquias,mca1989reclas out=Tmp/parroquias_mca89.csv sep="|" --o
#r.stats -aln in=parroquias,mca2009reclas out=Tmp/parroquias_mca09.csv sep="|" --o


