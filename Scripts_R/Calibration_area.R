# Calibration area (M) consist of ecoregions in area of interest where FLA
# have been recorded

# Load required packages

gc()

rm(list=ls(all=TRUE))

if(!require(tidyverse)){
  install.packages("tidyverse")
}
if(!require(sf)){
  install.packages("sf")
}
if(!require(readr)){
  install.packages("readr")
}
if(!require(raster)){
  install.packages("raster")
}
if(!require(udunits2)){
  install.packages("udunits2")
}
if(!require(ncdf4)){
  install.packages("ncdf4")
}
if(!require(rgdal)){
  install.packages("rgdal")
}
if(!require(rgeos)){
  install.packages("rgeos")
}


library("rgdal")
library("rgeos")
library("readr")
library("sf")
library("tidyverse")
library("raster")
library("udunits2")
library("ncdf4")
library("magrittr")

#------------------------------------------------------------------------------------
# Estimation of M
#------------------------------------------------------------------------------------

# Carga de archivo vectorial de cuencas 

watersheds <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/hydrosheds/sa_bas_30s_beta")
class(watersheds)
str(watersheds)

valid = st_is_valid(watersheds)
length(valid)

which(valid=="FALSE") # 15 (numbers 3749  12788  15508  34768  36779  42630  45894  51724  
                      # 57719  59742  60178  65296 69751 116697 131432)

which(valid=="TRUE")  # 163640

# Ver donde se ubican los poligonos con geometrias invalidas

corrupt = watersheds %>% filter(
  BASIN_ID == 3749 | 
  BASIN_ID == 12788 | 
  BASIN_ID == 15508 |
  BASIN_ID == 34768 |
  BASIN_ID == 36779 |
  BASIN_ID == 42630 |
  BASIN_ID == 45894 |
  BASIN_ID == 51724 |
  BASIN_ID == 57719 |
  BASIN_ID == 59742 |
  BASIN_ID == 60178 |
  BASIN_ID == 65296 |
  BASIN_ID == 69751 |
  BASIN_ID == 116697 |
  BASIN_ID == 131432)  
  
plot(corrupt$geometry)

arg <- st_read("C:/Users/User/Documents/Analyses/Wild boar diseases/Shapefiles/ARG_adm/ARG_adm2.shp")
class(arg)
str(arg)

bsas = arg %>% filter(NAME_1 == "Buenos Aires")
plot(bsas$geometry, add = TRUE)

# Result: none of the invalid geometries corresponds with those to be used as M. 
# Therefore, we filter out the 15 invalid ones and proceed with the rest.

valid_watersheds = watersheds[!(watersheds$BASIN_ID == 3749|
                                  watersheds$BASIN_ID == 12788| 
                                  watersheds$BASIN_ID == 15508|
                                  watersheds$BASIN_ID == 34768|
                                  watersheds$BASIN_ID == 36779|
                                  watersheds$BASIN_ID == 42630|
                                  watersheds$BASIN_ID == 45894|
                                  watersheds$BASIN_ID == 51724|
                                  watersheds$BASIN_ID == 57719|
                                  watersheds$BASIN_ID == 59742|
                                  watersheds$BASIN_ID == 60178|
                                  watersheds$BASIN_ID == 65296|
                                  watersheds$BASIN_ID == 69751|
                                  watersheds$BASIN_ID == 116697|
                                  watersheds$BASIN_ID == 131432),]

plot(valid_watersheds$geometry, col = "blue")

st_write(valid_watersheds, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Valid_watersheds.gpkg", driver = "gpkg")


#------------------------------------------------------------------------
# Carga de archivo vectorial de ocurrencias
#------------------------------------------------------------------------

rm(list=ls(all=TRUE))

avl <- st_read("C:/Users/User/Documents/Analyses/AVL/occs/AVL_occs.gpkg")

summary(avl)
class(avl)
head(avl)

avl_df <- as.data.frame(sf::st_coordinates(avl))  # Retrieve coordinates in matrix form
head(avl_df)

colnames(avl_df) <- c("Long", "Lat")  # Name columns Long and Lat
head(avl_df)
length(avl_df$Long)  # 96


#------------------------------------------------------------------------------------
# Intersect between world watersheds and FLA occurrences
#------------------------------------------------------------------------------------

# Create a points collection
# The do.call function executes a function by its name (here, "st_sfc") and a list of 
# corresponding arguments. 
# st_sfc = Create simple feature geometry list column (the column "geometry" in sf dataframe)
# st_point = Create simple feature from a numeric vector, matrix or list
# lapply() es un caso especial de apply(), diseñado para aplicar funciones a todos 
# los elementos de una lista (de alli la letra "l")
# Referencia para lapply: https://bookdown.org/jboscomendoza/r-principiantes4/lapply.html

# avl_df is "data.frame" and needs to be transformed into "sf" for further spatial operations

avl_sf <- do.call("st_sfc", c(lapply(1:nrow(avl_df),  
                              function(i) {
                              st_point(as.numeric(avl_df[i, ]))}), list("crs" = 4326))) 

head(avl_sf)
class(avl_sf)  # "sfc_POINT" "sfc"

sf::sf_use_s2(TRUE)

watersheds <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Valid_watersheds.gpkg")
str(watersheds)

avl_df$Watershed <- apply(st_intersects(watersheds, avl_sf, sparse = FALSE), MARGIN = 2,
                               function(water) { 
                                 watersheds[which(water), ]$BASIN_ID
                               })
avl_df
is.data.frame(avl_df)

unique_water <- unique(avl_df[c("Watershed")])
unique_water

#----------------------------------------------------------------------
# Select watersheds by their ID (long way)
#----------------------------------------------------------------------

unique_watersheds_occs = watersheds %>% filter(BASIN_ID == 61397| 
                                        BASIN_ID == 61676|
                                        BASIN_ID == 62368|
                                        BASIN_ID == 62779|
                                        BASIN_ID == 59931|
                                        BASIN_ID == 63766|
                                        BASIN_ID == 63524|
                                        BASIN_ID == 61957|
                                        BASIN_ID == 61832|
                                        BASIN_ID == 61863|
                                        BASIN_ID == 61843|
                                        BASIN_ID == 61888|
                                        BASIN_ID == 58476|
                                        BASIN_ID == 61712|
                                        BASIN_ID == 61032|
                                        BASIN_ID == 60630|
                                        BASIN_ID == 60671|
                                        BASIN_ID == 60734)
class(unique_watersheds_occs)
plot(unique_watersheds_occs$geom)

st_write(unique_watersheds_occs, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_AVL.gpkg", driver = "gpkg")

#-------------------------------------------------------------------------
# Disolve ecoregions with AVL occurrences
#-------------------------------------------------------------------------

watersheds <- readOGR("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_avl.gpkg")

watersheds_dissolved <- rgeos::gUnaryUnion(watersheds)   # fc in rgeos pkg
class(watersheds_dissolved)  # SpatialPolygons, no need to SpatialPolygonsDataFrame
str(watersheds_dissolved)

watersheds_spdf = as(watersheds_dissolved, "SpatialPolygonsDataFrame")
class(watersheds_spdf)

plot(watersheds_dissolved, col = "blue")
plot(watersheds_spdf, col = "blue")

writeOGR(watersheds_spdf, layer = "watersheds_spdf", "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_dissolved.shp", driver = "ESRI Shapefile")
