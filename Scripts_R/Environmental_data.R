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

remove(valid_watersheds)

plot(valid_watersheds$geometry, col = "blue")

st_write(valid_watersheds, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Watersheds.gpkg", driver = "gpkg")

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


#-------------------------------------------------------------------------
# Disolve ecoregions with AVL occurrences
#-------------------------------------------------------------------------

install.packages("rgdal")
install.packages("rgeos")

turicata_dis <- readOGR("C:/Users/User/Documents/Analyses/Ticks ENM/Vector data/O_turicata_M/turicata_ecoregions.gpkg")

turicata_dis <- st_read("C:/Users/User/Documents/Analyses/Ticks ENM/Vector data/O_turicata_M/turicata_ecoregions.gpkg")
class(turicata_dis)


turicata_dissolved <- rgeos::gUnaryUnion(turicata_dis)   # fc in rgeos pkg
class(turicata_dissolved)  # SpatialPolygons, no need to SpatialPolygonsDataFrame

str(turicata_dissolved)
plot(turicata_dissolved, col = "blue")








#------------------------------------------------------------------------------------
# Load valid watersheds
#------------------------------------------------------------------------------------

remove(watersheds)

watersheds <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Watersheds.gpkg")

avl_df$Watershed <- apply(st_intersects(watersheds, avl_sf, sparse = FALSE), MARGIN = 2,
                               function(watershed) { 
                                 watersheds[which(watershed), ]$BASIN_ID
                               })

avl_df

#----------------------------------------------------------------------
# Set working directory, load environmental variables (global extent), 
# and check resolution and layer extent
#----------------------------------------------------------------------

path1 = ("C:/Users/User/Documents/Analyses/AVL/Rasters/Original/") 
setwd("C:/Users/User/Documents/Analyses/AVL/Rasters/Original/")

files = list.files(path = path1, pattern = ".nc$", all.files = TRUE, full.names = FALSE)
files 

#------------------------------------------------------------------------------------
# Load ncd4 layers and save as ascii
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# Topography: Stream length and flow accumulation (two bands)
#------------------------------------------------------------------------------------

flow = raster("./flow_acc.nc")
class(flow)
flow  # loads 1  of  2  bands

flow@file@nbands  # 2 bands

flow_stack = raster::stack("./flow_acc.nc")
flow_stack

flow_ind <- unstack(flow_stack)
class(flow_ind)  # list

plot(flow_ind[[1]])

variables <- as.factor(c("Flow_length","Flow_acc"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(flow_ind[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii/", variables[i]), format = "ascii")
}


#------------------------------------------------------------------------------------
# Topography: Upstream elevation (min, max, range, avg)
#------------------------------------------------------------------------------------

topography_stack <- raster::stack("./elevation.nc")

topography_ind <- unstack(topography_stack)

variables <- as.factor(c("Minimum elevation","Maximum elevation","Elevation range","Average elevation"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(topography_ind[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii/", variables[i]), format = "ascii")
}


#------------------------------------------------------------------------------------
# Topography: Upstream slope (min, max, range, avg)
#------------------------------------------------------------------------------------

topography_slope <- raster::stack("./slope.nc")

slope_ind <- unstack(topography_slope)

variables <- as.factor(c("Minimum slope","Maximum slope","Slope range","Average slope"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(slope_ind[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii/", variables[i]), format = "ascii")
}


#------------------------------------------------------------------------------------
# Climate	Monthly minimum temperature (average)
# Raster stack with 12 bands (one for each month).
# First we crop global extent to calibration area extent, then mask, and fillany
# calculate mean
#------------------------------------------------------------------------------------

monthly_tmin <- raster::stack("./monthly_tmin_average.nc")

monthly_tmin_mean = mean(monthly_tmin)
monthly_tmin_mean






monthly_tmin_ind <- unstack(monthly_tmin)



# Si precisaramos guardar la variable para cada mes en el serie temporal:

jan = monthly_tmin_ind[[1]]
writeRaster(jan, filename = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii/Min. monthly air temperature January", format = "ascii", overwrite=TRUE)

variables <- as.factor(c("Min. monthly air temperature January",
                         "Min. monthly air temperature February",
                         "Min. monthly air temperature March",
                         "Min. monthly air temperature April",
                         "Min. monthly air temperature May",
                         "Min. monthly air temperature June",
                         "Min. monthly air temperature July",
                         "Min. monthly air temperature August",
                         "Min. monthly air temperature September",
                         "Min. monthly air temperature October",
                         "Min. monthly air temperature November",
                         "Min. monthly air temperature December"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(monthly_tmin_ind[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii/", variables[i]), format = "ascii", overwrite=TRUE)
}

#------------------------------------------------------------------------------------
# Climate	Monthly maximum temperature (average)
#------------------------------------------------------------------------------------

monthly_tmax <- raster::stack("./monthly_tmax_average.nc")

monthly_tmax_ind <- unstack(monthly_tmax)

variables <- as.factor(c("Max. monthly air temperature January",
                         "Max. monthly air temperature February",
                         "Max. monthly air temperature March",
                         "Max. monthly air temperature April",
                         "Max. monthly air temperature May",
                         "Max. monthly air temperature June",
                         "Max. monthly air temperature July",
                         "Max. monthly air temperature August",
                         "Max. monthly air temperature September",
                         "Max. monthly air temperature October",
                         "Max. monthly air temperature November",
                         "Max. monthly air temperature December"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(monthly_tmax_ind[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii/", variables[i]), format = "ascii", overwrite=TRUE)
}








#----------------------------------------------------------------------------
# Check spatial resolution and raster extent for final environmental layers
#----------------------------------------------------------------------------

mytable <- NULL

for(i in 1:23){
  r <- raster(files[i])
  mytable <- rbind(mytable, c(files[i], round(c(res(r), as.vector(extent(r))), 8)))
}

colnames(mytable1) <- c("File","Resol.x","Resol.y","xmin","xmax","ymin","ymax")
mytable1

xlsx::write.xlsx(mytable, file = "C:/Users/User/Documents/Analyses/Ticks ENM/Modeling/O_turicata/Raster_props_calibration.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


