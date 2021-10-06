#------------------------------------------------------------------------------------
# Load valid watersheds
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

# Steps: 
# 1) Save all rasters as ascii
# 2) Crop and mask to M

# Load M

M <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_AVL.gpkg")

#----------------------------------------------------------------------
# Set working directory, load environmental variables (global extent), 
# and check resolution and layer extent
#----------------------------------------------------------------------

path1 = ("C:/Users/User/Documents/Analyses/AVL/Rasters/Rasters_procesar/") 
setwd("C:/Users/User/Documents/Analyses/AVL/Rasters/Rasters_procesar/")

files = list.files(path = path1, pattern = ".nc$", all.files = TRUE, full.names = FALSE)
files 

#------------------------------------------------------------------------------------
# Load ncd4 layers and save as ascii
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# Elevation
#------------------------------------------------------------------------------------

elevation = raster("./elevation.nc")
class(elevation)
elevation  # loads 1  of  2  bands (flow_acc)

elevation@file@nbands  # 4 bands

elevation_stack = raster::stack("./elevation.nc")
elevation_stack

elev_cropped <- crop(elevation_stack, M)

# Mask raster stack using the vector

elev_mask <- mask(elev_cropped, M)
class(elev_mask)  # "RasterBrick"

plot(elev_mask[[1]])  #
str(elev_mask)

elev_mask@file@nbands  # Outputs 1 band but has 4
elev_mask@data@names  # Nombre de las bandas: "X1" "X2" "X3" "X4" 

individual_elevation <- unstack(vars_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_elevation)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Elevation_min","Elevation_max","Elevation_range","Elevation_average"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_elevation[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}


#------------------------------------------------------------------------------------
# Flow
#------------------------------------------------------------------------------------

flow = raster("./flow_acc.nc")
flow  # loads 1  of  2  bands (flow_acc)

flow@file@nbands  # 2 bands

flow_stack = raster::stack("./flow_acc.nc")
flow_stack

flow_cropped <- crop(flow_stack, M)

# Mask raster stack using the vector

flow_mask <- mask(flow_cropped, M)
class(flow_mask)  # "RasterBrick"

plot(flow_mask[[1]])
str(flow_mask)

flow_mask@file@nbands  # Outputs 1 band but has 2
flow_mask@data@names  # Nombre de las bandas: "X1" "X2"

individual_flow <- unstack(flow_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_flow)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Upstream stream grid cells","Upstream catchment grid cells"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_flow[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}
























# Crop raster stack with 19 variables using the vector

allrasters <- stack(files)
class(allrasters)  # "RasterStack"

cropped <- crop(allrasters, M)
extent(cropped[[1]])

str(cropped)
class(cropped)  # Changes from RasterStack to RasterBrick

plot(cropped[[1]])


# Mask raster stack using the vector

vars_mask <- mask(cropped, M)
class(vars_mask)  # "RasterBrick"

plot(vars_mask[[1]])
str(vars_mask)

vars_mask@file@nbands  # 105 bands
vars_mask@data@names  # Nombre de las bandas 

up_elevation <-vars_mask[[1:4]]
up_elevation  

individual_r <- unstack(vars_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_r)  # list

plot(individual_r[[1]])


variables <- as.factor(c("elevation","flow_acc",                      
                         "geology_weighted_sum.nc","hydroclim_average+sum",         
                         "hydroclim_weighted_average+sum","landcover_average",             
                         "landcover_maximum","landcover_minimum",             
                         "landcover_range","landcover_weighted_average",    
                         "monthly_prec_sum","monthly_prec_weighted_sum",     
                         "monthly_tmax_average","monthly_tmax_weighted_average", 
                         "monthly_tmin_average","monthly_tmin_weighted_average", 
                         "quality_control","slope","soil_average","soil_maximum",                  
                         "soil_minimum","soil_range","soil_weighted_average"))

# Guardar como GTiff to keep multiple bands if necessary

for(i in 1:length(variables)) {
  writeRaster(individual_r[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_1/", variables[i]), format = "GTiff", options = "INTERLEAVE=BAND", overwrite=TRUE)
}







#------------------------------------------------------------------------------------
# Procesamiento y cálculo de estadísticas en capas individuales
#------------------------------------------------------------------------------------

path2 = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_1/"

setwd("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_1/")

files = list.files(path = path2, pattern = ".asc$", all.files = TRUE, full.names = FALSE)
files 

#------------------------------------------------------------------------------------
# Topography: Stream length and flow accumulation (two bands)
#------------------------------------------------------------------------------------

flow = raster("./flow_acc.asc")
class(flow)
flow  # loads 1  of  2  bands (flow_acc)

flow@file@nbands  # 1 band

flow_stack = raster::stack("./flow_acc.asc")
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


