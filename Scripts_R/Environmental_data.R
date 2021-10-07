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


#------------------------------------------------------------------------------------
# Hydroclimatic variables
#------------------------------------------------------------------------------------

hydroclim = raster("./hydroclim_weighted_average+sum.nc")
hydroclim  # loads 1 of 19 bands

hydroclim@file@nbands  # 19 bands

hydroclim_stack = raster::stack("./hydroclim_weighted_average+sum.nc")
hydroclim_stack

hydroclimatic_cropped <- crop(hydroclim_stack, M)

# Mask raster stack using the vector

hydroclimatic_cropped_mask <- mask(hydroclimatic_cropped, M)
class(hydroclimatic_cropped_mask)  # "RasterBrick"

plot(hydroclimatic_cropped_mask[[1]])
str(hydroclimatic_cropped_mask)

hydroclimatic_cropped_mask@file@nbands  # 19
hydroclimatic_cropped_mask@data@names  # Nombre de las bandas: "X1" ... "X19"

individual_hydroclimatic <- unstack(hydroclimatic_cropped_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_hydroclimatic)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Bioclim 1","Bioclim 2","Bioclim 3","Bioclim 4","Bioclim 5",
                         "Bioclim 6","Bioclim 7","Bioclim 8","Bioclim 9","Bioclim 10",
                         "Bioclim 11","Bioclim 12","Bioclim 13","Bioclim 14","Bioclim 15",
                         "Bioclim 16","Bioclim 17","Bioclim 18","Bioclim 19"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_flow[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
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


