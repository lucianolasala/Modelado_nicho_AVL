#----------------------------------------------------------------------
# Set working directory, load environmental variables (global extent), 
# and check resolution and layer extent
#----------------------------------------------------------------------

rm(list=ls(all=TRUE))

path = ("C:/Users/User/Documents/Analyses/AVL/Rasters/Rasters_procesar/") 
setwd("C:/Users/User/Documents/Analyses/AVL/Rasters/Rasters_procesar/")

files = list.files(path = path, pattern = ".nc$", all.files = TRUE, full.names = FALSE)
files 

G <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_projection/G_final.shp")

#------------------------------------------------------------------------------------
# Load ncd4 layers and save as ascii
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
# Slope range
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

slope = raster("./slope.nc")
slope  # loads 1  of  4  bands

slope@file@nbands  # 4 bands

slope_stack = raster::stack("./slope.nc")
slope_stack

slope_range <- slope_stack[[3]]
slope_range

slope_cropped <- crop(slope_range, G)

# Mask raster stack using the vector

slope_mask <- mask(slope_cropped, G)
class(slope_mask)  # "RasterBrick"

slope_mask@file@nbands  # Outputs 1 band
slope_mask@data@names  # Nombre de las bandas: "X3"

# Save as ascii

writeRaster(slope_mask, filename = "C:/Users/User/Documents/Analyses/AVL/Rasters/Projection_area/Slope range", format = "ascii")


#------------------------------------------------------------------------------------
# Flow
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

flow = raster("./flow_acc.nc")
flow  # loads 1  of  2  bands (flow_acc)

flow@file@nbands  # 2 bands

flow_stack = raster::stack("./flow_acc.nc")
flow_stack

flow_stack = flow_stack[[1]]

flow_cropped <- crop(flow_stack, G)

# Mask raster stack using the vector

flow_mask <- mask(flow_cropped, G)
class(flow_mask)  # "RasterBrick"

plot(flow_mask)
str(flow_mask)

flow_mask@file@nbands  # Outputs 1 band 
flow_mask@data@names  # Nombre de las bandas: "X1"

# Save as ascii

writeRaster(flow_mask, filename = "C:/Users/User/Documents/Analyses/AVL/Rasters/Projection_area/Upstream stream grid cells", format = "ascii")


#------------------------------------------------------------------------------------
# Soil: Upstream soil (average)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

soil_avg = raster("./soil_weighted_average.nc")
soil_avg  # loads 1  of  10  bands

soil_avg@file@nbands  # 10 bands

soil_avg_stack = raster::stack("./soil_weighted_average.nc")
soil_avg_stack

soil_avg_stack_B256 = soil_avg_stack[[c(2,5,6)]]

soil_avg_cropped <- crop(soil_avg_stack_B256, G)

# Mask raster stack using the vector

soil_avg_mask <- mask(soil_avg_cropped, G)
class(soil_avg_mask)  # "RasterBrick"

soil_avg_mask@file@nbands  # 1 band 
soil_avg_mask@data@names  # Nombre de las bandas: "X2" "X5" "X6"

print(soil_avg_mask)

individual_soil_avg <- unstack(soil_avg_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_soil_avg)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Soil_avg_02","Soil_avg_05","Soil_avg_06"))
                         
# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_soil_avg[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/Projection_area/", variables[i]), format = "ascii")
}


#------------------------------------------------------------------------------------
# Hydroclimatic variables
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

hydroclim = raster("./hydroclim_weighted_average+sum.nc")
hydroclim  # loads 1 of 19 bands

hydroclim@file@nbands  # 19 bands

hydroclim_stack = raster::stack("./hydroclim_weighted_average+sum.nc")
hydroclim_stack

hydroclim_stack_3.13.15 <- hydroclim_stack[[c(3,13,15)]]  

hydroclimatic_cropped <- crop(hydroclim_stack_3.13.15, G)

# Mask raster stack using the vector

hydroclimatic_cropped_mask <- mask(hydroclimatic_cropped, G)
class(hydroclimatic_cropped_mask)  # "RasterBrick"

plot(hydroclimatic_cropped_mask[[1]])
str(hydroclimatic_cropped_mask)

hydroclimatic_cropped_mask@file@nbands  # 1
hydroclimatic_cropped_mask@data@names  # Nombre de las bandas: "X3" "X13" "X15"

individual_hydroclimatic <- unstack(hydroclimatic_cropped_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_hydroclimatic)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Bioclim 3","Bioclim 13","Bioclim 15"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_hydroclimatic[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/Projection_area/", variables[i]), format = "ascii")
}
