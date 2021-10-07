### Load watersheds

``` r
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

rm(list=ls(all=TRUE))

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

rm(list=ls(all=TRUE))

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
  writeRaster(individual_hydroclimatic[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}


#------------------------------------------------------------------------------------
# Monthly upstream precipitation (distance-weighted sum)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

upstream_prec = raster("./monthly_prec_weighted_sum.nc")
upstream_prec  # loads 1 of 12 bands

upstream_prec@file@nbands  # 12 bands

upstream_prec_stack = raster::stack("./monthly_prec_weighted_sum.nc")
upstream_prec_stack

upstream_prec_cropped <- crop(upstream_prec_stack, M)

# Mask raster stack using the vector

upstream_prec_mask <- mask(upstream_prec_cropped, M)
class(upstream_prec_mask)  # "RasterBrick"

upstream_prec_mask@file@nbands  # 12
upstream_prec_mask@data@names  # Nombre de las bandas: "X1" ... "X12"

#---------------------------------------------------------------------------------------
# Pixel-wise stats for raster brick 
#---------------------------------------------------------------------------------------
# Reduction using mean: each layer represents the mean value for each month over the 1970-2000
# period. Then, one can apply a pixel-wise reduction functions (mean, min, max, etc.) 
# across layers to obtain summary statistics for each location (pixel) during this period. 
#---------------------------------------------------------------------------------------

upstream_prec_mean = mean(upstream_prec_mask)
class(upstream_prec_mean)

str(upstream_prec_mean)
upstream_prec_mean@file@nbands  # 1

writeRaster(upstream_prec_mean, filename = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/Monthly upstream precipitation", format = "GTiff", overwrite = TRUE)


#------------------------------------------------------------------------------------
# Monthly maximum temperature (distance-weighted average)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

monthly_tmax = raster("./monthly_tmax_weighted_average.nc")
monthly_tmax  # loads 1 of 12 bands

monthly_tmax@file@nbands  # 12 bands

monthly_tmax_stack = raster::stack("./monthly_tmax_weighted_average.nc")
monthly_tmax_stack

monthly_tmax_cropped <- crop(monthly_tmax_stack, M)

# Mask raster stack using the vector

monthly_tmax_mask <- mask(monthly_tmax_cropped, M)
class(monthly_tmax_mask)  # "RasterBrick"

monthly_tmax_mask@file@nbands  # 12
monthly_tmax_mask@data@names  # Nombre de las bandas: "X1" ... "X12"

#---------------------------------------------------------------------------------------
# Pixel-wise stats for raster brick 
#---------------------------------------------------------------------------------------
# Reduction using mean: each layer represents the mean value for each month over the 1970-2000
# period. Then, one can apply a pixel-wise reduction functions (mean, min, max, etc.) 
# across layers to obtain summary statistics for each location (pixel) during this period. 
#---------------------------------------------------------------------------------------

monthly_tmax_mask_mean = mean(monthly_tmax_mask)
class(monthly_tmax_mask_mean)

str(monthly_tmax_mask_mean)
monthly_tmax_mask_mean@file@nbands  # 1

writeRaster(monthly_tmax_mask_mean, filename = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/Monthly maximum temperature", format = "GTiff", overwrite = TRUE)


#------------------------------------------------------------------------------------
# Monthly minimum temperature (distance-weighted average)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

monthly_tmin = raster("./monthly_tmin_weighted_average.nc")
monthly_tmin  # loads 1 of 12 bands

monthly_tmin@file@nbands  # 12 bands

monthly_tmin_stack = raster::stack("./monthly_tmin_weighted_average.nc")
monthly_tmin_stack

monthly_tmin_cropped <- crop(monthly_tmin_stack, M)

# Mask raster stack using the vector

monthly_tmin_mask <- mask(monthly_tmin_cropped, M)
class(monthly_tmin_mask)  # "RasterBrick"

monthly_tmin_mask@file@nbands  # 12
monthly_tmin_mask@data@names  # Nombre de las bandas: "X1" ... "X12"

#---------------------------------------------------------------------------------------
# Pixel-wise stats for raster brick 
#---------------------------------------------------------------------------------------
# Reduction using mean: each layer represents the mean value for each month over the 1970-2000
# period. Then, one can apply a pixel-wise reduction functions (mean, min, max, etc.) 
# across layers to obtain summary statistics for each location (pixel) during this period. 
#---------------------------------------------------------------------------------------

monthly_tmin_mask_mean = mean(monthly_tmin_mask)
class(monthly_tmin_mask_mean)

str(monthly_tmin_mask_mean)
monthly_tmin_mask_mean@file@nbands  # 1

writeRaster(monthly_tmin_mask_mean, filename = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/Monthly minimum temperature", format = "GTiff", overwrite = TRUE)


#------------------------------------------------------------------------------------
# Slope
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

slope = raster("./slope.nc")
slope  # loads 1  of  4  bands (flow_acc)

slope@file@nbands  # 4 bands

slope_stack = raster::stack("./slope.nc")
slope_stack

slope_cropped <- crop(slope_stack, M)

# Mask raster stack using the vector

slope_mask <- mask(slope_cropped, M)
class(slope_mask)  # "RasterBrick"

slope_mask@file@nbands  # Outputs 1 band but has 4
slope_mask@data@names  # Nombre de las bandas: "X1" "X2" "X3" "X4"

individual_slope <- unstack(slope_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_slope)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Slope min","Slope max","Slope range","Slope average"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_slope[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}


#------------------------------------------------------------------------------------
# Soil: Upstream soil (maximum)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

slope = raster("./slope.nc")
slope  # loads 1  of  4  bands (flow_acc)

slope@file@nbands  # 4 bands

slope_stack = raster::stack("./slope.nc")
slope_stack

slope_cropped <- crop(slope_stack, M)

# Mask raster stack using the vector

slope_mask <- mask(slope_cropped, M)
class(slope_mask)  # "RasterBrick"

slope_mask@file@nbands  # Outputs 1 band but has 4
slope_mask@data@names  # Nombre de las bandas: "X1" "X2" "X3" "X4"

individual_slope <- unstack(slope_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_slope)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Slope min","Slope max","Slope range","Slope average"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_slope[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}

```

