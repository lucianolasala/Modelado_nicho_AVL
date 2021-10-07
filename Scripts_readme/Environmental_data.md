>Load watersheds to be used as calibration area (M)

```r

rm(list=ls(all=TRUE))

M <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_AVL.gpkg")
```

>Set working directory, load environmental variables (global extent), and process by cropping, masking, calculating summary statistics, and exploring raster extent and resolution.
Layers are available as near-global netCDF-4 files, where each file contains one 
variable and aggregation type (e.g. all 12 average landcover layers).

```r
path1 = ("C:/Users/User/Documents/Analyses/AVL/Rasters/Rasters_procesar/") 
setwd("C:/Users/User/Documents/Analyses/AVL/Rasters/Rasters_procesar/")

files = list.files(path = path1, pattern = ".nc$", all.files = TRUE, full.names = FALSE)
files 
```

#### Elevation

```r
elevation = raster("./elevation.nc")

elevation_stack = raster::stack("./elevation.nc")

elev_cropped <- crop(elevation_stack, M)

elev_mask <- mask(elev_cropped, M)

individual_elevation <- unstack(vars_mask)  # Must unstack to save each raster separately

variables <- as.factor(c("Elevation_min","Elevation_max","Elevation_range","Elevation_average"))

for(i in 1:length(variables)) {
  writeRaster(individual_elevation[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}
```

#### Flow

```r
rm(list=ls(all=TRUE))

flow = raster("./flow_acc.nc")

flow_stack = raster::stack("./flow_acc.nc")

flow_cropped <- crop(flow_stack, M)

flow_mask <- mask(flow_cropped, M)

individual_flow <- unstack(flow_mask)  

variables <- as.factor(c("Upstream stream grid cells","Upstream catchment grid cells"))

for(i in 1:length(variables)) {
  writeRaster(individual_flow[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}
```

#### Hydroclimatic variables

```r
hydroclim = raster("./hydroclim_weighted_average+sum.nc")

hydroclim_stack = raster::stack("./hydroclim_weighted_average+sum.nc")

hydroclimatic_cropped <- crop(hydroclim_stack, M)

hydroclimatic_cropped_mask <- mask(hydroclimatic_cropped, M)

individual_hydroclimatic <- unstack(hydroclimatic_cropped_mask)

variables <- as.factor(c("Bioclim 1","Bioclim 2","Bioclim 3","Bioclim 4","Bioclim 5",
                         "Bioclim 6","Bioclim 7","Bioclim 8","Bioclim 9","Bioclim 10",
                         "Bioclim 11","Bioclim 12","Bioclim 13","Bioclim 14","Bioclim 15",
                         "Bioclim 16","Bioclim 17","Bioclim 18","Bioclim 19"))

for(i in 1:length(variables)) {
  writeRaster(individual_hydroclimatic[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}
```

#### Monthly upstream precipitation (distance-weighted sum)

```r
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
```

#### Monthly maximum temperature (distance-weighted average)

```r
monthly_tmax = raster("./monthly_tmax_weighted_average.nc")

monthly_tmax_stack = raster::stack("./monthly_tmax_weighted_average.nc")

monthly_tmax_cropped <- crop(monthly_tmax_stack, M)

monthly_tmax_mask <- mask(monthly_tmax_cropped, M)

#### Pixel-wise stats for raster brick 

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
```

#------------------------------------------------------------------------------------
# Slope
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

slope = raster("./slope.nc")
slope  # loads 1  of  4  bands

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

soil_max = raster("./soil_maximum.nc")
soil_max  # loads 1  of  10  bands

soil_max@file@nbands  # 10 bands

soil_max_stack = raster::stack("./soil_maximum.nc")
soil_max_stack

soil_max_cropped <- crop(soil_max_stack, M)

# Mask raster stack using the vector

soil_max_mask <- mask(soil_max_cropped, M)
class(soil_max_mask)  # "RasterBrick"

soil_max_mask@file@nbands  # 10 bands
soil_max_mask@data@names  # Nombre de las bandas: "X1" ... "X10"

individual_soil_max <- unstack(soil_max_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_soil_max)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Soil_max_01","Soil_max_02","Soil_max_03","Soil_max_04",
                         "Soil_max_05","Soil_max_06","Soil_max_07","Soil_max_08",
                         "Soil_max_09","Soil_max_10"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_soil_max[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}


#------------------------------------------------------------------------------------
# Soil: Upstream soil (minimum)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

soil_min = raster("./soil_minimum.nc")
soil_min  # loads 1  of  10  bands

soil_min@file@nbands  # 10 bands

soil_min_stack = raster::stack("./soil_minimum.nc")
soil_min_stack

soil_min_cropped <- crop(soil_min_stack, M)

# Mask raster stack using the vector

soil_min_mask <- mask(soil_min_cropped, M)
class(soil_min_mask)  # "RasterBrick"

soil_min_mask@file@nbands  # 10 bands
soil_min_mask@data@names  # Nombre de las bandas: "X1" ... "X10"

individual_soil_min <- unstack(soil_min_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_soil_min)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Soil_min_01","Soil_min_02","Soil_min_03","Soil_min_04",
                         "Soil_min_05","Soil_min_06","Soil_min_07","Soil_min_08",
                         "Soil_min_09","Soil_min_10"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_soil_min[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}


#------------------------------------------------------------------------------------
# Soil: Upstream soil (range)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

soil_range = raster("./soil_range.nc")
soil_range  # loads 1  of  10  bands

soil_range@file@nbands  # 10 bands

soil_range_stack = raster::stack("./soil_range.nc")
soil_range_stack

soil_range_cropped <- crop(soil_range_stack, M)

# Mask raster stack using the vector

soil_range_mask <- mask(soil_range_cropped, M)
class(soil_range_mask)  # "RasterBrick"

soil_range_mask@file@nbands  # 10 bands
soil_range_mask@data@names  # Nombre de las bandas: "X1" ... "X10"

individual_soil_range <- unstack(soil_range_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_soil_range)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Soil_range_01","Soil_range_02","Soil_range_03","Soil_range_04",
                         "Soil_range_05","Soil_range_06","Soil_range_07","Soil_range_08",
                         "Soil_range_09","Soil_range_10"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_soil_range[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}


#------------------------------------------------------------------------------------
# Soil: Upstream soil (average)
#------------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

soil_avg = raster("./soil_weighted_average.nc")
soil_avg  # loads 1  of  10  bands

soil_avg@file@nbands  # 10 bands

soil_avg_stack = raster::stack("./soil_weighted_average.nc")
soil_avg_stack

soil_avg_cropped <- crop(soil_avg_stack, M)

# Mask raster stack using the vector

soil_avg_mask <- mask(soil_avg_cropped, M)
class(soil_avg_mask)  # "RasterBrick"

soil_avg_mask@file@nbands  # 10 bands
soil_avg_mask@data@names  # Nombre de las bandas: "X1" ... "X10"

individual_soil_avg <- unstack(soil_avg_mask)  # Hay que hacer unstack para luego guardar cada raster individual
class(individual_soil_avg)  # list

# Define names for each of the bands to be saved 

variables <- as.factor(c("Soil_avg_01","Soil_avg_02","Soil_avg_03","Soil_avg_04",
                         "Soil_avg_05","Soil_avg_06","Soil_avg_07","Soil_avg_08",
                         "Soil_avg_09","Soil_avg_10"))

# Save as ascii

for(i in 1:length(variables)) {
  writeRaster(individual_soil_avg[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/", variables[i]), format = "GTiff")
}


#----------------------------------------------------------------------------
# Check spatial resolution and raster extent for final environmental layers
#----------------------------------------------------------------------------

mytable <- NULL

path2 = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/" 
setwd("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/")

files = list.files(path = path2, pattern = ".tif$", all.files = TRUE, full.names = FALSE)
files 

for(i in 1:72){
  r <- raster(files[i])
  mytable <- rbind(mytable, c(files[i], round(c(res(r), as.vector(extent(r))), 8)))
}

colnames(mytable) <- c("File","Resol.x","Resol.y","xmin","xmax","ymin","ymax")
mytable

xlsx::write.xlsx(mytable, file = "C:/Users/User/Documents/Analyses/Ticks ENM/Modeling/O_turicata/Raster_props_calibration.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

```

