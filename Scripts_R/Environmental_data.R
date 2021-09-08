# Calibration area (M) consist of ecoregions in area of interest (i.e., Argentina) where O. turicata
# has been recorded

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

# Set working directory, load environmental variables (global extent), and check resolution
# and layer extent

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

variables <- as.factor(c("flow_length","flow_acc"))

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


