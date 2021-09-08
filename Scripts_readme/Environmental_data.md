----------
Calibration area (M) consist of ecoregions in area of interest where FLA have been recorded.

``` r
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

# Set working dir

setwd("C:/Users/User/Documents/Analyses/AVL/Rasters/Original/") 

files = list.files(pattern = ".nc$", all.files = TRUE, full.names = FALSE)
files 

#------------------------------------------------------------------------------------
# Load one raster ncd4
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

# Guardar como ascii

for(i in 1:length(variables)) {
  writeRaster(flow_ind[[i]], filename = paste0("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii/", variables[i]), format = "ascii")
}

```

