----
#### R packages and libraries necessary for modeling process

```r
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
```