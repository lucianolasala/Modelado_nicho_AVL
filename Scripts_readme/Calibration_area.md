----
#### Estimation of M
>Calibration area (M) consist of ecoregions in area of interest where FLA have been recorded.

```r
watersheds <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/hydrosheds/sa_bas_30s_beta")
class(watersheds)
str(watersheds)

valid = st_is_valid(watersheds)
length(valid)

which(valid=="FALSE") 
```
>Invalid IDs: 15 (3749  12788  15508  34768  36779  42630  45894  51724  57719  59742  60178  65296 69751 116697 131432)

```r
which(valid=="TRUE")  # length = 163640 elements

# Locate invalid geometries

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

bsas = arg %>% filter(NAME_1 == "Buenos Aires")
plot(bsas$geometry, add = TRUE)
```

>Result: none of the invalid geometries corresponds with those to be used as M.
Therefore, we filter out the 15 invalid ones and proceed with the rest.

```r
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
```

----
#### Load occurrences vector file

```r
avl <- st_read("C:/Users/User/Documents/Analyses/AVL/occs/AVL_occs.gpkg")
avl_df <- as.data.frame(sf::st_coordinates(avl))  # Retrieve coordinates in matrix form
colnames(avl_df) <- c("Long", "Lat")  # Name columns Long and Lat
```

----
#### Intersection between world watersheds and FLA occurrences

```r
avl_sf <- do.call("st_sfc", c(lapply(1:nrow(avl_df),  
                              function(i) {
                              st_point(as.numeric(avl_df[i, ]))}), list("crs" = 4326))) 
sf::sf_use_s2(TRUE)

watersheds <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Watersheds.gpkg")

avl_df$Watershed <- apply(st_intersects(watersheds, avl_sf, sparse = FALSE), MARGIN = 2,
                               function(water) { 
                                 watersheds[which(water), ]$BASIN_ID
                               })

unique_water <- unique(avl_df[c("Watershed")])
```

----
#### Select watersheds by their ID

```r
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

st_write(unique_watersheds_occs, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_AVL.gpkg", driver = "gpkg")
```

----
#### Disolve ecoregions with AVL occurrences

```r
watersheds <- readOGR("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_avl.gpkg")

watersheds_dissolved <- rgeos::gUnaryUnion(watersheds)

writeOGR(watersheds_spdf, layer = "watersheds_spdf", "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_dissolved.shp", driver = "ESRI Shapefile")
```





