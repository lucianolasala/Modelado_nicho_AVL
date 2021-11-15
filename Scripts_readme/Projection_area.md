----
#### Estimation of G
>Projection area (G) consist of ecoregions in area of interest where FLA have not been recorded.

----
#### Loading watersheds file 

```r
watersheds <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/hydrosheds/sa_bas_30s_beta")
class(watersheds)
str(watersheds)

valid = st_is_valid(watersheds)
length(valid)

which(valid == "FALSE") # 15 (numbers 3749  12788  15508  34768  36779  42630  45894  51724  
# 57719  59742  60178  65296 69751 116697 131432)

which(valid=="TRUE")  # 163640
```

----
#### Explore where the polygons with invalid geometries are

```r
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

st_write(corrupt, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Corrupt_watersheds.gpkg", driver = "gpkg")

arg <- st_read("C:/Users/User/Documents/Analyses/Wild boar diseases/Shapefiles/ARG_adm/ARG_adm2.shp")
class(arg)
str(arg)

bsas = arg %>% filter(NAME_1 == "Buenos Aires")
plot.new()
plot(bsas$geometry, add = TRUE)

st_write(corrupt, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Bs_As.gpkg", driver = "gpkg")
```

>Result: There are 3 watersheds out of 15 that have invalid geoms. and correspond to G.

----
#### Loading all watersheds

watersheds <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/hydrosheds/sa_bas_30s_beta/sa_bas_30s_beta.shp")
str(watersheds)

----
#### Loading watersheds in M

```r
watersheds_M <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Watersheds_AVL.gpkg")
```

----
#### Watersheds difference

```r
ids <- watersheds_M$BASIN_ID
```

----
#### Filgter watersheds that are not part of M

```r
watersheds_not_M <- watersheds[!(watersheds$BASIN_ID == 58476|
                                     watersheds$BASIN_ID == 59931|
                                     watersheds$BASIN_ID == 60630|
                                     watersheds$BASIN_ID == 60671|
                                     watersheds$BASIN_ID == 60734|
                                     watersheds$BASIN_ID == 61032|
                                     watersheds$BASIN_ID == 61397|
                                     watersheds$BASIN_ID == 61676|
                                     watersheds$BASIN_ID == 61712|
                                     watersheds$BASIN_ID == 61832|
                                     watersheds$BASIN_ID == 61843|
                                     watersheds$BASIN_ID == 61973|
                                     watersheds$BASIN_ID == 62368|
                                     watersheds$BASIN_ID == 62779|
                                     watersheds$BASIN_ID == 63524|
                                     watersheds$BASIN_ID == 63766),] 
    
plot(watersheds_not_M$geometry, col = "blue")

st_write(watersheds_not_M, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Valid watersheds/Watersheds_not_M.gpkg", driver = "gpkg")
```

>Since there are many watersheds with topology errors, these were corrected using QGIS. For practica purposes, only those overlapping Bs. As. province were explored and processed, ignoring non-overlapping ones. 

----
#### Loading Bs. As. province and watersheds and selecting those overlapping (totally or partially) with Bs. As. province

```r
bsas <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_calibracion/Bs.As_province.gpkg")

watersheds_bsas <- st_read("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Watersheds/Watersheds_not_M_subset.gpkg")

G <- st_intersection(watersheds_bsas, bsas)

plot(G$geom)

st_write(G, "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_proyeccion/G.gpkg")
```

----
#### Disolve ecoregions without AVL occurrences

```r
G <- readOGR("C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_proyeccion/G.gpkg")

G_dissolved <- rgeos::gUnaryUnion(G)   # fc in rgeos pkg
class(G_dissolved)  # SpatialPolygons, no need to SpatialPolygonsDataFrame
str(G_dissolved)

plot(G_dissolved, col = "blue")

G_dissolved = as(G_dissolved, "SpatialPolygonsDataFrame")
class(G_dissolved)

writeOGR(G_dissolved, layer = "G_dissolved", "C:/Users/User/Documents/Analyses/AVL/Vectoriales/Area_proyeccion/G_final.shp", driver = "ESRI Shapefile")
```
