#------------------------------------------------------------------------------------
# Correlation analysis for variable selection
#------------------------------------------------------------------------------------

gc()
rm(list=ls(all=TRUE))

library(tidyverse)
library(sf)
library(stars)
library(magrittr)
library(raster)
library(xlsx)
library(virtualspecies)
library(corrplot)
library(ggcorrplot)

path = ("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas")

files <- list.files(path = path, pattern = ".tif$", full.names = TRUE)
files

vnames <- list.files(path = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas", pattern = ".tif$", full.names = FALSE) %>%
    str_replace(".tif", "")
vnames

#------------------------------------------------------------------------
# Check spatial resolution and raster extent for layers
# Bio8 and Bio9 were removed due to aberrant data patterns in the study region
# Bio18 (Precipitation of Warmest Quarter) and Bio19 (Precipitation of Coldest Quarter)
# were removed because they combine precip. and temp. variables. 
#------------------------------------------------------------------------

mytable <- NULL

for(i in 1:40){
  r <- raster(files[i])
  mytable <- rbind(mytable, c(files[i], round(c(res(r), as.vector(extent(r))), 8)))
}

colnames(mytable) <- c("File","Resol.x","Resol.y","xmin","xmax","ymin","ymax")
mytable

xlsx::write.xlsx(mytable, file = "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Raster_props_calibration.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


#------------------------------------------------------------------------------------
# Identify cells with data
#------------------------------------------------------------------------------------

st <- read_stars(files[1]) %>% set_names("z")
n <- which(!is.na(st$z))
length(n)  # 27607


#----------------------------------------------------------------
# Draw random sample from rasters
#----------------------------------------------------------------

set.seed(100)
ssize = 10000

sm <- sample(n, size = ssize)
length(sm)

## Sample random data to perform correlation analysis

dt <- NULL

for(i in 1:40){
    st <- read_stars(files[i]) %>% set_names("z")
    dt <- cbind(dt, st$z[sm])
}

dt <- dt %>%
    as_tibble(.name_repair = "minimal") %>%
    set_names(vnames)

dim(dt)
head(dt)

write.table(dt, file = "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_sample.txt")

class(dt)  # "data.frame"
dim(dt)    # 10000 (rows) * 40 (columns)

#---------------------------------------------------------------------------------------
# Explore correlation on sampled matrix and remove highly correlated variables.
# Remove each variable in turn and re-run this bit until all correlations are below 0.8.
#---------------------------------------------------------------------------------------

dt = read.table("C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_sample.txt", header = TRUE)

get.corr <- function(x){
    crr <- Hmisc::rcorr(as.matrix(x), type = "spearman")
    ut <- upper.tri(crr$r)
    vnames <- colnames(crr$r)
    crr <- data.frame(v1 = vnames[row(crr$r)[ut]],
                      v2 = vnames[col(crr$r)[ut]],
                      cor = crr$r[ut]) %>%
        as_tibble() %>%
        mutate(cor = abs(cor)) %>%
        arrange(desc(cor)) %>%
        filter(cor >= .8)
    return(crr)
}

cr <- get.corr(dt)
cr

to.remove <- names(sort(table(c(cr$v1, cr$v2)), decreasing = TRUE))
to.remove  # "22" "4"  "9"  "21" "1"  "3"  "6"  "19" "23" "2"  "8"  "10" "18" "25" "33" "5"  "13"
           # "14" "15" "28" "7"  "16" "20" "24" "27" "31" "39" "11" "17" "26"


# Extract each variable in turn and then run the flattenCorrMatrix function:

while(length(to.remove) > 0){
  dt <- dt %>%
    dplyr::select(-as.numeric(to.remove[1]))
  cr <- get.corr(dt)
  to.remove <- names(sort(table(c(cr$v1,cr$v2)), decreasing=TRUE))
}

dt
dim(dt)  # 10000 * 20
is.list(dt)
colnames(dt)

summary(dt)

write.table(dt, file = "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_selected_vars.txt")
xlsx::write.xlsx(dt, file = "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_selected_vars.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


#----------------------------------------------------------------
# Transform selected variables to ascii
#----------------------------------------------------------------

path = ("C:/Users/User/Documents/Analyses/AVL/Rasters/Corr_selected_rasters")

files <- list.files(path = path, pattern = ".tif$", full.names = TRUE)
files

ftif <- gsub("\\.tif$", ".asc", files)

for(i in 1:length(files)) {
  r <- raster(files[i])
  r <- writeRaster(r, ftif[i])
}


#----------------------------------------------------------------
# LFLS. Correlation. Uso de funcion rcorr del paquete Hmisc para 
# obtener matriz de corr. y evaluar Bio19
#----------------------------------------------------------------

dt = read.table("C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_sample.txt", header = TRUE)

mat_1 = as.matrix(dt)

cor_1 = Hmisc::rcorr(mat_1, type = "spearman")

DF_1 <- cor_1$r
DF_1

dim(DF_1)  # 40*40

write.xlsx(DF_1, "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_r_completa.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


#----------------------------------------------------------------
# LFLS. Correlation. Uso de funcion rcorr del paquete Hmisc
#----------------------------------------------------------------

matrix_final = read.table("C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_selected_vars.txt", header = TRUE)
is.matrix(matrix_final)
is.data.frame(matrix_final)

mat = as.matrix(matrix_final)
is.matrix(mat)

cor = Hmisc::rcorr(mat, type = "spearman")

class(cor)
str(cor)
is.list(cor)

cor$r
cor$P
cor$n

# Extraigo el elemento r de la lista  

DF <- cor$r

class(DF)  # Matrix
dim(DF)    # 20*20
colnames(DF)

colnames(DF) <- c("Bio13","Bio15","Bio19", "Bio2",                    
                  "Bio3","Bio5","Bio7","Month min temp.","Slope min",
                  "Slope range","Soil_avg_01","Soil_avg_02","Soil_avg_04",
                  "Soil_avg_05","Soil_avg_06","Soil_avg_07","Soil_avg_08",
                  "Soil_avg_09","Soil_avg_10","Upstream grid cells")

rownames(DF) <- c("Bio13","Bio15","Bio19", "Bio2",                    
                  "Bio3","Bio5","Bio7","Month min temp.","Slope min",
                  "Slope range","Soil_avg_01","Soil_avg_02","Soil_avg_04",
                  "Soil_avg_05","Soil_avg_06","Soil_avg_07","Soil_avg_08",
                  "Soil_avg_09","Soil_avg_10","Upstream grid cells")
DF
summary(DF)

class(DF)  # Matrix
dim(DF)

write.xlsx(cor$r, "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_r.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(cor$P, "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_P.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(DF, "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Matrix_values.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

# Compute a matrix of correlation p-values

p.mat <- cor$P  # rstatix
p.mat
plot.new()

r.mat <- cor$r
r.mat

corr_plot <- ggcorrplot(DF, method = "square", hc.order = TRUE, lab = TRUE, colors = c("blue", "grey", "red"), outline.col = "white", type = "lower", 
                          tl.cex = 8, lab_size = 1.5, tl.col = "black", tl.srt = 90, show.diag = TRUE, legend.title = "Ro",
                          ggtheme = ggplot2::theme_dark)
corr_plot

ggsave(corr_plot, filename = "C:/Users/User/Documents/Analyses/AVL/Rasters/Outputs correlation/Corr_plot.png")


#-------------------------------------------------------------------------
# Introduction to Feature selection for bioinformaticians using R, 
# correlation matrix filters, PCA & backward selection
# https://www.r-bloggers.com/2013/10/introduction-to-feature-selection-for-bioinformaticians-using-r-correlation-matrix-filters-pca-backward-selection/
#-------------------------------------------------------------------------

install.packages("caret")
library(caret)

# La matriz DF es la matriz de correlacion anterior que tiene todos los valores de
# correlacion.
# The function findCorrelation searches through a "correlation matrix" and returns a 
# vector of integers corresponding to columns to remove to reduce pair-wise correlations.
# Apply correlation filter at 0.8

highlyCor <- findCorrelation(DF2, cutoff = 0.8, verbose = FALSE, names = FALSE)

highlyCor  # 35 columns to remove
length(highlyCor)  # 35

sort(highlyCor, decreasing = FALSE)

# Remove all the variable correlated with more 0.8.

Filtered <- DF2[,-highlyCor]
class(Filtered)
dim(Filtered)  # 71 36 (se sacaron 35)

mat_keep_rows <- c("Bioclim 1","Bioclim 13","Bioclim 15","Bioclim 2","Bioclim 3","Bioclim 5","Bioclim 7",
                   "Bioclim 8","Elevation_min","Slope average","Slope min","Slope range","Soil_avg_01",
                   "Soil_avg_02","Soil_avg_04","Soil_avg_05","Soil_avg_07","Soil_avg_08","Soil_avg_09","Soil_avg_10",
                   "Soil_max_02","Soil_max_04","Soil_max_05","Soil_max_07","Soil_max_08","Soil_max_09",
                   "Soil_max_10","Soil_min_01", "Soil_min_03","Soil_min_04","Soil_min_05","Soil_min_06","Soil_min_07",
                   "Soil_min_09","Soil_range_08","Upstream catchment grid cells","Upstream stream grid cells")

mat_keep_cols <- c("Bioclim 1","Bioclim 13","Bioclim 15","Bioclim 2","Bioclim 3","Bioclim 5","Bioclim 7",
                   "Bioclim 8","Elevation_min","Slope average","Slope min","Slope range","Soil_avg_01",
                   "Soil_avg_02","Soil_avg_04","Soil_avg_05","Soil_avg_07","Soil_avg_08","Soil_avg_09","Soil_avg_10",
                   "Soil_max_02","Soil_max_04","Soil_max_05","Soil_max_07","Soil_max_08","Soil_max_09",
                   "Soil_max_10","Soil_min_01", "Soil_min_03","Soil_min_04","Soil_min_05","Soil_min_06","Soil_min_07",
                   "Soil_min_09","Soil_range_08","Upstream catchment grid cells","Upstream stream grid cells")

mat_subset <- DF[rownames(DF) %in% mat_keep_rows, colnames(DF) %in% mat_keep_cols]  # Extract rows from matrix
mat_subset

write.xlsx(mat_subset, "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/Matrix_subset.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


# Ahora, si hiciera un analisis de correlacion sobre esa nueva seleccion de variables
# no deberia haber variables con correlacion > 0.8.


#---------------------------------------------------------------------------
# Alternativa usando paquete usdm
#---------------------------------------------------------------------------

rm(list=ls(all=TRUE))

install.packages("usdm")
library(usdm)

path = ("C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas")

files <- list.files(path = path, pattern = ".tif$", full.names = TRUE)
files

vnames <- list.files(path = "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas", pattern = ".tif$", full.names = FALSE) %>%
    str_replace(".tif", "")
vnames

# Creo stack sin variable 49

mystack <- stack(files[c(1:48, 50:72)])
dim(mystack)

# Evaluar multicolinealidad usando 0.8 como punto de corte

avl_cor <- usdm::vifcor(mystack, th = 0.8)
class(avl_cor)
str(avl_cor)

avl_cor@excluded  # Variables excluded
avl_cor@results  # VIF values

matrix_usdm <- avl_cor@corMatrix

is.matrix(matrix_usdm)

DF <- as.data.frame(matrix_usdm)
str(DF)
is.data.frame(DF)
dim(DF)

corr_plot <- ggcorrplot(DF, outline.col = "white", type = "lower", 
                        tl.cex = 12, tl.col = "black", tl.srt = 90, 
                        ggtheme = ggplot2::theme_gray, 
                        insig = "pch")
corr_plot

# Eliminar las variablas con alta correlacion

bioclim_cor <- exclude(mystack, avl_cor)

# Plot 

cor = Hmisc::rcorr(dt1, type = "spearman")
class(cor)
str(cor)
is.list(cor)

cor$r
cor$P
cor$n

# Extraigo el elemento r de la lista  

DF2 <- cor$r

class(DF2)  # Matrix
dim(DF2)    # 71*71


colnames(DF2) <- c("")

rownames(DF2) <- c("")

DF2

summary(DF2)

class(DF2)  # Matrix
dim(DF2)

write.xlsx(DF, "C:/Users/User/Documents/Analyses/AVL/Rasters/ascii_procesadas/Matrix_1.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

