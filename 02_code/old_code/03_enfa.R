### ------------------------------------------- ###
### --- Environmental Niche Factor Analysis --- ###
### ------------------------------------------- ###


# setup -----------------------------------------------------------------------------
library(CENFA)
library(raster)
library(sp)
library(sf)
library(data.table)
library(dplyr)
library(magrittr)
# library(terra)
# library(mapview)
# library(corrr)


# load data -------------------------------------------------------------------------
# - Load earthworm data to establish bounding box and crop rasters
earthworm <- readRDS("01_data/05_full_data.rds")
sites <- unique(earthworm, by = "site_id")
sites %<>% st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
sites$abundance <-1
bbox <- st_bbox(sites)
#earthworm_sf <- st_as_sf(earthworm, coords = c("lon", "lat"), crs = "EPSG:4326")

sites %<>% as_Spatial()
# 
# # load raster files  ----------------------------------------------------------------


index <- c(5,6,7,8,13,14,15)
#raster.list <- vector(mode = "list", length = length(index))
raster.list <- list()
for (i in 1:length(index)){
        
        i2 <- index[i]
        i.filename <- paste0("D://Arbeit/Data/LULC/worldclim/wc2.1_2.5m_bio/wc2.1_2.5m_bio_",i2,".tif")
        print(i.filename)
        raster.list[[i]] <- raster(i.filename)
        remove(i.filename)
        remove(i2)
        
}
rm(i)
rm(index);gc()
# prepare raster data ---------------------------------------------------------------

# - crop rasters to the extend of biological data 
raster.list %<>% lapply(crop, bbox) %>% 
        #lapply(raster::scale) %>% 
        brick()
rm(bbox)
rm(earthworm)
# - determine correlation between variables 
# cov <- layerStats(raster.list, 'pearson', na.rm=T)
# 
# cor.dt <- cov$`pearson correlation coefficient` %>% data.table()
# cor.dt[,var2 := rownames(cov$`pearson correlation coefficient`)]
# cor.dt


# fit ENFA model --------------------------------------------------------------------
# - We use the enfa() function from the CENFA package to compute a environmental niche factor analysis 
fit.mod <- enfa(x = raster.list, s.dat = sites)
# glc <- GLcenfa(x = raster.list)
# scatter(x = fit.mod, y = glc)
# prd.mod <- predict(fit.mod, raster.brick)
# CENFA:::enfa
