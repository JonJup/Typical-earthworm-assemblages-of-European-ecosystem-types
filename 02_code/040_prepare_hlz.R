# --------------------------------------------- #
# ---- Geocomputation: Holdridge Lifezones ---- #
# --------------------------------------------- #

# WorldClim tifs: https://www.worldclim.org/data/worldclim21.html


# setup -----------------------------------------------------------------------------
pacman::p_load(terra,
               data.table)


# load data -------------------------------------------------------------------------
eur <- rast("D://Arbeit/Data/misc/ras_EU_100.tif")
tif_names <- list.files(path = "D://Arbeit/Data/LULC/worldclim/wc2.1_5m_tavg/", pattern = "*.tif")
r_list    <- rast(paste0("D://Arbeit/Data/LULC/worldclim/wc2.1_5m_tavg/", tif_names))

# ------------------------------------------------------------------------------------------------------------------------- #
#### Data Preparation #### ------------------------------------------------------------------------------------------------ #

## Process:
# Load and crop raster files to the extent of the eur raster
# Replace values lower 0°C/greater 30°C
# Calculate mean temperature, excluding NAs to yield layer with bio temperature.

## Crop each file in list
r_list_crop <- crop(r_list, y = ext(-11, 32, 34, 72))

## Replace values in each 
# Create dictionary matrix
m <- c(-100, 0, 0,
       30, +100, 30)

mat <- matrix(m, ncol = 3, byrow = TRUE)

# Classify rasters with values according to classification matrix
r_rcl <- classify(r_list_crop, mat)

# Calculate mean
r_mean <- app(r_rcl, mean)

# Project to new CRS
r_proj <- project(r_mean, y = "epsg:3035")
r_rs <- resample(r_proj, eur)

# Save rasters
writeRaster(r_mean, "01_data/ras_mean_biotemperature_4326.tif",overwrite = TRUE)
writeRaster(r_proj, "01_data/ras_mean_biotemperature_3035.tif", overwrite = TRUE)

## Replace values with respective categories according to HLZ
# Construct dictionary
dict_full <- data.table(lower = c(0, 3, 6, 12),
                        upper = c(3, 6, 12, 30),
                        cat = c("polar", "boreal", "cool temperate", "warm temperate"),
                        cat_id = 1:4)

## Replace raster values with classes
# Create matrix
dict_sub <- dict_full[, .(lower, upper, cat_id)]

# Classify raster values with values according to classification matrix
r_rcl <- classify(r_rs, dict_sub, include.lowest = TRUE)


## Save raster and lookup table
writeRaster(r_rcl, "01_data/ras_hlz_biotemperature_3035.tif", overwrite = TRUE)
write.csv(dict_full, "01_data/lu_hlz_biotemperature_3035.csv", row.names = FALSE)