# ---------------------------------- #
# --- Geocomputation: CLC Raster --- #
# ---------------------------------- #

# Script contains the preparation of the data for the established landscape typology.
# CLC tif: https://land.copernicus.eu/en/products/corine-land-cover/clc2018

# ----------------------------------------------------------------------------------------------------------------------- #
#### Setup #### --------------------------------------------------------------------------------------------------------- #

## Packages
pacman::p_load(terra,
               data.table)

## Reset environment
rm(list = ls())

## Paths
path <- "H:/GETREAL_dat"

# ----------------------------------------------------------------------------------------------------------------------- #
#### Data Preparation: CLC #### ----------------------------------------------------------------------------------------- #

## Load data
r <- rast(file.path(path, "01_spatial", "11_template", "ras_EU_100.tif"))
clc <- rast(file.path(path, "01_spatial", "07_corine", "corine_100m_old", "CLC2018_CLC2018_V2018_20.tif"))

## Prepare table for substitution of raster values (based on look up table)
clc_l <- setDT(
    readxl::read_excel(file.path(path, "01_spatial", "07_corine", "corine_100m", "ras_clc_QGIS_exp_lookup.xlsx"))
)

# Add column for new category
clc_l[, v := NA]
clc_l[, v := ifelse(clc_cat > 200 & clc_cat < 300, 20, v)]  # Open space
clc_l[, v := ifelse(clc_cat > 320 & clc_cat < 330, 20, v)]  # Open space
clc_l[, v := ifelse(clc_cat > 300 & clc_cat < 320, 30, v)]  # Forest
clc_l[, v := ifelse(clc_cat == 324, 30, v)]                 # Forest
clc_l[, v := ifelse(clc_cat > 400, 40, v)]                  # Wetland
clc_l[, v := ifelse(clc_cat < 200 | clc_cat > 500 | clc_cat == 335, NA, v)] # Remove artifical surfaces, waterbodies and glaciers

# Tidy lookup table
clc_l[, val := NULL]
setnames(clc_l, "clc_cat", "id")

## Replace raster categories via rasterDT::subsDT
t <- Sys.time()
clc <- classify(clc, clc_l,
                othersNA = TRUE)
Sys.time() - t; beepr::beep(4)

## Compare clc and EU raster
compareGeom(clc, r) # Cut landuse raster to extent of template

# Crop
clc <- crop(clc, r)

## Save modified CLC raster
writeRaster(clc, file.path(path, "01_spatial", "07_corine", "ras_clc_100_mod.tif"), overwrite = TRUE)
