# | ——————————————————————————————————————|
# | ——— Add variables to combined data ———|
# | ——————————————————————————————————————|


# setup -----------------------------------------------------------------------------
pacman::p_load(terra, sf, data.table, magrittr, tidyverse, stringr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/03_full_data_incomplete.rds")
sites <- unique(data, by = "site_id")
# small fixes  ----------------------------------------------------------------------
data[stringr::str_detect(site_id, "edapho"), ref:="db_edaphobase"]

# add bioregions  -------------------------------------------------------------------
bgr  <- st_read("D://Arbeit/Data/typology_systems/eea_bioregions/BiogeoRegions2016.shp")

bgr %<>% 
        st_make_valid() %>% 
        select(code)

sites <- unique(data, by = "site_id") %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
        st_transform(crs = "EPSG:3035") %>% 
        st_join(bgr) %>% 
        select(!bgr) %>% 
        rename(bgr = code)

data[, bgr := NULL]
sites %<>% 
        select(site_id, bgr) %>%
        st_drop_geometry()
setDT(sites)
data <- sites[data, on = "site_id"]
rm(bgr)
# EUNIS -----------------------------------------------------------------------------
eunis <- rast("D://Arbeit/Data/typology_systems/EUNIS/Ecosystem types of Europe - version 3.1 Full map/eea_r_3035_100_m_etm-full_2012_v3-1_r00.tif")

sites <- unique(data, by = "site_id") %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
        st_transform(crs = "EPSG:3035")

ext <- terra::extract(x = eunis, sites)
sites$eunis_code <- ext$EUNIS
sites %<>% 
        mutate(eunis_code = str_remove_all(eunis_code, "[0-9]")) %>%
        mutate(
                eunis_habitat = case_when(
                        eunis_code == "E" ~ "Grasslands and lands dominated by forbs, mosses or lichens",
                        eunis_code == "I" ~ "Regularly or recently cultivated agricultural, horticultural and domestic habitats",
                        eunis_code == "C" ~ "Inland surface waters",
                        eunis_code == "G" ~ "Woodland, forest and other wooded land",
                        eunis_code == "H" ~ "Inland unvegetated or sparsely vegetated habitats",
                        eunis_code == "FB" ~ "Shrub plantations",
                        eunis_code == "F" ~ "Heathland, scrub and tundra",
                        eunis_code == "D" ~ "Mires, bogs and fens",
                        eunis_code == "A" ~ "Marine habitats",
                        eunis_code == "J" ~ "Constructed, industrial and other artificial habitats"
                )
        )

data[, c("eunis_code", "eunis_habitat") := NULL]
sites %<>% 
        select(site_id, eunis_code, eunis_habitat) %>% 
        st_drop_geometry()
setDT(sites)
data <- sites[data, on = "site_id"]
rm(eunis, sites)


# Add Corine Land Cover --------------------------------------------------------------
clc <- rast("D://Arbeit/Data/LULC/corine18/raster/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
sites <- unique(data, by = "site_id") %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
        st_transform(crs = "EPSG:3035")
ext <- terra::extract(x = clc, sites)
sites %<>% 
        mutate(clc_code = ext$LABEL3) %>%
        mutate(clc_code = as.character(clc_code))


sites %<>%
        mutate(clc = 
                case_when(
                        clc_code %in% c("Non-irrigated arable land",
                                        "Pastures",
                                        "Bare rocks",
                                        "Fruit trees and berry plantations",
                                        "Vineyards",
                                        "Natural grasslands", 
                                        "Rice fields", 
                                        "Complex cultivation patterns",
                                        "Land principally occupied by agriculture, with significant areas of natural vegetation",
                                        "Sparsely vegetated areas",
                                        "Sclerophyllous vegetation",
                                        "Olive groves",
                                        "Agro-forestry areas",
                                        "Permanently irrigated land")  ~ 
                                "Open Land" ,
                        clc_code %in% c("Discontinuous urban fabric", 
                                        "Dump sites", 
                                        "Green urban areas",
                                        "Airports",
                                        "Mineral extraction sites",
                                        "Industrial or commercial units",
                                        "Sport and leisure facilities",
                                        "Continuous urban fabric",
                                        "Construction sites",
                                        "Road and rail networks and associated land") ~ 
                                "Artifical Surface",
                        clc_code %in% c("Broad-leaved forest", 
                                        "Coniferous forest",
                                        "Transitional woodland-shrub",
                                        "Mixed forest") ~ 
                                "Forest",
                        clc_code %in% c("Peat bogs", 
                                        "Salt marshes", 
                                        "Moors and heathland",
                                        "Inland marshes") ~ 
                                "Wetland",
                        clc_code %in% c("Water courses",
                                        "Water bodies",
                                        "Sea and ocean",
                                        "Estuaries",
                                        "Intertidal flats") ~ 
                                "Waterbody",
                        
                )
        )
sites %<>% 
        select(site_id, clc) %>% 
        st_drop_geometry()
setDT(sites)
data <- sites[data, on = "site_id"]
rm(clc, sites)

# Add HLZ ---------------------------------------------------------------------------
hlz <- rast("01_data/ras_hlz_biotemperature_3035.tif")

sites <- unique(data, by = "site_id") %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
        st_transform(crs = "EPSG:3035")

ext <- terra::extract(x = hlz, sites)
sites$hlz_code <- ext$mean
sites %<>% mutate(hlz = case_when(
        hlz_code == 1 ~ "polar", 
        hlz_code == 2 ~ "boreal", 
        hlz_code == 3 ~ "cool temperate", 
        hlz_code == 4 ~ "warm temperate"
        ))
data[, c("hlz") := NULL]
sites %<>% 
        select(site_id, hlz) %>% 
        st_drop_geometry()
setDT(sites)
data <- sites[data, on = "site_id"]
rm(hlz, sites)

# add biotemperature ----------------------------------------------------------------

biotemp <- rast("01_data/ras_mean_biotemperature_3035.tif")

sites <- unique(data, by = "site_id") %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
        st_transform(crs = "EPSG:3035")

ext <- terra::extract(x = biotemp, sites)
sites$biotemp <- ext$mean
data[, c("biotemp") := NULL]
sites %<>% 
        select(site_id, biotemp) %>% 
        st_drop_geometry()
setDT(sites)
data <- sites[data, on = "site_id"]

# drop family level data ------------------------------------------------------------
data <- data[!is.na(species)]

rm(biotemp, sites)

# save to file ----------------------------------------------------------------------
saveRDS(data, "01_data/05_full_data.rds")
rm(list=ls());gc()
