# —————————————————————————————————————————————————————— #
# ——— How dispersed are the BGR-EUNIS habitat types? ——— # 
# ——— Creating the test data set                       ——— # 
# —————————————————————————————————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 06.12.2023

# setup -----------------------------------------------------------------------------

pacman::p_load(terra, sf, dplyr, data.table, magrittr,stars)

# load data -------------------------------------------------------------------------
hlz   <- rast("01_data/ras_hlz_biotemperature_3035.tif")
clc   <- rast("D://Arbeit/Data/LULC/corine18/raster/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
eunis <- rast("D://Arbeit/Data/typology_systems/EUNIS/Ecosystem types of Europe - version 3.1 Full map/eea_r_3035_100_m_etm-full_2012_v3-1_r00.tif")        
bgr   <- st_read("D://Arbeit/Data/typology_systems/eea_bioregions/BiogeoRegions2016.shp")

# prep data -------------------------------------------------------------------------

#clc   %<>% crop(hlz)
eunis %<>% crop(hlz)
bgr   %<>% 
        vect() %>%
        rasterize(y = rast(clc),
                  field = "code")

rm(hlz,clc);gc()
# prep1  ----------------------------------------------------------------------------
# - define function 
prep1 <- function(x){
        
        x %<>% 
                aggregate(fact = 100, fun="modal") %>%
                st_as_stars() %>%
                st_as_sf(as_points=TRUE)
        x
        
}
# - apply function 
prep1.res <- lapply(list(
        #hlz, 
       # clc, 
        bgr, 
        eunis), prep1)

prep1.res[[2]] %<>% st_transform(crs = st_crs(prep1.res[[1]])) 


res <- 
        prep1.res[[1]] %>%
        st_join(prep1.res[[2]]) %>%
        filter(!is.na(code) & !is.na(EUNIS)) %>%
        rename(bgr = code, eunis=EUNIS) %>%
        # mutate(
        #         hlz = case_when(
        #                 hlz == 1 ~ "polar",
        #                 hlz == 2 ~ "boreal",
        #                 hlz == 3 ~ "cool temperate",
        #                 hlz == 4 ~ "warm temperate"
        #         )
        # ) %>% 
        # mutate(
        #         clc = 
        #                 case_when(
        #                         clc %in% c(
        #                                 "Continuous urban fabric",
        #                                 "Discontinuous urban fabric" ,
        #                                 "Industrial or commercial units",
        #                                 "Road and rail networks and associated land" ,
        #                                 "Port areas",
        #                                 "Airports" ,
        #                                 "Mineral extraction sites",
        #                                 "Dump sites" ,
        #                                 "Construction sites",
        #                                 "Green urban areas" ,
        #                                 "Sport and leisure facilities"
        #                         ) ~ "urban",
        #                         clc %in% c(
        #                                 "Non-irrigated arable land",
        #                                 "Permanently irrigated land",
        #                                 "Rice fields",
        #                                 "Vineyards",
        #                                 "Fruit trees and berry plantations",
        #                                 "Olive groves",
        #                                 "Pastures",
        #                                 "Annual crops associated with permanent crops",
        #                                 "Complex cultivation patterns",
        #                                 "Land principally occupied by agriculture, with significant areas of natural vegetation",
        #                                 "Agro-forestry areas"  
        #                         ) ~ "Open Land", 
        #                         clc %in% c(
        #                                 "Broad-leaved forest",
        #                                 "Coniferous forest",
        #                                 "Mixed forest",
        #                                 "Natural grasslands",
        #                                 "Moors and heathland",
        #                                 "Sclerophyllous vegetation",
        #                                 "Transitional woodland-shrub",
        #                                 "Beaches, dunes, sands",
        #                                 "Bare rocks",
        #                                 "Sparsely vegetated areas",
        #                                 "Burnt areas",
        #                                 "Glaciers and perpetual snow"
        #                         ) ~ "Forest",
        #                         clc %in% c(
        #                                 "Water courses",
        #                                 "Water bodies",
        #                                 "Coastal lagoons",
        #                                 "Estuaries",
        #                                 "Sea and ocean"
        #                         ) ~ "waterbodies",
        #                         clc %in% c(
        #                                 "Glaciers and perpetual snow",
        #                                 "Inland marshes",
        #                                 "Peat bogs",
        #                                 "Salt marshes",
        #                                 "Salines",
        #                                 "Intertidal flats"  
        #                         ) ~ "wetlands"
        #                 )
        # ) %>%
        mutate(eunis = stringr::str_remove_all(eunis, "[0-9]")) %>%
        mutate(
                eunis = case_when(
                        eunis == "E" ~ "Grasslands",
                        eunis == "I" ~ "Cultivated",
                        eunis == "C" ~ "Inland surface waters",
                        eunis == "G" ~ "Forest",
                        eunis == "H" ~ "Inland unvegetated or sparsely vegetated habitats",
                        eunis == "FB" ~ "Shrub plantations",
                        eunis == "F" ~ "Heathland, scrub and tundra",
                        eunis == "D" ~ "Mires, bogs and fens",
                        eunis == "A" ~ "Marine habitats",
                        eunis == "J" ~ "Constructed, industrial and other artificial habitats"
                )
        )

# save to file ----------------------------------------------------------------------
x <- 
        rstudioapi::getActiveDocumentContext()[2]$path %>%
        stringr::str_remove(".*02_code")%>%
        readr::parse_number()

saveRDS(res, paste0("01_data/helper/",x,"_habitat_reference_points.rds"))

rm(list = ls())
gc()
