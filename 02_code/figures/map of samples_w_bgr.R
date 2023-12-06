pacman::p_load(sf, data.table, tmap, maptiles, magrittr,dplyr)

bgr  <- st_read("D://Arbeit/Data/typology_systems/eea_bioregions/BiogeoRegions2016.shp")
data <- readRDS("01_data/13_no_rare_types.rds")
sites <- unique(data, by = "site_id")
sites <- st_as_sf(sites, coords = c("lon", "lat"), crs= 4326)
sites <- st_transform(sites, crs = st_crs(bgr))

countries <- st_read("D://Arbeit/Data/misc/NUTS_Europa_20M_2021_3035/NUTS_RG_20M_2021_3035.shp")
countries <- st_transform(countries, crs = st_crs(bgr))
countries2 <- x <- aggregate(countries, 
                             by = list(countries$CNTR_CODE),
                             FUN = mean)
countries2 <- st_crop(countries2, sites)

#- no basemap covers the high latitutdes reached by some of the data 
# basemap.tile <-
#         get_tiles(
#                 x = countries2,
#                 provider = "Esri.WorldTerrain",
#                 zoom = 4, crop = TRUE)

bgr2 <- st_crop(bgr, countries2)
bgr2 <- mutate(bgr2, 'Biogeographic Region' = short_name)
map <- 
        # tm_shape(basemap.tile) +
        # tm_rgb() +
        tm_shape(bgr2) +
                tm_fill(col="Biogeographic Region") +
        tm_shape(countries2) +
                tm_borders(lwd=1.5) +
        tm_shape(sites) +
                tm_bubbles(size = .2, col = "orange", border.col = "black",border.lwd = 1) + 
                tm_compass(type = "4star",
                           size = 2,
                           position = c("left", "top"),
                           text.color = "black",
                           text.size = 1.3) +
                tm_scale_bar(text.color = "black", 
                             text.size = 2,
                             position = c(0.23,.9)) + 
                tm_layout(bg.color = "#c4ecea",
                          legend.outside = T,
                          legend.title.size = 2)
map

tmap_save(map, filename = "03_figures/map_of_samles_bgr.png")
