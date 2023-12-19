pacman::p_load(sf, data.table, tmap, maptiles, magrittr,dplyr,terra)

hlz  <- st_read("01_data/raw/hlz_polygons.gpkg")
data <- readRDS("01_data/13_no_rare_types.rds")
sites <- unique(data, by = "site_id")
sites <- st_as_sf(sites, coords = c("lon", "lat"), crs= 4326)
sites <- st_transform(sites, crs = st_crs(hlz))

countries <- 
        st_read("D://Arbeit/Data/misc/NUTS_Europa_20M_2021_3035/NUTS_RG_20M_2021_3035.shp")
countries %<>%
        st_transform(crs = st_crs(hlz))
countries2 <- 
        aggregate(countries,
                        by = list(countries$CNTR_CODE),
                        FUN = mean) %>%
        st_crop(sites)


#- no basemap covers the high latitutdes reached by some of the data
basemap.tile <-
        get_tiles(
                x = countries2,
                provider = "Esri.WorldTerrain",
                zoom = 4, crop = TRUE)

hlz %<>% 
        st_crop(countries2) %>%
        mutate(DN = case_when(
        DN == 1 ~ "polar",
        DN == 2 ~ "boreal",
        DN == 3 ~ "cool temperate",
        DN == 4 ~ "warm temperate"
        )) %>%
        mutate('Holdridge Life Zones' = factor(DN))

map <- 
        tm_shape(basemap.tile) +
        tm_rgb() +
        tm_shape(hlz) +
        tm_fill(col="Holdridge Life Zones") +
        tm_shape(countries2) +
        tm_borders(lwd=1.5) +
        tm_shape(sites) +
        tm_bubbles(size = .2, 
                   col = "orange", 
                   border.col = "black",
                   border.lwd = 1) + 
        tm_layout(#bg.color = "#c4ecea",
                legend.outside = T,
                legend.title.size = 2)

map

tmap_save(map, filename = "03_figures/map_of_samles_hlz.png")
