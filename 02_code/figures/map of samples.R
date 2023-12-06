pacman::p_load(sf, data.table, tmap, maptiles, magrittr)


data <- readRDS("01_data/13_no_rare_types.rds")
sites <- unique(data, by = "site_id")
sites <- st_as_sf(sites, coords = c("lon", "lat"), crs= 4326)

countries <- st_read("D://Arbeit/Data/misc/NUTS_Europa_20M_2021_3035/NUTS_RG_20M_2021_3035.shp")
countries2 <- x <- aggregate(countries, 
                             by = list(countries$CNTR_CODE),
                             FUN = mean)
sites %<>% st_transform(crs = st_crs(countries))
countries2 <- st_crop(countries2, sites)

basemap.tile <-
        get_tiles(
                x = countries2,
                provider = "Esri.WorldTerrain",
                zoom = 4, crop = TRUE)

(map <- 
        #tm_shape(basemap.tile) +
        #tm_rgb() +
        tm_shape(countries2) +
        tm_polygons(lwd=1.5) +
        tm_shape(sites) +
        tm_bubbles(size = .2, col = "orange", border.col = "black",border.lwd = 1) + 
        tm_compass(type = "4star", size = 2, position = c("left", "top"), text.color = "white", text.size = 1.3) + 
        tm_scale_bar(text.color = "white", text.size = 2) + 
        tm_layout(bg.color = "lightblue")
)
tmap_save(map, filename = "03_figures/map_of_samles.png")
