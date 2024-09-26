library(sf)
library(tmap)
library(dplyr)
library(terra)


bgr <- st_read("E://arbeit/data/typology_systems/eea_bioregions/BiogeoRegions2016.shp")

bgr <- filter(bgr, code != "Outside")
bgr <- filter(bgr, code != "Macaronesia")

tm_shape(bgr) + 
        tm_polygons(col = "code") + 
        tm_layout(legend.show=FALSE)

tmap_save(filename = "001_Uni/001_projects/earthorms/03_figures/overview flowchart/parts/bgr.png")

clc <- rast("E://arbeit/data/LULC/corine18/raster/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")
clc2 <- terra::aggregate(x= clc, fact = 10)

tm_shape(clc2) + 
        tm_raster(palette = "Accent") + 
        tm_layout(legend.show=FALSE)
tmap_save(filename = "001_Uni/001_projects/earthorms/03_figures/overview flowchart/parts/clc.png")

hlz <- "001_Uni/001_projects/earthorms/01_data/ras_hlz_biotemperature_3035.tif"
hlz <- rast(hlz)
hlz <- terra::aggregate(hlz, fact = 10)

hlz_map <- tm_shape(hlz) + 
        tm_raster(legend.show = FALSE)
tmap_save(filename = "001_Uni/001_projects/earthorms/03_figures/overview flowchart/parts/hlz.png")

eunis <- rast("E://arbeit/data/typology_systems/EUNIS/Ecosystem types of Europe - version 3.1 Full map/eea_r_3035_100_m_etm-full_2012_v3-1_r00.tif")
eunis <- terra::aggregate(eunis, fact = 10, fun = "modal")


values(eunis)[which(values(eunis) > 100)] <- NA


unique(values(eunis))
values(eunis) <- factor(values(eunis))
tm_shape(eunis) + tm_raster(legend.show = FALSE, palette = "Accent")
tmap_save(filename = "001_Uni/001_projects/earthorms/03_figures/overview flowchart/parts/eunis.png")
