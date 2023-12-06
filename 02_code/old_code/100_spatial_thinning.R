### ------------------------ ###
### --- Spatial Thinning --- ### 
### ------------------------ ###

# - Apply spatial thinning to the earthworm data to counter spatial biases in the 
# - distribution of sampling sites. 
# - We use the spThin algorithm from the eponymous R package presented in 
# - Aiello‐Lammens, M.E., Boria, R.A., Radosavljevic, A., Vilela, B., Anderson, R.P., 2015.
# - spThin: an R package for spatial thinning of species occurrence records for use in 
# - ecological niche models. Ecography 38, 541–545. https://doi.org/10.1111/ecog.01132

# - For additional details about the method see the R package vignette with 
# - vignette("spThin_vignette", package='spThin').

# setup -----------------------------------------------------------------------------
library(spThin)
library(data.table)
library(sf)
library(mapview)
# load data -------------------------------------------------------------------------
dt <- readRDS("01_data/05_full_data.rds")
dt[, lowest_taxon := ifelse(!is.na(species), species, ifelse (!is.na(genus), genus, family))]

# prepare data ----------------------------------------------------------------------

#sites <- unique(dt, by = "site_id")

# thin data -------------------------------------------------------------------------
# 10:37

seperation_distances <- c(50, 100, 150, 200, 250, 300, 400)
result.list <- list()
for (i in 1:7){
        result.list[[i]] <- 
                thin(
                        loc.data = dt,
                        lat.col = "lat",
                        long.col = "lon",
                        spec.col = "lowest_taxon",
                        thin.par = seperation_distances[i],
                        ### Thinning parameter - the distance (in kilometers) that you want records to be separated by.
                        reps = 10,
                        locs.thinned.list.return = TRUE,
                        write.files = FALSE,
                        write.log.file = FALSE,
                        verbose = T
                )
}


# save_to_file ----------------------------------------------------------------------
saveRDS(result.list, "01_data/07_results_spatial_thinning.rds")
result.list <- readRDS("01_data/07_results_spatial_thinning.rds")

# - turn each first element of the list into an sf object for plotting. 

result.list2 <- lapply(result.list, function(x) st_as_sf(x[[1]], coords = c("Longitude", "Latitude"), crs = "EPSG:4326"))

km50 <- result.list2[[1]]
km100 <- result.list2[[2]]


mapview(km50) | mapview(km100)
