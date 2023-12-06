# |————————————————————————————————————|
# |——— Combine sworm and edaphobase ———|
# |————————————————————————————————————|

# setup -----------------------------------------------------------------------------
pacman::p_load(sf, data.table, dplyr, magrittr)
source("02_code/functions/prep_distances.R")

# load data -------------------------------------------------------------------------
sworm <- readRDS("01_data/01_prepared_sworm.rds")
edaph <- readRDS("01_data/02_prepared_edapho.rds")

# prepare data for join -------------------------------------------------------------

# - add missing columns to edaphobase data 
for (i in 1:ncol(sworm)){
        if (names(sworm)[i] %in% names(edaph)) next()
        edaph$x <- NA
        names(edaph)[which(names(edaph) == "x")] <- names(sworm)[i]
}
for (i in 1:ncol(edaph)){
        if (names(edaph)[i] %in% names(sworm)) next()
        sworm$x <- NA
        names(sworm)[which(names(sworm) == "x")] <- names(edaph)[i]
}

# - adjust the order of columns
comb.list <- list(edaph, sworm)
comb.list %<>% lapply(FUN = function(x) select(x, order((colnames(x)))))

# join data -------------------------------------------------------------------------
combined <- rbindlist(comb.list)

# select only the most recent sample from time series -------------------------------


# check for duplicates  -------------------------------------------------------------
sites <- 
        unique(combined, by = "site_id") %>%
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

# - this function checks for sites that are closer than 1 meter
x <- prep_distances(sites)
if (nrow(x) > 0) print("INCIDENT SITES LEFT! NEEDS FIXING.")

# save to file ----------------------------------------------------------------------
saveRDS(combined, "01_data/03_full_data_incomplete.rds")
rm(list = ls());gc()



