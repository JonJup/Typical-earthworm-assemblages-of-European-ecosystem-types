# ———————————————————————————————————————— #
# ——— Prepare sworm+ data for analysis ——— #
# ———————————————————————————————————————— #

# ——— Authors: Sebatian Scheu & Jonathan Jupke 
# ——— Contact: jonjup@protonmail.com
 
# setup -----------------------------------------------------------------------------
pacman::p_load(
        data.table,
        tidyverse,
        magrittr,
        sf
        )
source("02_code/functions/prep_distances.R")

# load data -------------------------------------------------------------------------

dt <- readRDS("01_data/raw/sworm.rds")

# aggregate data ----------------------------------------------------------------------

# add up abundances for each species at each site 
agg_col <- names(dt)[!grepl("abundance", names(dt))]

# Aggregate
dt %<>%
        group_by(across(all_of(agg_col))) %>%
        mutate(abu_sum = sum(abundance))  %>%
        ungroup() %>%
        dplyr::select(-abundance) %>%
        rename(abundance = abu_sum) %>%
        unique()

# - drop sites that have fewer than three species 
setDT(dt)
# - assign new site_id based on coordinates 
dt[, site_id := .GRP, by = c("lon", "lat")]
dt[, date_id := .GRP, by = "sample_year"]
dt[, sample_id := paste(site_id, date_id, sep = "_")]
subset <- dt[, .(sample_id, species, abundance)]

# Compute number of species per site
subset[, no_species := uniqueN(species, na.rm = TRUE), by = sample_id]

# Reduce to one row per site
subset %<>% unique(by = c("sample_id"))

# Remove sites that have less than three species 
subset <- subset[no_species > 1, .(sample_id, no_species)]

# Remove those sites from the complete data set 
dt <- dt[sample_id %in% subset$sample_id]
uniqueN(dt, "site_id")


# for every site only take the most recent sample 
# which sites have multiple sampling dates? 
multiple_samples <- dt[,uniqueN(sample_id), by="site_id"] %>% 
        filter(V1 != 1) %>% 
        pull(site_id)

for (i in multiple_samples){
        
        
        i.year <- dt[site_id == i, unique(sample_year)]
        dt <- dt[site_id != i | sample_year == i.year[which.max(i.year)]]
        
        rm(list = ls()[grepl("^i\\.", x = ls())])
        rm(i)
}

# check that no sites have multiple samples after the loop
multiple_samples <- dt[,uniqueN(sample_id), by="site_id"] %>% 
        filter(V1 != 1) %>% 
        pull(site_id)
if(length(multiple_samples) != 0) print("MULTIPLE SAMPLES PER SITES LEFT. NEEDS FIXING.")

uniqueN(dt, "site_id")

# - The code below combines all sites from the same data set that are closer than 50 meters. 
# - We have decided against this step.

#distance.table <- prep_distances(sites, 1)

# for (i in 1:nrow(distance.table)){
#         
#         id1 <- sites$site_id[distance.table[i, id3]]
#         id2 <- sites$site_id[distance.table[i, id4]]
#         
#         joined.id  <- dt[site_id %in% c(id1, id2), unique(site_id)[1]]
#         joined.lat <- dt[site_id %in% c(id1, id2), unique(lat)[1]]
#         joined.lon <- dt[site_id %in% c(id1, id2), unique(lon)[1]]
#         
#         dt[site_id %in% c(id1, id2), c("site_id", "lat", "lon") := .(joined.id, joined.lat, joined.lon)]
#         
# }
# 
# 
# done <- FALSE
# while (done == FALSE) {
#         sites <- unique(dt, by = "site_id") %>% st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
#         distance.table <- prep_distances(sites, 100)
#         if (nrow(distance.table) == 0) { 
#                 done <- TRUE
#         } else {
#                 # - add new id columns to remove duplicates
#                 print(nrow(distance.table))
#                 distance.table <- arrange(distance.table, distances)
#                 j.id1 <- sites$site_id[distance.table[1, id3]]
#                 j.id2 <- sites$site_id[distance.table[1, id4]]
#                 j.joined.lat <- dt[site_id %in% c(j.id1, j.id2), unique(lat)[1]]
#                 j.joined.lon <- dt[site_id %in% c(j.id1, j.id2), unique(lon)[1]]
#                 
#                 dt[site_id %in% c(j.id1, j.id2), c("site_id", "lat", "lon") := .(j.id1, j.joined.lat, j.joined.lon)]
#         }
#         
# }
# rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
# rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
# rm(done)

#- check results 
# sites <- unique(dt, by = "site_id") %>% st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
# distances <- st_distance(sites) %>% units::drop_units() %>% c()
# id1 <- rep(1:nrow(sites), each = nrow(sites))
# id2 <- rep(1:nrow(sites), times = nrow(sites))
# distance.table <- data.table(id1, id2, distances)
# distance.table <- distance.table[distances < 100 & id1 != id2]

# save to file ----------------------------------------------------------------------
saveRDS(object = dt, 
        file = "01_data/01_prepared_sworm.rds")


# empty environment -----------------------------------------------------------------
rm(list = ls())

