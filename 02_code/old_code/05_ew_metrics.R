# --------------------------------------------------- #
# ---- Assemblages: Data Preparation and Metrics ---- #
# --------------------------------------------------- #

# This script contains the extraction of landscape types via the coordinates of the sampling sites


# setup -----------------------------------------------------------------------------
pacman::p_load(data.table,
               tidyverse,
               fields,
               vegan,
               sf,
               mapview,
               magrittr,
               spatstat)


# load data -------------------------------------------------------------------------

dt <- readRDS("01_data/ALL_earthworm.rds")

# aggregate data ----------------------------------------------------------------------

# Aggregate data for site_id and coordinates
agg_col <- names(dt)[!grepl("abundance", names(dt))]

# Aggregate
dt %<>%
    group_by(across(all_of(agg_col))) %>%
    mutate(abu_sum = sum(abundance)) %>%
    ungroup() %>%
    dplyr::select(-abundance) %>%
    rename(abundance = abu_sum) %>%
    unique()

# ------------------------------------------------------------------------------------------------------------------ #
#### SELECTION #### ------------------------------------------------------------------------------------------------ #

## EXCLUDE COMMUNITIES BASED ON CRITERIA
# Subset
setDT(dt)
sub <- dt[, .(site_id, species, abundance)]

## COMPUTE METRICS
# Store as backup
metrics <- copy(sub)

# Compute number of individuals, family, species, ... per community_ID
metrics[, no_species := uniqueN(species, na.rm = TRUE), by = site_id]
metrics[, no_individuals := sum(abundance), by = site_id]

# Keep unique entries
metrics <- unique(metrics, by = "site_id")

# Remove sites with only one species
mt_crit <- metrics[no_species <= 2, .(site_id, no_species, no_individuals)]

## REMOVE ENTRIES
# Bind site_ids of unwanted entries
id_rem <- mt_crit$site_id

# Keep relevant rows
dt <- dt[!(site_id %in% id_rem), ]


# ------------------------------------------------------------------------------------------------------------------ #
#### SPATIAL CLUSTERING: VARIANT 1 #### ---------------------------------------------------------------------------- #

dt[, analysis_type := ifelse(is.na(habitat) | grepl("Family|Genus", tax_level), NA, "variant_1")]

# ------------------------------------------------------------------------------------------------------------------ #
#### SPATIAL CLUSTERING: VARIANT 2 #### ---------------------------------------------------------------------------- #

## SUBSET
id <- unique(dt[grepl("variant_1", analysis_type), .(site_id, lon, lat, eunis_habitat)])

# Add count per EUNIS habitat
id[, eunis_count := .N, by = eunis_habitat]

# Split off site_id with eunis_count < 3
fin <- id[eunis_count < 3, ]

# For sites with one or two observations: Add EUNIS ID and cluster manually
fin <- fin %>%
    group_by(eunis_habitat) %>%
    mutate(eunis_id = cur_group_id()) %>%
    arrange(eunis_id) %>%
    as.data.table()

fin[, cluster := 1]

## Remove from id data.table
id <- id[eunis_count >= 3, ]

# Add unique identifier for each EUNIS habitat
id <- id %>%
    group_by(eunis_habitat) %>%
    mutate(eunis_id = cur_group_id()) %>%
    arrange(eunis_id) %>%
    as.data.table()

## Spatial clustering
# Define threshold
threshold_km <- 1000

# For loop clustering
for (i in 1:max(id$eunis_id)) {
    sub <- id[eunis_id == i, ]
    dis <- rdist.earth(sub[, .(lon, lat)], miles = FALSE, R = 6371)
    fit <- hclust(as.dist(dis), method = "single")
    sub$cluster <- cutree(fit, h = threshold_km)
    fin <- rbind(fin, sub)
}

#- Visual test of clusters 
# fin_sf <- st_as_sf(fin, coords = c("lon", "lat"), crs = "EPSG:4326")
# fin_sf %<>% mutate(cluster = factor(cluster))
# mapview(fin_sf, zcol = "cluster")

# Update eunis_id, since data tables were merged and IDs might now be duplicated
fin <- fin %>%
    group_by(eunis_habitat) %>%
    mutate(eunis_id = cur_group_id()) %>%
    arrange(eunis_id) %>%
    as.data.table()

# Clean cluster numbers to yield unique cluster numbers
clean <- unique(fin[, .(eunis_id, cluster)])
clean[, cluster_id := 1:nrow(clean)]
fin <- merge(fin, clean, by = c("eunis_id", "cluster"))
fin[, cluster := NULL]

setcolorder(fin, c("site_id", "lon", "lat", "eunis_habitat", "eunis_id", "eunis_count", "cluster_id"))

# Calculate cluster count
fin[, cluster_count := .N, by = cluster_id]

## Using betadipser {vegan} to find the most typical community in a cluster
# First, split data with 1 habitat 
disper <- fin[cluster_count == 1]

# Second, chose random site_id for eunis_count == 2
random <- fin[cluster_count == 2]
rand_v <- seq(1, nrow(random), by = 2)

disper <- rbind(disper, random[rand_v, ])

# Third calculate betadisper for eunis_count >= 3
sub <- fin[cluster_count >= 3, ]

# Merge with species data 
species <- merge(sub[, .(site_id, lon, lat, cluster_id)],
                 dt[, .(site_id, lon, lat, species, abundance, tax_level)],
                 by = c("site_id", "lon", "lat"),
                 all.x = TRUE)

# Set abundance to 1
#species[, abundance := 1]

# # To use a for loop the cluster_ids need to be cleaned
# clean <- unique(species[, .(site_id, cluster_id)])
# clean[, clean_cluster := 1:nrow(clean)]
# species <- merge(species, clean, by = c("site_id", "cluster_id"))
# setcolorder(species, c("site_id", "cluster_id", "clean_cluster"))

# Transpose
spec_mat <- dcast(species[, .(site_id, cluster_id, species, abundance)],
                  site_id + cluster_id ~ species,
                  fun.aggregate = length)

# Define species table and group vecor
groups <- spec_mat$cluster_id
spec_tab <- spec_mat[, 3:ncol(spec_mat)]
#spec_tab[spec_tab > 0] <- 1

# Calculate distance matrix and model
dis <- vegdist(spec_tab, method = "bray")
mod <- betadisper(dis, groups)

# anova(mod)
# plot(mod)


# Get distances and bind to site_id + cluster_id
centroid_d <- data.table(site_id = spec_mat$site_id,
                         cluster_id = groups,
                         distance = mod$distances)

centroid_d <- centroid_d[order(cluster_id)]

# Get minimum distance within each cluster
min_dist <- centroid_d[, .SD[which.min(distance)], by = cluster_id]

# Use site_id to keep only those with minimum distance to group centroid and bind to data.table disper
disper <- rbind(disper, fin[site_id %in% min_dist$site_id, ])

## Use data.table disper to filter the original spider data
# and save the subset for computation of typical assemblages
dt <- dt %>%
    mutate(analysis_type = ifelse(site_id %in% disper$site_id,
                                  "variant_2",
                                  analysis_type))

dt_ds <- dt[analysis_type == "variant_2"] %>%
        unique(by="site_id") %>% 
        st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") 

dt_ds

dt_ds %<>% st_transform(crs="EPSG:3035")
mapview(dt_ds)
dt_ds_ppp <- dt_ds %>% select(geometry) %>% as.ppp()

k <- Kest(dt_ds_ppp, correction = "isotropic")
sum(k$iso) - sum(k$theo)

plot(k)
# L <- Lest(dt_ds_ppp)
# plot(L)
# mapview(dt_ds)
# 
# # ------------------------------------------------------------------------------------------------------------------ #
# #### SPATIAL CLUSTERING: VARIANT 3 #### ---------------------------------------------------------------------------- #
# 
# ## SUBSET
# id <- unique(dt[grepl("variant_2", analysis_type), .(site_id, lon, lat, habitat, hlz)])
# 
# # Add unique identifier for each EUNIS habitat # HLZ
# id <- id %>%
#     group_by(habitat, hlz) %>%
#     mutate(id = cur_group_id()) %>%
#     arrange(id) %>%
#     as.data.table()
# 
# # Add count per unique id
# id[, id_count := .N, by = id]
# 
# # Several observations per habitat-climate combination
# 
# # Merge with species data 
# species <- merge(id[, .(site_id, lon, lat, id)],
#                  dt[, .(site_id, lon, lat, species, abundance, tax_level)],
#                  by = c("site_id", "lon", "lat"),
#                  all.x = TRUE)
# 
# # # To use a for loop the cluster_ids need to be cleaned
# # clean <- unique(species[, .(site_id, cluster_id)])
# # clean[, clean_cluster := 1:nrow(clean)]
# # species <- merge(species, clean, by = c("site_id", "cluster_id"))
# # setcolorder(species, c("site_id", "cluster_id", "clean_cluster"))
# 
# # Transpose
# spec_mat <- dcast(species[, .(site_id, id, species, abundance)],
#                   site_id + id ~ species,
#                   fun.aggregate = length)
# 
# # Define species table and group vecor
# groups <- spec_mat$id
# spec_tab <- spec_mat[, 3:ncol(spec_mat)]
# spec_tab[spec_tab != 0] <- 1
# 
# # Calculate distance matrix and model
# dis <- vegdist(spec_tab, method = "raup"); beepr::beep(4)
# mod <- betadisper(dis, groups); beepr::beep(4)
# 
# # Get distances and bind to site_id + cluster_id
# centroid_d <- data.table(site_id = spec_mat$site_id,
#                          id = groups,
#                          distance = mod$distances)
# 
# ## Subset distances via for loop: keep only those with distance 90% of max distance
# # Add empty dataframe
# ids_keep <- data.table()
# 
# # For loop
# for(i in 1:max(centroid_d$id)) {
#     
#     # Subset
#     sub <- centroid_d[id == i, ]
#     
#     # Sort
#     sub <- sub[order(distance)]
#     
#     # Calculate range of communities
#     range <- round(nrow(sub) * 0.9)
#     
#     # Subset id's to keep
#     ids_keep <- rbind(ids_keep, sub[1:range, ])
# }
# 
# ## Use data.table ids_keep to filter the original earworm data
# # and save the subset for computation of typical assemblages
# dt <- dt %>%
#     mutate(analysis_type = ifelse(site_id %in% ids_keep$site_id,
#                                   "variant_3",
#                                   analysis_type))
# 
# dt[, analysis_type := ifelse(grepl("Family|Genus", tax_level), NA, analysis_type)]
# 
# 
# # ------------------------------------------------------------------------------------------------------------------ #
# #### FINAL STEPS ####
# 
# ## Remove entries manually
# dt <- dt %>%
#     
#     # Questionable coordinates
#     filter(!is.na(bgr)) %>%
#     
#     # Strange site/habitat decription
#     filter(!grepl("FRA1161|FRA1285|SWORM494|SWORM793", site_id)) %>%
#     
#     # Some sites have missing abundance (SWORM data issue)
#     filter(!is.na(abundance))
# 
# 
# # ------------------------------------------------------------------------------------------------------------------ #
# #### STORE DATA #### ----------------------------------------------------------------------------------------------- #
# 
# ## Quick save
# saveRDS(dt, file.path(home, "data", "03_assemblages", paste0(Sys.Date(), "_TA_data_earthworm.rds")))
# write.csv(dt, file.path(home, "data", "03_assemblages", paste0(Sys.Date(), "_TA_data_earthworm.csv")),
#           row.names = FALSE,
#           fileEncoding = "UTF-8")
# 
# ## Store everyting in one workbook
# # Create workbook
# wb <- openxlsx::createWorkbook()
# 
# # Add Working sheets
# openxlsx::addWorksheet(wb, "earthworm_dat")
# 
# # Store data in working sheets
# openxlsx::writeData(wb,
#                     sheet = "earthworm_dat",
#                     x = fin)
# 
# # Export
# openxlsx::saveWorkbook(wb,
#                        file = file.path(home, "data", "03_assemblages",
#                                         paste0(Sys.Date(), "_TA_data_earthworm.xlsx")),
#                        overwrite = TRUE)
