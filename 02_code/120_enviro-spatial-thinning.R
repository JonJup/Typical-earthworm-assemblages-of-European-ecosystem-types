# ——————————————————————————————— #
# ——— Enviro-Spatial-Thinning ——— #
# ——————————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 6.12.2023

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, fields, magrittr, dplyr, vegan, spatstat,sf)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/08_full_data_intersected_classifications.rds")
distance_vector <- readRDS("01_data/11_thinning_distance_by_habitat.rds")

# prepare ---------------------------------------------------------------------------
sites <- unique(data, by = "site_id")

# - how many samples do we have for each habitat type? 
sites[,habitat_id := .GRP, by = c("bgr", "eunis_habitat")]

sites %>% 
        select(habitat_id) %>%
        table %>%
        sort

sites[, habitat_count := .N, by = "habitat_id"]
sites <- sites[habitat_count >= 10]
sites[,habitat_id := .GRP, by = c("bgr", "eunis_habitat")]
uniqueN(sites$habitat_id)

# - create data.frame that holds results of the following for loop
cluster_dt <- data.frame()

# - The following for-loop loops over all thinning types (habitat_id) in sites.
# - skip types for which this would mean the removal from the study 
# - boreal forest
# - "Pannonian_Open Land"

for (j in 1:max(sites$habitat_id)) {
        
        #if (j %in% c(6,9,15,17))  next()     

        j.dist <- distance_vector$thinng_distance[j]
        # - Subset to sites with the focal eunis_id
        j.sub <- sites[habitat_id == j,]
        # - computes a matrix of pairwise great circle distances
        j.dis <-
                rdist.earth(j.sub[, .(lon, lat)], miles = FALSE, R = 6371) %>%
        # - convert to distance matrix of class "dist"
                as.dist
        # - compute a single linkage hierarchical clustering based on that
        # - distance matrix.
        j.dis %<>% hclust(method = "single")
        # - If objects are closer than this loops distance limit they constitute
        # - one cluster. Add this cluster id to the j.sub data.table create above.
        j.sub$cluster <- cutree(j.dis, h = j.dist)
        # - This j.sub object is the result of this loop.
        # - Store the version from  this iteration in the object hold_j.
        cluster_dt %<>% rbind(j.sub)
        # - remove all objects specific to this loop iteration.
        rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
        rm("j")
}

# - Count clusters for each type. This tells us how many sites will be left.
# cluster_dt[, n_cluster := uniqueN(cluster), by = "habitat_id"]
# skip_types <- cluster_dt[n_cluster < 10] %>% unique(by = "habitat_id") %>% pull(habitat_id)
# cluster_dt[, n_cluster := NULL]

# - Each cluster id is only unique within habitat_types. 
# - For the next steps we need globally unique cluster_ids 
# - and drop the old variable 
cluster_dt[, cluster_id := paste(habitat_id, cluster, sep = "_")]
cluster_dt[, cluster_id := .GRP, by = "cluster_id"]
cluster_dt[, cluster := NULL]
# -  How many sites do we have per cluster?
cluster_dt[, cluster_count := .N, by = cluster_id]

# - Next we aim to find the most typical observation in each cluster.
# - For all clusters with only one site, the solution is that site.
singeltons <- cluster_dt[cluster_count == 1]
# - Some types would drop below the 10 sample threshold if we apply thinning. 
# - We collect them in a separate table 
# rare_types <- cluster_dt[habitat_id %in% skip_types & cluster_id %in% singeltons$cluster_id]
# - For all clusters with two sites, a random site is chosen, since both are
# - equally far away from they joined centroid.
two_points <- 
        copy(cluster_dt) %>%
        .[cluster_count == 2] %>%
        arrange(cluster_id)
n <- nrow(two_points)/2
two_points %<>%
        .[, date_id  := rep(c(0,1), times = n)] %>%
        .[date_id == 1]

manual <- rbindlist(list(singeltons,
                        # rare_types,
                         two_points))

rm(singeltons, 
   #rare_types, 
   two_points,n)

# - For cases with three or more observatios we compute the multivariate centroid,
# - with the vegan::betadisper() function
three_or_more <- cluster_dt[cluster_count >= 3]
# Merge with species data
three_or_more  <- 
        merge(three_or_more[, .(site_id, lon, lat, cluster_id)],
              data[, .(site_id, lon, lat, species, abundance)],
              by = c("site_id", "lon", "lat"),
              all.x = TRUE)
# - Transpose to one column per species
three_or_more <-
        dcast(three_or_more[, .(site_id, cluster_id, species, abundance)],
              site_id + cluster_id ~ species,
              fun.aggregate = length)
# - any empty columns, i.e., species missing from all columns? 
rs <- rowSums(three_or_more[,3:ncol(three_or_more)])
if (any(rs == 0)) three_or_more <- three_or_more[-which(rs==0), ]
# - Define species table and group vecor
site_id      <- three_or_more$site_id
cluster_id   <- three_or_more$cluster_id
three_or_more <- three_or_more[, 3:ncol(three_or_more)]

# Calculate distance matrix and model
three_or_more_dis <- vegdist(three_or_more, method = "jaccard")
three_or_more_mod <- betadisper(three_or_more_dis, 
                                cluster_id)
# extract distances to centroid and combine with cluster id
centroid_d <- data.table(
        site_id    = site_id,
        cluster_id = cluster_id,
        distance   = three_or_more_mod$distances
)

centroid_d <- centroid_d[order(cluster_id)]

# Get minimum distance within each cluster
min_dist <- centroid_d[, .SD[which.min(distance)], by = cluster_id]

# Use site_id to keep only those with minimum distance to group centroid and bind to data.table disper
thinned_sites <- rbind(manual, cluster_dt[site_id %in% min_dist$site_id])
data2 <- data[site_id %in% thinned_sites$site_id]

# save to file ----------------------------------------------------------------------
saveRDS(data2, "01_data/12_thinned_data.rds")
rm(list = ls());gc()
