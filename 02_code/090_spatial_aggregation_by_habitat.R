# ———————————————————————————————————————————————————————— #
# ——— Establish optimal distance for spatial filtering ——— #
# ———————————————————————————————————————————————————————— #

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, fields, magrittr, mapview, dplyr, vegan, spatstat, sf)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/08_full_data_intersected_classifications.rds") 

# prepare ---------------------------------------------------------------------------
sites <- unique(data, by="site_id")
sites[,habitat_id := .GRP, by = c("hlz", "bgr", "clc", "eunis_habitat")]
sites[, habitat_count := .N, by = "habitat_id"]
#table(sites$habitat_count)
#sites <- sites[habitat_count >= 10]

# - assign habitat_id again to have a continuous numbering. As we removed some of the 
# - types in the previous steps this is not the case anymore. 
sites[,habitat_id := .GRP, by = c("clc", "hlz", "bgr", "eunis_habitat")]

# - Create a vector with all the distances for which the K statistics will be evaluated.
distance <- seq(from=10, to=150, by = 10)
# - Prepare a data table that will store the results of the for-loop.
out.dt <- unique(sites, by = "habitat_id")
out.dt <- data.table(distance = rep(distance, each=uniqueN(sites$habitat_id)),
                     habitat_id = 1:uniqueN(sites$habitat_id),
                     bgr        = out.dt$bgr,
                     hlz        = out.dt$hlz,
                     clc        = out.dt$clc,
                     eunis      = out.dt$eunis_habitat,
                     K_diff = 0)


for (i in seq_along(distance)){
        
        # - create data.frame that holds results of the following for loop
        i.hold_j <- data.frame()
        i.dist   <- distance[i]
        # - For loop
        # - The following for-loop loops over all valies of "eunis_id" in "sites". 
        # - "eunis_id" is a numerical representation of the "eunis_habitat" variable. 
        # - "eunis_habitat" gives the ecosystem type of a sampling site, following 
        # - the EUNIS typology system. 
        for (j in 1:max(sites$habitat_id)) {
                # - Subset to sites with the focal eunis_id 
                j.sub <- sites[habitat_id == j, ]
                if(nrow(j.sub) == 1){
                        j.sub$cluster <- 1
                        i.hold_j %<>% rbind(j.sub)
                        rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
                        rm("j")
                        next()
                } 
                # - computes a matrix of pairwise great circle distances,
                # - convert to distance matrix of class "dist",
                # - and compute a single linkage hierarchical clustering based on that 
                # - distance matrix.
                j.dis <- rdist.earth(j.sub[, .(lon, lat)], miles = FALSE, R = 6371) %>%
                        as.dist %>%
                        hclust(method = "single")
                # - If objects are closer than this loops distance limit they constitute
                # - one cluster. Add this cluster id to the j.sub data.table create above.  
                j.sub$cluster <- cutree(j.dis, h = i.dist)
                # - This j.sub object is the result of this loop. 
                # - Store the version from this iteration in the object hold_j. 
                i.hold_j %<>% rbind(j.sub)
                # - remove all objects specific to this loop iteration.
                rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
                rm("j")
        }
        i.hold_j[, n_cluster := uniqueN(cluster), by = "habitat_id"]
        i.skip_types <- i.hold_j[n_cluster < 10] %>% unique(by = "habitat_id") %>% pull(habitat_id)
        i.hold_j[, n_cluster := NULL]
        
        
        # - Each cluster number was given multiple times, i.e., once per eunis_id. 
        # - In this step, we sort this out. 
        # - We create an objekt for each unique combination of eunis_id and cluster.
        i.clean <- unique(i.hold_j[, .(habitat_id, cluster)])
        # - And now create a new cluster_id variable that counts the rows. 
        i.clean[, cluster_id := 1:nrow(i.clean)]
        # - We join the new table to the former using both the eunis_id and the cluster 
        # - variables
        i.hold_j %<>%
                merge(i.clean, by = c("habitat_id", "cluster"))
        #- We can now drop the old cluster variable 
        i.hold_j[, cluster := NULL]
        # How many sites do we have per cluster? 
        i.hold_j[, cluster_count := .N, by = cluster_id]
        
        # - Next we aim to find the most typical observation in each cluster. 
        
        # - For all clusters with only one site, the solution must be that site. 
        i.singeltons <- i.hold_j[cluster_count == 1 & ! habitat_id %in% i.skip_types]
        i.rare_types <- i.hold_j[habitat_id %in% i.skip_types]
        
        # - For all clusters with two sites, a random site is chosen, since both are 
        # - equally far away from they joined centroid. 
        i.random_point_for_two <- i.hold_j[cluster_count == 2]
        
        
        if (nrow(i.random_point_for_two) != 0){
                i.rand_v <- seq(from = 1, to = nrow(i.random_point_for_two), by = 2)
                i.random_point_for_two <- i.random_point_for_two[i.rand_v, ]   
        }
        
        
        if (nrow(i.singeltons) != 0 & nrow(i.random_point_for_two) != 0 & nrow(i.rare_types) != 0){
                
                i.manual <- rbind(i.singeltons, i.random_point_for_two, i.rare_types)
                
        } else if (nrow(i.singeltons) != 0 & nrow(i.random_point_for_two) != 0 & nrow(i.rare_types) == 0){
                
                i.manual <- rbind(i.singeltons, i.random_point_for_two)      
                
        } else if (nrow(i.singeltons) != 0 & nrow(i.random_point_for_two) == 0 & nrow(i.rare_types) != 0){
                
                i.manual <- rbind(i.singeltons, i.rare_types)
                
        } else if (nrow(i.singeltons) == 0 & nrow(i.random_point_for_two) != 0 & nrow(i.rare_types) != 0){
                
                i.manual <- rbind(i.random_point_for_two, i.rare_types)
                
        } else if (nrow(i.singeltons) == 0 & nrow(i.random_point_for_two) != 0 & nrow(i.rare_types) == 0){
                
                i.manual <- rbind(i.random_point_for_two)
                
        } else if (nrow(i.singeltons) == 0 & nrow(i.random_point_for_two) == 0 & nrow(i.rare_types) == 0){
                
                i.manual <- data.table()
                
        } else if (nrow(i.singeltons) == 0 & nrow(i.random_point_for_two) == 0 & nrow(i.rare_types) != 0){
                i.manual <- i.rare_types
        }

        # - For cases with three or more observatios we compute the multivariate centroid,
        # - with the vegan::betadisper() function
        
        # Third calculate betadisper for eunis_count >= 3
        i.sub <- i.hold_j[cluster_count >= 3,]
        
        # Merge with species data
        i.species <-
                merge(i.sub[, .(site_id, lon, lat, cluster_id)],
                      data[, .(site_id, lon, lat, species, abundance, tax_level)],
                      by = c("site_id", "lon", "lat"),
                      all.x = TRUE)
        # Transpose to one column per species 
        i.spec_mat <-
                dcast(i.species[, .(site_id, cluster_id, species, abundance)],
                      site_id + cluster_id ~ species,
                      fun.aggregate = length)
        i.rs <- rowSums(i.spec_mat[,4:ncol(i.spec_mat)])
        if (any(i.rs == 0)) i.spec_mat <- i.spec_mat[-which(i.rs==0), ]
        # Define species table and group vecor
        i.groups   <- i.spec_mat$cluster_id
        i.spec_tab <- i.spec_mat[, 4:ncol(i.spec_mat)]
        
        # - Calculate the distance matrix and fit the model
        i.dis <- vegdist(i.spec_tab, method = "bray")
        i.mod <- betadisper(i.dis, i.groups)
        
        # extract distances to centroid and combine with cluster id 
        i.centroid_d <- data.table(
                site_id    = i.spec_mat$site_id,
                cluster_id = i.groups,
                distance   = i.mod$distances
        )
        
        i.centroid_d <- i.centroid_d[order(cluster_id)]
        
        # Get minimum distance within each cluster
        i.min_dist <-
                i.centroid_d[, .SD[which.min(distance)], by = cluster_id]
        
        # Use site_id to keep only those with minimum distance to group centroid and bind to data.table disper
        i.disper <-
                rbind(i.manual, i.hold_j[site_id %in% i.min_dist$site_id,])
        
        out.dt[distance == i.dist, 
               n_sites := nrow(i.disper)]
        
        for (k in 1:uniqueN(i.disper$habitat_id)){
                
                k.disper <- 
                        sites %>%
                                dplyr::filter(site_id %in% i.disper$site_id & 
                                                      habitat_id == k) %>%
                                st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
                                st_transform(crs = "EPSG:3035") %>%
                                select(geometry) %>%
                                as.ppp()
                if (k.disper[[2]]>2) {
                        k.k <- Kest(k.disper, correction = "isotropic")
                        k.distance <- sum(k.k$iso) - sum(k.k$theo)
                } else {
                        k.distance <- NA
                }
                out.dt[distance == i.dist & habitat_id == k,K_diff:=k.distance]
                rm(list = ls()[grepl(pattern = "^k\\.", x = ls())])
                rm(k)
        }
        rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
        print(i)
        rm(i)
}
saveRDS(out.dt, "01_data/09_k_statistic.rds")
rm(list = ls())
gc()
