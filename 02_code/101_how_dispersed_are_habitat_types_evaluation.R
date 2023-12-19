# ---------------------------------------- #
# - How dispersed are the habitat types? - # 
# - Evaluating habitat distribution      - # 
# ---------------------------------------- #

# setup -----------------------------------------------------------------------------
pacman::p_load(sf, dplyr, data.table, magrittr,spatstat)

# load data -------------------------------------------------------------------------
habitats <- readRDS("01_data/helper/100_habitat_reference_points.rds")
samples  <- readRDS("01_data/08_full_data_intersected_classifications.rds")

# prepare samples -------------------------------------------------------------------
# - Add a habitat id for the thinning type (combination of EUNIS, CLC, HLZ, and BGR)
# - Remove all thinning types with less than 10 samples and reassign ID. 
samples %<>%
        unique(by = "site_id") %>%
        group_by(bgr, eunis_habitat) %>%
        mutate(habitat_id := cur_group_id()) %>% 
        group_by(habitat_id) %>% 
        add_count(name = "nn") %>% 
        ungroup()

#- subset habitats to remaining combinations 
remaining_combinations <- 
        samples %>%
        select(bgr, eunis_habitat) %>%
        unique(by = c("bgr", "eunis_habitat")) %>%
        mutate(combo = paste(bgr, eunis_habitat, sep="_"))

# - Add the same variable to the habitats data. 
habitats %<>% 
        mutate(combo = paste(bgr, eunis, sep = "_")) %>% 
# - subset habitat data to those types that occur in samples
        filter(combo %in% remaining_combinations$combo)
rm(remaining_combinations)
rm(samples)
# evaluate dispersal ----------------------------------------------------------------

# - Create empty table to hold results of loop
out.dt <- 
        data.table(
                combo = unique(habitats$combo), 
                distance = 0
                )
# - Loop over unique types 
for (i in seq_along(unique(habitats$combo))){
        
        # - print current iteration as feedback 
        print(i)
        # - subset data to focal habitat type of this iteration
        i.sub <- 
                habitats %>% 
                filter(combo == unique(combo)[i])
        if (nrow(i.sub) == 1){
                
                out.dt$distance[i] <- 0
                # - empty environment of loop specific variables
                rm(i)
                rm(list = ls()[grepl("^i\\.", ls())])
                # - skip to next loop iteration
                next()
        }
        # - extract geometry column and turn into ppp object from spatstat package
        i.sub %<>% 
                select(geometry) %>% 
                as.ppp()
        # - compute Ripley's K
        i.k <- Kest(i.sub, correction = "isotropic")
        # - compute difference between observed pattern and isotropic null model
        out.dt$distance[i] <- sum(i.k$iso) - sum(i.k$theo)
        # - empty environment of loop specific variables
        rm(i)
        rm(list = ls()[grepl("^i\\.", ls())])
}
rm(habitats)
# - Create a list where each element is a vector holding four objects. Each of those 
# - objects is the name of a CLC, EUNIS, BGR, or HLZ type. 
split <- 
        out.dt$combo %>% 
        stringr::str_split(pattern="_")

# - loop over the elements of the newly created list and assign the respective habitat types 
# - to each row of out.dt 
for (i in 1:length(split)){
        
        out.dt$bgr[i]   <- split[[i]][1]
        out.dt$eunis[i] <- split[[i]][2]
        rm(i)
}
rm(split)

# save to file ----------------------------------------------------------------------
x <- 
        rstudioapi::getActiveDocumentContext()[2]$path %>%
        stringr::str_remove(".*02_code")%>%
        readr::parse_number()

saveRDS(out.dt, paste0("01_data/helper/",x,"_habitat_evaluations.rds"))
rm(list = ls());gc()
