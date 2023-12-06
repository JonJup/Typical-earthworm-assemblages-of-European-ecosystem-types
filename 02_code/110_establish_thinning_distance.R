# ———————————————————————————————————————————————————————— #
# ——— Establish distance to thin for each habitat type ——— # 
# ———————————————————————————————————————————————————————— #

# Jonathan Jupke 

# Establish the distance at which spatial aggregation (difference between observed and null model Ripley's K)
# for the sampling sites is below 75% the distance observed for the habitat types themselves. 

# setup -----------------------------------------------------------------------------
pacman::p_load(dplyr, data.table, magrittr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/09_k_statistic.rds")
ref  <- readRDS("01_data/helper/101_habitat_evaluations.rds")

# prepare data ----------------------------------------------------------------------
types <- unique(data, by = "habitat_id") %>% setorderv("habitat_id")
types[, combo := paste(hlz, clc, bgr, eunis, sep ="_")]

which(!types$combo %in% ref$combo)

data[habitat_id %in% c(11,17,19,48), K_diff]

data[habitat_id == 11]


uniqueN(types$combo)
uniqueN(ref$combo)

# - subset reference data to thinning types in samples 
ref <- ref[combo %in% types$combo]

# - arrange the data according to the types so that thinning types are in the same order
ref   %<>% arrange(hlz, bgr, clc, eunis)
types %<>% arrange(hlz, bgr, clc, eunis)

# - assign thinning types to habitat data and remove other columns
ref %<>% 
        select(threshold = distance) %>%
        mutate(habitat_id = types$habitat_id)

#- join ref and data on habitat id
comb <- ref[data, on = "habitat_id"]
# - turn habitat id into a factor (for plotting)
comb[, habitat_id := factor(habitat_id)]
# - identify the distance at which sample aggregation is smaller than 75% of habitat aggregation
comb[, thin := K_diff <= 0.75*threshold | K_diff <= 0]

# - For each type, determine the smallest distance for which the data meet the criterion defined above. 
# - For this, first create a list where each element is a subset of comb holding data from just one type.  
comb2 <- split(comb, comb$habitat_id)

# - From this list extract the minimal value of distance for which the variable thin equals true. 
# - This is the smallest distance for which spatial aggregation is below 75% of the habitat types' aggregation. 
comb2 %<>% 
        lapply(function(x) x[thin==TRUE]) %>%
        lapply(function(x) ifelse(nrow(x) == 0, 666, min(x$distance))) %>%
        unlist
    
comb3 <- unique(comb, by = "habitat_id")    
comb3[, thinng_distance := comb2]

# save to file ----------------------------------------------------------------------
saveRDS(comb3, "01_data/11_thinning_distance_by_habitat.rds")
rm(list=ls());gc()
