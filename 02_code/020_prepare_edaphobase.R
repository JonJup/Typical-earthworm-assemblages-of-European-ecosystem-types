# |————————————————————————————————————|
# |——— Prepare data from Edaphobase ———|
# |————————————————————————————————————|


# setup -----------------------------------------------------------------------------
pacman::p_load(sf, tidyverse, data.table, magrittr, taxize, stringr, mapview)
source("02_code/functions/prep_distances.R")
# load data -------------------------------------------------------------------------
edapho <- read_delim("01_data/raw/edaphobase.csv", n_max = 21636, col_select = !6)

# prepare data  ---------------------------------------------------------------------
# - reformat edaphobase data
# - Add site ID based on coordinates 
names(edapho) <- c("id", "source", "comment", "taxon", "valid_taxon", "country", "state", "zone", "area", "plot", "biotopetype", "lat", "lon", "radius", "abundance1", "abundance2")
setDT(edapho)
edapho <- edapho[lon != "Feld gesperrt"]
edapho[, site_id := .GRP, by = c("lat", "lon")]
edapho[, site_id := paste("edapho", site_id,sep = "_")]
edapho[, c("lon", "lat") := .(
        as.numeric(stringr::str_replace(lon, pattern = ",", replacement = "\\.")),
        as.numeric(stringr::str_replace(lat, pattern = ",", replacement = "\\."))
)]
sites <- unique(edapho, by = "site_id")
sites %<>% st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

edapho[, richness := uniqueN(taxon), by = "site_id"]
#edapho <- edapho[richness > 1]
sites <- unique(edapho, by = "site_id")

# - The code below combines all sites from the same data set that are closer than 50 meters. 
# - We have decided against this step.


# - check size of individual data sets 
# unique_sources <- sites$source %>% unique
# 
# for (i in c(1,3,4,8,98,109,188, 192, 55, 103, 121, 124)) {
#         
#         done <- FALSE
#         
#         while (done == FALSE) {
#                 
#                 sites <- unique(edapho, by = "site_id") %>% st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
#                 i.data <- sites %>% filter(source == unique_sources[i])
#                 i.distance.table <- prep_distances(i.data)
#                 if (nrow(i.distance.table) == 0) {
#                         done <- TRUE
#                 } else {
#                         i.distance.table %<>% arrange(distances)
#                         j.id1 <- i.distance.table[1, id3]
#                         j.id2 <- i.distance.table[1, id4]
#                         j.id1 <- i.data$site_id[j.id1]
#                         j.id2 <- i.data$site_id[j.id2]
#                         j.joined.lat <-
#                                 edapho[source == unique_sources[i] &  site_id %in% c(j.id1, j.id2), unique(lat)[1]]
#                         j.joined.lon <-
#                                 edapho[source == unique_sources[i] & site_id %in% c(j.id1, j.id2), unique(lon)[1]]
#                         
#                         edapho[site_id %in% c(j.id1, j.id2), c("site_id", "lat", "lon") := .(j.id1, j.joined.lat, j.joined.lon)]
#                         
#                         rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
#                         rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
# 
#                 }
#                 
#         }
#         rm(i)
#         rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
#         rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
# }
# rm(unique_sources)
# rm(done)
# 
# edapho[site_id %in% paste0("edapho_", c(683, 684)), c("site_id", "lon", "lat") := .("edapho_683", 10.6042, 51.7719)]
# edapho <- unique(edapho, by = c("site_id", "valid_taxon"))


# - find taxa names 
edapho[valid_taxon == "Artengruppe (Octolasion lacteum (Örley, 1881); Octolasion tyrtaeum (Savigny, 1826))", valid_taxon := "Octolasion"]
edapho[valid_taxon == "Artengruppe (Octolasion lacteum (Örley, 1881); Octolasion tyrtaeum (Savigny, 1826))", valid_taxon := "Octolasion"]
edapho[valid_taxon == "Aporrectodea/Allolobophora", valid_taxon := "Aporrectodea"]
edapho[valid_taxon == "Dendrobaena/Dendrodrilus", valid_taxon := "Dendrobaena"]

# - This code was used to remove synonyms and find high taxonomic levels 
# - It requires interaction to determine the correct entry where gbif has different ones. 
# - Thus it is commented out and the resulting object (tu_table) is directly loaded. 
# - This makes it easier to repeat the complete analysis automatically.
# - You can uncomment and run this section to check how it works. 

# tu <- unique(edapho$valid_taxon) %>% sort()
#ntu <- length(tu)
# tu_table <- data.table(valid_taxon = tu,
#                        species = character(length = ntu),
#                        genus   = character(length = ntu),
#                        family  = character(length = ntu)
#                        )
# 
# #- parameters for loop 
# start = 1
# end = ntu
# db = "gbif"
# 
# for (i in start:end){
#         
#         i.code <- get_ids(tu[i], db = db)
#         i.code <- classification(i.code$gbi)%>%
#                 {\(x) x[[1]]}() %>%
#                 setDT()
#         if ("family" %in% i.code$rank) {
#                 tu_table[valid_taxon == tu[i], family := i.code[rank == "family", name]]
#         }
#         if ("genus" %in% i.code$rank) {
#                 tu_table[valid_taxon == tu[i], genus := i.code[rank == "genus", name]]
#         }
#         if ("species" %in% i.code$rank) {
#                 tu_table[valid_taxon == tu[i], species := i.code[rank == "species", name]]
#         }
# }
# rm(i, i.code, start, end, db,tu, ntu)     
# saveRDS(tu_table, "01_data/021_tu_table.rds")

tu_table <- readRDS("01_data/helper/021_tu_table.rds")

edapho <- tu_table[edapho, on = "valid_taxon"]
rm (tu_table)

edapho[, c("comment", "valid_taxon", "source", "taxon", "state", "zone", "area", "plot", "biotopetype") := NULL]

edapho[species == "", species := NA]
edapho[genus == "",   genus   := NA]
edapho[family == "",  family  := NA]

edapho[, country := str_remove_all(country, pattern = "\\ \\(.*")]

unique(edapho$radius)

edapho <- edapho[, radius := as.numeric(str_replace(radius, ",", "\\."))]
edapho <- edapho[, abundance2 := as.numeric(str_replace(abundance2, ",", "\\."))]

edapho[, abundance := abundance2]
edapho[, c("abundance1", "abundance2") := NULL]
sites <- unique(edapho, by = "site_id")
sites %<>% st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
#sites %<>% filter(radius <= 1000)
edapho <- edapho[site_id %in% sites$site_id]
edapho[, precision_m := radius]
edapho[, radius := NULL]
edapho[, id := NULL]
edapho[, tax_level := case_when(!is.na(species) ~ "species",
                                is.na(species) & !is.na(genus) ~ "genus",
                                is.na(species) & is.na(genus) & is.na(family) ~ "family")
       ]
sites <- unique(edapho, by = "site_id")
# save to file ----------------------------------------------------------------------
saveRDS(edapho, "01_data/02_prepared_edapho.rds")

# empty environment -----------------------------------------------------------------
rm(list = ls())

