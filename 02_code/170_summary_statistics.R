# —————————————————————————— #
# ——— Summary statistics ——— # 
# —————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 06.12.24

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/13_no_rare_types.rds")
data2<- readRDS("01_data/15_typical_assemblages.rds")

#- how many taxa?
uniqueN(data$species)

# - most common taxa 
data$species %>% 
        table %>% 
        sort(decreasing = T)%>%
        .[1:3]

# most common types 
sites <- unique(data, by = "site_id")
table(sites$hlz)
table(sites$bgr)
table(sites$clc)
table(sites$eunis_habitat)

table(sites$bgr_clc)
table(sites$hlz_clc)
table(sites$bgr_eunis)
table(sites$hlz_eunis)

# - taxa in all TAs or at least most 
table(data2$typical_taxa) %>% sort
ut <- unique(data2, by = c("typology_system", "type"))
dplyr::setdiff(
               as.character(unique(ut$type)),
               as.character(unique(ut[typical_taxa == "Allolobophora chlorotica"]$type))
               )

data2[typology_system=="bgr_clc"]

# - Explore PERMANOVA
data <- readRDS("01_data/14_permanova.rds")
