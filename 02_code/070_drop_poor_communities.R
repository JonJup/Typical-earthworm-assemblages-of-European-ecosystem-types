# drop poor communities and clc classes

library(data.table)
data <- readRDS("01_data/06_full_data_taxa_checked.rds")

data[, lowest_taxon := ifelse(!is.na(species), species, ifelse(!is.na(genus), genus, family))]
data[, richness := uniqueN(species), by = "site_id"]
table(data$richness)
data <- data[richness > 1]
data <- data[!clc %in% c("Artifical Surface", "Waterbody")]
data <- data[!eunis_code %in% c("J", "FB", "A", "H", "D", "F")]
saveRDS(data, "01_data/07_full_data_no_poor.rds")
rm(list = ls())

