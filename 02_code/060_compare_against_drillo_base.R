# setup -----------------------------------------------------------------------------
library(dplyr)
library(stringr)
library(magrittr)
library(RCurl)
library(data.table)
# create driloBASE taxa list --------------------------------------------------------
URL <- "http://taxo.drilobase.org/index.php?title=List_of_taxa/Lumbricidae"

drilo_taxa <- 
        RCurl::getURL(URL) %>%
        str_split("\\\n") %>% 
        {\(x) x[[1]][237:938]}() %>%
        lapply(str_remove_all, "\\(.*\\)") %>%
        lapply(str_remove_all, "<[a-z]>") %>%
        lapply(str_remove_all, "<\\/[a-z]>") %>% 
        lapply(str_remove_all, "<.*>") %>%
        lapply(str_trim) %>%
        unlist
#- drop empty elements from list
drilo_taxa <- drilo_taxa[-which(drilo_taxa == "")]
rm(URL);gc()

# load our earthworm data -----------------------------------------------------------
data <- readRDS("01_data/05_full_data.rds")

# - fix known issues 
data[species == "Allolobophora carneluttii", species := "Allolobophora carnelutti"]
data[species == "Octodrilus argoviensis", species := "Octodrilus argoviense"]
data[species == "Proctodrilus antipae", species := "Proctodrilus antipai"]

# - according to GBIF "Allolobophora exacystis" is corrected and "Octodrilus exacystis" the synonym. 
# - However, the latter is included in the DriloBase and the former not.
data[species == "Allolobophora exacystis", species := "Octodrilus exacystis"]

# - other species found via gbis synonym 
data[species == "Bimastos rubidus", species := "Lumbricus victoris"]
data[species == "Eophila bartolii", species := "Allolobophora bartolii"]

data <- data[!species %in% c("Fitzingeria platyura", "Octolasium lissaensis")]

# - completely missing from driloBase
data[species %in% c("Allolobophora parva", 
                    "Cernosvitovia dofleini", 
                    "Cernosvitovia paratuleskovi", 
                    "Cernosvitovia strumicae", 
                    "Cernosvitovia treskavicensis", 
                    "Dendrobaena veneta"), species := NA]

# - extract unique species 
us <- unique(data$species)
us <- us[-which(is.na(us))]

# - any species not in drilobase?
if (any(!us %in% drilo_taxa)){
        print(sort(us[which(!us %in% drilo_taxa)]))
}

# - recheck richness 
data <- data[!is.na(species)]

saveRDS(data, "01_data/06_full_data_taxa_checked.rds")
rm(list=ls());gc()
