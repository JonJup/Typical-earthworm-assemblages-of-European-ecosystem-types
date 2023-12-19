## Compute turnover between typical assemblages


# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr, magrittr, stringr, vegan)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/15_typical_assemblages.rds")

# prepare data ----------------------------------------------------------------------
data2 <- split(data, by="typology_system")
data2 <- lapply(data2, function(x) x[,value:=1])
data3 <- lapply(data2, dcast, type ~ typical_taxa, value.var = "value", fun.aggregate = sum)
data4 <- lapply(data3, function(x) vegdist(x=x[,-1], 
                                           method="jaccard"))
data4 %<>% lapply(c)
n <- lapply(data4, length)
data5 <- 
        data.table(
                typology_system = rep(names(n), 
                                      times = unlist(n)
                ),
                distance        = unlist(data4)
        )
data5 %<>%
        mutate(typology_system = str_to_upper(typology_system)) %>%
        mutate(typology_system = str_replace_all(typology_system, "_", "-"))
# save to file ----------------------------------------------------------------------
x <- 
        rstudioapi::getActiveDocumentContext()[2]$path %>%
        stringr::str_remove(".*02_code")%>%
        readr::parse_number()
saveRDS(data5, paste0("01_data/",x,"_turnover_between_TAs.rds"))

