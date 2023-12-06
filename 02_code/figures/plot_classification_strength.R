## 

# setup -----------------------------------------------------------------------------
pacman::p_load(ggplot2,data.table,dplyr,tidyr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/19_classification_strength_turnover2.rds")

# prepare data ----------------------------------------------------------------------
# data2 <- 
#         unique(data, by = "typology_system")

data %>%
        tidyr::pivot_longer(cols = !typology_system) %>%
        ggplot(aes(y = typology_system, x = value, fill = name)) + 
        geom_col(position = "dodge")

# save to file ----------------------------------------------------------------------
#saveRDS(,"")
