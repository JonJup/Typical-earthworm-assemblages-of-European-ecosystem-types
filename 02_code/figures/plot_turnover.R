## Plot turnover analysis

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, ggplot2, ggdist, magrittr,dplyr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/160_turnover.rds")

# prepare data ----------------------------------------------------------------------
data2 <- 
        data %>% 
        melt(id.vars = c("typology_system", "within"), measure.vars = c("turnover", "beta.total"))

# plot ------------------------------------------------------------------------------
ggplot(data2, aes(y = value, x = typology_system, fill = within)) + 
        geom_boxplot() + 
        facet_wrap(.~variable)

data %>%
        filter(within) %>%
        ggplot(aes(x = turnover, y = type1)) + 
        geom_boxplot() + 
        facet_wrap(.~typology_system, scales="free")
        
# save to file ----------------------------------------------------------------------
saveRDS(,"")
