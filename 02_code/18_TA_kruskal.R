# ———————————————————————— #
# ——— Kruskal Wallis H ——— # 
# ———————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 06.12.23

# setup -----------------------------------------------------------------------------
pacman::p_load()

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/160_turnover_between_TAs.rds")

kruskal.test(data$distance, data$typology_system)

# save to file ----------------------------------------------------------------------
saveRDS(,"")