# —————————————————————————————— #
# ——— Recheck for rare types ——— # 
# —————————————————————————————— #

# Jonathan Jupke (jonjup@protonmail.com)
# 06.12.2023

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/12_thinned_data.rds")

# prepare data ----------------------------------------------------------------------

sites <- unique(data, by = "site_id")
sites_og <- copy(sites)

all.good  <- FALSE

# remove rare types -----------------------------------------------------------------
while (all.good == FALSE) {
        if (any(table(sites$hlz_clc) < 10)) {
                drop_type <- names(which(table(sites$hlz_clc) < 10))
                sites <- sites[!hlz_clc %in% drop_type]
                rm(drop_type)
        }
        if (any(table(sites$hlz_eunis) < 10)) {
                drop_type <- names(which(table(sites$hlz_eunis) < 10))
                sites <- sites[!hlz_eunis %in% drop_type]
                rm(drop_type)
        }
        if (any(table(sites$bgr_clc) < 10)) {
                drop_type <- names(which(table(sites$bgr_clc) < 10))
                sites <- sites[!bgr_clc %in% drop_type]
                rm(drop_type)
        }
        if (any(table(sites$bgr_eunis) < 10)) {
                drop_type <- names(which(table(sites$bgr_eunis) < 10))
                sites <- sites[!bgr_eunis %in% drop_type]
                rm(drop_type)
        }
        
        if (all(table(sites$hlz_clc) > 9) &
            all(table(sites$hlz_enus) > 9) &
            all(table(sites$bgr_clc) > 9) &
            all(table(sites$bgr_eunis) > 9)) {
                all.good <- TRUE
        }
        
}
data <- data[site_id %in% sites$site_id]

# save to file ----------------------------------------------------------------------
saveRDS(data, "01_data/13_no_rare_types.rds")
rm(list=ls())
gc()
