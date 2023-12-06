# Intersect classification systems 


# setup -----------------------------------------------------------------------------
pacman::p_load(
        
        data.table,
        dplyr,
        magrittr
        
)
# load data -------------------------------------------------------------------------
data <- readRDS("01_data/07_full_data_no_poor.rds")

# intersect_classifications ---------------------------------------------------------
data[, hlz_clc   := paste(hlz, clc,        sep="_")]
data[, hlz_eunis := paste(hlz, eunis_code, sep="_")]
data[, bgr_clc   := paste(bgr, clc,        sep="_")]
data[, bgr_eunis := paste(bgr, eunis_code, sep="_")]

# drop rare types -------------------------------------------------------------------

sites <- unique(data, by = "site_id")

no_rare_left <- FALSE 

while(!no_rare_left){
        if (any (table(sites$hlz_clc) < 10)){
                sites <- sites[!hlz_clc %in% names(which(table(sites$hlz_clc)<10))]
        }
        if (any (table(sites$hlz_eunis) < 10)){
                sites <- sites[!hlz_eunis %in% names(which(table(sites$hlz_eunis)<10))]
        }
        if (any (table(sites$bgr_clc) < 10)){
                sites <- sites[!bgr_clc %in% names(which(table(sites$bgr_clc)<10))]
        }
        if (any (table(sites$bgr_eunis) < 10)){
                sites <- sites[!bgr_eunis %in% names(which(table(sites$bgr_eunis)<10))]
        }
        
        if (!any(table(sites$hlz_clc) < 10) & !any(table(sites$hlz_eunis) < 10) & 
            ! any(table(sites$bgr_clc) < 10) & !any(table(sites$bgr_eunis) < 10)){
                no_rare_left <- TRUE 
        }                 

}

data <- data[site_id %in% sites$site_id]

# save to file ----------------------------------------------------------------------
saveRDS(data, "01_data/08_full_data_intersected_classifications.rds")

# clean environment -----------------------------------------------------------------
rm(list = ls())
gc()

