## -- turnover analysis -- ## 

pacman::p_load(BAT, data.table, dplyr,conflicted, vegan)

conflict_prefer(name = "beta", winner = "BAT")

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/13_no_rare_types.rds")

# prepare data ----------------------------------------------------------------------
data[, occurrence := 1]
data2 <- dcast(data, formula = site_id ~ species, value.var = "occurrence", fun.aggregate = sum)
data3 <- data[,c("site_id", "bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc")]
data3 <- unique(data3, by = "site_id")
data4 <- left_join(data3, data2, by = "site_id")

beta.div <- beta(data4[, -c(1:5)], comp = T)


cs.turnover <- vector(mode="list", length = 4)
cs.turnover <- lapply(
        list(data4$bgr_eunis, data4$bgr_clc, data$hlz_clc, data4$hlz_eunis), 
        function(x) meandist(beta.div$Brepl, grouping = x)
        )
cs.turnover <- lapply(cs.turnover, summary)
cs.turnover <- lapply(cs.turnover, function(x) x$CS)
cs.turnover <- unlist(cs.turnover)

cs.total<- vector(mode="list", length = 4)
cs.total<- lapply(
        list(data4$bgr_eunis, data4$bgr_clc, data$hlz_clc, data4$hlz_eunis), 
        function(x) meandist(beta.div$Btotal, grouping = x)
)
cs.total<- lapply(cs.total, summary)
cs.total<- lapply(cs.total, function(x) x$CS)
cs.total<- unlist(cs.total)


dt.out <- data.table(typology_system = c("bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc"),
                     cs.turnover = cs.turnover,
                     cs.total = cs.total
)

# save to file ----------------------------------------------------------------------
saveRDS(dt.out, "01_data/19_classification_strength_turnover2.rds")

