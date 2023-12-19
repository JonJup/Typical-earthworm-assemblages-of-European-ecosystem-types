## -- turnover analysis -- ## 

pacman::p_load(BAT, data.table, dplyr,conflicted, ggplot2, ggdist, purrr)

conflict_prefer(name = "beta", winner = "BAT")

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/13_no_rare_types.rds")

# prepare data ----------------------------------------------------------------------
data[, occurrence := 1]
data2 <- dcast(data, formula = site_id ~ species, value.var = "occurrence", fun.aggregate = sum)
data3 <- data[,c("site_id", "bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc")]
data3 <- unique(data3, by = "site_id")
data4 <- left_join(data3, data2, by = "site_id")

# what I need to do is compute the turnover between sites within one type 

rs.list <- list()
for (typology in c("bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc")){
        
        #if (typology == "bgr_eunis") next()
        print(paste("Starting", typology))
        #- extract unique types of that system 
        t.types <- 
                data4[, ..typology] %>% 
                unique %>% 
                unlist
        t.id <- which(names(data4) == typology)
        for (j in t.types){
                j.in <- which(data4[, ..t.id] == j)
                j.in <- data4[j.in]
                j.ou <-which(data4[, ..t.id] != j)
                j.ou <- data4[j.ou]
                
                # turnover within the type 
                j.beta <- beta(j.in[, 6:ncol(j.in)], comp=T)$Brepl  %>% c
                rs.list[[length(rs.list) + 1]] <- 
                        data.table(
                                typology_system = typology, 
                                type = j,
                                turnover = j.beta
                        )
                
                rm(list = ls()[grepl("^j\\.", x = ls())])
                rm(j)
        }
        rm(list = ls()[grepl("^t\\.", x = ls())])
        rm(typology)
}

rs.list2 <- rbindlist(rs.list)
saveRDS(rs.list2, "01_data/16_within_type_turnover.rds")

all.beta <- beta(data4[,-c(1:5)], comp = T)
all.beta.vec <- c(all.beta$Brepl)

mean(all.beta.vec, na.rm = T)


# - now compute turnover between sites 

# for (j in c("bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc")) {
#         
#         # - create vector with all unique types of the focal typology system
#         j.types <- 
#                 data4[,..j] %>%
#                 unique %>%
#                 unlist %>%
#                 as.list
#         
#         # - for each type create a subset that contains all sites that are not within this type
#         j.other <- lapply(i.types, function(x) data4[c(data4[,..i] != x)])
#         j.same  <- lapply(i.types, function(x) data4[c(data4[,..i] == x)])
#         test <- j.same %>%
#                 lapply(split, by = "site_id")
#         
#         test <- test[1:2]
#         
#         map_function <- function(x,i){
#                 lapply(x, function(y,i) rbind(y, i.other[[i]]))
#         }
#         
#         test2 <- imap(test, map_function)
#         lapply(test, function(x) lapply(x, function(y) rbind(y, other)))
#         
#         
#         test3 <- lapply(
#                 1:length(j.types), 
#                 function(index) lapply(
#                         test[[index]], 
#                         function(x) beta(
#                                 rbind(
#                                         x, 
#                                         j.other[[index]]
#                                 )[,-c(1:5)]
#                                 )
#                         )
#                 )
#         
#         
#         
#         
#         
#         rm(list = ls()[grepl("^i\\.", x = ls())])
#         rm(i)
# }


rs.list2

rs.add <- data.table(typology_system = "all",
                     type = "all", 
                     turnover = all.beta.vec)

rs.list3 <- rbindlist(list(rs.list2, rs.add))

# make plot -------------------------------------------------------------------------
ggplot(rs.list3, aes(x=typology_system, y = turnover)) + 
        geom_boxplot()

ggplot(rs.list, aes(y=type, x = turnover, fill = type)) + 
        geom_boxplot() + 
        facet_wrap(.~typology_system, scales = "free")

ggplot(rs.list, aes(y=type, x = turnover)) + 
        stat_halfeye(
                adjust = 5,
                aes(fill = after_stat(level)),
                .width = c(.66,.95,1)
        ) + 
        facet_wrap(.~typology_system, scales = "free") + 
        scale_fill_brewer(direction = 1) + 
        theme(
                legend.position = "none",
                panel.background = element_blank()
        )

saveRDS(rs.list, "01_data/160_turnover.rds")
