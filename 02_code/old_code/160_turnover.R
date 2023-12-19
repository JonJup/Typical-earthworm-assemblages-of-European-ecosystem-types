## -- turnover analysis -- ## 

pacman::p_load(BAT, data.table, dplyr,conflicted)

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
        for (i in 1:nrow(data4)){
                for (j in (i+1):nrow(data4)){
                        print(paste(typology,i,j, sep ="_"))
                        if (i == nrow(data4)) next()
                        
                        j.beta <- beta(comm = data4[c(i,j),6:ncol(data4)], abund=FALSE)
                        j.res <- data.table(
                                typology_system = typology,
                                type1 = data4[i, ..typology],
                                type2 = data4[j, ..typology], 
                                turnover = as.numeric(j.beta$Brepl),
                                beta.total = as.numeric(j.beta$Btotal)
                                )
                        names(j.res)[c(2,3)] <- c("type1", "type2")
                        j.res$within = j.res$type1 == j.res$type2
                        rs.list[[length(rs.list) + 1]] <- j.res
                        rm(list = ls()[grepl(pattern = "^j\\.", x = ls())])
                }
        }   
}

rs.list <- rbindlist(rs.list)
library(ggplot2)
ggplot(rs.list, aes(x=typology_system, y = turnover, fill = within)) + 
        geom_boxplot()
saveRDS(rs.list, "01_data/160_turnover.rds")
                    