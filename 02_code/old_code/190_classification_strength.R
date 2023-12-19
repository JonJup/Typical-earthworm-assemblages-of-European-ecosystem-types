## 

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, dplyr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/160_turnover.rds")

# prepare data ----------------------------------------------------------------------

data1 <- copy(data)
data2 <- copy(data) 
data1 <- data1[,c("type", "type2", "type1") := .(type1, NULL,NULL)]
data2 <- data2[,c("type", "type2", "type1") := .(type2, NULL,NULL)]

data3 <- rbindlist(list(data1, data2))
rm(data1, data2)
# - compute cs for typology systems 
r.list <- vector(mode="list", length = uniqueN(data$typology_system))
for (i in unique(data$typology_system)){
        
        i.data1 <- copy(data3)
        i.data1 <- i.data1[typology_system == i]
        i.data2 <- i.data1[within==T, mean(turnover), by = "type"]
        i.data3 <- i.data1[within==F, mean(turnover), by = "type"]
        names(i.data2)[2] <- "within"
        names(i.data3)[2] <- "between"
        #- combine into one data.table 
        i.data4 <- i.data3[i.data2, on = "type"]
        
        #- CS is usually build upon similarities rather than dissimilarities (such as turnover).
        #- However, the absolute difference is the same. 
        #- Thus, we do not convert the dissimilarities into similarities.
        #- Instead we compute CS as beteen type dissimiliarty - within type dissimilarity
        i.data4[, cs := between - within]
        i.data4[, typology_system := i]
        
        
        r.list[[which(unique(data$typology_system) == i)]] <- 
                i.data4
        rm(list = ls()[grepl("^i\\.", x = ls())])
        rm(i)
}
r.data <- rbindlist(r.list)
r.data[, cs_overall := mean(cs), by = "typology_system"]

r.list2 <- vector(mode="list", length = uniqueN(data$typology_system))
for (i in unique(data$typology_system)){
        
        i.data1 <- copy(data3)
        i.data1 <- i.data1[typology_system == i]
        i.cs <- mean(i.data1[within == FALSE, turnover]) - mean(i.data1[within == TRUE, turnover])
        r.list2[[which(unique(data$typology_system) == i)]] <- 
                data.table(
                        typology_system = i,
                        cs              = i.cs
                )
        rm(list = ls()[grepl("^i\\.", x = ls())])
        rm(i)
}
r.data2 <- rbindlist(r.list2)

#- vegan 



# save to file ----------------------------------------------------------------------
saveRDS(r.data,"01_data/19_classification_strength_turnover.rds")
