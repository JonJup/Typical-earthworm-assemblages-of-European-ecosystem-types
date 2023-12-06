prep_distances <- function(x, threshold = 1){
        distances <- st_distance(x) %>% 
                units::drop_units() %>%
                c()
        ns <- nrow(x)
        id1 <- rep(1:ns, each=ns)
        id2 <- rep(1:ns, times=ns)
        rm(ns)
        
        distance.table <- data.table(id1, id2, distances)
        distance.table <- distance.table[distances < threshold & id1 != id2]
        
        # - add new id columns to remove duplicates 
        distance.table[, c("id3", "id4") := .(ifelse(id1>id2, id2, id1), ifelse(id1<id2, id2, id1))]
        distance.table[, c("id1", "id2"):= NULL]
        distance.table %<>% unique(by=c("distances", "id4", "id3"))
        distance.table
}
