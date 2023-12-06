# typical assemblages 

pacman::p_load(indicspecies, data.table, dplyr, sf, DoE.base)


# load data -------------------------------------------------------------------------
data <- readRDS("01_data/13_no_rare_types.rds")
data <- data[!is.na(species)]
data[, occurrence := 1]
# prepare data ----------------------------------------------------------------------
data2 <- dcast(data, formula = site_id ~ species, value.var = "occurrence", fun.aggregate = sum)
data3 <- data[,c("site_id", "bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc")]
data3 <- unique(data3, by = "site_id")
data4 <- left_join(data3, data2, by = "site_id")


## loop over typology systems 
cut_off_typical <- 0.25

res_lst <- list()

for (typology in c("bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc")){
        ty.dat1 <-
                data4[, lapply(.SD, sum), by = typology, .SDcols = 6:ncol(data4)]
        ty.dat2 <-
                table(data4[[typology]]) %>%
                data.frame() 
        names(ty.dat2)[1] <- typology
        ty.dat3 <-
                ty.dat1 %>%
                left_join(ty.dat2, by = typology)
        ty.dat4 <-
               ty.dat3[, lapply(.SD, function(x) x / Freq), .SDcols = 2:(ncol(ty.dat3) - 1)]
        ## loop over types
        for (i in 1:nrow(ty.dat2)) {
                i.id <- which(ty.dat4[i,] > cut_off_typical)
                i.typical <- names(ty.dat4)[i.id]
                typical <- data.table(
                        typology_system = typology,
                        type = ty.dat2[[typology]][i],
                        typical_taxa = i.typical
                )
                res_lst[[length(res_lst) + 1]] <- typical
        }
        
}
res_lst <- rbindlist(res_lst)

# res_lst2 <- list()
# ## check similarity
# for (i in c("bgr_eunis", "bgr_clc", "hlz_eunis", "hlz_clc")) {
#         i.res_lst   <-  res_lst[typology_system == i]
#         i.types     <- unique(i.res_lst$type)
#         i.factorial <-
#                 setDT(fac.design(
#                         nlevels = c(length(i.types), length(i.types)),
#                         randomize = F
#                 ))
#         ## drop rows with equal factor levels
#         i.factorial <- i.factorial[A != B]
#         i.factorial[, A := i.types[A]]
#         i.factorial[, B := i.types[B]]
#         names(i.factorial) <- c("type1", "type2")
#         i.factorial[, typology_system := i]
#         i.factorial[, c("total", "shared", "only1", "only2") := 0]
#         for (k in 1:nrow(i.factorial)) {
#                 ## extract typical taxa
#                 k.1 <-
#                         res_lst[typology_system == i &
#                                         type == i.factorial$type1[k], typical_taxa]
#                 k.2 <-
#                         res_lst[typology_system == i &
#                                         type == i.factorial$type2[k], typical_taxa]
#                 
#                 ## combine
#                 k.comb <- append(k.1, k.2) %>% unique()
#                 ## which are in type1
#                 k.B <- k.comb %in% k.1
#                 ## which are in type2
#                 k.C <- k.comb %in% k.2
#                 
#                 k.A <- sum(k.B & k.C)
#                 
#                 k.D <-
#                         (k.comb %in% k.1) &
#                         (!k.comb %in% k.2)
#                 k.D <- sum(k.D)
#                 k.E <-
#                         (!k.comb %in% k.1) &
#                         (k.comb %in% k.2)
#                 k.E <- sum(k.E)
#                 i.factorial$total[k]  <- length(k.comb)
#                 i.factorial$shared[k] <- k.A
#                 i.factorial$only1[k]  <- k.D
#                 i.factorial$only2[k]  <- k.E
#                 
#                 rm(list = ls()[grepl(pattern = "^k\\.", x = ls())])
#                 rm(list = ls()[grepl(pattern = "^K\\.", x = ls())])
#         }
#         res_lst2[[length(res_lst2) + 1]] <-
#                 i.factorial
#         rm(list = ls()[grepl(pattern = "^i\\.", x = ls())])
# }



# res_lst2 <- rbindlist(res_lst2)
# res_lst2[, jaccard_similarity := shared / (total)]
# res_lst2[, jaccard_dissimilarity := 1 - jaccard_similarity]


# save to file ----------------------------------------------------------------------
#saveRDS(res_lst2, "01_data/15_similarity_typical_assemblages.rds")
saveRDS(res_lst, "01_data/15_typical_assemblages.rds")

rm(list=ls());gc()
