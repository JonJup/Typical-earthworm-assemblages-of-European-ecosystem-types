pacman::p_load(
        
        BAT,
        data.table,
        dplyr,
        magrittr,
        parallelDist,
        vegan,
        ggplot2
               )



# load data -------------------------------------------------------------------------
data <- readRDS("01_data/13_no_rare_types.rds")
source("02_code/functions/aic_permanova.R")
data$occurrence <- 1
# prepare data ----------------------------------------------------------------------

# make type names shorter for subsequent plotting
data[hlz == "cool temperate", hlz := "cool"]
data[hlz == "warm temperate", hlz := "warm"]
data[clc == "Open Land", clc := "Open"]
data[bgr == "Mediterranean", bgr := "Medi."]
data[bgr == "Continental", bgr := "Cont."]
data[bgr == "Atlantic", bgr := "Atlan."]


data[,hlz_clc   := paste(hlz,clc, sep="_")]
data[,hlz_eunis := paste(hlz,eunis_code, sep="_")]
data[,bgr_clc   := paste(bgr,clc, sep="_")]
data[,bgr_eunis := paste(bgr,eunis_code, sep="_")]


# - pivot wide 
data_wide <- dcast(data, formula = site_id ~ lowest_taxon, value.var = "occurrence", fun.aggregate = sum)
data_wide %<>% arrange(site_id)
data_wide_matrix <- as.matrix(data_wide[,-1])
data_dist <- parallelDist(data_wide_matrix, method = "binary", threads = 3)
#data_turnover <- beta(data_wide_matrix, comp = T)

# - table with types 
sites <- unique(data, by = "site_id")
sites %<>% arrange(site_id)

# run betadisper --------------------------------------------------------------------
bd_hc <- betadisper(d = data_dist, group = sites$hlz_clc)
bd_he <- betadisper(d = data_dist, group = sites$hlz_eunis)
bd_bc <- betadisper(d = data_dist, group = sites$bgr_clc)
bd_be <- betadisper(d = data_dist, group = sites$bgr_eunis)

bd_anova <- lapply(list(bd_hc,bd_he,bd_bc, bd_be), anova)

jpeg(filename="03_figures/permdisp/permdisp_hc.jpg", width = 280, height = 200, units="mm", res=400)
par(mfrow=c(1,2))
plot(bd_hc,main="");boxplot(bd_hc,horizontal=T,cex.axis = .8, xlab="HLZ-CLC")
dev.off()

jpeg(filename="03_figures/permdisp/permdisp_he.jpg", width = 280, height = 200, units="mm", res=400)
par(mfrow=c(1,2))
plot(bd_he,main="");boxplot(bd_he,horizontal=T,cex.axis = .8, xlab="HLZ-EUNIS")
dev.off()

jpeg(filename="03_figures/permdisp/permdisp_bc.jpg", width = 280, height = 200, units="mm", res=400)
par(mfrow=c(1,2))
plot(bd_bc,main="");boxplot(bd_bc,horizontal=T,cex.axis = .5, xlab="BGR-CLC")
dev.off()

jpeg(filename="03_figures/permdisp/permdisp_be.jpg", width = 280, height = 200, units="mm", res=400)
par(mfrow=c(1,2))
plot(bd_be,main="");boxplot(bd_be,horizontal=T,cex.axis = .5, xlab="BGR-EUNIS")
dev.off()

# run permanova ---------------------------------------------------------------------
ad.h.c <- adonis2(data_dist ~ sites$hlz + sites$clc)
ad.h.e <- adonis2(data_dist ~ sites$hlz + sites$eunis_code)
ad.b.c <- adonis2(data_dist ~ sites$bgr + sites$clc)
ad.b.e <- adonis2(data_dist ~ sites$bgr + sites$eunis_code)

# #- compute AIC 
ad.h.c.aic <- AICc.PERMANOVA2(ad.h.c)$AIC
ad.h.e.aic <- AICc.PERMANOVA2(ad.h.e)$AIC
ad.b.c.aic <- AICc.PERMANOVA2(ad.b.c)$AIC
ad.b.e.aic <- AICc.PERMANOVA2(ad.b.e)$AIC

# run anosim ------------------------------------------------------------------------
an.h.c <- anosim(x = data_dist, grouping = sites$hlz_clc)
an.h.e <- anosim(x = data_dist, grouping = sites$hlz_eunis)
an.b.c <- anosim(x = data_dist, grouping = sites$bgr_clc)
an.b.e <- anosim(x = data_dist, grouping = sites$bgr_eunis)

# run NMDS --------------------------------------------------------------------------
# nmds.h.c <- metaMDS(comm = data_dist, trymax = 20, k = 2, parllel = 6)
# 
# nmds.added <- bind_cols(sites, data.table(x = nmds.h.c$points[,1], 
#                             y = nmds.h.c$points[,2]))
# 
# nmds.added %>%
#         filter(x < 0.001 & y < 0.002) -> no.outliers 
# 
# data2 <- data2[site_id %in% no.outliers$site_id]
# data_wide2 <- dcast(data2, formula = site_id ~ lowest_taxon, value.var = "occurrence", fun.aggregate = sum)
# data_wide2 %<>% arrange(site_id)
# data_wide_matrix2 <- as.matrix(data_wide2[,-1])
# data_dist2 <- parallelDist(data_wide_matrix2, method = "binary", threads = 3)
# nmds.h.c2 <- metaMDS(comm = data_dist2, trymax = 20, k = 2, parllel = 6)     
# 
# nmds.added2 <- bind_cols(unique(data2, by = "site_id"),
#                          data.table(x = nmds.h.c2$points[,1], 
#                                           y = nmds.h.c2$points[,2]))
# nmds.added2 %>% ggplot(aes(x,y)) + geom_point(); beepr::beep()
# 
# filter.coords <- nmds.added2 %>% filter(x < 0.24 & x > -.025)
#         
# filter.coords %>% ggplot(aes(x,y)) + geom_point()
# 
# nmds.added2 %>% filter(x < 0.24 & x > -.025) -> no.outliers

# save to file ----------------------------------------------------------------------

# - results PERMDISPER
saveRDS(list(bd_hc, bd_he, bd_bc, bd_be), "01_data/14_permdisper.rds")
# - results PERMANOVA
saveRDS(list(ad.h.c, ad.h.e, ad.b.c, ad.b.e), "01_data/14_permanova.rds")
# - results AIC
#saveRDS(list(ad.h.c.aic, ad.h.e.aic, ad.b.c.aic, ad.b.e.aic), "01_data/14_permanova_aic.rds")
# - results ANOSIM 
saveRDS(list(an.h.c, an.h.e, an.b.c, an.b.e), "01_data/14_anosim.rds")
# - save results NMDS 
#saveRDS(nmds.h.c, "01_data/14_nmds.rds")

rm(list = ls())
