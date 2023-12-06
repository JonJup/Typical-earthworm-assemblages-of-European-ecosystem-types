# --------------------------------------- #
# ---- COMMUNITY ANALYSIS: FULL DATA ---- #
# --------------------------------------- #

## Overview
# This script contains the analysis of the full spider dataset,
# including cross tables, PERMANOVA, ANOSIM, and NMDS

# ---------------------------------------------------------------------------- # 
#### SETUP #### 

## Packages
pacman::p_load(
    tidyverse,
    vegan,
    ggplot2,
    ggrepel,
    ggpubr,
    pairwiseAdonis)

## Load data - adapted threshold
(rds_names <- list.files(file.path(here::here(), "data", "03_assemblages"),
                         pattern = "\\.rds",
                         full.names = TRUE))

# Chose respective file
full <- readRDS(rds_names[length(rds_names)])

## Functions
# Path
func_path <- list.files(file.path("H:/GETREAL_dat/02_species/03_spec_data/02_spider", "functions"),
                        pattern = "\\.r",
                        full.names = TRUE)

# Load
source(func_path[1])


# ---------------------------------------------------------------------------- # 
#### PREPARATION #### 

## Set permutations
no_perms <- 999

## Full data
prep_full <- full %>%
    
    # Filter for relevant rows and select columns
    filter(grepl("2|3", analysis_type)) %>%
    filter(grepl("A|C|D|E|G|H|I", eunis_code)) %>%
    select(site_id, lon, lat, hlz, biotemp, habitat, bgr, eunis_code, species) %>%
    
    # Add occurrence
    mutate(occurrence = 1) %>%
    
    # # Add group column
    # mutate(group = paste0(word(hlz, 1, 1), "_", word(habitat, 1, 1))) %>%
    # relocate(site_id, group) %>%
    unique() %>%
    
    # From long to wide
    pivot_wider(names_from = species, values_from = occurrence, values_fill = 0)

## Prepare environmental data
prep_full %<>%
    
    # Only keep EUNIS level 1
    mutate(eunis_code = substr(eunis_code, 1, 1)) %>%
    
    # Add missing temperature for habitat types
    mutate(biotemp = ifelse(grepl("Cool", hlz), 9, biotemp))

## Count categories to see if data needs to be excluded
count(prep_full, hlz)
count(prep_full, bgr)
count(prep_full, habitat)
count(prep_full, eunis_code)
# Remove 'Polar' (HLZ), 'Pannonian' (BGR), 'D' (EUNIS)
prep_full <- filter(prep_full,
       !grepl("Polar", hlz), !grepl("Pannonian", bgr), !grepl("D", eunis_code))

## Subset data and create PERMANOVA factors
perm_env <- prep_full %>%
    mutate(hlz_clc = paste0(word(hlz, 1, 1), "_", word(habitat, 1, 1)),
           bgr_clc = paste0(bgr, "_", word(habitat, 1, 1)),
           hlz_eun = paste0(word(hlz, 1, 1), "_", eunis_code),
           bgr_eun = paste0(bgr, "_", eunis_code)) %>%
    select(site_id, hlz_clc, bgr_clc, hlz_eun, bgr_eun)

## Split prepared data into env_full variables and distance matrix on community
# composition (species data)
# 1. Environmental data
env_full <- select(prep_full, site_id, lon, lat, hlz, biotemp,
                   bgr, habitat, eunis_code)

# 2. Distance matrix
spec_dist <- prep_full %>%
    
    # Remove environmental information and keep species data
    select(-names(env_full)) %>%
    
    # Use vegdist() {vegan} to compute distance matrix
    vegdist(method = "jaccard")

# ---------------------------------------------------------------------------- # 
#### NMDS ####

## Set seed
set.seed(2304)

## NMDS
species_nmds <- prep_full %>%
    
    # Select relevant columns
    select(-names(env_full)) %>%
    
    # Compute NMDS
    metaMDS(., distance = "jaccard", try = 200) %>%
    
    # Extract site scores and add site_id for later merge
    scores("sites") %>%
    as_tibble() %>%
    mutate(site_id = env_full$site_id) %>%
    relocate(site_id)

## Join with environmental information
meta_nmds <- inner_join(env_full, species_nmds)

## Compute mean centroids for HLZ, BGR, CLC, EUNIS
centroid_hlz <- meta_nmds %>%
    group_by(hlz) %>%
    summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

centroid_bgr <- meta_nmds %>%
    group_by(bgr) %>%
    summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

centroid_eun <- meta_nmds %>%
    group_by(eunis_code) %>%
    summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

centroid_clc <- meta_nmds %>%
    group_by(habitat) %>%
    summarize(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2))

## Define palettes
pal_hlz <- wesanderson::wes_palette("Zissou1")[c(1, 3, 5)]
pal_bgr <- RColorBrewer::brewer.pal(6, "Set1")
pal_eun <- wesanderson::wes_palette("Darjeeling1")[c(5, 3, 2, 1)]
pal_clc <- wesanderson::wes_palette("Darjeeling1")[c(2, 4, 5)]

## Plot via ggplot2 for HLZ
(p_hlz <- meta_nmds %>%
        
        # Rename column for plotting
        rename(HLZ = hlz) %>%
        
        # Plot
        ggplot(aes(x = NMDS1, y = NMDS2)) +
        
        # Theme, labs, ...
        theme_bw() + 
        theme(axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              panel.background = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_blank()) +
        labs(x = "") +
        
        # Plot points with individual colors
        geom_point(aes(color = HLZ), size = 2) +
        scale_color_manual(name = "HLZ", values = pal_hlz) +
        coord_equal() +
        stat_ellipse(aes(color = HLZ)))
    

## Plot via ggplot2 for BGR
(p_bgr <- meta_nmds %>%
        
        # Rename column for plotting
        rename(BGR = bgr) %>%
        
        # Plot
        ggplot(aes(x = NMDS1, y = NMDS2)) +
        
        # Theme, labs, ...
        theme_bw() + 
        theme(axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              panel.background = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_blank()) +
        labs(x = "", y = "") +
        
        # Plot points with individual colors
        geom_point(aes(color = BGR), size = 2) +
        scale_color_manual(name = "BGR", values = pal_bgr) +
        coord_equal() +
        stat_ellipse(aes(color = BGR)))

## Plot via ggplot2 for EUNIS
(p_eun <- meta_nmds %>%
        
        # Rename column for plotting
        rename(EUNIS = eunis_code) %>%
        
        # Plot
        ggplot(aes(x = NMDS1, y = NMDS2)) +
        
        # Theme, labs, ...
        theme_bw() + 
        theme(axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              panel.background = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_blank()) +
        labs(y = "") +
        
        # Plot points with individual colors
        geom_point(aes(color = EUNIS), size = 2) +
        scale_color_manual(name = "EUNIS", values = pal_eun) +
        coord_equal() +
        stat_ellipse(aes(color = EUNIS)))

## Plot via ggplot2 for CLC
(p_clc <- meta_nmds %>%
        
        # Rename column for plotting
        rename(CLC = habitat) %>%
        
        # Plot
        ggplot(aes(x = NMDS1, y = NMDS2)) +
        
        # Theme, labs, ...
        theme_bw() + 
        theme(axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              panel.background = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_blank()) +
        
        # Plot points with individual colors
        geom_point(aes(color = CLC), size = 2) +
        scale_color_manual(name = "CLC", values = pal_clc) +
        coord_equal() +
        stat_ellipse(aes(color = CLC)))

## Arrange plots
plot <- ggarrange(p_hlz, p_bgr, p_clc, p_eun,
                  labels = c("A", "B", "C", "D"),
                  align = "v")

# Save plot
ggsave("20221124_nmds_all.png", plot, path = file.path(here::here(), "figures"),
       width = 10, height = 6)


# ---------------------------------------------------------------------------- # 
#### PERMANOVA: HLZ - CLC #### 

## PERMANOVA
perm1 <- adonis2(spec_dist ~ hlz + habitat,
                 data = env_full,
                 permutations = no_perms)

## Dispersion
disp1 <- betadisper(spec_dist, group = perm_env$hlz_clc)
anova(disp1)

## Adjusted p-values
pairwise.adonis2(spec_dist ~ hlz_clc, data = perm_env, nperm = no_perms)

# ---------------------------------------------------------------------------- # 
#### PERMANOVA: BGR - CLC #### 

## PERMANOVA
perm2 <- adonis2(spec_dist ~ bgr + habitat,
                 data = env_full,
                 permutations = no_perms)

## Dispersion
disp2 <- betadisper(spec_dist, group = perm_env$bgr_clc)
anova(disp2)

## Adjusted p-values
pairwise.adonis2(spec_dist ~ bgr_clc, data = perm_env, nperm = no_perms)

# ---------------------------------------------------------------------------- # 
#### PERMANOVA: HLZ - EUNIS #### 

## PERMANOVA
perm3 <- adonis2(spec_dist ~ hlz + eunis_code,
                 data = env_full,
                 permutations = no_perms)

## Dispersion
disp3 <- betadisper(spec_dist, group = perm_env$hlz_eun)
anova(disp3)

## Adjusted p-values
pairwise.adonis2(spec_dist ~ hlz_eun, data = perm_env, nperm = no_perms)

# ---------------------------------------------------------------------------- # 
#### PERMANOVA: BGR - EUNIS #### 

## PERMANOVA
perm4 <- adonis2(spec_dist ~ bgr + eunis_code,
                 data = env_full,
                 permutations = no_perms)

## Dispersion
disp4 <- betadisper(spec_dist, group = perm_env$bgr_eun)
anova(disp4)

## Adjusted p-values
pairwise.adonis2(spec_dist ~ eunis_code, data = env_full, nperm = no_perms)

# ---------------------------------------------------------------------------- # 
#### PERMANOVA: AIC ####

## Create list of models
mod_list <- list(perm1, perm2, perm3, perm4)

# Rename
names(mod_list) <- c("HLZ-CLC", "BGR-CLC", "HLZ-EUNIS", "BGR-EUNIS")

## Compute AICc
for (i in 1:length(mod_list)) {
    aicc <- AICc.PERMANOVA2(mod_list[[i]])
    print(paste("The AIC for", names(mod_list)[i], "is:", aicc$AIC))
}

# ---------------------------------------------------------------------------- #
#### ANOSIM #### 

## Compute ANOSIM
ano1 <- with(env_full, anosim(dist_full, hlz_hab))
ano2 <- with(env_full, anosim(dist_full, hlz_eun))
ano3 <- with(env_full, anosim(dist_full, bgr_hab))
ano4 <- with(env_full, anosim(dist_full, bgr_eun))

## Save ANOSIM results
anosim_list <- list(ano1, ano2, ano3, ano4)
saveRDS(anosim_list, file = file.path(here::here(), "results_rds", "ANOSIM_list.rds"))
