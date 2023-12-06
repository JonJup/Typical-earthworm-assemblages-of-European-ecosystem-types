# ------------------------------------------ #
# ---- TA COMMUNITY ANALYSIS: FULL DATA ---- #
# ------------------------------------------ #

## Overview
# This script contains the analysis of the full spider dataset using beta diversity measures

# ------------------------------------------------------------------------------------------------------ #
#### SETUP ####

## Packages
pacman::p_load(
    tidyverse,
    BAT,
    broom,
    ggpubr
)

## Load full data
(rds_names <- list.files(file.path(here::here(), "data", "03_assemblages"), pattern = "\\.rds",
                         full.names = TRUE))

full <- readRDS(rds_names[length(rds_names)])

## Load typical assemblages
typical <- readRDS(file.path(here::here(), "data", "05_ta_comparison",
                             paste0(Sys.Date(), "_ta_comparison_full.rds")))

## Load cross table
ct <- read_csv(file.path(here::here(), "results_csv", "crosstable_all.csv"))
ct <- rename(ct, group = habitat_type)


# ------------------------------------------------------------------------------------------------------ #
#### DATA PREPARATION ####

# HLZ-CLC
hlz_clc <- full %>%
    
    # Filter for relevant rows and select columns
    filter(grepl("2|3", analysis_type)) %>%
    select(species, hlz, habitat, abundance) %>%
    
    # Add occurrence column
    mutate(occurrence = ifelse(abundance != 0, 1, 0)) %>%
    select(-abundance) %>%
    
    # Compute sum per groups
    mutate(species = gsub(" ", "_", species)) %>%
    group_by(hlz, habitat, species) %>%
    summarise(count = sum(occurrence), .groups = "drop") %>%
    
    # Add group column
    mutate(typology = "HLZ-CLC") %>%
    rename(climate = hlz, land_cover = habitat) %>%
    relocate(typology, climate, land_cover, species, count)

# HLZ-EUNIS
hlz_eun <- full %>%
    
    # Filter for relevant rows and select columns
    filter(grepl("2|3", analysis_type)) %>%
    select(species, hlz, eunis_code, abundance) %>%
    
    # Add occurrence column
    mutate(occurrence = ifelse(abundance != 0, 1, 0)) %>%
    select(-abundance) %>%
    
    # Prepare EUNIS column
    mutate(eunis_code = substr(eunis_code, 1, 1)) %>%
    
    # Compute sum per groups
    mutate(species = gsub(" ", "_", species)) %>%
    group_by(hlz, eunis_code, species) %>%
    summarise(count = sum(occurrence), .groups = "drop") %>%
    
    # Add group column
    mutate(typology = "HLZ-EUNIS") %>%
    rename(climate = hlz, land_cover = eunis_code) %>%
    relocate(typology, climate, land_cover, species, count)

# BGR-CLC
bgr_clc <- full %>%
    
    # Filter for relevant rows and select columns
    filter(grepl("2|3", analysis_type)) %>%
    select(species, bgr, habitat, abundance) %>%
    
    # Add occurrence column
    mutate(occurrence = ifelse(abundance != 0, 1, 0)) %>%
    select(-abundance) %>%
    
    # Compute sum per groups
    mutate(species = gsub(" ", "_", species)) %>%
    group_by(bgr, habitat, species) %>%
    summarise(count = sum(occurrence), .groups = "drop") %>%
    
    # Add group column
    mutate(typology = "BGR-CLC") %>%
    rename(climate = bgr, land_cover = habitat) %>%
    relocate(typology, climate, land_cover, species, count)

# BGR-EUNIS
bgr_eun <- full %>%
    
    # Filter for relevant rows and select columns
    filter(grepl("2|3", analysis_type)) %>%
    select(species, bgr, eunis_code, abundance) %>%
    
    # Add occurrence column
    mutate(occurrence = ifelse(abundance != 0, 1, 0)) %>%
    select(-abundance) %>%
    
    # Prepare EUNIS column
    mutate(eunis_code = substr(eunis_code, 1, 1)) %>%
    
    # Compute sum per groups
    mutate(species = gsub(" ", "_", species)) %>%
    group_by(bgr, eunis_code, species) %>%
    summarise(count = sum(occurrence), .groups = "drop") %>%
    
    # Add group column
    mutate(typology = "BGR-EUNIS") %>%
    rename(climate = bgr, land_cover = eunis_code) %>%
    relocate(typology, climate, land_cover, species, count)

# Bind into one data frame
full <- bind_rows(hlz_clc, hlz_eun, bgr_clc, bgr_eun)


# ---- HLZ-CLC -----------------------------------------------------------------------------------------
# Community matrix
prep1 <- hlz_clc %>%
    
    # Filter for relevant typology
    filter(grepl("HLZ-CLC", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology)

sites <- prep1$group
comm1 <- data.frame(prep1[, -1])
rownames(comm1) <- sites

# Beta diversity components
beta_dist1 <- beta(comm1, abund = FALSE)

# Prepare for saving
b_tot1 <- tidy(beta_dist1$Btotal) %>%
    pivot_wider(item1, names_from = item2, values_from = distance) %>%
    rename(b_total = item1)

b_rich1 <- tidy(beta_dist1$Brich) %>%
    pivot_wider(item1, names_from = item2, values_from = distance) %>%
    rename(b_rich = item1)

b_repl1 <- tidy(beta_dist1$Brepl) %>%
    pivot_wider(item1, names_from = item2, values_from = distance) %>%
    rename(b_repl = item1)

# Average beta
beta_multi1 <- beta.multi(comm1, abund = FALSE)

# ---- HLZ-EUNIS ---------------------------------------------------------------------------------------
# Beta diversity
# Community matrix
prep2 <- full %>%
    
    # Filter for relevant typology
    filter(grepl("HLZ-EUNIS", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology)

sites <- prep2$group
comm2 <- data.frame(prep2[, -1])
rownames(comm2) <- sites

# Beta diversity components
beta_dist2 <- beta(comm2, abund = FALSE)

# Average beta
beta_multi2 <- beta.multi(comm2, abund = FALSE)

# ---- BGR-CLC -----------------------------------------------------------------------------------------
# Beta diversity
# Community matrix
prep3 <- full %>%
    
    # Filter for relevant typology
    filter(grepl("BGR-CLC", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology)

# Average beta
beta_multi1 <- beta.multi(comm1, abund = FALSE)sites <- prep3$group
comm3 <- data.frame(prep3[, -1])
rownames(comm3) <- sites

# Beta diversity components
beta_dist3 <- beta(comm3, abund = FALSE)

# Average beta
beta_multi3 <- beta.multi(comm3, abund = FALSE)

# ---- BGR-EUNIS ---------------------------------------------------------------------------------------
# Beta diversity
# Community matrix
prep4 <- full %>%
    
    # Filter for relevant typology
    filter(grepl("BGR-EUNIS", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology)

sites <- prep4$group
comm4 <- data.frame(prep4[, -1])
rownames(comm4) <- sites

# Beta diversity components
beta_dist4 <- beta(comm4, abund = FALSE)

# Average beta
beta_multi4 <- beta.multi(comm4, abund = FALSE)


# ------------------------------------------------------------------------------------------------------ #
#### TA: COMPARISON ####

## Prepare cross table
ct_prep <- select(ct, typology, habitat_type, count)

## Prepare TA data
ta_prep <- typical %>%
    
    # Convert typology name to upper case
    mutate(typology = toupper(gsub("_", "-", typology))) %>%
    
    # Add count of lists per habitat type
    left_join(., ct_prep, by = c("typology", "habitat_type")) %>%
    
    # Split habitat_type column
    separate(habitat_type, into = c("climate", "land_cover"), sep = "_") %>%
    
    # Keep B_val >= 0.25 and count >= 10
    filter(B_val > 0.25, count >= 10) %>%
    select(-count, -B_val)


# ---- TA: HLZ-CLC ---------------------------------------------------------------------------------------

## Beta diversity
# Community matrix
prep1 <- ta_prep %>%
    
    # Filter for relevant typology
    filter(grepl("HLZ-CLC", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    mutate(count = 1) %>%
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology) %>%
    
    # Change group column so it can be used as label
    mutate(group = gsub("_", ",\n", group))

sites <- prep1$group
comm1 <- data.frame(prep1[, -1])
rownames(comm1) <- sites

## Overall beta diversity
beta.multi(comm1, abund = FALSE)

# Beta diversity components
beta_dist1 <- beta(comm1, abund = FALSE)

# ---- TA: HLZ-EUNIS -------------------------------------------------------------------------------------

## Beta diversity
# Community matrix
prep1 <- ta_prep %>%
    
    # Filter for relevant typology
    filter(grepl("HLZ-EUNIS", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    mutate(count = 1) %>%
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology) %>%
    
    # Change group column so it can be used as label
    mutate(group = gsub("_", ",\n", group))

sites <- prep1$group
comm1 <- data.frame(prep1[, -1])
rownames(comm1) <- sites

## Overall beta diversity
beta.multi(comm1, abund = FALSE)

# Beta diversity components
beta_dist1 <- beta(comm1, abund = FALSE)

# ---- TA: BGR-CLC ---------------------------------------------------------------------------------------

## Beta diversity
# Community matrix
prep1 <- ta_prep %>%
    
    # Filter for relevant typology
    filter(grepl("BGR-CLC", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    mutate(count = 1) %>%
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology) %>%
    
    # Change group column so it can be used as label
    mutate(group = gsub("_", ",\n", group))

sites <- prep1$group
comm1 <- data.frame(prep1[, -1])
rownames(comm1) <- sites

## Overall beta diversity
beta.multi(comm1, abund = FALSE)

# Beta diversity components
beta_dist1 <- beta(comm1, abund = FALSE)

# ---- TA: HLZ-CLC ---------------------------------------------------------------------------------------

## Beta diversity
# Community matrix
prep1 <- ta_prep %>%
    
    # Filter for relevant typology
    filter(grepl("BGR-EUNIS", typology)) %>%
    
    # Add grouping column
    mutate(group = paste0(word(climate, 1, 1), "_", word(land_cover, 1, 1))) %>%
    select(-climate, -land_cover) %>%
    
    # From long to wide
    mutate(count = 1) %>%
    pivot_wider(c(typology, group), names_from = "species", values_from = count, values_fill = 0) %>%
    
    # Remove typology column
    select(-typology) %>%
    
    # Change group column so it can be used as label
    mutate(group = gsub("_", ",\n", group))

sites <- prep1$group
comm1 <- data.frame(prep1[, -1])
rownames(comm1) <- sites

## Overall beta diversity
beta.multi(comm1, abund = FALSE)

# Beta diversity components
beta_dist1 <- beta(comm1, abund = FALSE)