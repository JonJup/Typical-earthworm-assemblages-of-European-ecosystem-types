# | ————————————————————— |
# | ——— Parent Script ——— |
# | ————————————————————— |

# This parent scirpt calls all the child scripts in sequence. 

source("02_code/010_prepare_sworm_data.R")
source("02_code/020_prepare_edaphobase.R")
source("02_code/030_combine_sworm_and_edaphobase.R")
source("02_code/050_add_variables_to_combined_data.R")
source("02_code/060_compare_against_drillo_base.R")
source("02_code/070_drop_poor_communities.R")
source("02_code/080_intersect_classification_systems.R")
source("02_code/090_spatial_aggregation_by_habitat.R")
source("02_code/100_how_dispersed_are_habitat_types_create_data.R")
source("02_code/101_how_dispersed_are_habitat_types_evaluation.R")
source("02_code/110_establish_thinning_distance.R")
source("02_code/120_enviro-spatial-thinning.R")
source("02_code/130_recheck_for_rare_types.R")
source("02_code/140_permanova.R")
source("02_code/150_determine_typical_assembalges.R")
source("02_code/180_turnover_between_TA.R")


# figures ---------------------------------------------------------------------------
source("02_code/figures/map of samples.R")
#source("02_code/figures/plot_anosim.R")
source("02_code/figures/plot_permanova.R")
#source("02_code/figures/plot_number_of_species_per_TA.R")
source("02_code/figures/K_statistic.R")
source("02_code/figures/plot_typical_assemblages.R")


#source("02_code/figures/plot_turnover.R")




data <- readRDS("01_data/13_no_rare_types.rds")
sitets <- unique(data, by = "site_id")
unique(data$species)
