# ----------------------------------------------------------- #
# ---- Establish optimal distance for spatial filtering  ---- #
# --- Plot K - Statistic over different distances        ---- #
# ----------------------------------------------------------- #


# setup -----------------------------------------------------------------------------
pacman::p_load(ggplot2, dplyr, data.table, magrittr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/09_k_statistic.rds")
ref  <- readRDS("01_data/helper/101_habitat_evaluations.rds")

# prepare data ----------------------------------------------------------------------
data.l <- list(copy(data),
               copy(ref))

#--- add habitat ID to ref --#
data.l[[3]] <- unique(data.l[[1]], by = "habitat_id")
data.l %<>% lapply(function(x) x[, habitat := paste(bgr, eunis)])
data.l[[2]] %<>% (\(x) x[habitat %in% data.l[[3]]$habitat])()
data.l %<>% lapply(arrange, bgr, eunis)
data.l[[2]]$habitat_id <- data.l[[3]]$habitat_id

#-- prepare for plotting --# 
# - boring types - nothing happens they don't need to clutter the graph
#drop.id <- c(16,17,11,12,6)
#data.l %<>% lapply(mutate, habitat_id = factor(habitat_id))
#data.l[[1]] %<>% filter(!is.na(K_diff) & !habitat_id %in% drop.id)
data.l[[2]] %<>% filter(habitat_id %in% data.l[[1]]$habitat_id)

#-- identify point where Ripley's K is half the habitat correlation
data.l[[4]] <-
        left_join(data.l[[1]],
                  select(data.l[[2]], threshold = distance, habitat_id),
                  by = "habitat_id")
data.l[[4]]$below <- FALSE
data.l[[4]]$threshold2 <- 0.75 * data.l[[4]]$threshold
data.l[[4]][threshold2 > K_diff | K_diff <= 0, below := TRUE]

data.l[[4]][below == TRUE, mindist := min(distance), by = "habitat_id"]
#data.l[[4]] <- data.l[[4]][!habitat_id %in% c(25:27)]

#--- shorten EUNIS name for facet titles 
# data.l[[4]][, habitat := stringr::str_replace_all(habitat, "Forest Woodland, forest and other wooded land", "Forest")]
# data.l[[4]][, habitat := stringr::str_replace_all(habitat, "Grasslands and lands dominated by forbs, mosses or lichens", "Grassland")]
# data.l[[4]][, habitat := stringr::str_replace_all(habitat, "Regularly or recently cultivated agricultural, horticultural and domestic habitats", "Cultivated")]
# data.l[[4]][, habitat := stringr::str_replace_all(habitat, "Open Land", "Open")]
# data.l[[4]][, habitat := stringr::str_replace_all(habitat, "Woodland, forest and other wooded land", "Forest")]
# data.l[[4]][, habitat := stringr::str_replace_all(habitat, "cool temperate", "Cool")]
# data.l[[4]][, habitat := stringr::str_replace_all(habitat, "warm temperate", "Warm")]
# data.l[[2]][, habitat := stringr::str_replace_all(habitat, "Forest Woodland, forest and other wooded land", "Forest")]
# data.l[[2]][, habitat := stringr::str_replace_all(habitat, "Grasslands and lands dominated by forbs, mosses or lichens", "Grassland")]
# data.l[[2]][, habitat := stringr::str_replace_all(habitat, "Regularly or recently cultivated agricultural, horticultural and domestic habitats", "Cultivated")]
# data.l[[2]][, habitat := stringr::str_replace_all(habitat, "Open Land", "Open")]
# data.l[[2]][, habitat := stringr::str_replace_all(habitat, "Woodland, forest and other wooded land", "Forest")]
# data.l[[2]][, habitat := stringr::str_replace_all(habitat, "cool temperate", "Cool")]
# data.l[[2]][, habitat := stringr::str_replace_all(habitat, "warm temperate", "Warm")]

data.l[[4]][, habitat := factor(habitat)]

# plot ------------------------------------------------------------------------------

data.l[[4]] %>% 
        ggplot(aes(x=distance, y = K_diff, col = habitat, group = habitat)) + 
        geom_hline(yintercept = 0) + 
        geom_line(lwd = .5) + 
        geom_point(size = 3, aes(shape = below)) + 
        facet_wrap(.~habitat, scales = "free_y") + 
        geom_hline(data = data.l[[2]][!habitat_id %in% c(25:27)], aes(yintercept = distance), col = "red") + 
        theme(legend.position ="none",
             strip.background = element_blank(),
             strip.text.x = element_text(size=12),
             axis.title.y = element_text(size = 13),
             axis.title.x = element_text(size = 13),
             panel.grid = element_blank(), 
             axis.text.y = element_text(size=5)
             
             ) +
        xlab("Minimal nearest neighbor distance [km]") + 
        ylab(parse(text="K[samples]~~-~~K[CSR]"))
              # panel.background = element_blank(),
              #panel.grid.major.y = element_line(color="gray"))


#saveRDS(ripley_plot, "03_figures/exploratory/05_k_statistic_plot.rds")
ggsave("03_figures/k_statistic.png", width = 9.64, height = 6.49)

