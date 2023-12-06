## figure: how many species per TA
pacman::p_load(data.table,ggplot2,ggdist)
data <- readRDS("01_data/15_typical_assemblages.rds")

data[, count := uniqueN(typical_taxa), by = "type"]
data <- unique(data, by = c("typology_system", "type"))
data[typology_system == "bgr_clc", typology_system   := "BGR CLC"]
data[typology_system == "bgr_eunis", typology_system := "BGR EUNIS"]
data[typology_system == "hlz_clc", typology_system   := "HLZ CLC"]
data[typology_system == "hlz_eunis", typology_system := "HLZ EUNIS"]
data[, typology := factor(typology_system)]
data[, typology_num := as.numeric(typology)]


ggplot(data, aes(x = typology, y = count, fill = typology)) +
        #stat_slab(aes(thickness = after_stat(pdf * n)), scale = 0.7) +
        stat_dotsinterval(side = "both",
                          scale = 2,
                          slab_linewidth = NA,
                          binwidth=1,
                          dotsize = .2,
                          .width = 0,
                          fatten_point = 1) + 
        scale_fill_brewer(palette = "Set2") +
        theme(legend.position = "none", 
              panel.background = element_blank(), 
              panel.grid.major.y = element_line(color="gray"),
              axis.title.x = element_blank(), 
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_text(size = 13, 
                                          margin = margin(t = 0, 
                                                          r = 2,
                                                          b = 0, 
                                                          l = 0),
                                          ),
              axis.text.x = element_text(color = "black", size = 13)
              ) + 
        ylab("No. of speces in typical assemblage")
         
ggsave("03_figures/number_of_species_per_TA.png", dpi = 600, width = 5, height = 4)        
     