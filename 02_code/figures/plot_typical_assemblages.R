## 

# setup -----------------------------------------------------------------------------
pacman::p_load(ggplot2, data.table, magrittr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/180_turnover_between_TAs.rds")

# prepare data ----------------------------------------------------------------------
data %<>% lapply(c)
n <- lapply(data, length)
data2 <- 
        data.table(
                typology_system = rep(names(n), 
                                      times = unlist(n)
                                      ),
                distance        = unlist(data)
        )

# plot ------------------------------------------------------------------------------
ggplot(data2, 
       aes(x=typology_system, 
           y=distance)) + 
        geom_violin(aes(fill = typology_system), draw_quantiles = .5) + 
        geom_jitter(width=.1, alpha = .9, shape = 21) + 
        scale_fill_brewer(palette = "Set2") + 
        theme(
                legend.position = "none", 
                panel.background = element_blank(), 
                panel.grid.major.y = element_line(color="gray"),
                axis.title.x = element_blank(), 
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_text(size = 13, 
                                            margin = margin(t = 0, 
                                                            r = 20,
                                                            b = 0, 
                                                            l = 0),
                ),
                axis.text.x = element_text(color = "black", size = 13)
        ) + 
        ylab("Dissimilarity between Typical Assemblages")

# save to file ----------------------------------------------------------------------
ggsave(filename = "03_figures/dissimilarity_TA.png")
