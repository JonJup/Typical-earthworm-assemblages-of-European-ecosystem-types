## Plot ANOSIM

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, ggplot2)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/14_anosim.rds")

# prepare data ----------------------------------------------------------------------
res.dat <- data.table(
        typology_system = rep(c("HLZ CLC", "HLZ EUNIS", "BGR CLC", "BGR EUNIS"), each = 1), 
        ANOSIM_R              = numeric(4),
        p.value               = numeric(4)
)
for(i in 1:4){
        res.dat$ANOSIM_R[i] <- data[[i]]$statistic
        res.dat$p.value[i] <- data[[i]]$signif
}


# plot ------------------------------------------------------------------------------
ggplot(
        res.dat, 
        aes(x = typology_system,
            y = ANOSIM_R,
            fill = typology_system)
) + 
        geom_col(col="black") + 
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
        ylab("ANOSIM R")


# save to file ----------------------------------------------------------------------
ggsave("03_figures/anosim.png")
