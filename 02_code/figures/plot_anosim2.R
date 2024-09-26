## Plot ANOSIM2

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, ggplot2, dplyr)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/14_anosim.rds")

# prepare data ----------------------------------------------------------------------
out.list <- vector(mode = "list", length = 4 )
for (i in 1:4){
        x <- data[[i]]
        y <- data.table(what = x$class.vec, 
                        how = x$dis.rank, 
                        class = c("HLZ-CLC", "HLZ-EUNIS", "BGR-CLC", "BGR-EUNIS")[i]
        )
        out.list[[i]]<-y
}

out <- rbindlist(out.list)

# plot ------------------------------------------------------------------------------
out %>%
        filter(what != "Between") %>%
        ggplot(aes(x = what, y = how)) +
        geom_violin(draw_quantiles = 0.5) +
        facet_wrap(. ~ class, scales = "free") +
        theme(
                legend.position = "none",
                panel.background = element_blank(),
                panel.grid.major.y = element_line(color = "gray"),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_text(size = 13, margin = margin(
                        t = 0,
                        r = 20,
                        b = 0,
                        l = 0
                ), ),
                axis.text.x = element_text(
                        color = "black",
                        size = 5,
                        angle = 45
                ),
                axis.text.y = element_blank() 
                
                        
        ) +
        ylab("Dissimilarity Rank")


# save to file ----------------------------------------------------------------------
ggsave("03_figures/anosim2.png", width = 150, height = 100, units = "mm")
