## 

# setup -----------------------------------------------------------------------------
pacman::p_load(data.table, ggplot2, wesanderson, vegan)

# load data -------------------------------------------------------------------------
data <- readRDS("01_data/14_permanova.rds")

# compute PERMANOVA AIC -------------------------------------------------------------

# prepare data ----------------------------------------------------------------------
res.c <- vector(mode = "numeric", length = 8)
for (i in 1:4){
        if (i == 1) counter = 1
        res.c[counter] <- data[[i]]$R2[1] 
        counter <- counter + 1
        res.c[counter] <- data[[i]]$R2[2]
        counter <- counter + 1
}
rm(counter, i);gc()

res.dat <- data.table(
        typology_system = rep(c("HLZ-CLC", "HLZ-EUNIS", "BGR-CLC", "BGR-EUNIS"), each = 2), 
        sub_system      = c("HLZ", "CLC", "HLZ", "EUNIS", "BGR", "CLC", "BGR", "EUNIS"), 
        r2              = res.c
)

rm(res.c)

res.dat[, sub_system := factor(sub_system, level = c("EUNIS", "CLC", "HLZ", "BGR"))]

#- average lead of BGR 
res.dat[sub_system == "BGR", mean(r2)] - res.dat[sub_system == "HLZ", mean(r2)]
res.dat[sub_system == "EUNIS", mean(r2)] - res.dat[sub_system == "CLC", mean(r2)]

# plot ------------------------------------------------------------------------------

palette <- wes_palette(
        name = "Darjeeling2",
        n = 4
)

ggplot(res.dat,
       aes(x = typology_system,
           y = r2,
           fill = sub_system)) +
        geom_col(col="black")+
        scale_fill_manual(values = palette) + 
        theme(
                panel.background = element_blank(), 
                panel.grid.major.y = element_line(color="gray"),
                axis.title.x = element_blank(), 
                axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.x = element_text(color = "black", size = 10),
                axis.title.y = element_text(size = 13, 
                                            margin = margin(t = 0, 
                                                            r = 20,
                                                            b = 0, 
                                                            l = 0)),
                legend.title = element_blank(),
                legend.text = element_text(color = "black", size = 12)
        ) + 
        ylab(parse(text="PERMANOVA~~R^{2}"))
        
# save to file ----------------------------------------------------------------------
ggsave("03_figures/PERMANOVA.png", width = 150, height = 100, units = "mm")

