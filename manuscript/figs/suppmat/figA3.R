
burke_climatenovelty <- readRDS("C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/burke_climatenovelty_CRUbaseline.rds")
burke_climatenovelty[burke_climatenovelty$year == 15, "year"] <- 0

hypervolume_past_plot <- ggplot() +
  geom_ribbon(data = past_climdiss, aes(x = clim_hpv_sorensen.year, 
                                        ymin = 1-clim_hpv_sorensen.q2.5, ymax = 1-clim_hpv_sorensen.q97.5), 
              fill = "#6867ac", alpha = 0.2) + 
  geom_line(data = past_climdiss,
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "#6867ac") +
  coord_cartesian(xlim = c(12000, 1000), 
                  ylim =  c(0, 0.60),
                  clip = "on") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "Years (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,0.6, 0.1),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        legend.position="none", legend.title=element_blank(),
        axis.line.x = element_blank(), axis.ticks.x = element_blank(),
        plot.margin = margin(t = 5.5, b = 5.5, r = 9, l = 5.5))

burke_past_plot <- ggplot() +
  geom_ribbon(data = burke_climatenovelty, 
              aes(x = year, ymin = q25, ymax = q75), 
              fill = "#89ac67", alpha = 0.2) +
  geom_line(data = burke_climatenovelty,
            aes(x = year, y = mean), col = "#89ac67") +
  coord_cartesian(xlim = c(12000, 1000), 
                  ylim =  c(0.75, 2.5),
                  clip = "on") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "Years (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0.75,2.5, 0.25),
                     name = "Climatic distance") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5)),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(t = 4.5)),
        legend.position="none", legend.title=element_blank(),
        plot.margin = margin(t = 5.5, b = 5.5, r = 9, l = 5.5))

figA3_main <- plot_grid(hypervolume_past_plot, burke_past_plot, ncol = 1, 
                        labels = c("a", "b"), label_fontfamily = "Helvetica Narrow", label_size = 11,
                        align = "hv", axis = "tblr")



