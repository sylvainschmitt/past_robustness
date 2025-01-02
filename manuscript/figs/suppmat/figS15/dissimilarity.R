

dissimilarity <- ggplot() +
  geom_ribbon(data = past_climdiss %>% dplyr::filter(clim_hpv_sorensen.year %in% (8000:12000)), aes(x = clim_hpv_sorensen.year, 
                                        ymin = 1-clim_hpv_sorensen.q2.5, ymax = 1-clim_hpv_sorensen.q97.5), 
              fill = "#6867ac", alpha = 0.2) + 
  geom_line(data = past_climdiss %>% dplyr::filter(clim_hpv_sorensen.year %in% (8000:12000)),
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "#6867ac") +
  coord_cartesian(xlim = c(12250, 8000), 
                  ylim =  c(-0.4, 0.60),
                  clip = "off") +
  scale_x_reverse(breaks = c(seq(12000,8000, -500)),
                  labels = c(seq(12000,8000, -500))/1000,
                  expand = c(0, 0),
                  name = "kyrs (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = c(0.1, 0.6, 1-past_climdiss[past_climdiss$clim_hpv_sorensen.year %in% c(seq(12000,11250,-250)), "clim_hpv_sorensen.mean"]),
                     labels = c("", "", round(1-past_climdiss[past_climdiss$clim_hpv_sorensen.year %in% c(seq(12000,11250,-250)), "clim_hpv_sorensen.mean"],2)),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 9, margin = margin(r = 4.5), hjust = 0.9),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.position="none", legend.title=element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(t = 5.5, b = 5.5, r = 15.5, l = 5.5)) +
  geom_segment(aes(x = 12250, xend = 12250, y= 0.1, yend = 0.6), color = "black", linewidth = 0.5) + 
  geom_segment(aes(x = 12000, xend = 8000, y= -0.4, yend = -0.4), color = "black", linewidth = 0.5) +
  
  geom_segment(aes(x = 12000, xend = 12000, y= 0.05, yend = 1-past_climdiss[past_climdiss$clim_hpv_sorensen.year == 12000, "clim_hpv_sorensen.mean"]), 
               color = "grey70", linewidth = 0.3, linetype = "dashed") +
  geom_segment(aes(x = 12000-250, xend = 12000-250, y= 0.05-0.1, yend = 1-past_climdiss[past_climdiss$clim_hpv_sorensen.year == 11750, "clim_hpv_sorensen.mean"]), 
               color = "grey70", linewidth = 0.3, linetype = "dashed") +
  geom_segment(aes(x = 12000-500, xend = 12000-500, y= 0.05-0.2, yend = 1-past_climdiss[past_climdiss$clim_hpv_sorensen.year == 11500, "clim_hpv_sorensen.mean"]), 
               color = "grey70", linewidth = 0.3, linetype = "dashed") +
  geom_segment(aes(x = 12000-750, xend = 12000-750, y= 0.05-0.3, yend = 1-past_climdiss[past_climdiss$clim_hpv_sorensen.year == 11250, "clim_hpv_sorensen.mean"]), 
               color = "grey70", linewidth = 0.3, linetype = "dashed") +
  geom_segment(aes(x = 12000-1000, xend = 12000-1000, y= 0.05-0.4, yend = 1-past_climdiss[past_climdiss$clim_hpv_sorensen.year == 11250, "clim_hpv_sorensen.mean"]), 
               color = "grey70", linewidth = 0.3, linetype = "dashed") +
  
  # simulations from 12k
  geom_segment(aes(x = 12010, xend = 8250, y= 0.05, yend = 0.05), color = "grey20", linewidth = 0.5) +
  geom_segment(aes(x = 8250, xend = 7950, y= 0.05, yend = 0.05), color = "grey20", linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03, x = 9500, label = "Sim. from 12k BP"), size = 8*5/14, family= "Helvetica Narrow") +
  geom_text(aes(y = 0.05, x = 12250, label = "A."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010, y = 0.05), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 3) +
  # simulations from 11.75k
  geom_segment(aes(x = 12010-250, xend = 8250, y= 0.05-0.1, yend = 0.05-0.1), color = "grey20", linewidth = 0.5) +
  geom_segment(aes(x = 8250, xend = 7950, y= 0.05-0.1, yend = 0.05-0.1), color = "grey20", linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.1, x = 9500, label = "Sim. from 11.75k BP"), size = 8*5/14, family= "Helvetica Narrow") +
  geom_text(aes(y = 0.05-0.1, x = 12250-250, label = "B."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-250, y = 0.05-0.1), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 3) +
  # simulations from 11.5k
  geom_segment(aes(x = 12010-500, xend = 8250, y= 0.05-0.2, yend = 0.05-0.2), color = "grey20", linewidth = 0.5) +
  geom_segment(aes(x = 8250, xend = 7950, y= 0.05-0.2, yend = 0.05-0.2), color = "grey20", linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.2, x = 9500, label = "Sim. from 11.5k BP"), size = 8*5/14, family= "Helvetica Narrow") +
  geom_text(aes(y = 0.05-0.2, x = 12250-500, label = "C."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-500, y = 0.05-0.2), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 3) +
  # simulations from 11.25k
  geom_segment(aes(x = 12010-750, xend = 8250, y= 0.05-0.3, yend = 0.05-0.3), color = "grey20", linewidth = 0.5) +
  geom_segment(aes(x = 8250, xend = 7950, y= 0.05-0.3, yend = 0.05-0.3), color = "grey20", linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.3, x = 9500, label = "Sim. from 11.25k BP"), size = 8*5/14, family= "Helvetica Narrow") +
  geom_text(aes(y = 0.05-0.3, x = 12250-750, label = "D."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-750, y = 0.05-0.3), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 3) +
  # simulations from 11k
  geom_segment(aes(x = 12010-1000, xend = 8250, y= 0.05-0.4, yend = 0.05-0.4), color = "grey20", linewidth = 0.5) +
  geom_segment(aes(x = 8250, xend = 7950, y= 0.05-0.4, yend = 0.05-0.4), color = "grey20", linewidth = 0.5, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.4, x = 9500, label = "Sim. from 11k BP"), size = 8*5/14, family= "Helvetica Narrow") +
  geom_text(aes(y = 0.05-0.4, x = 12250-1000, label = "E."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-1000, y = 0.05-0.4), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 3)
  
