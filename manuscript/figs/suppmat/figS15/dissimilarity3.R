

dissimilarity <- ggplot() +
  geom_ribbon(data = past_climdiss %>% dplyr::filter(clim_hpv_sorensen.year %in% (8250:12000)), aes(x = clim_hpv_sorensen.year, 
                                                                                                    ymin = 1-clim_hpv_sorensen.q2.5, ymax = 1-clim_hpv_sorensen.q97.5), 
              fill = "#6867ac", alpha = 0.2) + 
  geom_line(data = past_climdiss %>% dplyr::filter(clim_hpv_sorensen.year %in% (8250:12000)),
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "#6867ac") +
  geom_line(data = past_climdiss %>% dplyr::filter(clim_hpv_sorensen.year %in% (8000:8250)),
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "#6867ac", linetype = "dotted", linewidth = 0.4) +
  coord_cartesian(xlim = c(12250, 7750), 
                  ylim =  c(-0.4, 0.65),
                  clip = "off") +
  scale_x_reverse(breaks = c(seq(12000,8000, -500)),
                  labels = c(seq(12000,8000, -500))/1000,
                  expand = c(0, 0),
                  name = "kyrs (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = c(0.1, 0.65, round(1-past_climdiss[past_climdiss$clim_hpv_sorensen.year %in% c(seq(12000,11250,-250)), "clim_hpv_sorensen.mean"],2)),
                     labels = c("", "", round(1-past_climdiss[past_climdiss$clim_hpv_sorensen.year %in% c(seq(12000,11250,-250)), "clim_hpv_sorensen.mean"],2)),
                     name = "Climatic dissimilarity", 
                     sec.axis = sec_axis(transform=~.*1, name="", breaks = c(0.1,0.65, round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095, "median_ssp"]),2)), 
                                         labels = c("", "", round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095, "median_ssp"]),2)))) +
  
  # Add CMIP6 projections
  geom_text(aes(y = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp245", 
                                                         "median_ssp"]),2)+0.03, 
                x = 9500, label = "SSP2"), size = 7.5*5/14, family= "Helvetica Narrow", color = "#f69320") +
  geom_text(aes(y = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp585", 
                                                         "median_ssp"]),2)+0.03, 
                x = 9500, label = "SSP5"), size = 7.5*5/14, family= "Helvetica Narrow", color = "#bf1d1e") +
  geom_segment(aes(x = 7900, xend = 11550, 
                   y= round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp245", 
                                                           "median_ssp"]),2), 
                   yend = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp245", 
                                                               "median_ssp"]),2)), 
               color = "#f69320", linewidth = 0.4, linetype = "dashed") +
  
  geom_segment(aes(x = 8100, xend = 11750, y= round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp585", 
                                                                                   "median_ssp"]),2), 
                   yend = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp585", 
                                                               "median_ssp"]),2)), 
               color = "#bf1d1e", linewidth = 0.4, linetype = "dashed") +
  
  
  geom_segment(aes(x = 8100, xend = 8100, 
                   y= round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp585", 
                                                                                   "q5"]),2), 
                   yend = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp585", 
                                                               "q95"]),2)), 
               color = "#bf1d1e", linewidth = 0.5) +
  geom_segment(aes(x = 7900, xend = 7900, 
                   y= round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp245", 
                                                           "q5"]),2), 
                   yend = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp245", 
                                                               "q95"]),2)), 
               color = "#f69320", linewidth = 0.5) +
  
  ggstar::geom_star(aes(x = 7900+80, 
                        y = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp245", 
                                                                 "median_ssp"]),2)), 
                    col = "grey20", fill = "#f69320", angle = 90, starshape = 11, size = 2) +
  ggstar::geom_star(aes(x = 8100+80, 
                        y = round(1 - unlist(future_climdiss_ssp[future_climdiss_ssp$year == 2095 & future_climdiss_ssp$scenario == "ssp585", 
                                                                 "median_ssp"]),2)), 
                    col = "grey20", fill = "#bf1d1e", angle = 90, starshape = 11, size = 2) +
  
  
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title.y = element_text(colour = "black", family= "Helvetica Narrow", size = 8, margin = margin(r = 4.5), hjust = 0.92),
        axis.title.x = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        legend.position="none", legend.title=element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(t = 5.5, b = 5.5, r = 0.5, l = 5.5),
        plot.background = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_segment(aes(x = 12250, xend = 12250, y= 0.1, yend = 0.65), color = "black", linewidth = 0.5) + 
  geom_segment(aes(x = 12000, xend = 8000, y= -0.4, yend = -0.4), color = "black", linewidth = 0.5) +
  geom_segment(aes(x = 7750, xend = 7750, y= 0.1, yend = 0.65), color = "black", linewidth = 0.5) +
  
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
  geom_segment(aes(x = 12010, xend = 8250, y= 0.05, yend = 0.05), color = "grey20", linewidth = 0.4) +
  geom_segment(aes(x = 8250, xend = 7850, y= 0.05, yend = 0.05), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03, x = 9500, label = "from 12k BP"), size = 7.5*5/14, family= "Helvetica Narrow", color = "grey30") +
  geom_text(aes(y = 0.05, x = 12250, label = "A."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010, y = 0.05), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 2) +
  # simulations from 11.75k
  geom_segment(aes(x = 12010-250, xend = 8250, y= 0.05-0.1, yend = 0.05-0.1), color = "grey20", linewidth = 0.4) +
  geom_segment(aes(x = 8250, xend = 7850, y= 0.05-0.1, yend = 0.05-0.1), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.1, x = 9500, label = "from 11.75k BP"), size = 7.5*5/14, family= "Helvetica Narrow", color = "grey30") +
  geom_text(aes(y = 0.05-0.1, x = 12250-250, label = "B."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-250, y = 0.05-0.1), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 2) +
  # simulations from 11.5k
  geom_segment(aes(x = 12010-500, xend = 8250, y= 0.05-0.2, yend = 0.05-0.2), color = "grey20", linewidth = 0.4) +
  geom_segment(aes(x = 8250, xend = 7850, y= 0.05-0.2, yend = 0.05-0.2), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.2, x = 9500, label = "from 11.5k BP"), size = 7.5*5/14, family= "Helvetica Narrow", color = "grey30") +
  geom_text(aes(y = 0.05-0.2, x = 12250-500, label = "C."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-500, y = 0.05-0.2), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 2) +
  # simulations from 11.25k
  geom_segment(aes(x = 12010-750, xend = 8250, y= 0.05-0.3, yend = 0.05-0.3), color = "grey20", linewidth = 0.4) +
  geom_segment(aes(x = 8250, xend = 7850, y= 0.05-0.3, yend = 0.05-0.3), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.3, x = 9500, label = "from 11.25k BP"), size = 7.5*5/14, family= "Helvetica Narrow", color = "grey30") +
  geom_text(aes(y = 0.05-0.3, x = 12250-750, label = "D."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-750, y = 0.05-0.3), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 2) +
  # simulations from 11k
  geom_segment(aes(x = 12010-1000, xend = 8250, y= 0.05-0.4, yend = 0.05-0.4), color = "grey20", linewidth = 0.4) +
  geom_segment(aes(x = 8250, xend = 7850, y= 0.05-0.4, yend = 0.05-0.4), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_text(aes(y = 0.05 + 0.03 -0.4, x = 9500, label = "from 11k BP"), size = 7.5*5/14, family= "Helvetica Narrow", color = "grey30") +
  geom_text(aes(y = 0.05-0.4, x = 12250-1000, label = "E."), size = 9*5/14,  fontface = "bold", family= "Helvetica Narrow") +
  ggstar::geom_star(aes(x = 12010-1000, y = 0.05-0.4), col = "grey20", fill = "grey60", angle = 90, starshape = 11, size = 2) +
  # evaluation points
  geom_point(data = past_climdiss %>% dplyr::filter(clim_hpv_sorensen.year %in% seq(11500,8500,-500)),
             aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "#6867ac") +
  geom_point(data = past_climdiss %>% dplyr::filter(clim_hpv_sorensen.year %in% seq(11500,8500,-500)),
             aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "white", size = 0.9)

