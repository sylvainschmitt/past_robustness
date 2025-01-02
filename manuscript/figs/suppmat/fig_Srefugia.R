
dir <- file.path(wd, "data/simulations/additional_migration_for_reviewer")

models <- c("csdm/brt", "csdm/random_forest", "csdm/gam", "csdm/lasso_glm", "csdm/maxent", 
            "phenofit/expert", "phenofit/fitted", "castanea/expert", "castanea/fitted")
species <- c("abies_alba", "fagus_sylvatica", "quercus_deciduous", "quercus_evergreen")

model_names <- list("csdm/brt" = "BRT", "csdm/random_forest" = "Random Forest", "csdm/gam" = "GAM", 
                    "csdm/lasso_glm" = "GLM", "csdm/maxent" = "MaxEnt", 
                    "phenofit/expert" = "PHENOFIT", "castanea/expert" = "CASTANEA", 
                    "phenofit/fitted" = "PHENOFIT fitted",  "castanea/fitted" = "CASTANEA fitted")

species_names <- list("abies_alba" = "Abies alba", "fagus_sylvatica" = "Fagus sylvatica", 
                      "quercus_deciduous" = "Quercus (deciduous)", "quercus_evergreen" = "Quercus (evergreen)")

plotlist <- unlist(lapply(models, function(m){
  
  plots <- lapply(species, function(s){
    if(s == "fagus_sylvatica" & m %in% c("csdm/gam","csdm/lasso_glm","phenofit/expert", "csdm/maxent")){
      y <- 11750
    }else if(s == "quercus_evergreen"){
      y <- 11750
    }else if(s == "quercus_deciduous" & m %in% c("csdm/random_forest")){
      y <- 11750
    }else{
      y <- 12000
    }

    simdir <- file.path(dir, m, paste0(s, "_expandLDD_scprb_2km20km_fullmodel_from", 12000), paste0(y, "BP.rds"))
    out <- crop(readRDS(simdir), ext(-10,30,34,66)) %>%
      as.data.frame(xy = TRUE)
    names(out) <- c("lon", "lat", "value")
    
    yr_ICE6G <- 12
    ice_sheet <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), ext(-10,30,34,66))
    ice_sheet_pr <- ice_sheet
    ice_sheet[ice_sheet == 0] <- NA
    
    plot <- ggplot() +
      geom_raster(data = out, aes(x=lon, y=lat, fill = factor(value))) +
      scale_fill_manual(values = c("0" = "#ededed", "1" = "#4f928e", "2" = "#4f928e")) +
      new_scale_fill() + 
      geom_spatraster(data = ice_sheet) +
      geom_spatraster_contour(data = ice_sheet_pr, linewidth = 0.2, breaks = c(90), col = "#a6bfe1") +
      scale_fill_gradient2(
        low = "#e3ebf5", 
        mid = "#e3ebf5", 
        high = "#e3ebf5", 
        na.value = NA,
        limits = c(0,100), 
        breaks = c(0,50,100), 
        midpoint = 50) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      coord_cartesian(xlim=c(-10, 30), ylim = c(34.5, 66), clip = "off") +
      theme_void() +
      theme(legend.position = "none",
            plot.margin = unit(c(0.2, 0.05, 0, 0.05), "cm"), 
            panel.border = element_rect(colour = "darkgrey", fill=NA, size=0.2))
    
    # add model name
    if(s == "abies_alba"){
      plot <- plot +
        annotate("text", x = -12.7, y = 50, label = model_names[[m]], color = "black",
                 family= "Helvetica", size = 3.1, angle = 90)
    }
    
    # add species (only first line)
    if(m == "csdm/brt"){
      plot <- plot +
        annotate("text", x = 10, y = 70, label = species_names[[s]], color = "black",
                 family= "Helvetica", size = 3.1, fontface = 'italic')
    }
    
    # particular case when migration had to start at 11750 rather than 12000
    if(y == 11750){
      plot <- plot +
        annotate("text", x = -8, y = 62.5, label = "*", color = "black",
                 family= "Helvetica", size = 6)
    }
    
    
    return(plot)
  })
  return(plots)}), 
recursive = F)

plotlist <- rlist::list.append(rep(NA,5),
                               NA, plotlist[1:4],
                               NA, plotlist[5:8],
                               NA, plotlist[9:12],
                               NA, plotlist[13:16],
                               NA, plotlist[17:20],
                               NA, plotlist[21:24],
                               NA, plotlist[25:28],
                               NA, plotlist[29:32],
                               NA, plotlist[33:36])

fig_refugia <- cowplot::plot_grid(plotlist = plotlist, ncol = 5, align = "hv", 
                                rel_widths = c(0.13,1,1,1,1), 
                                rel_heights = c(0.1,rep(1,9)))
