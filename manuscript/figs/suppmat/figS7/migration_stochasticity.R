
# Assess whether migration stochasticity has an important impact

quercusdeciduous_models_withrep <- data.frame(
  name = c(
    "BRT",
    "Random Forest",
    "GAM",
    "GLM",
    "MaxEnt",
    "PHENOFIT",
    "CASTANEA",
    "PHENOFIT (fitted)",
    "CASTANEA (fitted)"),
  type = c(
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "1Correlative",
    "3Expertprocessbased",
    "3Expertprocessbased",
    "2Fittedprocessbased",
    "2Fittedprocessbased"),
  simfolder = c(
    paste0(wd, "/data/simulations/migration/csdm/brt/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/random_forest/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from11750"),
    paste0(wd, "/data/simulations/migration/csdm/gam/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/lasso_glm/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000"),
    paste0(wd, "/data/simulations/migration/csdm/maxent/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/expert/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000"),
    paste0(wd, "/data/simulations/migration/phenofit/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000"),
    paste0(wd, "/data/simulations/migration/castanea/fitted/quercus_deciduous_expandLDD_scprb_2km20km_fullmodel_rep_from12000")))

quercusdeciduous_performance <- load_model_performance_withrep(
  models = quercusdeciduous_models_withrep, 
  years = c(seq(500,11500, 500)), nrep =10,
  pollen_folder = "D:/species/pollen/processed/quercus_deciduoustype/025deg/0025thr_500yrunc",
  add_pollen_folder <- "D:/species/pollen/processed/quercus_indist/025deg/0025thr_500yrunc",
  evergreen = FALSE)
quercusdeciduous_performance$species <- "quercusdeciduous"

figS7_migration_stochasticity <- ggplot(data = quercusdeciduous_performance) +
  facet_grid(~ type) +
  geom_boxplot(aes(x = rep, y = mig_sorensen, group = rep, color = type, fill = type),
               alpha = 0.3, outlier.size = 0.1, width = 0.6, linewidth = 0.3, fatten = 2) +
  scale_color_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                     values= c( "#457b9d", "#995D81", "#018530"),
                     labels = c("CSDM     ", "Fitted PBM   ", "Expert PBM     ")) +
  scale_fill_manual(breaks= c("1Correlative", "2Fittedprocessbased", "3Expertprocessbased"),
                    values= c( "#457b9d", "#995D81", "#018530"),
                    labels = c("CSDM     ", "Fitted PBM   ", "Expert PBM     ")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,1,0.2),
                     name = "Performance") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        axis.title = element_text(colour = "black", family= "Helvetica Narrow", size = 9),
        legend.text = element_text(colour = "black", family= "Helvetica Narrow", size = 8),
        legend.title=element_blank(), legend.position = "bottom",
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(50, 5.5, 60, 10, unit = "pt")) +
  coord_cartesian(ylim=c(0, 1), clip = "off")
