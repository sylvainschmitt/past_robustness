
#-------------------------#
# MAXENT paleosimulations #
#-------------------------#

model_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/correlative_models/fit/ecv/maxent/fit"
sim_dir <- "D:/simulations/csdm/maxent/paleo/025deg"
clim_dir <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/csdm_format/025deg"

library(dismo)
library(dplyr)
library(foreach)

# Setup
species <- "quercus_ilex"
bc_covars <- c("bio6", "bio12") # bioclim predictors
soil_covars <- c("WHC", "pH") # soil predictors
cc_covars <- c("sum_apsep_GDD5", "w_bal") # custom climatic predictors
covars <- c(bc_covars, soil_covars, cc_covars)
dir.create(file.path(sim_dir, species), showWarnings = F, recursive = TRUE)

# Load model
maxmod <- readRDS(file.path(model_dir, species, "maxent_finalcov_fullmodel.rds"))

# Simulation loop
var_importance <- sapply(seq(250,12500,250), function(year){
  predictors <- readRDS(file.path(clim_dir, paste0("predictors_", year, "BP.rds")))
  
  sim <- predictors[, c("lat", "lon")]
  
  predictors <- predictors %>% 
    dplyr::select(all_of(covars))
  
  # normalize predictors
  i <- 1
  for(v in covars){
    predictors[, v] <- (predictors[, v] - maxmod$meanv_l[i]) / maxmod$sdv_l[i]
    i <- i+1
  }
  
  # make and save predictions
  sim$pred <-  predict(maxmod$model, predictors, args = c("outputformat=cloglog"))
  saveRDS(sim, file.path(sim_dir, species, paste0(year, "BP.rds")))
  
  # var_importance <- bm_VariablesImportance(bm.model=maxmod$model, predictors, variables = c(bc_covars, cc_covars), 
  #                                          method = "full_rand", nb.rep = 3, do.progress = FALSE)
  # 
  # var_importance <- var_importance %>% 
  #   group_by(expl.var) %>%
  #   summarize(var.imp = mean(var.imp))
  
  # return(c(year, t(var_importance$var.imp)))
  
  return(c(year))
  
})

# var_importance <- as.data.frame(t(var_importance))
# names(var_importance) <- c("year", bc_covars, cc_covars)
# saveRDS(var_importance, file.path(sim_dir, species, "variable_importance.rds"))
