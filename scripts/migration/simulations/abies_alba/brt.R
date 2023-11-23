
#---------------------#  
# BRT: tree migration #
#---------------------#

## Abies alba ##

# General setup
wd <- "E:/USERS/VanderMeersch/migration/simulations"
source(file.path(wd, "scripts", "setup.R"))
ncores <- 10
res = 500 # resolution of the grid, in meters
extent <- c(-10,30,36,66)
starting_year <- 11750

# Model setup
sim_dir <- "E:/USERS/VanderMeersch/data/simulations/csdm/brt/paleo/025deg/abies_alba"
output_dir <- file.path(wd, "abies_alba", "output", 
                        paste0("expandLDD_scprb_2km20km_fullmodel_from", starting_year), "brt")
save_dir <- file.path("E:/USERS/VanderMeersch/data/simulations", "csdm", "brt", "paleo/migration",
                      paste0("abies_alba", "_expandLDD_scprb_2km20km_fullmodel_from", starting_year))
  
# Species parameter
best_threshold <- 0.484
source(file.path(wd, "scripts", "abies_parameters.R"))
gc()

# Simulate migration
source(file.path(wd, "scripts", "run_migration.R"))

# Save migration outputs
source(file.path(wd, "scripts", "save_migration.R"))
Ê
