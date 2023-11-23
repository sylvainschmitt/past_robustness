
#---------------------------------#  
# CASTANEA fitted: tree migration #
#---------------------------------#

## Fagus sylvatica ##

# General setup
wd <- "E:/USERS/VanderMeersch/migration/simulations"
source(file.path(wd, "scripts", "setup.R"))
ncores <- 2
res = 500 # resolution of the grid, in meters
extent <- c(-10,30,36,66)
starting_year <- 11750

# Model setup
sim_dir <- "E:/USERS/VanderMeersch/data/simulations/castanea/paleo/fitted/025deg/fagus_sylvatica/30yr_inventory_modcode/NPP"
output_dir <- file.path(wd, "fagus_sylvatica", "output", 
                        paste0("expandLDD_scprb_2km20km_fullmodel_from", starting_year), "castanea_fitted")
save_dir <- file.path("E:/USERS/VanderMeersch/data/simulations", "castanea", "paleo/migration/fitted",
                      paste0("fagus_sylvatica", "_expandLDD_scprb_2km20km_fullmodel_from", starting_year))

# Species parameter
best_threshold <-  55.6
source(file.path(wd, "scripts", "fagus_parameters.R"))
gc()

# Simulate migration
source(file.path(wd, "scripts", "run_migration.R"))

# Save migration outputs
source(file.path(wd, "scripts", "save_migration.R"))

