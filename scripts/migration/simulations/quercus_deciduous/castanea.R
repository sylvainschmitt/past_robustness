
#--------------------------#  
# CASTANEA: tree migration #
#--------------------------#

## Quercus (deciduous) ##

# General setup
wd <- "E:/USERS/VanderMeersch/migration/simulations"
source(file.path(wd, "scripts", "setup.R"))
ncores <- 2
res = 500 # resolution of the grid, in meters
extent <- c(-10,30,36,66)
starting_year <- 11750

# Model setup
sim_dir_qrobur <- "E:/USERS/VanderMeersch/data/simulations/castanea/paleo/expert/025deg/quercus_robur/32yr_inventory_modcode/NPP"
sim_dir_qpetraea <- "E:/USERS/VanderMeersch/data/simulations/castanea/paleo/expert/025deg/quercus_petraea/32yr_inventory_modcode/NPP"
output_dir <- file.path(wd, "quercus_deciduous", "output", 
                        paste0("expandLDD_scprb_2km20km_fullmodel_from", starting_year), "castanea")
save_dir <- file.path("E:/USERS/VanderMeersch/data/simulations", "castanea", "paleo/migration/expert",
                      paste0("quercus_deciduous", "_expandLDD_scprb_2km20km_fullmodel_from", starting_year))

# Species parameter
best_threshold_qrobur <- 486.4
best_threshold_qpetraea <- 482.5
source(file.path(wd, "scripts", "quercusdeciduous_parameters.R"))
gc()

# Simulate migration
source(file.path(wd, "scripts", "run_migration_2species.R"))

# Save migration outputs
source(file.path(wd, "scripts", "save_migration_2species.R"))

