
#--------------------------------#  
# Script to save migration steps #
#--------------------------------#

dir.create(save_dir, recursive = T)

# Save 250-year step raster
plan(multisession, workers = ncores)
foreach(yr = years) %dopar% {
  
  step <- (starting_year-yr)
  
  # load migration step
  rst <- terra::rast(file.path(output_dir, "steps", paste0("steps_step_", step, ".asc")))
  terra::crs(rst) <- "EPSG:3035" 
  rst[rst >= 30000] <- 0
  rst[rst <0] <- 0
  
  # check
  if(as.numeric(global(rst, max, na.rm = T)) > step){error("Check output file!")} 
  
  # load habitat suitability
  hs_map <- readRDS(file.path(sim_dir, paste0(yr, "BP.rds")))
  hs_map <- rast(hs_map[,c(2,1,3)], crs = "EPSG:4326")
  hs_map[hs_map<best_threshold] <- 0
  hs_map[hs_map>=best_threshold] <- 1
  
  # process migration output
  rst[rst > 0] <- 1
  rcopy <- rst
  res(rcopy) <- terra::res(rcopy)*40 # near 0.1deg
  rst <- terra::resample(rst, rcopy, method="bilinear")
  rst <- project(rst, "EPSG:4326", method = "bilinear")
  rst <- resample(rst, hs_map, method = "bilinear") 
  
  # summarize
  mig <- mask(rst, hs_map)
  mig[mig>0] <- 1
  mig <- min(mig, hs_map) # correct the effect of resample and reprojection
  mig_hsuit <- mig+hs_map # 0: not suitable, 1: suitable, 2: occupied
  
  saveRDS(mig_hsuit, file = file.path(save_dir, paste0(yr, "BP.rds")))
  
}
plan(sequential)
gc()

# Clean unnecessary files
files_to_remove <- list.files(output_dir, include.dirs = F, full.names = T, recursive = F)
file.remove(files_to_remove, showWarnings = FALSE)