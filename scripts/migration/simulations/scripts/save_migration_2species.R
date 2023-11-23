
#---------------------------------------------------------------------------#  
# Script to save migration steps with two species (Q. robur and Q. petraea) #
#---------------------------------------------------------------------------#

dir.create(save_dir, recursive = T)

# Save 250-year step raster
plan(multisession, workers = ncores)
foreach(yr = years) %dopar% {
  
  print(yr)
  step <- (12000-yr)
  
  # load migration step
  rst <- terra::rast(file.path(output_dir, "steps", paste0("steps_step_", step, ".asc")))
  terra::crs(rst) <- "EPSG:3035" 
  rst[rst >= 30000] <- 0
  rst[rst <0] <- 0
  
  # check
  if(as.numeric(global(rst, max, na.rm = T)) > step){error("Check output file!")} 
  
  # Load quercus maps
  qrobur_hsmap <- rast(readRDS(file.path(sim_dir_qrobur, paste0(yr, "BP.rds")))[c(2,1,3)], crs = "EPSG:4326")
  qrobur_hsmap <- crop(qrobur_hsmap, ext(extent))
  qrobur_hsmap[qrobur_hsmap < best_threshold_qrobur] <- 0 # best threshold
  qrobur_hsmap[qrobur_hsmap >= best_threshold_qrobur] <- 1 # best threshold
  qpetraea_hsmap <- rast(readRDS(file.path(sim_dir_qpetraea, paste0(yr, "BP.rds")))[c(2,1,3)], crs = "EPSG:4326")
  qpetraea_hsmap <- crop(qpetraea_hsmap, ext(extent))
  qpetraea_hsmap[qpetraea_hsmap < best_threshold_qpetraea] <- 0 # best threshold
  qpetraea_hsmap[qpetraea_hsmap >= best_threshold_qpetraea] <- 1 # best threshold
  
  hs_map <- max(qrobur_hsmap, qpetraea_hsmap)
  
  # process migration output
  rst[rst > 0] <- 1
  rcopy <- rst
  res(rcopy) <- terra::res(rcopy)*40 # near 0.1deg
  rst <- terra::resample(rst, rcopy, method="bilinear")
  rst <- project(rst, "EPSG:4326", method = "bilinear")
  rst <- resample(rst, hs_map, method = "bilinear") 
  
  mig <- mask(rst, hs_map)
  mig[mig>0] <- 1
  mig <- min(mig, hs_map)
  mig_hsuit <- mig+hs_map
  
  saveRDS(mig_hsuit, file = file.path(save_dir, paste0(yr, "BP.rds")))
  
}
plan(sequential)
gc()

# Clean unnecessary files
f <- list.files(output_dir, include.dirs = F, full.names = T, recursive = F)
file.remove(f, showWarnings = FALSE)