
#-------------------------#  
# Script to run migration #
#-------------------------#

setwd(output_dir)
years <- seq(starting_year-250, 250, -250)

cat("Processing files...\n")
start_time <- Sys.time()


# 1. initial distribution
starting_points <- readRDS(file.path(sim_dir, paste0(years[1]+250, "BP.rds"))) # initial points
starting_points <- rast(starting_points[,c(2,1,3)], crs = "EPSG:4326")
starting_points <- crop(starting_points, ext(extent))
starting_points <- ifel(starting_points < best_threshold, 0 , 1)
rcopy <- starting_points
res(rcopy) <- res(rcopy)/40 # near 500m
starting_points <- terra::resample(starting_points, rcopy, method="max")
starting_points <- terra::ifel(is.na(starting_points), 0, starting_points) # trick so land-sea mask can change during migration
starting_points <- project(starting_points, "EPSG:3035", method = "max")
rcopy <- starting_points
res(rcopy) <- c(res,res)
init_dist_ds <- terra::resample(starting_points, rcopy, method="max") 
terra::writeRaster(init_dist_ds, file.path(output_dir, paste0("init_dist.asc")), 
                   overwrite = T, NAflag = -9999, datatype="INT2S")
gc()


plan(multisession, workers = ncores)
.rcopy <- wrap(rcopy)
.init_dist_ds <- wrap(init_dist_ds)


# 2. prepare habitat suitability rasters
foreach(yr = years) %dopar% {
  
  rcopy <- rast(.rcopy)
  init_dist_ds <- rast(.init_dist_ds)
  
  cat(paste0("Processing files for year ", yr, "\n"))
  hs_map <- readRDS(file.path(sim_dir, paste0(yr, "BP.rds")))
  hs_map <- rast(hs_map[,c(2,1,3)], crs = "EPSG:4326")
  hs_map <- crop(hs_map, ext(extent))
  
  # change to chosen resolution 
  rcopy <- hs_map
  terra::res(rcopy) <- terra::res(rcopy)/40 # near 500m
  hs_map <- terra::resample(hs_map, rcopy, method="max")
  hs_map <- project(hs_map, "EPSG:3035", method = "bilinear") # project in meters
  rcopy <- hs_map
  terra::res(rcopy) <- c(res,res)
  hs_map_ds <- terra::resample(hs_map, rcopy, method="max") 
  # hs_map_ds <- (hs_map_ds * 1000) # required by MigClim
  # hs_map_ds[hs_map_ds < (best_threshold * 1000)] <- 0 # best threshold
  
  # applying best threshold, and rescaling probabilities between 1 and 1000
  max_hs <- as.numeric(global(hs_map_ds, max, na.rm = T))
  hs_map_ds <- ifel(hs_map_ds < (best_threshold), 0, (1000-1) * 
                      (hs_map_ds - (best_threshold))/(max_hs - (best_threshold)) + 1)
  
  
  # hs_map_ds <- max(hs_map_ds, init_dist_ds) # no zero fitness policy on refugia 
  
  terra::writeRaster(hs_map_ds, file.path(output_dir, paste0("hs_map_", which(years==yr),".asc")), 
                     overwrite = T, NAflag = -9999, datatype="INT2S")
  
}
plan(sequential)
gc()


# 3. let's migrate !
MigClimCustom2.migrate(iniDist= paste0("init_dist"), # initial distribution
                       
                       hsMap= paste0("hs_map_"), # habitat suitability, between 0 and 1000
                       rcThreshold=0, # continuous mode
                       
                       envChgSteps = length(years), # number of times the hsMap should be updated (max 295)
                       dispSteps=250,  # number of dispersal steps per hsMap (max ?)
                       # if the interval between hsMap is 5 years, and the species disperse once a year
                       # disSteps must be set to 5
                       
                       dispKernel= disp_kernel_SD,
                       
                       barrier="", barrierType="strong", # cells across which dispersal cannot occur, only affect SD events
                       
                       iniMatAge = MatAge, # just for example
                       propaguleProd = c(1), # propagule production probability as a function of time
                       # since the cell became colonized
                       # note that cell age is measured in dispersal steps
                       
                       lddFreq=0.01, # long distance dispersal events
                       dispKernel_LDD = disp_kernel_LD,
                       
                       simulName="steps", replicateNb=1,
                       overWrite=TRUE,
                       
                       testMode=FALSE, fullOutput=TRUE, keepTempFiles=FALSE)
end_time <- Sys.time()
cat(paste0("Runtime: ",  round(as.double(end_time-start_time, units = "mins"), 1), "min \n"))