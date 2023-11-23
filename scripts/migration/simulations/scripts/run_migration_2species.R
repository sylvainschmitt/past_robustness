
#--------------------------------------------------------------------#  
# Script to run migration with two species (Q. robur and Q. petraea) #
#--------------------------------------------------------------------#

years <- seq(starting_year-250, 250, -250)

cat("Processing files...\n")
start_time <- Sys.time()


# 1. initial distribution
starting_points_qpetraea <- readRDS(file.path(sim_dir_qpetraea, paste0(years[1]+250, "BP.rds"))) # initial points
starting_points_qpetraea <- rast(starting_points_qpetraea[,c(2,1,3)], crs = "EPSG:4326")
starting_points_qpetraea <- crop(starting_points_qpetraea, ext(extent))
starting_points_qpetraea <- ifel(starting_points_qpetraea < best_threshold_qpetraea, 0 , 1)
starting_points_qrobur <- readRDS(file.path(sim_dir_qrobur, paste0(years[1]+250, "BP.rds"))) # initial points
starting_points_qrobur <- rast(starting_points_qrobur[,c(2,1,3)], crs = "EPSG:4326")
starting_points_qrobur <- crop(starting_points_qrobur, ext(extent))
starting_points_qrobur <- ifel(starting_points_qrobur < best_threshold_qrobur, 0 , 1)

starting_points <- max(starting_points_qrobur, starting_points_qpetraea)

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
.init_dist_ds <- wrap(init_dist_ds)


# 2. prepare habitat suitability rasters
foreach(yr = years) %dopar% {
  
  init_dist_ds <- rast(.init_dist_ds)
  
  cat(paste0("Processing files for year ", yr, "\n"))
  
  # Load 2 quercus maps
  qrobur_hsmap <- rast(readRDS(file.path(sim_dir_qrobur, paste0(yr, "BP.rds")))[c(2,1,3)], crs = "EPSG:4326")
  qrobur_hsmap <- crop(qrobur_hsmap, ext(extent))
  qpetraea_hsmap <- rast(readRDS(file.path(sim_dir_qpetraea, paste0(yr, "BP.rds")))[c(2,1,3)], crs = "EPSG:4326")
  qpetraea_hsmap <- crop(qpetraea_hsmap, ext(extent))
  
  # Change to migration resolution
  rcopy <- qrobur_hsmap
  terra::res(rcopy) <- terra::res(rcopy)/40 # near 500m
  qrobur_hsmap <- terra::resample(qrobur_hsmap, rcopy, method="max")
  qrobur_hsmap <- project(qrobur_hsmap, "EPSG:3035", method = "bilinear") # project in meters
  rcopy <- qrobur_hsmap
  terra::res(rcopy) <- c(res,res)
  qrobur_hsmap_ds <- terra::resample(qrobur_hsmap, rcopy, method="max") 
  # qrobur_hsmap_ds <- qrobur_hsmap_ds*1000 # needed by MigClim
  rcopy <- qpetraea_hsmap
  terra::res(rcopy) <- terra::res(rcopy)/40 # near 500m
  qpetraea_hsmap <- terra::resample(qpetraea_hsmap, rcopy, method="max")
  qpetraea_hsmap <- project(qpetraea_hsmap, "EPSG:3035", method = "bilinear") # project in meters
  rcopy <- qpetraea_hsmap
  terra::res(rcopy) <- c(res,res)
  qpetraea_hsmap_ds <- terra::resample(qpetraea_hsmap, rcopy, method="max") 
  # qpetraea_hsmap_ds <- qpetraea_hsmap_ds*1000 # needed by MigClim
  
  # applying best threshold, and rescaling probabilities between 1 and 1000
  max_hs <- as.numeric(global(qrobur_hsmap_ds, max, na.rm = T))
  qrobur_hsmap_ds <- ifel(qrobur_hsmap_ds < (best_threshold_qrobur), 0, (1000-1) * 
                            (qrobur_hsmap_ds - (best_threshold_qrobur))/(max_hs - (best_threshold_qrobur)) + 1)
  max_hs <- as.numeric(global(qpetraea_hsmap_ds, max, na.rm = T))
  qpetraea_hsmap_ds <- ifel(qpetraea_hsmap_ds < (best_threshold_qpetraea), 0, (1000-1) * 
                              (qpetraea_hsmap_ds - (best_threshold_qpetraea))/(max_hs - (best_threshold_qpetraea)) + 1)
  
  # Assemble
  qdeciduous_hsmap_ds <- max(qrobur_hsmap_ds, qpetraea_hsmap_ds)
  
  # hs_map_ds <- max(qdeciduous_hsmap_ds, init_dist_ds) # no zero fitness policy on refugia 
  
  terra::writeRaster(qdeciduous_hsmap_ds, file.path(output_dir, paste0("hs_map_", which(years==yr),".asc")), 
                     overwrite = T, NAflag = -9999, datatype="INT2S")
  
}
plan(sequential)


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