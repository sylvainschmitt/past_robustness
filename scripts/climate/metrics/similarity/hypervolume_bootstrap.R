library(hypervolume)
library(future.apply)
set.seed(1997)

#--------------------------------------#
# PALEOCLIMATE: hypervolume similarity #
#--------------------------------------#

data_dir <- "E:/USERS/VanderMeersch/data/hypervolume/pca"

# climate similarity future v.s. present, based on bootstraps (to calculate SD & CI)
setwd(data_dir)
CI <- 0.95   

## generate bootstraps and distribution of overlap index (20*20 overlaps)
calibration_domain <- readRDS(file.path(data_dir, "present_domain.rds"))
calibration_domain_path = hypervolume_resample("calibration_domain", calibration_domain, n = 20,
                                               method = "bootstrap", cores = 20, verbose = FALSE)

plan(multisession, workers = 3)
hypervolume_similarity <- future_sapply(c(15, seq(250, 12000, 250)), function(yr){
  projection_domain <- readRDS(file.path(data_dir, paste0("projection_domain_", yr, "BP.rds")))
  projection_domain_path <- hypervolume_resample(paste0("projection_domain_", yr), projection_domain, n = 20,
                                                 method = "bootstrap", cores = 20, verbose = FALSE) 
  hyp_ov <- hypervolume_overlap_confidence(projection_domain_path, calibration_domain_path)
  return(list(year = yr, hyp_ov))
}, future.seed=TRUE, future.stdout = FALSE)
plan(sequential)
gc()

## calculate statistics       
hypervolume_similarity_statistics <- apply(hypervolume_similarity, 2, function(d){
  year <- d[[1]]
  print(year)
  distribution <- d[[2]][["distribution"]]
  
  mean <- mean(distribution[, "sorensen"])
  sd <- sd(distribution[, "sorensen"])
  median <- median(distribution[, "sorensen"])
  qnt <- quantile(distribution[, "sorensen"], c(0.5 - CI/2, 0.5 + CI/2))
  iqr <- quantile(distribution[, "sorensen"], c(0.25, 0.75))
  
  return(c(year, mean, sd, median, qnt, iqr))
})
hypervolume_similarity_statistics <- as.data.frame(t(hypervolume_similarity_statistics))
names(hypervolume_similarity_statistics) <- c("year", "mean", "sd", "median", "q2.5", "q97.5", "q25", "q75")             
saveRDS(hypervolume_similarity_statistics, file = file.path(data_dir, "hypervolume_similarity_statistics.rds"))


#----------------------------------------#
# FUTURE CLIMATE: hypervolume similarity #
#----------------------------------------#

data_dir <- "E:/USERS/VanderMeersch/data/hypervolume/pca_future"
scenarios <- c("ssp245", "ssp585")
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
years <- seq(2005, 2095, 1)
CI <- 0.95

# climate similarity future v.s. present, based on bootstraps (to calculate SD & CI)
setwd(data_dir)
hypervolume_similarity_statistics <- data.frame() # per model per scenario
hypervolume_similarity_statistics_scenario <- data.frame() # per scenario 
calibration_domain <- readRDS(file.path(data_dir, "present_domain.rds"))
calibration_domain_path = hypervolume_resample("calibration_domain", calibration_domain, n = 20,
                                               method = "bootstrap", cores = 20) 

for(s in scenarios){
  
  cat(paste0("Scenario ", s, "\n"))
  
  hypervolume_similarity_distribution_s <- c() # distribution of similarity metrics within a scenario
  
  for(m in models){
    
    cat(paste0("  Model ", m, "\n"))
    
    ## generate bootstraps and distribution of overlap index (20*20 overlaps)
    plan(multisession, workers = 3)
    hypervolume_bootstraps <- future_sapply(years, function(yr){
      projection_domain <- readRDS(file.path(data_dir, s, m, paste0("projection_domain_", yr, ".rds")))
      projection_domain_path <- hypervolume_resample(paste0("projection_domain_", yr), projection_domain, n = 20,
                                                     method = "bootstrap", cores = 20, verbose = FALSE) 
      return(list(year = yr))
    }, future.seed=TRUE, future.stdout = FALSE)
    plan(sequential)
    gc()
    
    ## compute distributions of overlap index (20*20 overlaps)
    plan(multisession, workers = 46)
    hypervolume_similarity <- future_sapply(years, function(yr){
      projection_domain_path <- file.path(data_dir, "Objects", paste0("projection_domain_", yr))
      hyp_ov <- hypervolume_overlap_confidence(projection_domain_path, calibration_domain_path)
      return(list(year = yr, hyp_ov))
    }, future.seed=TRUE, future.stdout = FALSE)
    plan(sequential)
    gc()
    
    
    ## calculate statistics per model
    hypervolume_similarity_statistics_sm <- apply(hypervolume_similarity, 2, function(d){
      year <- d[[1]]
      distribution <- d[[2]][["distribution"]] # distribution of similarity metrics within a model
      
      mean <- mean(distribution[, "sorensen"])
      sd <- sd(distribution[, "sorensen"])
      median <- median(distribution[, "sorensen"])
      qnt <- quantile(distribution[, "sorensen"], c(0.5 - CI/2, 0.5 + CI/2))
      iqr <- quantile(distribution[, "sorensen"], c(0.25, 0.75))
      
      return(c(year, mean, sd, median, qnt, iqr))
    })
    hypervolume_similarity_statistics_sm <- as.data.frame(t(hypervolume_similarity_statistics_sm))
    names(hypervolume_similarity_statistics_sm) <- c("year", "mean", "sd", "median", "q2.5", "q97.5", "q25", "q75") 
    hypervolume_similarity_statistics_sm$scenario <- s
    hypervolume_similarity_statistics_sm$model <- m
    hypervolume_similarity_statistics <- rbind(hypervolume_similarity_statistics, hypervolume_similarity_statistics_sm)
    
    ## save metric distribution (to compute statistics per scenario, see after)
    hypervolume_similarity_distribution_sm <- apply(hypervolume_similarity, 2, function(d){
      distribution <- d[[2]][["distribution"]]
      return(c(distribution[, "sorensen"]))
    })
    hypervolume_similarity_distribution_s <- rbind(hypervolume_similarity_distribution_s, hypervolume_similarity_distribution_sm)
    
  }
  
  ## calculate statistics per scenario
  hypervolume_similarity_statistics_s <- apply(hypervolume_similarity_distribution_s, 2, function(d){

    mean <- mean(d)
    sd <- sd(d)
    median <- median(d)
    qnt <- quantile(d, c(0.5 - CI/2, 0.5 + CI/2))
    iqr <- quantile(distribution[, "sorensen"], c(0.25, 0.75))
    
    return(c(mean, sd, median, qnt, iqr))
  })
  hypervolume_similarity_statistics_s <- as.data.frame(t(hypervolume_similarity_statistics_s))
  hypervolume_similarity_statistics_s$year <- years
  hypervolume_similarity_statistics_s$scenario <- s
  names(hypervolume_similarity_statistics_s) <- c("mean", "sd", "median", "q2.5", "q97.5", "q25", "q75", "year", "scenario")
  hypervolume_similarity_statistics_scenario <- rbind(hypervolume_similarity_statistics_scenario, 
                                                      hypervolume_similarity_statistics_s)
  
}
saveRDS(hypervolume_similarity_statistics, file = file.path(data_dir, "hypervolume_similarity_statistics.rds"))
saveRDS(hypervolume_similarity_statistics_scenario, 
        file = file.path(data_dir, "hypervolume_similarity_statistics_scenario.rds"))
