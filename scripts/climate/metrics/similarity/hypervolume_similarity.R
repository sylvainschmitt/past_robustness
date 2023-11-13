
library(hypervolume)
set.seed(1997)

#--------------------------------------#
# PALEOCLIMATE: hypervolume similarity #
#--------------------------------------#

data_dir <- "E:/USERS/VanderMeersch/data/hypervolume/pca"

# 1. Climate similarity past v.s. present
# calibration_domain <- readRDS(file.path(data_dir, "present_domain.rds"))
# 
# for(yr in seq(500, 11500, 500)){
#   projection_domain <- readRDS(file.path(data_dir, paste0("projection_domain_", yr, "BP.rds")))
#   hypv_set <- hypervolume_set(projection_domain, calibration_domain, check.memory = FALSE)
#   hyp_ovst <- hypervolume_overlap_statistics(hypv_set)
#   saveRDS(hyp_ovst, file = paste0("E:/USERS/VanderMeersch/data/hypervolume/pca/overlap_statistics_", yr,"BP.rds"))
# }
# 
# hypervolume_similarity <- sapply(seq(500, 11500, 500), function(yr){
#   hyp_ovst <- readRDS(paste0("E:/USERS/VanderMeersch/data/hypervolume/pca/overlap_statistics_", yr,"BP.rds"))
#   return(c(year = yr, hyp_ovst["sorensen"], hyp_ovst["jaccard"]))
# })
# hypervolume_similarity <- as.data.frame(t(hypervolume_similarity ))
# saveRDS(hypervolume_similarity, file = "E:/USERS/VanderMeersch/data/hypervolume/pca/hypervolume_similarity.rds")


# 2. Climate similarity past v.s. present, based on bootstraps (to calculate SD & CI)

## generate bootstraps and distribution of overlap index (20*20 overlaps)
setwd(data_dir)
calibration_domain <- readRDS(file.path(data_dir, "present_domain.rds"))
calibration_domain_path = hypervolume_resample("calibration_domain", calibration_domain, n = 20,
                                               method = "bootstrap", cores = 20) 
hypervolume_similarity <- sapply(seq(500, 11500, 500), function(yr){
  projection_domain <- readRDS(file.path(data_dir, paste0("projection_domain_", yr, "BP.rds")))
  projection_domain_path <- hypervolume_resample("projection_domain", projection_domain, n = 20,
                                                 method = "bootstrap", cores = 20) 
  hyp_ov <- hypervolume_overlap_confidence(projection_domain_path, calibration_domain_path)
  return(list(year = yr, hyp_ov))
})
## calculate statistics
CI <- 0.95             
hypervolume_similarity_statistics <- apply(hypervolume_similarity, 2, function(d){
  year <- d[[1]]
  distribution <- d[[2]][["distribution"]]
  
  mean <- mean(distribution[, "sorensen"])
  sd <- sd(distribution[, "sorensen"])
  median <- median(distribution[, "sorensen"])
  qnt <- quantile(distribution[, "sorensen"], c(0.5 - CI/2, 0.5 + CI/2))
  
  return(c(year, mean, sd, median, qnt))
})
hypervolume_similarity_statistics <- as.data.frame(t(hypervolume_similarity_statistics))
names(hypervolume_similarity_statistics) <- c("year", "mean", "sd", "median", "q2.5", "q97.5")             
saveRDS(hypervolume_similarity_statistics, file = file.path(data_dir, "hypervolume_similarity_statistics.rds"))


#----------------------------------------#
# FUTURE CLIMATE: hypervolume similarity #
#----------------------------------------#

data_dir <- "E:/USERS/VanderMeersch/data/hypervolume/pca_future"
scenarios <- c("ssp245", "ssp585")
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")


# Climate similarity future v.s. present, based on bootstraps (to calculate SD & CI)

setwd(data_dir)
hypervolume_similarity_statistics <- data.frame()
calibration_domain <- readRDS(file.path(data_dir, "present_domain.rds"))
calibration_domain_path = hypervolume_resample("calibration_domain", calibration_domain, n = 20,
                                               method = "bootstrap", cores = 20) 
for(s in scenarios){
  
  cat(paste0("Scenario ", s, "\n"))
  
  for(m in models){
    
    cat(paste0("Model ", m, "\n"))
    
    ## generate bootstraps and distribution of overlap index (20*20 overlaps)
    plan(multisession, workers = 2)
    hypervolume_similarity <- future_sapply(seq(2005, 2095, 1), function(yr){
      projection_domain <- readRDS(file.path(data_dir, s, m, paste0("projection_domain_", yr, ".rds")))
      projection_domain_path <- hypervolume_resample("projection_domain", projection_domain, n = 20,
                                                     method = "bootstrap", cores = 20) 
      hyp_ov <- hypervolume_overlap_confidence(projection_domain_path, calibration_domain_path)
    }, future.seed=TRUE)
    plan(sequential)
    gc()
    
    ## calculate statistics
    CI <- 0.95             
    hypervolume_similarity_statistics_sm <- apply(hypervolume_similarity, 2, function(d){
      year <- d[[1]]
      distribution <- d[[2]][["distribution"]]
      
      mean <- mean(distribution[, "sorensen"])
      sd <- sd(distribution[, "sorensen"])
      median <- median(distribution[, "sorensen"])
      qnt <- quantile(distribution[, "sorensen"], c(0.5 - CI/2, 0.5 + CI/2))
      
      return(c(year, mean, sd, median, qnt))
    })
    hypervolume_similarity_statistics_sm <- as.data.frame(t(hypervolume_similarity_statistics_sm))
    names(hypervolume_similarity_statistics_sm) <- c("year", "mean", "sd", "median", "q2.5", "q97.5") 
    hypervolume_similarity_statistics_sm$scenario <- s
    hypervolume_similarity_statistics_sm$model <- m
    
    hypervolume_similarity_statistics <- rbind(hypervolume_similarity_statistics, hypervolume_similarity_statistics_sm)
  }

}
saveRDS(hypervolume_similarity_statistics, file = file.path(data_dir, "hypervolume_similarity_statistics.rds"))
