
library(data.table)
library(terra)
library(future.apply)
library(pracma)

extent <- ext(c(-10,35,36,71))

#-----------------------------------------------------------------------#
# PALEOCLIMATE: Burke et al. approach + uniformization with CRU dataset #
#-----------------------------------------------------------------------#

cru_baseline <- compute_CRU_baseline(from = 1921, to = 1980, extent, data_dir = "D:/climate/CRU")

.cru_baseline <- wrap(cru_baseline )

# In the past: HadCM3B
yr <- 15
hadcm3b_present <- average_to_three_month_means((-yr-15):(-yr+15), name_sim = "HadCM3B", 
                                                data_dir = paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/", yr, "BP"))
hadcm3b_present <- resample(hadcm3b_present, cru_baseline, method = "average")
.hadcm3b_present <- wrap(hadcm3b_present)

plan(multisession, workers = 10)
past_climatenovelty <- future_sapply(c(seq(18000,250,-250), 15), function(yr){
  cru_baseline <- rast(.cru_baseline )
  hadcm3b_present <- rast(.hadcm3b_present)
  
  hadcm3b_past <- average_to_three_month_means((-yr-15):(-yr+15), name_sim = "HadCM3B", 
                                               data_dir = paste0("D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min/", yr, "BP"))
  hadcm3b_past <- resample(hadcm3b_past, cru_baseline, method = "average")
  
  tmp_anom <- subset(hadcm3b_past - hadcm3b_present, 1:4) # difference
  pre_anom <- subset(hadcm3b_past/hadcm3b_present, 5:8) # ratio
  
  hadcm3b_cru <- c(subset(cru_baseline,1:4)+tmp_anom, subset(cru_baseline,5:8)*pre_anom)
  
  # ice filter (less than 50% ice cover, as in Burke et al. 2019)
  yr_ICE6G <- yr%/%500*0.5
  extent <- ext(c(-10,35,36,71))
  ice_HR <- crop(rotate(rast(paste0("D:/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), extent)
  ice_HR <- resample(ice_HR, hadcm3b_cru)
  ice_HR[ice_HR >= 0.5] <- NA
  hadcm3b_cru <- mask(hadcm3b_cru, ice_HR)
  
  r_climdist <- climatic_dissimilarity(focal = hadcm3b_cru, baseline = cru_baseline,
                                       method = "mahalanobis")
  
  output <- c(yr, 
              as.numeric(global(r_climdist$mahalanobis_distance, mean, na.rm = T)$mean), 
              as.numeric(global(r_climdist$mahalanobis_distance, median, na.rm = T)$global), 
              as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X25), 
              as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X75),
              round(global((subset(r_climdist, 2:9)), mean, na.rm = T)$mean, 1),
              round(global(sum(subset(r_climdist, 2:5)), mean, na.rm = T)$mean, 1),
              round(global(sum(subset(r_climdist, 6:9)), mean, na.rm = T)$mean, 1),
              global(mean(subset(hadcm3b_cru, 1:4)), mean, na.rm = T)$mean,
              global(sum(subset(hadcm3b_cru, 5:8)), mean, na.rm = T)$mean,
              global(mean(subset(hadcm3b_cru, 1:4)), quantile, na.rm = T)$X25,
              global(mean(subset(hadcm3b_cru, 1:4)), quantile, na.rm = T)$X75,
              global(sum(subset(hadcm3b_cru, 5:8)), quantile, na.rm = T)$X25,
              global(sum(subset(hadcm3b_cru, 5:8)), quantile, na.rm = T)$X75,
              global(mean(subset(hadcm3b_cru, 2:3)), mean, na.rm = T)$mean,
              global(sum(subset(hadcm3b_cru, 6:7)), mean, na.rm = T)$mean)
  
  names(output) <- c("year", "mean", "median", "q25", "q75", names(subset(r_climdist, 2:9)), 
                     "contribution_tmp", "contribution_pre", 
                     "tmean", "pre", "tmean_q25", "tmean_q75", "pre_q25", "pre_q75",
                     "tmean_sprsum", "pre_sprsum")
  
  return(output)
})
plan(sequential)
gc()
past_climatenovelty <- as.data.frame(t(past_climatenovelty))
saveRDS(past_climatenovelty, file ="C:/Users/vandermeersch/Documents/CEFE/phd/hindcasting/metrics/climate_approach/data/past_climatenovelty.rds")


#-------------------------------------------------------------------------#
# FUTURE CLIMATE: Burke et al. approach + uniformization with CRU dataset #
#-------------------------------------------------------------------------#

terraOptions(memfrac=0.9)
cmip6_dir <- "D:/climate/CMIP6_Adjust"

cru_baseline <- compute_CRU_baseline(from = 1921, to = 1980, extent, data_dir = "D:/climate/CRU")
.cru_baseline <- wrap(cru_baseline )

scenarios <- c("ssp245", "ssp585")
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")

future_climatenovelty <- lapply(scenarios, function(s){
  
  cat(paste0("Scenario ", s, "\n"))
  
  climatenovelty <- lapply(models, function(m){
    
    cat(paste0("Model ", m, "\n"))
    
    pres_cond <- average_to_three_month_means(1951:1980, name_sim = m, 
                                              data_dir = file.path(cmip6_dir, s, m, "phenofit_format"))
    pres_cond  <- resample(pres_cond, cru_baseline, method = "average")
    .pres_cond <- wrap(pres_cond)
    
    plan(multisession, workers = 4)
    climatenovelty <- future_lapply(seq(2005, 2095, 1), function(yr){
      
      cru_baseline <- rast(.cru_baseline )
      pres_cond <- rast(.pres_cond )
      
      # process future climate variables (11-year window)
      fut_cond <- average_to_three_month_means((yr-5):(yr+5), name_sim = m, 
                                                data_dir = file.path(cmip6_dir, s, m, "phenofit_format"))
      fut_cond <- resample(fut_cond, cru_baseline, method = "average")
      
      # uniformization with CRU data
      tmp_anom <- subset(fut_cond - pres_cond , 1:4) # difference (bias correction)
      pre_anom <- subset(fut_cond/pres_cond , 5:8) # ratio (bias correction)
      fut_cond_cru <- c(subset(cru_baseline,1:4)+tmp_anom, subset(cru_baseline,5:8)*pre_anom) 
      
      # compute climate novelty
      r_climdist <- climatic_dissimilarity(focal = fut_cond_cru, baseline = cru_baseline,
                                           method = "mahalanobis") 
      
      saveRDS(r_climdist, file.path("C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/climate_data/climate_novelty", 
                                    s, m,
                                    paste0("climdist_", yr, "BP.rds")))
      
      output <- c(yr, 
                  as.numeric(global(r_climdist$mahalanobis_distance, mean, na.rm = T)$mean), 
                  as.numeric(global(r_climdist$mahalanobis_distance, median, na.rm = T)$global), 
                  as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X25), 
                  as.numeric(global(r_climdist$mahalanobis_distance, quantile, na.rm = T)$X75),
                  round(global((subset(r_climdist, 2:9)), mean, na.rm = T)$mean, 1),
                  round(global(sum(subset(r_climdist, 2:5)), mean, na.rm = T)$mean, 1),
                  round(global(sum(subset(r_climdist, 6:9)), mean, na.rm = T)$mean, 1),
                  global(mean(subset(fut_cond_cru, 1:4)), mean, na.rm = T)$mean,
                  global(sum(subset(fut_cond_cru, 5:8)), mean, na.rm = T)$mean)
      
      names(output) <- c("year", "mean", "median", "q25", "q75", names(subset(r_climdist, 2:9)), 
                         "contribution_tmp", "contribution_pre", "tmean", "pre")
      return(output)
    })
    plan(sequential)
    gc()
    climatenovelty <- as.data.frame(do.call(rbind, climatenovelty))
    climatenovelty$model <- m
    return(climatenovelty)
  })
  climatenovelty <- as.data.frame(do.call(rbind, climatenovelty))
  climatenovelty$scenario <- s
  return(climatenovelty)
})

future_climatenovelty <- as.data.frame(do.call(rbind, future_climatenovelty))
saveRDS(future_climatenovelty, file ="C:/Users/vandermeersch/Documents/CEFE/phd/forecasting/metrics/climate_approach/data/future_climatenovelty.rds")
