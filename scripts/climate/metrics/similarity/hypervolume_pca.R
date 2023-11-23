library(hypervolume)
library(terra)
library("FactoMineR")
library("factoextra")
library(data.table)
library(future.apply)
set.seed(1997)

#----------------------------------------------------------------------#
# PALEOCLIMATE: hypervolume approach + uniformization with CRU dataset #
#----------------------------------------------------------------------#

extent <- ext(c(-10,30,36,71))
cru_baseline <- compute_CRU_baseline(from = 1921, to = 1980, extent, data_dir = "E:/USERS/VanderMeersch/data/climate/CRU")

yr <- 15
hadcm3b_present <- average_to_three_month_means((-yr-15):(-yr+15), name_sim = "HadCM3B", 
                                                data_dir = paste0("E:/USERS/VanderMeersch/data/climate/HadCM3B/phenofit_format/025deg/", yr, "BP"))
hadcm3b_present <- resample(hadcm3b_present, cru_baseline, method = "average")

cru_baseline <- mask(cru_baseline, hadcm3b_present)
pca_present <- prcomp(as.data.frame(cru_baseline), center = TRUE, scale = TRUE)
pcaaxis_present <- predict(pca_present, as.data.frame(cru_baseline))[,1:3] # three first axis of PCA
present_domain = hypervolume(pcaaxis_present, method='gaussian', verbose = FALSE)
saveRDS(present_domain, file = "E:/USERS/VanderMeersch/data/hypervolume/pca/present_domain.rds")


for(yr in c(15, seq(5250, 12000, 250))){
  cat(paste0(yr,"...\n"))
  
  hadcm3b_past <- average_to_three_month_means((-yr-15):(-yr+15), name_sim = "HadCM3B", 
                                               data_dir = paste0("E:/USERS/VanderMeersch/data/climate/HadCM3B/phenofit_format/025deg/", yr, "BP"))
  hadcm3b_past <- resample(hadcm3b_past, cru_baseline, method = "average")
  
  tmp_anom <- subset(hadcm3b_past - hadcm3b_present, 1:4) # difference
  pre_anom <- subset(hadcm3b_past/hadcm3b_present, 5:8) # ratio
  
  hadcm3b_cru <- c(subset(cru_baseline,1:4)+tmp_anom, subset(cru_baseline,5:8)*pre_anom)
  
  # ice filter (less than 50% ice cover, as in Burke et al. 2019)
  yr_ICE6G <- yr%/%500*0.5
  extent <- ext(c(-10,35,36,71))
  ice_HR <- crop(rotate(rast(paste0("E:/USERS/VanderMeersch/data/climate/ICE-6G-C/I6_C.VM5a_10min.",yr_ICE6G ,".nc"), subds = "sftgif")), extent)
  ice_HR <- resample(ice_HR, hadcm3b_cru)
  ice_HR[ice_HR >= 0.5] <- NA
  hadcm3b_cru <- mask(hadcm3b_cru, ice_HR)
  
  pcaaxis_past <- predict(pca_present, as.data.frame(hadcm3b_cru))[,1:3]
  set.seed(1997) # strange, but need to repeat "set.seed" to obtain the same result...
  past_domain = hypervolume(pcaaxis_past, method='gaussian', verbose = FALSE)
  saveRDS(past_domain, file = paste0("E:/USERS/VanderMeersch/data/hypervolume/pca/projection_domain_", yr,"BP.rds"))

}

#------------------------------------------------------------------------#
# FUTURE CLIMATE: hypervolume approach + uniformization with CRU dataset #
#------------------------------------------------------------------------#

# terraOptions(memfrac=0.9)
cmip6_dir <- "E:/USERS/VanderMeersch/data/climate/CMIP6_Adjust"

scenarios <- c("ssp245", "ssp585")
models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")

extent <- ext(c(-10,30,36,71))
cru_baseline <- compute_CRU_baseline(from = 1921, to = 1980, extent, data_dir = "E:/USERS/VanderMeersch/data/climate/CRU")

# just to generate needed raster mask (before computing PCA axis)
pres_mask <- average_to_three_month_means(1951:1952, name_sim = models[1],
                                          data_dir = file.path(cmip6_dir, scenarios[1], models[1], "phenofit_format"))
pres_mask  <- resample(pres_mask, cru_baseline, method = "average")

cru_baseline <- mask(cru_baseline, pres_mask)
rm(pres_mask)
.cru_baseline <- wrap(cru_baseline)
pca_present <- prcomp(as.data.frame(cru_baseline), center = TRUE, scale = TRUE)
pcaaxis_present <- predict(pca_present, as.data.frame(cru_baseline))[,1:3] # three first axis of PCA
present_domain = hypervolume(pcaaxis_present, method='gaussian')
saveRDS(present_domain, file = "E:/USERS/VanderMeersch/data/hypervolume/pca_future/present_domain.rds")

for(s in scenarios){
  
  cat(paste0("Scenario ", s, "\n"))
  
  for(m in models){
    
    cat(paste0("Model ", m, "\n"))
    
    pres_cond <- average_to_three_month_means(1951:1980, name_sim = m,
                                              data_dir = file.path(cmip6_dir, s, m, "phenofit_format"))
    pres_cond  <- resample(pres_cond, cru_baseline, method = "average")
    .pres_cond <- wrap(pres_cond)
    
    
    dir.create(paste0("E:/USERS/VanderMeersch/data/hypervolume/pca_future/", s, "/", m), recursive = T)
    
    plan(multisession, workers = 31)
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
      
      pcaaxis_future <- predict(pca_present, as.data.frame(fut_cond_cru))[,1:3]
      set.seed(1997) # strange, but need to repeat "set.seed" to obtain the same result...
      future_domain = hypervolume(pcaaxis_future, method='gaussian')
      saveRDS(future_domain, file = paste0("E:/USERS/VanderMeersch/data/hypervolume/pca_future/", s, "/", m, "/projection_domain_", yr,".rds"))
      
    }, future.seed=TRUE, future.stdout = FALSE)
    plan(sequential)
    gc()
    
  }
  
}



