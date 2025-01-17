
# Setup script

library(dplyr)
library(ggplot2)
library(cowplot)
library(extrafont)
library(data.table)
library(dplyr)
library(ggplot2)
library(terra)
library(ordbetareg)
library(ggnewscale)
library(tidyterra)
library(ggrepel)
library(hypervolume)
library(ggh4x)

source(file.path(wd, "manuscript", "fig_processing", "load_model_performance.R"))
source(file.path(wd, "manuscript", "fig_processing", "suppmat", "load_model_performance_withrep.R"))
