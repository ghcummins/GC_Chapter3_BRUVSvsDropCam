#### Project: GC CHapter 3 PhD 
# Data:    BRUVS, BOSS Habitat data
# Task:    Cross validation & Kappa stats
# Author:  Claude Spencer & Gabby Cummins
# Date:    Feb 2024
##

rm(list = ls())
gc()

# Load libraries
library(tidyverse)
library(raster)
library(terra)
library(sp)
library(sf)
sf_use_s2(T)
library(ggplot2)
library(devtools)
# install_github("bleutner/RStoolbox")
library(RStoolbox)
library(blockCV)
library(ggnewscale)
library(scales)
library(mgcv)
library(pROC)
library(caret)
library(dplyr)


# Set your study name
name <- "PtCloates"                                                    

# Load data

dat <- readRDS(paste(paste0('data/tidy/', name), 
                     'habitat-bathy-derivatives.rds', sep = "_")) %>%         
  rename(sampleold = sample) %>% 
  mutate(sample =paste(sampleold, method, date, sep = "_")) %>%
  mutate(x = longitude) %>%
  mutate(y = latitude) %>%
  glimpse()