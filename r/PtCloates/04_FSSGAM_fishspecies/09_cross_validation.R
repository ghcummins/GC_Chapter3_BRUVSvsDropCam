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
library(GlobalArchive)

# Set your study name
name <- "PtCloates"                                                    

# Load data
dat1 <- readRDS("data/staging/PtCloates/PtCloates.fish.dat.maxn.rds")%>%
  #dplyr::mutate(reef =rock+inverts)%>%
  mutate(z = abs(z), name = scientific, scientific = paste(method,scientific,sep=".")) %>%
  #mutate(status = ifelse(is.na(status), "No-take", status)) %>%
  glimpse()

# Estimate spatial block size and separate data into spatial folds
# Load in the habitat data - reef and depth
#Note below file taken from output of Ch2
preddf <- readRDS("data/spatial/rasters/raw bathymetry/PtCloates_spatial_habitat_predictions.rds") %>%
  dplyr::rename(reef = pinverts.fit)%>%
  ga.clean.names()%>%
  mutate(z =abs(z))
  # rast(type = "xyz", crs = "epsg:4326")
##note preddf contains reef and depth columns that we need - extract below
 depthreef <- preddf %>%
   dplyr::select(x, y, z, depth, reef) %>%
   rast(type = "xyz", crs = "epsg:4326")
  
# Load in bathymetry predictors
stack <- readRDS(paste(paste0('data/spatial/rasters/', name),      # This is ignored - too big!
                       'spatial_covariates.rds', sep = "_")) %>%
  crop(depthreef) 
   
  stack <- c(stack, depthreef) %>%
  project("EPSG:9473")
rpc <- rasterPCA(stack, nComp = 1 , spca = TRUE, nSamples = 4364)               # Now accepts SpatRaster
# crs(rpc$map) <- "epsg:9473"
# test <- rast(rpc)

poly_sf <- st_sf(geometry = st_as_sfc(st_bbox(stack)))                          # convert the polygon to sf for later
dat_sf <- dat1 %>%                                                               # Any csv with lat lon that has your samples and rasters extracted at those points 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = "epsg:4326", remove = F) %>%
  st_transform(9473)

# AC range estimation
range1 <- cv_spatial_autocor(
  r = rpc$map,                                                            
  num_sample = 5000,
  progress = T,
  plot = T
)
###STUCK
png(filename = paste0("plots/PtCloates/fish_spatial-blocks.png"),
    units = "in", res = 300, height = 8, width = 10)
range1$plots + labs(title = "Spatial blocks - Fish")
dev.off()

sb1 <- cv_spatial(x = dat_sf,
                  r = rpc$map,
                  size = range1$range,
                  k = 5,
                  selection = "random",
                  iteration = 100,
                  seed = 1,
                  biomod2 = TRUE,
                  hexagon = FALSE)

# creating a dataframe for the RF model 
mod_df <- dat_sf %>%
  st_transform(4364) %>%
  data.frame() %>%
  add_column(block = sb1$folds_ids)

# Loop through cross validation for each taxa (habitat) and each fold
BOSS_lm_abund <- mod_df %>%
  #dplyr::select(-c(ID, geometry)) %>%
  dplyr::rename(x = longitude, y = latitude) %>%
  dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus")) %>% 
  dplyr::mutate(method = as.factor(method)) %>%
  glimpse()

resp.vars <- unique(BOSS_LMabund$scientific)
blocks <- unique(BOSS_LMabund$block)

lm_preddf <- stack %>%
  project("epsg:4326") %>%
  as.data.frame(xy = T, na.rm = T)


# Loop through each taxa
# Train GAM off 4 folds, test against last fold
# Absolute value of the distance from the observed to predicted data
for (i in 1:length(resp.vars)) {
  use.dat <- BOSS_lm_abund[BOSS_lm_abund$scientific == resp.vars[i],]
  use.dat   <- as.data.frame(use.dat)
  print(resp.vars[i])
  
  for (b in 1:length(blocks)) {
    print(blocks[b])
    train.dat <- use.dat %>% dplyr::filter(!block == blocks[b])
    test.dat  <- use.dat %>% dplyr::filter(block == blocks[b])
    
    # Letrinus miniatus depth
    mod1 <- gam(number ~ s(z, k = 3, bs = "cr") +
                  s(PROD, k = 3, bs = "cr") +
                  s(reef, k = 3, bs = "cr") +
                  s(roughness, k = 3, bs = "cr") + 
                  s(SST, k = 3, bs = "cr") +
                  s(year, bs = "re"), 
                data = fabund %>% dplyr::filter(scientific %in% "species.richness"), ###THIS NEEDS UPDATING + EVERYTHING  BELOW
                family = tw())
    
    # reef
    mod2 <- gam(number ~ s(reef, k = 3, bs = "cr") +
                  s(PROD, k = 3, bs = "cr") +
                  s(reef, k = 3, bs = "cr") + 
                  s(SST, k = 3, bs = "cr") +
                  s(year, bs = "re"), 
                data = fabund %>% dplyr::filter(scientific %in% "cti"),
                family = tw())
    
    # Greater than size of maturity
    mod3 <- gam(number ~ s(reef, k = 3, bs = "cr") +
                  s(roughness, k = 3, bs = "cr") +
                  s(SLA, k = 3, bs = "cr") +
                  s(SST, k = 3, bs = "cr") +
                  status +
                  s(year, bs = "re"), 
                data = fabund %>% dplyr::filter(scientific %in% "greater than Lm carinvores"),
                family = tw())
    
    # Smaller than size of maturity
    mod4 <- gam(number ~ s(reef, k = 3, bs = "cr") +
                  s(roughness, k = 3, bs = "cr") +
                  s(SLA, k = 3, bs = "cr") +
                  s(SST, k = 3, bs = "cr") +
                  s(year, bs = "re"), 
                data = fabund %>% dplyr::filter(scientific %in% "smaller than Lm carnivores"),
                family = tw())
    
    # Pink Snapper Smaller than Lm
    mod5 <- gam(number ~ s(reef, k = 3, bs = "cr") +
                  s(roughness, k = 3, bs = "cr") +
                  s(SLA, k = 3, bs = "cr") +
                  s(SST, k = 3, bs = "cr") +
                  s(year, bs = "re"), 
                data = fabund %>% dplyr::filter(scientific %in% "smaller than Lm Pink snapper"),
                family = tw())
    
    mod <- list(mod1, mod2, mod3, mod4, mod5)
    
    modpred <- cbind(preddf, 
                     "predicted" = predict(mod[[i]], preddf, type = "response"))
    modpredr <- rast(modpred %>% dplyr::select(x, y, predicted))
    fabund_sp <- vect(test.dat, geom = c("x", "y"), crs = "epsg:4326")
    fabund_df   <- cbind(test.dat, terra::extract(modpredr, fabund_sp)) %>%
      dplyr::filter(!is.na(predicted),
                    !is.na(number))
    if (b == 1) {
      pearsons <- data.frame(taxa = resp.vars[i],
                             fold = blocks[b],
                             pcc = cor(fabund_df$number, fabund_df$predicted, method = "pearson"))
      
    }
    else {
      pearsons <- data.frame(taxa = resp.vars[i],
                             fold = blocks[b],
                             pcc = cor(fabund_df$number, fabund_df$predicted, method = "pearson")) %>%
        bind_rows(pearsons)
      
    }
  }
  if (i == 1) {
    pearsons_table <- pearsons 
    
  }
  else {
    pearsons_table <- pearsons %>%
      bind_rows(pearsons_table)
    
  }
}

pearsons_final <- pearsons_table %>%
  dplyr::mutate(fold = as.character(fold)) %>%
  group_by(taxa) %>%
  group_modify(~ add_row(.x, pcc = mean(.$pcc), fold = "mean")) %>%
  ungroup() %>%
  pivot_wider(names_from = taxa, values_from = pcc) %>%
  glimpse()

# Save output
write.csv(pearsons_final, paste0("output/fish/", name, "_pearsons-coefficient.csv"),
          row.names = F)