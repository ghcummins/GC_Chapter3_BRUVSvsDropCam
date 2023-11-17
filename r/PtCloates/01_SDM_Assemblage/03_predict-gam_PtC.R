###
# Project: Gabby PhD Ch3 Abrolhos  Habitat & Fish
# Data:    BRUV & BOSS fish MaxN
# Task:    Fish model prediction
# author:  Claude Spencer & Gabby Cummins
# date:    November 2023
##

rm(list=ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(stringr)
library(terra)
library(sf)
library(gstat)

name <- "PtCloates"  # set study name

# read in
dat1 <- readRDS("data/staging/PtCloates/PtCloates.maxn.rds")%>%
  dplyr::mutate(reef = rock+inverts)%>%
  mutate(z = abs(z), scientific = paste(method,scientific,sep=".")) %>%
  # dplyr::filter(!is.na(mean.relief)) %>%
  #dplyr::rename(number = maxn) %>%                                              # Rename both to the same to join
  glimpse()

#dat2 <- readRDS(paste(paste0('data/tidy/', name), 
                      # 'gam-length.rds', sep = "_")) %>%
  #glimpse()

# fabund <- bind_rows(dat1,dat2)                                                  # Merged fish data from 02_fish_abundance.R & 03_fish_length.R

preddf <- readRDS(paste(paste0('data/spatial/rasters/raw bathymetry/', name),      # This is ignored - too big!
                        'spatial_covariates.rds', sep = "_")) %>%
  as.data.frame(xy = T) %>%
  mutate(z =abs(Z))%>%
  dplyr::select(-Z)%>%
  glimpse()

 pred_mean.relief <- readRDS("data/spatial/rasters/predicted_relief.rds") %>%
  dplyr::rename(mean.relief = prelief) %>%
  dplyr::select(x, y, mean.relief) %>%
  rast(type = "xyz", crs = "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs") %>%
  project("epsg:4326")
# 
 pred_mean.relief <- as(object = pred_mean.relief, Class = "Raster") %>%
   crop(extent(113, 114, -23, -22)) %>%
   trim()
 plot(pred_mean.relief)
# 
# preds <- rasterFromXYZ(preddf, crs = "+proj=longlat +datum=WGS84") %>%
#   crop(pred_mean.relief)
# plot(preds)
# 
# mean.relief_df <- pred_mean.relief %>%
#   as.data.frame(xy = T, na.rm = T)
# # plot(pred_mean.relief)
# 
# relief.sf <- st_as_sf(mean.relief_df, coords = c("x", "y"), crs = 4326)
# gs.relief <- gstat(id = "mean.relief", formula = mean.relief~1, data = relief.sf, 
#                 nmax = 5, set = list(idp = 0))
# 
# test <- raster::interpolate(object = pred_mean.relief, model = gs.relief, na.rm = T, debug.level = 0) %>%
#   raster::resample( preds[[1]], method = "bilinear")
# plot(test)
# test <- as(object = test, Class = "SpatRaster")
# names(test)[1] <- "mean.relief"
# 
# presp <- vect(preddf, geom = c("x", "y"))
# preddf <- cbind(terra::extract(test, presp), preddf)
# names(preddf)

# # Re-set the predictors for modeling----
pred.vars <- c("z", "reef",
               "tpi","roughness","detrended") 


# use formula from top model from FSSGam model selection
# Greater than size of maturity openness+recfish+reef+UCUR+VCUR
unique(dat1$scientific)

#Total abundance
m_BOSS_ta <-gam(maxn ~ s(reef, k=3, bs = "cr"),
                  data = dat1 %>% dplyr::filter(scientific %in% "BOSS.total.abundance"),
                family = gaussian(link="identity"))
summary(m_BOSS_ta)

m_BRUV_ta <-gam(maxn ~ s(z, k=3, bs = "cr"),
                data = dat1 %>% dplyr::filter(scientific %in% "BRUV.total.abundance"),
                family = gaussian(link="identity"))
summary(m_BRUV_ta)



#Species richness
m_BOSS_richness <- gam(maxn ~ s(z, k = 3, bs = "cr"),
                  data = dat1 %>% dplyr::filter(scientific %in% "BOSS.species.richness"), 
                  family = gaussian(link="identity"))
summary(m_BOSS_richness)

m_BRUV_richness <- gam(maxn ~s(z, k = 3, bs = "cr"),
                       data = dat1 %>% dplyr::filter(scientific %in% "BRUV.species.richness"),
                       family = gaussian(link="identity"))
summary(m_BRUV_richness)



# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_BOSS_ta" = predict(m_BOSS_ta, preddf, type = "response"),
                "p_BRUV_ta" = predict(m_BRUV_ta, preddf, type = "response"),
                "p_BOSS_richness" = predict(m_BOSS_richness, preddf, type = "response"),
                "p_BRUV_richness" = predict(m_BRUV_richness, preddf, type = "response"))
                

prasts <- rast(preddf %>% dplyr::select(x, y, starts_with("p_")),
                        crs = "epsg:4326") 
plot(prasts)

saveRDS(preddf, paste0("outputs/PtCloates/", name, "_predicted-fish.RDS"))

