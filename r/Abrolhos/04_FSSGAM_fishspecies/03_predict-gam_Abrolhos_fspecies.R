###
# Project: Gabby PhD Ch3 Abrolhos  Fish & habitat
# Data:    BRUV & BOSS fish MaxN
# Task:    Fish model prediction
# author:  Claude Spencer & Gabby Cummins
# date:    May 2024
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
library(GlobalArchive)

name <- "Abrolhos"  # set study name

# read in
dat1 <- readRDS("data/staging/Abrolhos/Abrolhos.maxn.rds")%>%
  dplyr::mutate(reef =rock+inverts)%>%
  mutate(z = abs(z), scientific = paste(method,scientific,sep=".")) %>%
  #mutate(status = ifelse(is.na(status), "No-take", status)) %>%
  glimpse()
  
  # dplyr::filter(!is.na(mean.relief)) %>%
  #dplyr::rename(number = maxn) %>%                                              # Rename both to the same to join
  # glimpse()

#dat2 <- readRDS(paste(paste0('data/tidy/', name), 
                      # 'gam-length.rds', sep = "_")) %>%
  #glimpse()

# fabund <- bind_rows(dat1,dat2)                                                  # Merged fish data from 02_fish_abundance.R & 03_fish_length.R

# preddf <- readRDS(paste(paste0('data/spatial/rasters/raw bathymetry/', name),      # This is ignored - too big!
#                         'spatial_covariates.rds', sep = "_")) %>%
#   as.data.frame(xy = T) %>%
#   mutate(z =abs(Z))%>%
#   dplyr::select(-Z)%>%
#   glimpse()

phab <- readRDS("data/spatial/rasters/raw bathymetry/PtCloates_spatial_habitat_predictions.rds") %>%
  dplyr::rename(reef = pinverts.fit)%>%
  ga.clean.names()%>%
  mutate(z =abs(z))

preddf <- phab

#
# 
# presp <- vect(preddf, geom = c("x", "y"))
# preddf <- cbind(terra::extract(test, presp), preddf)
 

# # Re-set the predictors for modeling----
pred.vars <- c("z", "reef",
               "tpi","roughness","detrended") 


# use formula from top model from FSSGam model selection
# Greater than size of maturity openness+recfish+reef+UCUR+VCUR
unique(dat1$scientific)

#Total abundance ##PUT IN TOP MODEL PREDICTORS
m_BOSS_ta <-gam(maxn ~ s(reef, k=3, bs = "cr"),
                  data = dat1 %>% dplyr::filter(scientific %in% "BOSS.total.abundance"),
                family = gaussian(link="identity"))
summary(m_BOSS_ta)

m_BRUV_ta <-gam(maxn ~ s(z, k=3, bs = "cr"),
                data = dat1 %>% dplyr::filter(scientific %in% "BRUV.total.abundance"),
                family = gaussian(link="identity"))
summary(m_BRUV_ta)


#Species richness
m_BOSS_richness <- gam(maxn ~ s(reef, k = 3, bs = "cr") +s(roughness, k = 3, bs = "cr"),  
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

