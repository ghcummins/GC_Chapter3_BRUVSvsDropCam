###
# Project: Gabby PhD Ch3 PtCloates  Habitat & Fish
# Data:    BRUV & BOSS fish MaxN
# Task:    Fish model prediction for nebulsa and miniatus at Pt Cloates
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

name <- "PtCloates"  # set study name

# read in
dat1 <- readRDS("data/staging/PtCloates/PtCloates.fish.dat.maxn.rds")%>%
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

#Note below file taken from output of Ch2
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
pred.vars <- c("z", "reef", "aspect",
               "tpi","roughness","detrended") 


# use formula from top model from FSSGam model selection
# Greater than size of maturity openness+recfish+reef+UCUR+VCUR
unique(dat1$scientific)

#Parapercis nebulosa ##PUT IN TOP MODEL PREDICTORS should k = 5? as ok at 80% zeros
Parapercis_nebulosa_BRUV <-gam(number ~ s(reef, k=3, bs = "cr") + s(z, k = 3, bs = "cr"), 
                  data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Pinguipedidae Parapercis nebulosa"),
                family = tw())
summary(Parapercis_nebulosa_BRUV)

Parapercis_nebulosa_BOSS <-gam(number ~ s(detrended, k=3, bs = "cr")+ s(reef, k = 3, bs = "cr"), 
                data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Pinguipedidae Parapercis nebulosa"),
                family = tw())
summary(Parapercis_nebulosa_BOSS)


#Lethrinus miniatus
Lethrinus_miniatus_BRUV <- gam(number ~ s(reef, k = 3, bs = "cr") +s(z, k = 3, bs = "cr"),  
                  data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Lethrinidae Lethrinus miniatus"), 
                  family = tw())
summary(Lethrinus_miniatus_BRUV)

Lethrinus_miniatus_BOSS <- gam(number ~s(detrended, k = 3, bs = "cr") +s(z, k = 3, bs = "cr"), 
                       data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Lethrinidae Lethrinus miniatus"),
                       family = tw())
summary(Lethrinus_miniatus_BOSS)



# predict, rasterise and plot ### CHANGE TO se.fit = TRUE fo individual fish species!
preddf <- cbind(preddf, 
                "p_P_nebulosa_BRUV" = predict(Parapercis_nebulosa_BRUV, preddf, type = "response", se.fit = T),
                "p_P_nebulosa_BOSS" = predict(Parapercis_nebulosa_BOSS, preddf, type = "response", se.fit = T),
                "p_L_miniatus_BRUV" = predict(Lethrinus_miniatus_BRUV, preddf, type = "response", se.fit = T),
                "p_L_miniatus_BOSS" = predict(Lethrinus_miniatus_BOSS, preddf, type = "response", se.fit = T))
                
# # reduce prediction area to within sampled range
# preddf <- preddf %>%
#   filter(z >= 215, z <= 71)

prasts <- rast(preddf %>% dplyr::select(x, y, starts_with("p_")),
                        crs = "epsg:4326") 
plot(prasts)

saveRDS(preddf, paste0("outputs/PtCloates/", name, "_predicted-fish.RDS"))

