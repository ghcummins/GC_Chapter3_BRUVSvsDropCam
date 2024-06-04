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
dat1 <- readRDS("data/staging/Abrolhos/Abrolhos.fish.dat.maxn.rds")%>%
  # dplyr::mutate(reef =rock+inverts)%>%
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

phab <- readRDS("data/spatial/rasters/raw bathymetry/Abrolhos_spatial_habitat_predictions.rds") %>%
  ga.clean.names()%>%
  mutate(z =abs(z)) %>%
  dplyr::mutate(reef =prock.fit+pinverts.fit+pmacroalg.fit)

preddf <- phab
preddf$method <- "BRUV"
preddf_bruv <-preddf
preddf_boss <-preddf
preddf_boss$method <- "BOSS"


# presp <- vect(preddf, geom = c("x", "y"))
# preddf <- cbind(terra::extract(test, presp), preddf)
 

# # Re-set the predictors for modeling----
pred.vars <- c("z", "reef", "aspect",
               "tpi","roughness","detrended") 


# use formula from top model from FSSGam model selection
# Greater than size of maturity openness+recfish+reef+UCUR+VCUR
unique(dat1$scientific)

#Relative abundance of individual fish species
Choerodon_rubescens_BOSS <-gam(number ~ s(reef, k=3, bs = "cr"),
                  data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Labridae Choerodon rubescens"),
                family = tw())
summary(Choerodon_rubescens_BOSS)

Choerodon_rubescens_BRUV <-gam(number ~ s(z, k=3, bs = "cr"),
                data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Labridae Choerodon rubescens"),
                family = tw())
summary(Choerodon_rubescens_BRUV)

Coris_auricularis_BOSS <- gam(number ~ s(z, k = 3, bs = "cr"),   
                  data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Labridae Coris auricularis"), 
                  family = tw())
summary(Coris_auricularis_BOSS)

Coris_auricularis_BRUV <- gam(number ~s(reef, k = 3, bs = "cr"),
                       data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Labridae Coris auricularis"),
                       family = tw())
summary(Coris_auricularis_BRUV)

Suezichthys_cyanolaemus_BOSS <- gam(number ~ s(z, k=3, bs = "cr"),
                                    data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Labridae Suezichthys cyanolaemus"),
                                    family = tw())
summary(Suezichthys_cyanolaemus_BOSS)

Suezichthys_cyanolaemus_BRUV <- gam(number ~ s(aspect, k=3, bs = "cr") + s(roughness, k=3, bs = "cr"),
                                    data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Labridae Suezichthys cyanolaemus"),
                                    family = tw())
summary(Suezichthys_cyanolaemus_BRUV)



# Lethrinus_miniatus_BOSS <- gam(number ~ s(z, k=3, bs = "cr", by = method),
#                                data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus")),
#                                family = tw())
# summary(Lethrinus_miniatus_BOSS)
# 
# Lethrinus_miniatus_BRUV <- gam(number ~ s(z, k=3, bs = "cr") + s(reef, k=3, bs = "cr"),
#                                data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Lethrinidae Lethrinus miniatus"),
#                                family = tw())
# summary(Lethrinus_miniatus_BRUV)

###30/05/2024 NEW MODELLING FOR RESIDUAL PLOTS
dat1$method <- as.factor(dat1$method)

Lethrinus_miniatus_BOSSBRUV <- gam(number ~ s(z, k=3, bs = "cr", by = method) + s(reef, k=3, bs = "cr", by = method) + method,
                                   data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus","BRUV.Lethrinidae Lethrinus miniatus")),
                                   family = tw())
summary(Lethrinus_miniatus_BOSSBRUV)

dat_total <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus", "BRUV.Lethrinidae Lethrinus miniatus"))

testdata <- expand.grid(z = seq(min(dat_total$z), max(dat_total$z), length.out = 20),
              reef = mean(Lethrinus_miniatus_BOSSBRUV$model$reef),
              method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

L.miniatus.fits <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata, type = 'response', se.fit = T)

predicts_total_z <- testdata %>%
  data.frame(L.miniatus.fits) %>%
  group_by(z, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup()

testdata1 <- expand.grid(method = c("BOSS", "BRUV"), 
                         z = mean(Lethrinus_miniatus_BOSSBRUV$model$z),
                         reef = mean(Lethrinus_miniatus_BOSSBRUV$model$reef)) %>%
  distinct() %>%
  glimpse() 

L.m.fits <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata1, type = 'response', se.fit = T)

predicts_total_method <- testdata1 %>%
  data.frame(L.m.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

testdata2 <- expand.grid(reef = seq(min(dat_total$reef), max(dat_total$reef), length.out = 20),
                         method = c("BOSS", "BRUV"),
                         z = mean(Lethrinus_miniatus_BOSSBRUV$model$z)) %>%
  distinct() %>%
  glimpse()

L.m.fits2 <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata2, type = 'response', se.fit = T)

predicts_total_reef <- testdata2 %>%
  data.frame(L.m.fits2) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()
  
#Plot L.miniatus residual abundance by depth (z)
gg_total_z <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  geom_line(data = predicts_total_z, aes(x = z, y = number, group=method, colour=method))+
  geom_line(data = predicts_total_z, aes(x = z, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_total_z, aes(x = z, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  #ylim(0,50)+
  labs(x = "Depth", y = "Abundance (residual)") +
  scale_colour_manual(values = c("BRUV" = "#666666", "BOSS" = "black")) +
  scale_fill_manual(values = c("BRUV" = "grey74", "BOSS" = "ghostwhite"))+
  theme(
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11),  # Y axis title size
    axis.text.x = element_text(size = 10),   # X axis text size
    axis.text.y = element_text(size = 10),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9)    # Legend text size
  )
gg_total_z

# Save gg_total_z as a PNG file
ggsave(filename = "plots/Abrolhos/BOSSBRUV_Lminiatus_depth_residualplot.png", 
       plot = gg_total_z, 
       width = 6, 
       height = 4, 
       dpi = 600, 
       units = "in")
  
#Plot L.miniatus residual abundance by reef
gg_total_reef <- ggplot() + 
  geom_ribbon(data = predicts_total_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_line(data = predicts_total_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_total_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_total_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  theme_classic() +
  #ylim(0,50)+
  labs(x = "Reef", y = "Abundance (residual)") +
  scale_colour_manual(values = c("BRUV" = "#666666", "BOSS" = "black")) +
  scale_fill_manual(values = c("BRUV" = "grey74", "BOSS" = "ghostwhite"))+
  theme(
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11),  # Y axis title size
    axis.text.x = element_text(size = 10),   # X axis text size
    axis.text.y = element_text(size = 10),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9)    # Legend text size
  )
gg_total_reef
  
# Save gg_total_reef as a PNG file
ggsave(filename = "plots/Abrolhos/BOSSBRUV_Lminiatus_reef_residualplot.png", 
       plot = gg_total_reef, 
       width = 6, 
       height = 4, 
       dpi = 600, 
       units = "in")

## Same as above but for Choerdon rubescens
Choerodon_rubescens_BOSSBRUV <- gam(number ~ s(z, k=3, bs = "cr", by = method) + s(reef, k=3, bs = "cr", by = method) + method,
                                   data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Labridae Choerodon rubescens","BRUV.Labridae Choerodon rubescens")),
                                   family = tw())
summary(Choerodon_rubescens_BOSSBRUV)

dat_total_cr <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Labridae Choerodon rubescens", "BRUV.Labridae Choerodon rubescens"))

testdata_cr <- expand.grid(z = seq(min(dat_total$z), max(dat_total$z), length.out = 20),
                        reef = mean(Choerodon_rubescens_BOSSBRUV$model$reef),
                        method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

C.rubescens.fits <- predict.gam(Choerodon_rubescens_BOSSBRUV, newdata = testdata, type = 'response', se.fit = T)

predicts_cr_z <- testdata_cr %>%
  data.frame(C.rubescens.fits) %>%
  group_by(z, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

testdata1_cr <- expand.grid(method = c("BOSS", "BRUV"), 
                         z = mean(Choerodon_rubescens_BOSSBRUV$model$z),
                         reef = mean(Choerodon_rubescens_BOSSBRUV$model$reef)) %>%
  distinct() %>%
  glimpse() 
 
C.r.fits <- predict.gam(Choerodon_rubescens_BOSSBRUV, newdata = testdata1, type = 'response', se.fit = T)

predicts_cr_method <- testdata1_cr %>%
  data.frame(C.r.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

testdata2_cr <- expand.grid(reef = seq(min(dat_total$reef), max(dat_total$reef), length.out = 20),
                         method = c("BOSS", "BRUV"),
                         z = mean(Choerodon_rubescens_BOSSBRUV$model$z)) %>%
  distinct() %>%
  glimpse()

C.r.fits2 <- predict.gam(Choerodon_rubescens_BOSSBRUV, newdata = testdata2, type = 'response', se.fit = T)

predicts_cr_reef <- testdata2_cr %>%
  data.frame(C.r.fits2) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

#Plot C rubescens residual abundance by depth (z)
gg_C_rubescens_z <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_cr_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  geom_line(data = predicts_cr_z, aes(x = z, y = number, group=method, colour=method))+
  geom_line(data = predicts_cr_z, aes(x = z, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_cr_z, aes(x = z, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  #ylim(0,50)+
  labs(x = "Depth", y = "Abundance (residual)") +
  scale_colour_manual(values = c("BRUV" = "#666666", "BOSS" = "black")) +
  scale_fill_manual(values = c("BRUV" = "grey74", "BOSS" = "ghostwhite"))+
  theme(
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11),  # Y axis title size
    axis.text.x = element_text(size = 10),   # X axis text size
    axis.text.y = element_text(size = 10),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9)    # Legend text size
  )
gg_C_rubescens_z

# Save gg_total_z as a PNG file
ggsave(filename = "plots/Abrolhos/BOSSBRUV_Crubescens_depth_residualplot.png", 
       plot = gg_C_rubescens_z, 
       width = 6, 
       height = 4, 
       dpi = 600, 
       units = "in")

#Plot C.rubescens residual abundance by reef
gg_C_rubescens_reef <- ggplot() + 
  geom_ribbon(data = predicts_cr_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_line(data = predicts_cr_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_cr_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_cr_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  theme_classic() +
  #ylim(0,50)+
  labs(x = "Reef", y = "Abundance (residual)") +
  scale_colour_manual(values = c("BRUV" = "#666666", "BOSS" = "black")) +
  scale_fill_manual(values = c("BRUV" = "grey74", "BOSS" = "ghostwhite"))+
  theme(
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11),  # Y axis title size
    axis.text.x = element_text(size = 10),   # X axis text size
    axis.text.y = element_text(size = 10),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9)    # Legend text size
  )
gg_C_rubescens_reef

# Save gg_total_reef as a PNG file
ggsave(filename = "plots/Abrolhos/BOSSBRUV_Crubescens_reef_residualplot.png", 
       plot = gg_C_rubescens_reef, 
       width = 6, 
       height = 4, 
       dpi = 600, 
       units = "in")


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_C_rubescens_BOSS" = predict(Choerodon_rubescens_BOSS, preddf, type = "response", se.fit = T),
                "p_C_rubescens_BRUV" = predict(Choerodon_rubescens_BRUV, preddf, type = "response", se.fit = T),
                "p_C_auricularis_BOSS" = predict(Coris_auricularis_BOSS, preddf, type = "response", se.fit = T),
                "p_C_auricularis_BRUV" = predict(Coris_auricularis_BRUV, preddf, type = "response", se.fit = T),
                "p_S_cyanolaemus_BOSS" = predict(Suezichthys_cyanolaemus_BOSS, preddf, type = "response", se.fit = T),
                "p_S_cyanolaemus_BRUV" = predict(Suezichthys_cyanolaemus_BRUV, preddf, type = "response", se.fit = T),
                "p_L_miniatus_BOSS" = predict(Lethrinus_miniatus_BOSSBRUV, preddf_boss, type = "response", se.fit = T),
                "p_L_miniatus_BRUV" = predict(Lethrinus_miniatus_BOSSBRUV, preddf_bruv, type = "response", se.fit = T))
                

prasts <- rast(preddf %>% dplyr::select(x, y, starts_with("p_")),
                        crs = "epsg:4326") 
plot(prasts)

saveRDS(preddf, paste0("outputs/Abrolhos/fish/", name, "_predicted-fish.RDS"))

