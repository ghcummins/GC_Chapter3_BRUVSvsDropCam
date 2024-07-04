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
library(patchwork)
library(png)
library(jpeg)
library(grid)

name <- "Abrolhos"  # set study name

# read in
dat1 <- readRDS("data/staging/Abrolhos/Abrolhos.fish.dat.maxn.rds")%>%
  # dplyr::mutate(reef =rock+inverts)%>%
  mutate(z = abs(z), scientific = paste(method,scientific,sep=".")) %>%
  # mutate(reef = if_else(reef>1, 1, reef)) %>%
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

preddf <- readRDS("data/spatial/rasters/raw bathymetry/Abrolhos_spatial_habitat_predictions.rds") %>%
  ga.clean.names()%>%
  mutate(reef = prock.fit + pinverts.fit + pmacroalg.fit)%>%
  mutate(reef = if_else(reef>1, 1, reef)) %>%
  mutate(z =abs(z)) 
   
 #hist(preddf$reef)

# preddf <- phab
# preddf$method <- "BRUV"
# preddf_bruv <-preddf
# preddf_boss <-preddf
# preddf_boss$method <- "BOSS"


# presp <- vect(preddf, geom = c("x", "y"))
# preddf <- cbind(terra::extract(test, presp), preddf)
 

# # Re-set the predictors for modeling----
pred.vars <- c("z", "reef", "aspect",
               "tpi","roughness","detrended") 


# use formula from top model from FSSGam model selection
# Greater than size of maturity openness+recfish+reef+UCUR+VCUR
unique(dat1$scientific)

#Relative abundance of individual fish species
# Choerodon_rubescens_BOSS <-gam(number ~ s(reef, k=3, bs = "cr"),
#                   data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Labridae Choerodon rubescens"),
#                 family = tw())
# summary(Choerodon_rubescens_BOSS)
# 
# Choerodon_rubescens_BRUV <-gam(number ~ s(z, k=3, bs = "cr"),
#                 data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Labridae Choerodon rubescens"),
#                 family = tw())
# summary(Choerodon_rubescens_BRUV)
# 
# Coris_auricularis_BOSS <- gam(number ~ s(z, k = 3, bs = "cr"),   
#                   data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Labridae Coris auricularis"), 
#                   family = tw())
# summary(Coris_auricularis_BOSS)
# 
# Coris_auricularis_BRUV <- gam(number ~s(reef, k = 3, bs = "cr"),
#                        data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Labridae Coris auricularis"),
#                        family = tw())
# summary(Coris_auricularis_BRUV)
# 
# Suezichthys_cyanolaemus_BOSS <- gam(number ~ s(z, k=3, bs = "cr"),
#                                     data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Labridae Suezichthys cyanolaemus"),
#                                     family = tw())
# summary(Suezichthys_cyanolaemus_BOSS)
# 
# Suezichthys_cyanolaemus_BRUV <- gam(number ~ s(aspect, k=3, bs = "cr") + s(roughness, k=3, bs = "cr"),
#                                     data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Labridae Suezichthys cyanolaemus"),
#                                     family = tw())
# summary(Suezichthys_cyanolaemus_BRUV)



# Lethrinus_miniatus_BOSS <- gam(number ~ s(z, k=3, bs = "cr", by = method),
#                                data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus")),
#                                family = tw())
# summary(Lethrinus_miniatus_BOSS)
# 
# Lethrinus_miniatus_BRUV <- gam(number ~ s(z, k=3, bs = "cr") + s(reef, k=3, bs = "cr"),
#                                data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Lethrinidae Lethrinus miniatus"),
#                                family = tw())
# summary(Lethrinus_miniatus_BRUV)

#adding in fish pics
l.min <- readPNG("data/images/Lethrinus miniatus.png")
l.min_grob <- rasterGrob(l.min, width = unit(2.5, "cm"), height = unit(1.25, "cm"), interpolate = TRUE)

c.rub <- readJPEG("data/images/abrolhos/Choerodon rubescens 3cm.jpg")
c.rub_grob <- rasterGrob(c.rub, width = unit(3.0, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

c.aur <-  readJPEG("data/images/abrolhos/Coris auricularis 3cm.jpg")
c.aur_grob <- rasterGrob(c.aur, width = unit(2.5, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

s.cya <- readJPEG("data/images/abrolhos/suezichthys.jpg")
s.cya_grob <- rasterGrob(s.cya, width = unit(1.5, "cm"), height = unit(0.75, "cm"), interpolate = TRUE)

###30/05/2024 NEW MODELLING FOR RESIDUAL PLOTS
dat1$method <- as.factor(dat1$method)

##Lethrinus miniatus
Lethrinus_miniatus_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method) + method,
                                   data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus","BRUV.Lethrinidae Lethrinus miniatus")),
                                   family = tw())
summary(Lethrinus_miniatus_BOSSBRUV)

dat_total <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus", "BRUV.Lethrinidae Lethrinus miniatus"))

testdata <- expand.grid(reef = seq(min(dat_total$reef), max(dat_total$reef), length.out = 20),
              # reef = mean(Lethrinus_miniatus_BOSSBRUV$model$reef),
              method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

L.miniatus.fits <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata, type = 'response', se.fit = T)

predicts_total_reef <- testdata %>%
  data.frame(L.miniatus.fits) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup()

testdata1 <- expand.grid(method = c("BOSS", "BRUV"), 
                         reef = mean(Lethrinus_miniatus_BOSSBRUV$model$reef))%>%
                         # reef = mean(Lethrinus_miniatus_BOSSBRUV$model$reef)) %>%
  distinct() %>%
  glimpse() 

L.m.fits <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata1, type = 'response', se.fit = T)

predicts_total_method <- testdata1 %>%
  data.frame(L.m.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

# testdata2 <- expand.grid(reef = seq(min(dat_total$reef), max(dat_total$reef), length.out = 20),
#                          method = c("BOSS", "BRUV"),
#                          z = mean(Lethrinus_miniatus_BOSSBRUV$model$z)) %>%
#   distinct() %>%
#   glimpse()
# 
# L.m.fits2 <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata2, type = 'response', se.fit = T)
# 
# predicts_total_reef <- testdata2 %>%
#   data.frame(L.m.fits2) %>%
#   group_by(reef, method) %>%
#   summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
#   ungroup()
  
#add custom text for fish name
l.min_text <- textGrob(label = expression(italic("L. miniatus")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add rug data ie raw data
lmin.dat <- dat1 %>%
  filter(name == "Lethrinidae Lethrinus miniatus")

#Plot L.miniatus residual abundance by reef
gg_total_reef <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_total_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_total_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_total_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_total_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = lmin.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) +  
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  # annotate("text", x = -55, y = 2, label = expression(italic("L. miniatus")),
  #           hjust = 0, size = 2.5, colour = "#000000") +
  annotation_custom(l.min_grob, xmin = -0.25, xmax = -0.35, ymin = -Inf, ymax = Inf) +
  annotation_custom(l.min_text, xmin = -0.28, xmax = -0.38, ymin = 2, ymax = 2) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 3.0, "cm" ))+
  #ylim(0,50)+
  labs(x = "Reef", y = "Abundance", colour = "Method", fill = "Method") +
  scale_colour_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00")) +
  scale_fill_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00"))+
   theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11, angle = 90, vjust = 1.5, margin = margin(4,4,0,5)),  # Y axis title size
    axis.text.x = element_text(size = 10, margin = margin(0, 0, 3, 0)),   # X axis text size
    axis.text.y = element_text(size = 10, margin = margin(0, 3, 0, 0)),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9) # Legend text size
    )
gg_total_reef


# # Save gg_total_reef as a PNG file
# ggsave(filename = "plots/Abrolhos/BOSSBRUV_Lminiatus_reef_residualplot.png", 
#        plot = gg_total_reef, 
#        width = 6, 
#        height = 4, 
#        dpi = 600, 
#        units = "in")

## Same as above but for Choerdon rubescens
Choerodon_rubescens_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method)  + method,
                                   data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Labridae Choerodon rubescens","BRUV.Labridae Choerodon rubescens")),
                                   family = tw())
summary(Choerodon_rubescens_BOSSBRUV)

dat_total_cr <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Labridae Choerodon rubescens", "BRUV.Labridae Choerodon rubescens"))

testdata_cr <- expand.grid(reef = seq(min(dat_total_cr$reef), max(dat_total_cr$reef), length.out = 20),
                        # reef = mean(Choerodon_rubescens_BOSSBRUV$model$reef),
                        method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

C.rubescens.fits <- predict.gam(Choerodon_rubescens_BOSSBRUV, newdata = testdata_cr, type = 'response', se.fit = T)

predicts_cr_reef <- testdata_cr %>%
  data.frame(C.rubescens.fits) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

testdata1_cr <- expand.grid(method = c("BOSS", "BRUV"), 
                         reef = mean(Choerodon_rubescens_BOSSBRUV$model$reef))%>%
                         # reef = mean(Choerodon_rubescens_BOSSBRUV$model$reef)) %>%
  distinct() %>%
  glimpse() 
 
C.r.fits <- predict.gam(Choerodon_rubescens_BOSSBRUV, newdata = testdata1_cr, type = 'response', se.fit = T)

predicts_cr_method <- testdata1_cr %>%
  data.frame(C.r.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

#add custom text for fish name
c.rub_text <- textGrob(label = expression(italic("C. rubescens")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add rug data ie raw data
crub.dat <- dat1 %>%
  filter(name == "Labridae Choerodon rubescens")


# testdata2_cr <- expand.grid(reef = seq(min(dat_total_cr$reef), max(dat_total_cr$reef), length.out = 20),
#                          method = c("BOSS", "BRUV"),
#                          z = mean(Choerodon_rubescens_BOSSBRUV$model$z)) %>%
#   distinct() %>%
#   glimpse()
# 
# C.r.fits2 <- predict.gam(Choerodon_rubescens_BOSSBRUV, newdata = testdata2_cr, type = 'response', se.fit = T)
# 
# predicts_cr_reef <- testdata2_cr %>%
#   data.frame(C.r.fits2) %>%
#   group_by(reef, method) %>%
#   summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
#   ungroup()

#Plot C rubescens residual abundance by reef
gg_C_rubescens_reef <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_cr_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_cr_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_cr_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_cr_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = crub.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) +  
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  # annotate("text", x = -55, y = 2, label = expression(italic("L. miniatus")),
  #           hjust = 0, size = 2.5, colour = "#000000") +
  annotation_custom(c.rub_grob, xmin = -0.25, xmax = -0.35, ymin = -Inf, ymax = Inf) +
  annotation_custom(c.rub_text, xmin = -0.30, xmax = -0.40, ymin = 0.9, ymax = 0.9) + 
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 3.0, "cm" ))+
  #ylim(0,50)+
  labs(x = "Reef", y = "Abundance", colour = "Method", fill = "Method") +
  scale_colour_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00")) +
  scale_fill_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00"))+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11, angle = 90, vjust = 1.5, margin = margin(4,4,0,5)),  # Y axis title size
    axis.text.x = element_text(size = 10, margin = margin(0, 0, 3, 0)),   # X axis text size
    axis.text.y = element_text(size = 10, margin = margin(0, 3, 0, 0)),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9) # Legend text size
  )
gg_C_rubescens_reef


# # Save gg_total_reef as a PNG file
# ggsave(filename = "plots/Abrolhos/BOSSBRUV_Crubescens_reef_residualplot.png", 
#        plot = gg_C_rubescens_reef, 
#        width = 6, 
#        height = 4, 
#        dpi = 600, 
#        units = "in")

## Same as above but for Coris auricularis
Coris_auricularis_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method)  + method,
                                    data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Labridae Coris auricularis","BRUV.Labridae Coris auricularis")),
                                    family = tw())
summary(Coris_auricularis_BOSSBRUV)

dat_total_ca <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Labridae Coris auricularis", "BRUV.Labridae Coris auricularis"))

testdata_ca <- expand.grid(reef = seq(min(dat_total_ca$reef), max(dat_total_ca$reef), length.out = 20),
                                  method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

C.auricularis.fits <- predict.gam(Coris_auricularis_BOSSBRUV, newdata = testdata_ca, type = 'response', se.fit = T)

predicts_ca_reef <- testdata_ca %>%
  data.frame(C.auricularis.fits) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

testdata1_ca <- expand.grid(method = c("BOSS", "BRUV"), 
                            reef = mean(Coris_auricularis_BOSSBRUV$model$reef))%>%
                            # reef = mean(Coris_auricularis_BOSSBRUV$model$reef)) %>%
  distinct() %>%
  glimpse() 

C.a.fits <- predict.gam(Coris_auricularis_BOSSBRUV, newdata = testdata1_ca, type = 'response', se.fit = T)

predicts_ca_method <- testdata1_ca %>%
  data.frame(C.a.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

#add custom text for fish name
c.aur_text <- textGrob(label = expression(italic("C. auricularis")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add rug data ie raw data
caur.dat <- dat1 %>%
  filter(name == "Labridae Coris auricularis")

#Plot C auricularis residual abundance by reef
gg_C_auricularis_reef <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_ca_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha=0.2) +
  geom_line(data = predicts_ca_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_ca_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_ca_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = caur.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) +
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  annotation_custom(c.aur_grob, xmin = -0.25, xmax = -0.35, ymin = -Inf, ymax = Inf) +
  annotation_custom(c.aur_text, xmin = -0.28, xmax = -0.38, ymin = 30, ymax = 30) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 3.5, "cm" ))+
  #ylim(0,50)+
  labs(x = "Reef", y = "Abundance", colour = "Method", fill = "Method") +
  scale_colour_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00")) +
  scale_fill_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00"))+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11, angle = 90, vjust = 1.5, margin = margin(4,4,0,5)),  # Y axis title size
    axis.text.x = element_text(size = 10, margin = margin(0, 0, 3, 0)),   # X axis text size
    axis.text.y = element_text(size = 10, margin = margin(0, 3, 0, 0)),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9) # Legend text size
  )
gg_C_auricularis_reef

# # Save gg_total_reef as a PNG file
# ggsave(filename = "plots/Abrolhos/BOSSBRUV_Cauricularis_reef_residualplot.png", 
#        plot = gg_C_auricularis_reef, 
#        width = 6, 
#        height = 4, 
#        dpi = 600, 
#        units = "in")

## Same as above but for Suezichthys cyanolaemus	
Suezichthys_cyanolaemus_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method)  + method,
                                    data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Labridae Suezichthys cyanolaemus","BRUV.Labridae Suezichthys cyanolaemus")),
                                    family = tw())
summary(Suezichthys_cyanolaemus_BOSSBRUV)

dat_total_sc <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Labridae Suezichthys cyanolaemus","BRUV.Labridae Suezichthys cyanolaemus"))

testdata_sc <- expand.grid(reef = seq(min(dat_total_sc$reef), max(dat_total_sc$reef), length.out = 20),
                           # reef = mean(Suezichthys_cyanolaemus_BOSSBRUV$model$reef),
                           method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

S.cyanolaemus.fits <- predict.gam(Suezichthys_cyanolaemus_BOSSBRUV, newdata = testdata_sc, type = 'response', se.fit = T)

predicts_sc_reef <- testdata_sc %>%
  data.frame(S.cyanolaemus.fits) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

testdata1_sc <- expand.grid(method = c("BOSS", "BRUV"), 
                            reef = mean(Suezichthys_cyanolaemus_BOSSBRUV$model$reef))%>%
                            # reef = mean(Suezichthys_cyanolaemus_BOSSBRUV$model$reef)) %>%
  distinct() %>%
  glimpse() 

S.c.fits <- predict.gam(Suezichthys_cyanolaemus_BOSSBRUV, newdata = testdata1_sc, type = 'response', se.fit = T)

predicts_sc_method <- testdata1_sc %>%
  data.frame(S.c.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>% 
  ungroup()

#add custom text for fish name
s.cya_text <- textGrob(label = expression(italic("S. cyanolaemus")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add rug data ie raw data
scya.dat <- dat1 %>%
  filter(name == "Labridae Suezichthys cyanolaemus")

#Plot S cyanolaemus residual abundance by reef
gg_S_cyanolaemus_reef <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_sc_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_sc_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_sc_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_sc_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = scya.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) +
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  annotation_custom(s.cya_grob, xmin = -0.25, xmax = -0.35, ymin = -Inf, ymax = Inf) +
  annotation_custom(s.cya_text, xmin = -0.30, xmax = -0.40, ymin = 0.8, ymax = 0.8) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 2.7, "cm" ))+
  #ylim(0,50)+
  labs(x = "Reef", y = "Abundance", colour = "Method", fill = "Method") +
  scale_colour_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00")) +
  scale_fill_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00"))+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    axis.title.x = element_text(size = 11),  # X axis title size
    axis.title.y = element_text(size = 11, angle = 90, vjust = 1.5, margin = margin(4,4,0,5)),  # Y axis title size
    axis.text.x = element_text(size = 10, margin = margin(0, 0, 3, 0)),   # X axis text size
    axis.text.y = element_text(size = 10, margin = margin(0, 3, 0, 0)),   # Y axis text size
    legend.title = element_text(size = 11),  # Legend title size
    legend.text = element_text(size = 9) # Legend text size
  )
gg_S_cyanolaemus_reef

# # Save gg_total_reef as a PNG file
# ggsave(filename = "plots/Abrolhos/BOSSBRUV_Scyanolaemus_reef_residualplot.png", 
#        plot = gg_S_cyanolaemus_reef, 
#        width = 6, 
#        height = 4, 
#        dpi = 600, 
#        units = "in")

p_Abrolhos_reefonly <- gg_total_reef  + gg_C_rubescens_reef  + gg_C_auricularis_reef  + gg_S_cyanolaemus_reef + (plot_layout(ncol=1))
p_Abrolhos_reefonly

ggsave(filename = "plots/Abrolhos/Effects_reef/BOSSBRUV_Abrolhos_reefonlyplots3.png", 
       plot = p_Abrolhos_reefonly, 
       width = 8.5, 
       height = 16, 
       dpi = 600, 
       units = "in")

# #adding in fish pics
# l.min <- readPNG("data/images/Lethrinus miniatus.png")
# l.min_grob <- rasterGrob(l.min, width = unit(2.5, "cm"), height = unit(1.25, "cm"), interpolate = TRUE)
# 
# c.rub <- readJPEG("data/images/abrolhos/Choerodon rubescens 3cm.jpg")
# c.rub_grob <- rasterGrob(c.rub, width = unit(3.0, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)
#   
# c.aur <-  readJPEG("data/images/abrolhos/Coris auricularis 3cm.jpg")
# C.aur_grob <- rasterGrob(c.aur, width = unit(2.0, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)
#   
# s.cya <- readJPEG("data/images/abrolhos/suezichthys.jpg")
# s.cya_grob <- rasterGrob(s.cya, width = unit(1.5, "cm"), height = unit(0.75, "cm"), interpolate = TRUE)

preddf_boss <- preddf %>%
  dplyr::mutate(method = "BOSS")

preddf_bruv <- preddf %>%
  dplyr::mutate(method = "BRUV")

preddf_new <- bind_rows(preddf_boss, preddf_bruv)

# predict, rasterise and plot
predicted_fish <- cbind(preddf_new, 
                "p_C_rubescens" = predict(Choerodon_rubescens_BOSSBRUV, preddf_new, type = "response", se.fit = T),
                # "p_C_rubescens_BRUV" = predict(Choerodon_rubescens_BOSSBRUV, preddf, type = "response", se.fit = T),
                "p_C_auricularis" = predict(Coris_auricularis_BOSSBRUV, preddf_new, type = "response", se.fit = T),
                # "p_C_auricularis_BRUV" = predict(Coris_auricularis_BOSSBRUV, preddf, type = "response", se.fit = T),
                "p_S_cyanolaemus" = predict(Suezichthys_cyanolaemus_BOSSBRUV, preddf_new, type = "response", se.fit = T),
                # "p_S_cyanolaemus_BRUV" = predict(Suezichthys_cyanolaemus_BOSSBRUV, preddf, type = "response", se.fit = T),
                # "p_L_miniatus_BOSS" = predict(Lethrinus_miniatus_BOSSBRUV, preddf_boss, type = "response", se.fit = T),
                "p_L_miniatus" = predict(Lethrinus_miniatus_BOSSBRUV, preddf_new, type = "response", se.fit = T))

prasts_boss <- predicted_fish %>%
  dplyr::filter(method %in% "BOSS") %>%
  dplyr::select(x, y, z, starts_with("p_")) %>%
  rast(crs = "epsg:4326")
plot(prasts_boss)

boss_fish <- as.data.frame(prasts_boss, xy = T)

prasts_bruv <- predicted_fish %>%
  dplyr::filter(method %in% "BRUV") %>%
  dplyr::select(x, y, z, starts_with("p_")) %>%
  rast(crs = "epsg:4326")
plot(prasts_bruv)

bruv_fish <- as.data.frame(prasts_bruv, xy = T)

# prasts_bruv <- rast(preddf %>% dplyr::select(x, y, starts_with("p_")) %>%
#                                                dplyr::filter(method %in% "BRUV"),
#                     crs = "epsg:4326")
# 
# prasts <- rast(preddf %>% dplyr::select(x, y, starts_with("p_")),
#                         crs = "epsg:4326") 
# plot(prasts)

saveRDS(boss_fish, paste0("outputs/Abrolhos/fish/", name, "_BOSS_predicted_fish.RDS"))

saveRDS(bruv_fish, paste0("outputs/Abrolhos/fish/", name, "_BRUV_predicted_fish.RDS"))
