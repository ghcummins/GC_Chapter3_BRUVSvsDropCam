###
# Project: Gabby PhD Ch3 Abrolhos  Fish & habitat
# Data:    BRUV & BOSS fish MaxN
# Task:    Fish model prediction
# author:  Gabby Cummins
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

name <- "SouthWest"  # set study name

# read in
dat1 <- readRDS("data/staging/SwC/Southwest.fish.dat.maxn.rds")%>%
  #dplyr::mutate(reef =rock+inverts)%>%
  mutate(z = abs(z), name = scientific, scientific = paste(method,scientific,sep=".")) %>%
  #mutate(status = ifelse(is.na(status), "No-take", status)) %>%
  glimpse()

unique(dat1$scientific)

###RESIDUAL PLOTS FOR SOUTHWEST
dat1$method <- as.factor(dat1$method)

##Neeed to add fish pics here once found
p.bis <- readJPEG("data/images/swc/Pseudolabrus biserialis-3cm.jpg")
p.bis_grob <- rasterGrob(p.bis, width = unit(2.5, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

o.lin <- readJPEG("data/images/swc/Opthalmolepis lineolatus-3cm.jpg")
o.lin_grob <- rasterGrob(o.lin, width = unit(2.5, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

c.aur <- readJPEG("data/images/swc/Coris auricularis 3cm.jpg")
c.aur_grob <- rasterGrob(c.aur, width = unit(3.0, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

n.obl <- readJPEG("data/images/swc/Neatypus obliquus-3cmL.jpg")
n.obl_grob <- rasterGrob(n.obl, width = unit(2.75, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

##Pseudolabrus_biserialis BOSS BRUV
Pseudolabrus_biserialis_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method) + method,
                                    data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Labridae Pseudolabrus biserialis","BRUV.Labridae Pseudolabrus biserialis")),
                                    family = tw())
summary(Pseudolabrus_biserialis_BOSSBRUV)


dat_total_pb <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Labridae Pseudolabrus biserialis","BRUV.Labridae Pseudolabrus biserialis"))

testdata_pb <- expand.grid(reef = seq(min(dat_total_pb$reef), max(dat_total_pb$reef), length.out = 20),
                            method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

P.biserlialis.fits <- predict.gam(Pseudolabrus_biserialis_BOSSBRUV, newdata = testdata_pb, type = 'response', se.fit = T)

predicts_pb_reef <- testdata_pb %>%
  data.frame(P.biserlialis.fits) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

predicts_pb_reef <- predicts_pb_reef %>%
  mutate(method = factor(method, levels = c("BRUV", "BOSS")))

testdata1_pb <- expand.grid(method = c("BOSS", "BRUV"), 
                            reef = mean(Pseudolabrus_biserialis_BOSSBRUV$model$reef)) %>%
                            
  distinct() %>%
  glimpse() 

P.b.fits <- predict.gam(Pseudolabrus_biserialis_BOSSBRUV, newdata = testdata1_pb, type = 'response', se.fit = T)

predicts_pb_method <- testdata1_pb %>%
  data.frame(P.b.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

#add custom text for fish name on plot
p.bis_text <- textGrob(label = expression(italic("P. biserialis")), x = 0, y = 0, 
                     just = "left", gp = gpar(col = "#000000", fontsize = 11))

# #add rug data ie raw data
pbis.dat <- dat1 %>%
  filter(name == "Labridae Pseudolabrus biserialis")

#Plot P biserialis residual abundance by reef
gg_P_biserialis_reef <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_pb_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_pb_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_pb_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_pb_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = pbis.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) +  # Rug plot on the bottom
  # geom_point(data = pbis.dat, x = )
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
   theme_classic() +
  annotation_custom(p.bis_grob, xmin = -0.43, xmax = -0.33, ymin = -Inf, ymax = Inf) +
  annotation_custom(p.bis_text, xmin = -0.49, xmax = -0.39, ymin = 2.7, ymax = 2.7) + # Text annotation outside the plot
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 2.0, "cm" ))+
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
gg_P_biserialis_reef


## Same as above but for Opthalmolepsis lineolatus
Opthalmolepsis_lineolatus_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method)  + method,
                                    data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Labridae Ophthalmolepis lineolatus","BRUV.Labridae Ophthalmolepis lineolatus")),
                                    family = tw())
summary(Opthalmolepsis_lineolatus_BOSSBRUV)

dat_total_ol <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Labridae Ophthalmolepis lineolatus", "BRUV.Labridae Ophthalmolepis lineolatus"))

testdata_ol <- expand.grid(reef = seq(min(dat_total_ol$reef), max(dat_total_ol$reef), length.out = 20),
                           method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

O.lineolatus.fits <- predict.gam(Opthalmolepsis_lineolatus_BOSSBRUV, newdata = testdata_ol, type = 'response', se.fit = T)

predicts_ol_reef <- testdata_ol %>%
  data.frame(O.lineolatus.fits) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

#swap so BRUV is on top of the legend
predicts_ol_reef <- predicts_ol_reef %>%
  mutate(method = factor(method, levels = c("BRUV", "BOSS")))

testdata1_ol <- expand.grid(method = c("BOSS", "BRUV"),
                            reef = mean(Opthalmolepsis_lineolatus_BOSSBRUV$model$reef))%>%
  distinct() %>%
  glimpse()

O.l.fits <- predict.gam(Opthalmolepsis_lineolatus_BOSSBRUV, newdata = testdata1_ol, type = 'response', se.fit = T)

predicts_ol_method <- testdata1_ol %>%
  data.frame(O.l.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

#add custom text for fish name
 o.lin_text <- textGrob(label = expression(italic("O. lineolatus")), x = 0, y = 0, 
                        just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add rug data ie raw data
olin.dat <- dat1 %>%
  filter(name == "Labridae Ophthalmolepis lineolatus")

#Plot Ophthalmolepis lineolatus residual abundance by reef
gg_O_lineolatus_reef <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_ol_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_ol_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_ol_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_ol_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = olin.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) + 
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  annotation_custom(o.lin_grob, xmin = -0.45, xmax = -0.35, ymin = -Inf, ymax = Inf) +
  annotation_custom(o.lin_text, xmin = -0.50, xmax = -0.50, ymin = 1.9, ymax = 1.9) +
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
gg_O_lineolatus_reef


## Same as above but for Coris auricularis
Coris_auricularis_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method) +  method,
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

#swap so BRUV is on top of the legend
predicts_ca_reef <- predicts_ca_reef %>%
  mutate(method = factor(method, levels = c("BRUV", "BOSS")))

testdata1_ca <- expand.grid(method = c("BOSS", "BRUV"), 
                            reef = mean(Coris_auricularis_BOSSBRUV$model$reef))%>%
  distinct() %>%
  glimpse() 

C.a.fits <- predict.gam(Coris_auricularis_BOSSBRUV, newdata = testdata1_ca, type = 'response', se.fit = T)

predicts_ca_method <- testdata1_ca %>%
  data.frame(C.a.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

# #add custom text for fish name
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
  annotation_custom(c.aur_grob, xmin = -0.45, xmax = -0.35, ymin = -Inf, ymax = Inf) +
  annotation_custom(c.aur_text, xmin = -0.50, xmax = -0.40, ymin = 7, ymax = 7) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 3.5, "cm" ))+
  #ylim(0,50)+
  labs(x = "Depth", y = "Abundance", colour = "Method", fill = "Method") +
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

# # Save gg_total_z as a PNG file
# ggsave(filename = "plots/Abrolhos/BOSSBRUV_Cauricularis_depth_residualplot.png", 
#        plot = gg_C_auricularis_z, 
#        width = 6, 
#        height = 4, 
#        dpi = 600, 
#        units = "in")

## Same as above but for Neatypus obliquus 
Neatypus_obliquus_BOSSBRUV <- gam(number ~ s(reef, k=3, bs = "cr", by = method) + method,
                                          data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Scorpididae Neatypus obliquus","BRUV.Scorpididae Neatypus obliquus")),
                                          family = tw())
summary(Neatypus_obliquus_BOSSBRUV)

dat_total_no <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Scorpididae Neatypus obliquus","BRUV.Scorpididae Neatypus obliquus"))

testdata_no <- expand.grid(reef = seq(min(dat_total_no$reef), max(dat_total_no$reef), length.out = 20),
                           method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

N.obliquus.fits <- predict.gam(Neatypus_obliquus_BOSSBRUV, newdata = testdata_ol, type = 'response', se.fit = T)

predicts_no_reef <- testdata_no %>%
  data.frame(N.obliquus.fits) %>%
  group_by(reef, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

#swap so BRUV is on top of the legend
predicts_no_reef <- predicts_no_reef %>%
  mutate(method = factor(method, levels = c("BRUV", "BOSS")))

testdata1_no <- expand.grid(method = c("BOSS", "BRUV"),
                            reef = mean(Neatypus_obliquus_BOSSBRUV$model$reef))%>%
  distinct() %>%
  glimpse()

N.o.fits <- predict.gam(Neatypus_obliquus_BOSSBRUV, newdata = testdata1_no, type = 'response', se.fit = T)

predicts_no_method <- testdata1_no %>%
  data.frame(N.o.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

#add custom text for fish name
 n.obl_text <- textGrob(label = expression(italic("N. obliquus")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add rug data ie raw data
nobl.dat <- dat1 %>%
  filter(name == "Scorpididae Neatypus obliquus") 

#Plot Neatypus obliquus residual abundance by reef
gg_N_obliquus_reef <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_no_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_no_reef, aes(x = reef, y = number, group=method, colour=method))+
  geom_line(data = predicts_no_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_no_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = nobl.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) + 
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  annotation_custom(n.obl_grob, xmin = -0.45, xmax = -0.35, ymin = -Inf, ymax = Inf) +
  annotation_custom(n.obl_text, xmin = -0.50, xmax = -0.40, ymin = 4.4, ymax = 4.4) +
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
gg_N_obliquus_reef

#join altogether
p_swc_reefonly <- gg_P_biserialis_reef +  gg_O_lineolatus_reef +  gg_C_auricularis_reef  +  gg_N_obliquus_reef +  plot_layout(ncol = 1)
p_swc_reefonly

#save
ggsave(filename = "plots/SwC/reefonly/BOSSBRUV_Southwest_reefonly_effectsplots5.png", 
       plot = p_swc_reefonly, 
       width = 7, 
       height = 16, 
       dpi = 600, 
       units = "in")







#######
#Note below file taken from output of Ch2
preddf <- readRDS("data/spatial/rasters/raw bathymetry/SouthWest_spatial_habitat_predictions.rds") %>%
  # dplyr::rename(reef = pinverts.fit + pmacroal)%>% ###DONT NEED
  ga.clean.names()%>%
  mutate(z =abs(z))

#creating boss and bruv dfs
preddf_boss <- preddf %>%
  dplyr::mutate(method = "BOSS")

preddf_bruv <- preddf %>%
  dplyr::mutate(method = "BRUV")

preddf_new <- bind_rows(preddf_boss, preddf_bruv)

# predict, rasterise and plot ### CHANGE TO se.fit = TRUE fo individual fish species!
predicted_fish <- cbind(preddf_new, 
                        "p_P_biserialis" = predict(Pseudolabrus_biserialis_BOSSBRUV, preddf_new, type = "response", se.fit = T),
                        "p_O_lineolatus" = predict(Opthalmolepsis_lineolatus_BOSSBRUV, preddf_new, type = "response", se.fit = T),
                        "p_C_auricularis" = predict(Coris_auricularis_BOSSBRUV, preddf_new, type = "response", se.fit = T),
                        "p_N_obliquus" = predict(Neatypus_obliquus_BOSSBRUV, preddf_new, type = "response", se.fit = T))

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

saveRDS(boss_fish, paste0("outputs/SwC/fish/", name, "_BOSS_predicted-fish.RDS"))

saveRDS(bruv_fish, paste0("outputs/SwC/fish/", name, "_BRUV_predicted-fish.RDS"))
