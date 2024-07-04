###
# Project: Gabby PhD Ch3 PtCloates  Habitat & Fish
# Data:    BRUV & BOSS fish MaxN
# Task:    Fish model prediction for nebulsa and miniatus at Pt Cloates
# author:  Claude Spencer & Gabby Cummins
# date:    June 2024
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

name <- "PtCloates"  # set study name

# read in
dat1 <- readRDS("data/staging/PtCloates/PtCloates.fish.dat.maxn.rds")%>%
  #dplyr::mutate(reef =rock+inverts)%>%
  mutate(z = abs(z), name = scientific, scientific = paste(method,scientific,sep=".")) %>%
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
preddf <- readRDS("data/spatial/rasters/raw bathymetry/PtCloates_spatial_habitat_predictions.rds") %>%
  dplyr::rename(reef = pinverts.fit)%>%
  ga.clean.names()%>%
  mutate(z =abs(z))

 
# presp <- vect(preddf, geom = c("x", "y"))
# preddf <- cbind(terra::extract(test, presp), preddf)
 

# # Re-set the predictors for modeling----
pred.vars <- c("z", "reef", "aspect",
               "tpi","roughness","detrended") 


# use formula from top model from FSSGam model selection
# Greater than size of maturity openness+recfish+reef+UCUR+VCUR
unique(dat1$scientific)

#OLD WAY FFSGAM Parapercis nebulosa ##PUT IN TOP MODEL PREDICTORS should k = 5? as ok at 80% zeros
# Parapercis_nebulosa_BRUV <-gam(number ~ s(reef, k=3, bs = "cr") + s(z, k = 3, bs = "cr"), 
#                   data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Pinguipedidae Parapercis nebulosa"),
#                 family = tw())
# summary(Parapercis_nebulosa_BRUV)
# 
# Parapercis_nebulosa_BOSS <-gam(number ~ s(detrended, k=3, bs = "cr")+ s(reef, k = 3, bs = "cr"), 
#                 data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Pinguipedidae Parapercis nebulosa"),
#                 family = tw())
# summary(Parapercis_nebulosa_BOSS)
# 
# 
# #Lethrinus miniatus
# Lethrinus_miniatus_BRUV <- gam(number ~ s(reef, k = 3, bs = "cr") +s(z, k = 3, bs = "cr"),  
#                   data = dat1 %>% dplyr::filter(scientific %in% "BRUV.Lethrinidae Lethrinus miniatus"), 
#                   family = tw())
# summary(Lethrinus_miniatus_BRUV)
# 
# Lethrinus_miniatus_BOSS <- gam(number ~s(detrended, k = 3, bs = "cr") +s(z, k = 3, bs = "cr"), 
#                        data = dat1 %>% dplyr::filter(scientific %in% "BOSS.Lethrinidae Lethrinus miniatus"),
#                        family = tw())
# summary(Lethrinus_miniatus_BOSS)

###RESIDUAL PLOTS FOR NINGALOO
dat1$method <- as.factor(dat1$method)

#adding in fish images
l.min <- readPNG("data/images/Lethrinus miniatus.png")
l.min_grob <- rasterGrob(l.min, width = unit(3, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

p.neb <- readPNG("data/images/Parapercis nebulosa.png")
p.neb_grob <- rasterGrob(p.neb, width = unit(1.5, "cm"), height = unit(0.75, "cm"), interpolate = TRUE)


## Parapercis nebulosa BOSS BRUV
Parapercis_nebulosa_BOSSBRUV <- gam(number ~ s(z, k=3, bs = "cr", by = method)  + method,
                                    data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Pinguipedidae Parapercis nebulosa","BRUV.Pinguipedidae Parapercis nebulosa")),
                                    family = tw())
summary(Parapercis_nebulosa_BOSSBRUV)

dat_total_pn <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Pinguipedidae Parapercis nebulosa", "BRUV.Pinguipedidae Parapercis nebulosa"))

testdata_pn <- expand.grid(z = seq(min(dat_total_pn$z), max(dat_total_pn$z), length.out = 20),
                           # reef = mean(Parapercis_nebulosa_BOSSBRUV$model$reef),
                           method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

P.nebulosa.fits <- predict.gam(Parapercis_nebulosa_BOSSBRUV, newdata = testdata_pn, type = 'response', se.fit = T)

predicts_pn_z <- testdata_pn %>%
  data.frame(P.nebulosa.fits) %>%
  group_by(z, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup() 

predicts_pn_z <- predicts_pn_z %>%
  mutate(method = factor(method, levels = c("BRUV", "BOSS")))

testdata1_pn <- expand.grid(method = c("BOSS", "BRUV"), 
                            z = mean(Parapercis_nebulosa_BOSSBRUV$model$z)) %>%
                              distinct() %>%
  glimpse() 

P.n.fits <- predict.gam(Parapercis_nebulosa_BOSSBRUV, newdata = testdata1_pn, type = 'response', se.fit = T)

predicts_pn_method <- testdata1_pn %>%
  data.frame(P.n.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

#add custom text for fish name on plot
p.neb_text <- textGrob(label = expression(italic("P. nebulosa")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))
#add rug data ie raw data
pneb.dat <- dat1 %>%
  filter(name == "Pinguipedidae Parapercis nebulosa")

#Plot P nebulosa residual abundance by depth (z)
gg_P_nebulosa_z <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_pn_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_pn_z, aes(x = z, y = number, group=method, colour=method))+
  geom_line(data = predicts_pn_z, aes(x = z, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_pn_z, aes(x = z, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = pneb.dat, aes(x = z, colour = method), sides = "b", alpha = 0.5) +  # Rug plot on the bottom
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
  theme_classic() +
  annotation_custom(p.neb_grob, xmin = -4, xmax = 1, ymin = -Inf, ymax = Inf) +
  annotation_custom(p.neb_text, xmin = -13, xmax = -3, ymin = 0.6, ymax = 0.6) + # Text annotation outside the plot
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 2.0, "cm" ))+
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
gg_P_nebulosa_z

# ##Plot P nebulosa residual abundance by reef
# gg_P_nebulosa_reef <- ggplot() + 
#   geom_ribbon(data = predicts_pn_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
#   # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
#   geom_line(data = predicts_pn_reef, aes(x = reef, y = number, group=method, colour=method))+
#   geom_line(data = predicts_pn_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
#   geom_line(data = predicts_pn_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
#   geom_rug(data = pneb.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) +  # Rug plot on the bottom
#   theme_classic() +
#   #ylim(0,50)+
#   labs(x = "Reef", y = "Abundance", colour = "Method", fill = "Method") +
#   scale_colour_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00")) +
#   scale_fill_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00"))+
#   theme(
#     axis.title.x = element_text(size = 11),  # X axis title size
#     axis.title.y = element_text(size = 11),  # Y axis title size
#     axis.text.x = element_text(size = 10),   # X axis text size
#     axis.text.y = element_text(size = 10),   # Y axis text size
#     legend.title = element_text(size = 11),  # Legend title size
#     legend.text = element_text(size = 9)    # Legend text size
#   )
# gg_P_nebulosa_reef

##Lethrinus miniatus
Lethrinus_miniatus_BOSSBRUV <- gam(number ~ s(z, k=3, bs = "cr", by = method) + method,
                                   data = dat1 %>% dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus","BRUV.Lethrinidae Lethrinus miniatus")),
                                   family = tw())
summary(Lethrinus_miniatus_BOSSBRUV)

dat_total <- dat1 %>%
  dplyr::filter(scientific %in% c("BOSS.Lethrinidae Lethrinus miniatus", "BRUV.Lethrinidae Lethrinus miniatus"))

testdata <- expand.grid(z = seq(min(dat_total$z), max(dat_total$z), length.out = 20),
                        method = c("BOSS", "BRUV")) %>%
  distinct() %>%
  glimpse

L.miniatus.fits <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata, type = 'response', se.fit = T)

predicts_total_z <- testdata %>%
  data.frame(L.miniatus.fits) %>%
  group_by(z, method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit))  %>%
  ungroup()

predicts_total_z <- predicts_total_z %>%
  mutate(method = factor(method, levels = c("BRUV", "BOSS")))

testdata1 <- expand.grid(method = c("BOSS", "BRUV"), 
                         z = mean(Lethrinus_miniatus_BOSSBRUV$model$z))%>%
  distinct() %>%
  glimpse() 

L.m.fits <- predict.gam(Lethrinus_miniatus_BOSSBRUV, newdata = testdata1, type = 'response', se.fit = T)

predicts_total_method <- testdata1 %>%
  data.frame(L.m.fits) %>%
  group_by(method) %>%
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

l.min_text <- textGrob(label = expression(italic("L. miniatus")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

lmin.dat <- dat1 %>%
  filter(name == "Lethrinidae Lethrinus miniatus")

#Plot L.miniatus residual abundance by depth (z)
gg_L_miniatus_z <- ggplot() + 
  # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
  geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
  geom_line(data = predicts_total_z, aes(x = z, y = number, group=method, colour=method))+
  geom_line(data = predicts_total_z, aes(x = z, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
  geom_line(data = predicts_total_z, aes(x = z, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
  geom_rug(data = lmin.dat, aes(x = z, colour = method), sides = "b", alpha = 0.5) +  # Rug plot on the bottom
  # geom_ribbon(data = predicts_total_z, aes(x = z, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method)) +
   theme_classic() +
    # annotate("text", x = -17, y = 0.5, label = expression(italic("L. miniatus")),
    #         hjust = 0, size = 3, colour = "#000000") +
  annotation_custom(l.min_grob, xmin = -4, xmax = 1, ymin = -Inf, ymax = Inf) +
  annotation_custom(l.min_text, xmin = -13, xmax = -3, ymin = 1.5, ymax = 1.5) + # Text annotation outside the plot
   theme_void() +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 3.3, "cm" ))+
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
gg_L_miniatus_z

# 
# # Save gg_total_z as a PNG file
# ggsave(filename = "plots/PtCloates/BOSSBRUV_Lminiatus_depth_residualplot_fish.png", 
#        plot = gg_total_z, 
#        width = 6, 
#        height = 4, 
#        dpi = 600, 
#        units = "in")

# #Plot L.miniatus residual abundance by reef
# gg_L_miniatus_reef <- ggplot() + 
#   geom_ribbon(data = predicts_total_reef, aes(x = reef, ymin = number - se.fit, ymax = number + se.fit, fill = method, group = method), alpha = 0.2) +
#   # geom_point(data = dat1, aes(x = z, y = number, group=method, fill=method), alpha = 0.2, size = 1, show.legend = T) +
#   geom_line(data = predicts_total_reef, aes(x = reef, y = number, group=method, colour=method))+
#   geom_line(data = predicts_total_reef, aes(x = reef, y = number - se.fit, group=method, colour=method), linetype = "dashed") +
#   geom_line(data = predicts_total_reef, aes(x = reef, y = number + se.fit, group=method, colour=method), linetype = "dashed") +
#   geom_rug(data = lmin.dat, aes(x = reef, colour = method), sides = "b", alpha = 0.5) +  # Rug plot on the bottom
#   theme_classic() +
#   #ylim(0,50)+
#   labs(x = "Reef", y = "Abundance", colour = "Method", fill = "Method") +
#   scale_colour_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00")) +
#   scale_fill_manual(values = c("BRUV" = "#56B4E9", "BOSS" = "#E69F00"))+
#   theme(
#     axis.title.x = element_text(size = 11),  # X axis title size
#     axis.title.y = element_text(size = 11),  # Y axis title size
#     axis.text.x = element_text(size = 10),   # X axis text size
#     axis.text.y = element_text(size = 10),   # Y axis text size
#     legend.title = element_text(size = 11),  # Legend title size
#     legend.text = element_text(size = 9)    # Legend text size
#   )
# gg_L_miniatus_reef

depthonly_PtCloates <- gg_P_nebulosa_z +  gg_L_miniatus_z + (plot_layout(ncol=1))
depthonly_PtCloates

ggsave(filename = "plots/PtCloates/Residual_plots/depthonly/depthonlyPtCloates3.png", 
       plot = depthonly_PtCloates, 
       width = 7, 
       height = 10, 
       dpi = 600, 
       units = "in")

#creating boss and bruv dfs
preddf_boss <- preddf %>%
  dplyr::mutate(method = "BOSS")

preddf_bruv <- preddf %>%
  dplyr::mutate(method = "BRUV")

preddf_new <- bind_rows(preddf_boss, preddf_bruv)


# predict, rasterise and plot ### CHANGE TO se.fit = TRUE fo individual fish species!
predicted_fish <- cbind(preddf_new, 
                "p_P_nebulosa" = predict(Parapercis_nebulosa_BOSSBRUV, preddf_new, type = "response", se.fit = T),
                #"p_P_nebulosa_BOSS" = predict(Parapercis_nebulosa_BOSS, preddf, type = "response", se.fit = T),
                "p_L_miniatus" = predict(Lethrinus_miniatus_BOSSBRUV, preddf_new, type = "response", se.fit = T))
                #"p_L_miniatus_BOSS" = predict(Lethrinus_miniatus_BOSS, preddf, type = "response", se.fit = T))
                
# # reduce prediction area to within sampled range
# preddf <- preddf %>
#   filter(z >= 215, z <= 71)

# prasts <- rast(preddf %>% dplyr::select(x, y, starts_with("p_")),
#                         crs = "epsg:4326") 
# plot(prasts)

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

saveRDS(boss_fish, paste0("outputs/PtCloates/fish/", name, "_BOSS_predicted-fish.RDS"))

saveRDS(bruv_fish, paste0("outputs/PtCloates/fish/", name, "_BRUV_predicted-fish.RDS"))
