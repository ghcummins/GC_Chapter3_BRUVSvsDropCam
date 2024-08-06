###
# Project: Ch3 PhD Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Join PtCloates BOSS and BRUV, format data for fssGAM
# author:  Gabby, Claude, Brooke, Kingsley
# date:    October 2023
##

rm(list=ls())

# libraries----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)
library(tidyverse)
library(sf)
library(here)
library(leaflet)
library(png)
library(jpeg)

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "PtCloates"   # set study name ##

# load and join datasets
#MaxN
boss.maxn   <- read.csv("data/tidy/PtCloates/PtCloates_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS",
                sample=as.character(sample)) %>% 
  dplyr::mutate(unique_id = paste0(campaignid, sep="_", sample)) %>% 
      glimpse()
bruv.maxn <- read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
   glimpse()


#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  filter(scientific !="SUS SUS sus")%>%
    glimpse()

      
#Format data
dat.response <- maxn %>%
  filter(str_detect(scientific, "Pinguipedidae Parapercis nebulosa|Lethrinidae Lethrinus miniatus|Lethrinidae Gymnocranius sp1|Nemipteridae Pentapodus nagasakiensis"))%>%
  dplyr::select(-id)%>%
  group_by(sample,scientific,campaignid,latitude,longitude,method,unique_id) %>%
  summarise(number = sum(maxn))%>%
  ungroup()%>%
  mutate(response = paste(scientific, method, sep = "_")) %>%
  glimpse()

unique_names <- unique(dat.response[["response"]])
print(unique_names)


# #check if we want to get rid of zero's'
# fish <- dat.response %>%
#   filter(number>0) 

P_nebulosa <- dat.response %>%
  filter(str_detect(scientific, "Pinguipedidae Parapercis nebulosa"))

# P_nebulosa_BOSS <- P_nebulosa %>%
#   filter(str_detect(method, "BOSS"))
# 
# summary(P_nebulosa_BOSS)

L_miniatus <- dat.response %>%
  filter(str_detect(scientific, "Lethrinidae Lethrinus miniatus"))

G_sp1 <- dat.response %>%
  filter(str_detect(scientific, "Lethrinidae Gymnocranius sp1"))

P_nag <- dat.response %>%
  filter(str_detect(scientific, "Nemipteridae Pentapodus nagasakiensis"))

#add custom text for fish name on plot
p.neb_text <- textGrob(label = expression(italic("Parapercis nebulosa***")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add custom text for fish name on plot
l.min_text <- textGrob(label = expression(italic("Lethrinus miniatus***")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add custom text for fish name on plot
g.sp1_text <- textGrob(label = expression(italic("Gymnocranius sp1***")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add custom text for fish name on plot
p.nag_text <- textGrob(label = expression(italic("Pentapodus nagasakiensis***")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#adding in fish images
l.min <- readPNG("data/images/Lethrinus miniatus.png")
l.min_grob <- rasterGrob(l.min, width = unit(3, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

p.neb <- readPNG("data/images/Parapercis nebulosa.png")
p.neb_grob <- rasterGrob(p.neb, width = unit(1.5, "cm"), height = unit(0.75, "cm"), interpolate = TRUE)

g.sp1 <- readPNG("data/images/Gymnocranius grandoculis.png")
g.sp1_grob <- rasterGrob(g.sp1, width = unit(2.75, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

p.nag <- readJPEG("data/images/Pentapodus_porosus.jpg")
p.nag_grob <- rasterGrob(p.nag, width = unit(2.5, "cm"), height = unit(1, "cm"), interpolate = TRUE)

# Define custom colors for jitter points
jitter_colors <- c("BOSS" = "#E69F00", "BRUV" = "#56B4E9")

# Create a boxplot with jittered points and mean overlay
p_neb <- ggplot(P_nebulosa, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  labs(title = "A",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  annotation_custom(p.neb_grob, xmin = 1, xmax = 0.5, ymin = 4.3, ymax = 4.8) +
  annotation_custom(p.neb_text, xmin = 1, xmax = 0.5, ymin = 5.1, ymax = 5.1) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(p_neb)

# Create a boxplot with jittered points and mean overlay
l_min <- ggplot(L_miniatus, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  labs(title = "B",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(l.min_grob, xmin = 1, xmax = 0.7, ymin = 11.5, ymax = 12.5) +
  annotation_custom(l.min_text, xmin = 1, xmax = 0.5, ymin = 14.2, ymax = 14.2) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(l_min)

# Create a boxplot with jittered points and mean overlay
g_sp1 <- ggplot(G_sp1, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  #scale_y_continuous(limits = c(0, 10), breaks = seq(0, 14, by = 2)) +
  labs(title = "C",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  annotation_custom(g.sp1_grob, xmin = 1, xmax = 0.4, ymin = 6.8, ymax = 7.6) +
  annotation_custom(g.sp1_text, xmin = 1, xmax = 0.5, ymin = 8.2, ymax = 68.2) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(g_sp1)

# Create a boxplot with jittered points and mean overlay
p_nag <- ggplot(P_nag, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 3)) +
  labs(title = "D",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(p.nag_grob, xmin = 1, xmax = 0.4, ymin = 12.5, ymax = 15) +
  annotation_custom(p.nag_text, xmin = 1, xmax = 0.5, ymin = 15.2, ymax = 15.2) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(p_nag)


fish_boxplots_PtCloates <- p_neb + l_min + g_sp1 +  p_nag + (plot_layout(ncol=4))
fish_boxplots_PtCloates

ggsave(filename = "plots/boxplots/fish_boxplots_PtCloates2.png", 
       plot = fish_boxplots_PtCloates, 
       width = 22, 
       height = 6, 
       dpi = 600, 
       units = "in")


#AS ABOV BUT FOR ABROLHOS
## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "Abrolhos"   # set study name ##

# load and join datasets
#MaxN
abrolhos.boss.maxn   <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS",
                sample=as.character(sample)) %>% 
  dplyr::mutate(unique_id = paste0(campaignid, sep="_", sample)) %>% 
  glimpse()
abrolhos.bruv.maxn <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_stereo-BRUVs.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  glimpse()


#join
abrolhos.maxn <- bind_rows(abrolhos.boss.maxn,abrolhos.bruv.maxn)%>%
  filter(scientific !="SUS SUS sus")%>%
  glimpse()

#Format data
abrolhos.dat.response <- abrolhos.maxn %>%
  filter(str_detect(scientific, "Labridae Coris auricularis|Lethrinidae Lethrinus miniatus|Labridae Choerodon rubescens|Labridae Suezichthys cyanolaemus"))%>%
  dplyr::select(-id)%>%
  group_by(sample,scientific,campaignid,latitude,longitude,method,unique_id) %>%
  summarise(number = sum(maxn))%>%
  ungroup()%>%
  mutate(response = paste(scientific, method, sep = "_")) %>%
  glimpse()

unique_names <- unique(abrolhos.dat.response[["response"]])
print(unique_names)

#format species at Abrolhos
ab_L_miniatus <- abrolhos.dat.response %>%
  filter(str_detect(scientific, "Lethrinidae Lethrinus miniatus"))

ab_C_rubescens <- abrolhos.dat.response %>%
  filter(str_detect(scientific, "Labridae Choerodon rubescens"))

ab_C_auricularis <- abrolhos.dat.response %>%
  filter(str_detect(scientific, "Labridae Coris auricularis"))

ab_S_cyanolaemus <- abrolhos.dat.response %>%
  filter(str_detect(scientific, "Labridae Suezichthys cyanolaemus"))

#read in fish pics for Abrolhos (ab)
ab.l.min <- readPNG("data/images/Lethrinus miniatus.png")
ab.l.min_grob <- rasterGrob(l.min, width = unit(2.75, "cm"), height = unit(1.25, "cm"), interpolate = TRUE)

ab.c.rub <- readJPEG("data/images/abrolhos/Choerodon rubescens 3cm.jpg")
ab.c.rub_grob <- rasterGrob(ab.c.rub, width = unit(3, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

ab.c.aur <- readJPEG("data/images/abrolhos/Coris auricularis 3cm.jpg")
ab.c.aur_grob <- rasterGrob(ab.c.aur, width = unit(2.75, "cm"), height = unit(1.1, "cm"), interpolate = TRUE)

ab.s.cya <- readJPEG("data/images/abrolhos/suezichthys.jpg")
ab.s.cya_grob <- rasterGrob(ab.s.cya, width = unit(2.75, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

#create custom text of fish species for plots
ab.l.min_text <- textGrob(label = expression(italic("Lethrinus miniatus")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

ab.c.rub_text <- textGrob(label = expression(italic("Choerodon rubescens")), x = 0, y = 0, 
                          just = "left", gp = gpar(col = "#000000", fontsize = 11))

ab.c.aur_text <- textGrob(label = expression(italic("Coris auricularis**")), x = 0, y = 0, 
                          just = "left", gp = gpar(col = "#000000", fontsize = 11))

ab.s.cya_text <- textGrob(label = expression(italic("Suezichthys cyanolaemus")), x = 0, y = 0, 
                          just = "left", gp = gpar(col = "#000000", fontsize = 11))

# Create a boxplot with jittered points and mean overlay for L miniatus
ab_l_min <- ggplot(ab_L_miniatus, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  scale_y_continuous(limits = c(0, 20)) +
  labs(title = "E",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(ab.l.min_grob , xmin = 0.5, xmax = 1.1, ymin = 17, ymax = 19) +
  annotation_custom(ab.l.min_text, xmin = 0.5, xmax = 1, ymin = 20.2, ymax = 20.2) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(ab_l_min)

# Create a boxplot with jittered points and mean overlay for C rubescens
ab_c_rub <- ggplot(ab_C_rubescens, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
 # scale_y_continuous(limits = c(0, 17)) +
  labs(title = "F",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(ab.c.rub_grob, xmin = 0.6, xmax = 1, ymin = 5, ymax = 6) +
  annotation_custom(ab.c.rub_text, xmin = 0.5, xmax = 0.7, ymin = 6.2, ymax = 6.2) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(ab_c_rub)

# Create a boxplot with jittered points and mean overlay for C rubescens
ab_c_aur <- ggplot(ab_C_auricularis, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
   #scale_y_continuous(limits = c(0, 10)) +
  labs(title = "G",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(ab.c.aur_grob, xmin = 0.5, xmax = 1, ymin = 145, ymax = 150) +
  annotation_custom(ab.c.aur_text, xmin = 0.5, xmax = 0.5, ymin = 165, ymax = 165) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(ab_c_aur)

# Create a boxplot with jittered points and mean overlay 
ab_s_cya <- ggplot(ab_S_cyanolaemus, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  #scale_y_continuous(limits = c(0, 10)) +
  labs(title = "H",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(ab.s.cya_grob, xmin = 0.5, xmax = 1, ymin = 4.2, ymax = 5) +
  annotation_custom(ab.s.cya_text, xmin = 0.5, xmax = 0.5, ymin = 5.2, ymax = 5.2) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(ab_s_cya)

#AS ABOV BUT FOR CAPES REGION
# load and join datasets
#MaxN
swc.boss.maxn   <- read.csv("data/staging/SwC/2020-2021_south-west_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS",
                sample=as.character(sample))%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))%>%
  dplyr::mutate(name = paste(genus, species))%>%
  glimpse()

swc.bruv.maxn <- read.csv("data/staging/SwC/2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  plyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))%>%
  dplyr::mutate(name = paste(genus, species))%>%
  glimpse()

#join
swc.bossbruvmaxn <- bind_rows(swc.boss.maxn, swc.bruv.maxn)%>%
  filter(scientific !="SUS SUS sus")%>%
  glimpse()


swc.maxn <- swc.bossbruvmaxn %>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05)

#Format data
swc.dat.response <- swc.maxn %>%
  filter(str_detect(scientific, "Labridae Pseudolabrus biserialis|Labridae Ophthalmolepis lineolatus|Labridae Coris auricularis|Scorpididae Neatypus obliquus"))%>%
  dplyr::select(-id)%>%
  group_by(sample,scientific,campaignid,latitude,longitude,method,unique_id) %>%
  summarise(number = sum(maxn))%>%
  ungroup()%>%
  mutate(response = paste(scientific, method, sep = "_")) %>%
  glimpse()

#format species at South-West
swc_P_biserialis <- swc.dat.response %>%
  filter(str_detect(scientific, "Labridae Pseudolabrus biserialis"))

swc_O_lineoloatus <- swc.dat.response %>%
  filter(str_detect(scientific, "Labridae Ophthalmolepis lineolatus"))

swc_C_auricularis <- swc.dat.response %>%
  filter(str_detect(scientific, "Labridae Coris auricularis"))

swc_N_obliquus <- swc.dat.response %>%
  filter(str_detect(scientific, "Scorpididae Neatypus obliquus"))

##read in fish pics for SouthWest
##Fish images
p.bis <- readJPEG("data/images/swc/Pseudolabrus biserialis-3cm.jpg")
p.bis_grob <- rasterGrob(p.bis, width = unit(2.5, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

o.lin <- readJPEG("data/images/swc/Opthalmolepis lineolatus-3cm.jpg")
o.lin_grob <- rasterGrob(o.lin, width = unit(2.5, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

c.aur <- readJPEG("data/images/swc/Coris auricularis 3cm.jpg")
c.aur_grob <- rasterGrob(c.aur, width = unit(3.0, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

n.obl <- readJPEG("data/images/swc/Neatypus obliquus-3cmL.jpg")
n.obl_grob <- rasterGrob(n.obl, width = unit(2.75, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

#create custom text of fish species for plots
swc.p.bis_text <- textGrob(label = expression(italic("Pseudolabrus biserialis")), x = 0, y = 0, 
                          just = "left", gp = gpar(col = "#000000", fontsize = 11))

swc.o.lin_text <- textGrob(label = expression(italic("Ophthalmolepis lineolatus***")), x = 0, y = 0, 
                           just = "left", gp = gpar(col = "#000000", fontsize = 11))

swc.c.aur_text <- textGrob(label = expression(italic("Coris auricularis***")), x = 0, y = 0, 
                           just = "left", gp = gpar(col = "#000000", fontsize = 11))

swc.n.obl_text <- textGrob(label = expression(italic("Neatypus obliquus**")), x = 0, y = 0, 
                           just = "left", gp = gpar(col = "#000000", fontsize = 11))

# Create a boxplot with jittered points and mean overlay for C rubescens
swc_p_bis <- ggplot(swc_P_biserialis, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  #scale_y_continuous(limits = c(0, 10)) +
  labs(title = "I",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(p.bis_grob, xmin = 0.5, xmax = 1, ymin = 18, ymax = 20) +
  annotation_custom(swc.p.bis_text, xmin = 0.5, xmax = 0.5, ymin = 21, ymax = 21) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(swc_p_bis)

# Create a boxplot with jittered points and mean overlay for C rubescens
swc_o_lin <- ggplot(swc_O_lineoloatus, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  #scale_y_continuous(limits = c(0, 10)) +
  labs(title = "J",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(o.lin_grob, xmin = 0.4, xmax = 1, ymin = 8.5, ymax = 10) +
  annotation_custom(swc.o.lin_text, xmin = 0.45, xmax = 0.5, ymin = 10.5, ymax = 10.5) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(swc_o_lin)

# Create a boxplot with jittered points and mean overlay for Coris auricularis
swc_c_aur <- ggplot(swc_C_auricularis, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
 # scale_y_continuous(limits = c(0, 10)) +
  labs(title = "K",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(c.aur_grob, xmin = 0.5, xmax = 1, ymin = 100, ymax = 110) +
  annotation_custom(swc.c.aur_text, xmin = 0.5, xmax = 0.5, ymin = 115, ymax = 115) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(swc_c_aur)

# Create a boxplot with jittered points and mean overlay 
swc_n_obl <- ggplot(swc_N_obliquus, aes(x = method, y = number)) +
  geom_boxplot(alpha = 0.5, outlier.shape= NA) +
  geom_jitter(aes(color = method), height = 0, alpha = 0.7) +  # Add jittering with default colors
  geom_point(stat = "summary", fun = mean, shape = 22, size = 3, color = "black") +  # Overlay mean as diamond
  scale_color_manual(values = jitter_colors) +  # Apply custom colors to jitter points
  #scale_y_continuous(limits = c(0, 10)) +
  labs(title = "L",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  coord_cartesian(clip = 'off') +  
  annotation_custom(n.obl_grob, xmin = 0.5, xmax = 1, ymin = 52, ymax = 60) +
  annotation_custom(swc.n.obl_text, xmin = 0.45, xmax = 0.5, ymin = 64, ymax = 64) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(swc_n_obl)


###CREATING THE COMPOSITE FIGURE
fish_box_plots <- p_neb + l_min + g_sp1 + p_nag + ab_l_min + ab_c_rub + ab_c_aur + ab_s_cya + 
  swc_p_bis + swc_o_lin + swc_c_aur + swc_n_obl + (plot_layout(ncol=4))
fish_box_plots

ggsave(filename = "plots/boxplots/allfish_boxplots7.png", 
       plot = fish_box_plots, 
       width = 22, 
       height = 12, 
       dpi = 600, 
       units = "in")

#looking at vabundance differences
swc_N_obliquus %>%
  group_by(method) %>%
  summarise(
    mean_number = mean(number),
    median_number = median(number)
  )

swc_C_auricularis %>%
  group_by(method) %>%
  summarise(
    mean_number = mean(number),
    median_number = median(number)
  )

swc_O_lineoloatus %>%
  group_by(method) %>%
  summarise(
    mean_number = mean(number),
    median_number = median(number)
  )

ab_C_rubescens %>%
  group_by(method) %>%
  summarise(
    mean_number = mean(number),
    median_number = median(number)
  )

ab_C_auricularis %>%
  group_by(method) %>%
  summarise(
    mean_number = mean(number),
    median_number = median(number)
  )
ab_S_cyanolaemus %>%
  group_by(method) %>%
  summarise(
    mean_number = mean(number),
    median_number = median(number)
  )
# Calculate variance by method
variance_stats <- swc_N_obliquus %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- swc_C_auricularis %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- swc_O_lineoloatus %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- swc_P_biserialis %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- P_nebulosa %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- L_miniatus %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- ab_C_auricularis %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)


variance_stats <- ab_C_rubescens %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- ab_S_cyanolaemus %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

variance_stats <- ab_L_miniatus %>%
  group_by(method) %>%
  summarise(
    variance_number = var(number)
  )

# Print variance statistics
print(variance_stats)

###### OK Variance WAS ALWAYS higher for BRUV than BOSS

### NOW to test for significant of each Figure.
library(tweedie)
library(statmod)
glm.p_nebulosa <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = P_nebulosa)
summary(glm.p_nebulosa)

glm.l_miniatus <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = L_miniatus)
summary(glm.l_miniatus)

# glm.g_sp1 <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = G_sp1)
# summary(glm.g_sp1)
# 
# glm.p_nag <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = P_nag)
# summary(glm.p_nag)

##species at Abrolhos
glm.ab.l_miniatus <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = ab_L_miniatus)
summary(glm.ab.l_miniatus)

glm.ab.c_rubescens <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = ab_C_rubescens)
summary(glm.ab.c_rubescens)

glm.ab.c_auricularis <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = ab_C_auricularis)
summary(glm.ab.c_auricularis)

glm.ab.s_cyanolaemus <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = ab_S_cyanolaemus)
summary(glm.ab.s_cyanolaemus)

### species at southwest
glm.sw.p_biserialis <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = swc_P_biserialis)
summary(glm.sw.p_biserialis)

glm.sw.o_lineoloatus <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = swc_O_lineoloatus)
summary(glm.sw.o_lineoloatus)

glm.sw.c_auricularis <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = swc_C_auricularis)
summary(glm.sw.c_auricularis)

glm.sw.n_obliquus <- glm(number ~ method, family = tweedie(var.power = 1, link.power = 1), data = swc_N_obliquus)
summary(glm.sw.n_obliquus)
