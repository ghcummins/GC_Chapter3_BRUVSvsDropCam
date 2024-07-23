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
p.neb_text <- textGrob(label = expression(italic("Parapercis nebulosa")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add custom text for fish name on plot
l.min_text <- textGrob(label = expression(italic("Lethrinus miniatus")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add custom text for fish name on plot
g.sp1_text <- textGrob(label = expression(italic("Gymnocranius sp1")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#add custom text for fish name on plot
p.nag_text <- textGrob(label = expression(italic("Pentapodus nagasakiensis")), x = 0, y = 0, 
                       just = "left", gp = gpar(col = "#000000", fontsize = 11))

#adding in fish images
l.min <- readPNG("data/images/Lethrinus miniatus.png")
l.min_grob <- rasterGrob(l.min, width = unit(3, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

p.neb <- readPNG("data/images/Parapercis nebulosa.png")
p.neb_grob <- rasterGrob(p.neb, width = unit(1.5, "cm"), height = unit(0.75, "cm"), interpolate = TRUE)

g.sp1 <- readPNG("data/images/Gymnocranius grandoculis.png")
g.sp1_grob <- rasterGrob(l.min, width = unit(3, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

p.nag <- readJPEG("data/images/Pentapodus_porosus.jpg")
p.nag_grob <- rasterGrob(l.min, width = unit(3, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

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
  annotation_custom(l.min_grob, xmin = 1, xmax = 0.5, ymin = 10.5, ymax = 11.5) +
  annotation_custom(l.min_text, xmin = 1, xmax = 0.5, ymin = 14, ymax = 14) + # Text annotation outside the plot
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
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  labs(title = "B",
       x = "Method",
       y = "Mean number of individuals per deployment") +
  annotation_custom(g.sp1_grob, xmin = 1, xmax = 0.5, ymin = 10.5, ymax = 11.5) +
  annotation_custom(g.sp1_text, xmin = 1, xmax = 0.5, ymin = 14, ymax = 14) + # Text annotation outside the plot
  theme_minimal()+
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank())
plot(g_sp1)

# look at top species ----
# maxn.sum <- maxn %>%
#   mutate(scientific = paste(genus, species, sep = " ")) %>%
#   group_by(scientific) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   top_n(10)%>%
#   ungroup()


