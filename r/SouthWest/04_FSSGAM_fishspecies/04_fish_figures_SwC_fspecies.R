###
# Project: Gabby PhD Ch3 Capes Region Southwest  Habitat & Fish
# Data:    BRUV & BOSS fish MaxN
# Task:    Plot fish model predictions
# author:  Claude Spencer & Gabby Cummins
# date:    May 2024
##

# Clear your environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(scales)
library(dismo)
library(viridis)
library(patchwork)
library(png)
library(grid)
library(jpeg)

# Set your study name
name <- "SouthWest"     

bossdat <- readRDS(paste0("outputs/SwC/fish/", name, "_BOSS_predicted-fish.RDS")) %>%
  filter(x >= 114.72 & x <= 114.95 &       ###filter for only the BOSS in my area
           y >= -34.15 & y <= -34.05) %>%
  # dplyr::rename(species.richness = p_richness) %>%
  # mutate(z = abs(z))%>%
  glimpse()

bruvdat <- readRDS(paste0("outputs/SwC/fish/", name, "_BRUV_predicted-fish.RDS")) %>%
  filter(x >= 114.72 & x <= 114.95 &     ### filter for only the BRUV in my area
           y >= -34.15 & y <= -34.05) %>%
  # dplyr::rename(species.richness = p_richness) %>%
  # mutate(z = abs(z))%>%
  glimpse()

# Set CRS for shapefiles
wgscrs <- "+proj=longlat +datum=WGS84"        # Lat Long Projection


# Bring in spatial layers
# Load aus outline, state and commonwealth marine parks
aus     <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                    # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))

# aus     <- aus[aus$FEAT_CODE == "mainland" ]                                   # Add islands here if needed
aumpa   <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")   # All aus mpas
st_crs(aus) <- st_crs(aumpa)                                                    # Set CRS to match - WGS84 and GDA94 effectively the same
e <- ext(112, 116, -23, -22)                                                 # Change your extent here
mpa <- st_crop(aumpa, e)                                                        # All commonwealth zones in the study area
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Only National Park Zones in the study area


# Load tidy habitat data from 03_mergedata
habi    <- readRDS(paste(paste0('data/staging/habitat/SouthWest_habitat-bathy-derivatives.rds')) )%>%
  glimpse()

# Load the coastal waters boundary
cwatr  <- st_read('data/spatial/shapefiles/amb_coastal_waters_limit.shp')    

# Load the coastal waters boundary
cwatr  <- st_read('data/spatial/shapefiles/amb_coastal_waters_limit.shp')              

# Load bathymetry data
bathdf <- readRDS(paste(paste0('data/spatial/rasters/',                         
                               name), 'ga_bathy.rds', sep = "_"))

npz_cols <- scale_colour_manual(values = c("National Park Zone" = "#7bbc63"),
                                name = "Australian Marine Parks")

##Fish images
p.bis <- readJPEG("data/images/swc/Pseudolabrus biserialis-3cm.jpg")
p.bis_grob <- rasterGrob(p.bis, width = unit(2.5, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

o.lin <- readJPEG("data/images/swc/Opthalmolepis lineolatus-3cm.jpg")
o.lin_grob <- rasterGrob(o.lin, width = unit(2.5, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

c.aur <- readJPEG("data/images/swc/Coris auricularis 3cm.jpg")
c.aur_grob <- rasterGrob(c.aur, width = unit(3.0, "cm"), height = unit(1.0, "cm"), interpolate = TRUE)

n.obl <- readJPEG("data/images/swc/Neatypus obliquus-3cmL.jpg")
n.obl_grob <- rasterGrob(n.obl, width = unit(2.75, "cm"), height = unit(1.5, "cm"), interpolate = TRUE)

# Build elements for plot 1; BOSS P biserialis
p1 <- ggplot() +
  geom_tile(data = bossdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_P_biserialis.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p1)
png(filename = "plots/SwC/SouthWest_BOSS_P_biserialis1.png",
width = 8, height = 6, res = 600, units = "in")      ##SAVEOUT
p1# Change the dimensions here as necessary
dev.off()

# Build elements for plot 2; BRUV P biserialis
p2 <- ggplot() +
  geom_tile(data = bruvdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_P_biserialis.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p2)
png(filename = "plots/SwC/SouthWest_BRUV_P_biserialis.png",
    width = 8, height = 6, res = 600, units = "in")      ##SAVEOUT
p2# Change the dimensions here as necessary
dev.off()

# Build elements for plot 2; BOSS O lineolatus
p3 <- ggplot() +
  geom_tile(data = bossdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_O_lineolatus.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p3)
png(filename = "plots/SwC/SouthWest_BOSS_O_lineolatus.png",
    width = 8, height = 6, res = 600, units = "in")      ##SAVEOUT
p3# Change the dimensions here as necessary
dev.off()

# Build elements for plot 2; BRUV O lineolatus
p4 <- ggplot() +
  geom_tile(data = bruvdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_O_lineolatus.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p4)
png(filename = "plots/SwC/SouthWest_BRUV_O_lineolatus.png")

# Build elements for plot 2; BOSS C auricularis
p5 <- ggplot() +
  geom_tile(data = bossdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_C_auricularis.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p5)
png(filename = "plots/SwC/SouthWest_BOSS_C_auricularis.png")

# Build elements for plot 6; BRUV C auricularis
p6 <- ggplot() +
  geom_tile(data = bruvdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_C_auricularis.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p6)
png(filename = "plots/SwC/SouthWest_BRUV_C_auricularis.png")

# Build elements for plot 7; BOSS N obliquus
p7 <- ggplot() +
  geom_tile(data = bossdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_N_obliquus.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p7)
png(filename = "plots/SwC/SouthWest_BOSS_N_obliquus.png")

# Build elements for plot 8; BRUV N obliquus
p8 <- ggplot() +
  geom_tile(data = bruvdat %>% filter(z >= 35 & z <=133), aes(x, y, fill = p_N_obliquus.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(114.7, 115.1),                              # Set plot limits
           ylim = c(-34.2, -34.0)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(115.0, 114.79, 114.51),          # Add contour labels manually
           y = c(-34.04, -34.04, -34.04), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.17, y = -28.13, label = expression(italic("P. biserialis")),
  #          hjust = 0.1, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.bis_grob, xmin = 113.16, xmax = 113.24, ymin = -28.08, ymax = -28.13) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"),
    # text_family = "ArcherPro Book"),
    height = unit(1, "cm"),
    width = unit(1, "cm"))+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    #axis.line.x.bottom = element_line(colour = "black", linewidth = 0.5),
    #axis.line.y.left = element_line(colour = "black", linewidth = 0.5),
    axis.ticks = element_line(colour = "black")
  )
print(p6)
png(filename = "plots/SwC/SouthWest_BRUV_N_obliquus.png")