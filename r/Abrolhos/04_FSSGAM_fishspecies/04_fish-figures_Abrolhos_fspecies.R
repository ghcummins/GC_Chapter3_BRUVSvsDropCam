###
# Project: Gabby PhD Ch3 Abrolhos  Habitat & Fish
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

# Set your study name
name <- "Abrolhos"                                                      

bossdat <- readRDS(paste0("outputs/Abrolhos/fish/", name, "_BOSS_predicted_fish.RDS")) %>%
  # dplyr::rename(species.richness = p_richness) %>%
  # mutate(z = abs(z))%>%
  glimpse()

bruvdat <- readRDS(paste0("outputs/Abrolhos/fish/", name, "_BRUV_predicted_fish.RDS")) %>%
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
habi    <- readRDS(paste(paste0('data/staging/PtCloates/PtCloates_habitat-bathy-derivatives.rds')) )%>%
  glimpse()

# Load the coastal waters boundary
cwatr  <- st_read('data/spatial/shapefiles/amb_coastal_waters_limit.shp')              

# Load bathymetry data
bathdf <- readRDS(paste(paste0('data/spatial/rasters/',                         
                               name), 'ga_bathy.rds', sep = "_"))

npz_cols <- scale_colour_manual(values = c("National Park Zone" = "#7bbc63"),
                                name = "Australian Marine Parks")

#CHANGE BELOW
# p.neb <- readPNG("data/images/Parapercis nebulosa.png")
# p.neb_grob <- rasterGrob(p.neb, width = unit(1, "cm"), height = unit(0.5, "cm"), interpolate = TRUE)

#Build elements for plot 1; BOSS C rubescens
p1 <- ggplot() +
  geom_tile(data = bossdat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_C_rubescens.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
    geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
 print(p1)
png(filename = "plots/Abrolhos/Abrolhos_BOSS_C_rubescens_z_reef.png", 
      
      
         width = 8, height = 4, res = 600, units = "in")        
p1# Change the dimensions here as necessary
dev.off()
  
#BRUV
p1.5 <- ggplot() +
  geom_tile(data = bruvdat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_C_rubescens.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
print(p1.5)
png(filename = "plots/Abrolhos/Abrolhos_BRUV_C_rubescens_z_reef.png", 
    
    
    width = 8, height = 4, res = 600, units = "in")        
p1.5# Change the dimensions here as necessary
dev.off()

  
#Build elements for plot 1; BOSS relative abundance Coris auricularis 
p2 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_C_auricularis_BOSS.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
print(p2)
png(filename = "plots/Abrolhos/Abrolhos_BOSS_C_auricularis.png", 
    
    
    width = 8, height = 4, res = 600, units = "in")        
p2# Change the dimensions here as necessary
dev.off()

#Build elements for plot 1; BRUV relative abundance Coris auricularis 
p2.5 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_C_auricularis_BRUV.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
print(p2.5)
png(filename = "plots/Abrolhos/Abrolhos_BRUV_C_auricularis.png", 
    
    
    width = 8, height = 4, res = 600, units = "in")        
p2.5# Change the dimensions here as necessary
dev.off()

l.min <- readPNG("data/images/Lethrinus miniatus.png")
l.min_grob <- rasterGrob(l.min, width = unit(1.5, "cm"), height = unit(0.75, "cm"), interpolate = TRUE)
 
#Build elements for plot 1; BOSS relative abundance L miniatus
p3 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_L_miniatus_BOSS.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
print(p3)
png(filename = "plots/Abrolhos/Abrolhos_BOSS_L_miniatus.png", 
    
    
    width = 8, height = 4, res = 600, units = "in")        
p3# Change the dimensions here as necessary
dev.off()
 
#Build elements for plot 1; BRUV relative abundance L miniatus
p3.5 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_L_miniatus_BRUV.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
print(p3.5)
png(filename = "plots/Abrolhos/Abrolhos_BRUV_L_miniatus.png", 
    
    
    width = 8, height = 4, res = 600, units = "in")        
p3.5# Change the dimensions here as necessary
dev.off()


#Build elements for plot 1; BOSS relative abundance S cyanolaemus
p4 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_S_cyanolaemus_BOSS.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BOSS\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
print(p4)
png(filename = "plots/Abrolhos/Abrolhos_BOSS_S_cyanolaemus.png", 
    
    
    width = 8, height = 4, res = 600, units = "in")        
p4# Change the dimensions here as necessary
dev.off()

#Build elements for plot 1; BOSS relative abundance S cyanolaemus
p4.5 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 26 & z <=189), aes(x, y, fill = p_S_cyanolaemus_BRUV.fit)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.169, 113.65),                              # Set plot limits
           ylim = c(-28.15, -27.95)) +
  labs(x = NULL, y = NULL, fill = "BRUV\nrelative\nabundance",                                    # Labels  
       colour = NULL) +
  annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
           y = c(-28.078038504, -28.078038504, -28.078038504), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  # annotate("text", x = 113.4, y = -28.0, label = expression(italic("C. rubescens")),
  #          hjust = 0, size = 2.5, colour = "#000000") + # Add italicized text at top left
  # annotation_custom(p.neb_grob, xmin = 113.39, xmax = 113.45, ymin = -22.70, ymax = -22.68) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),# Center title horizontally
    legend.title = element_text(hjust = 0.3, size =7),
    legend.text = element_text(size =6),
    axis.text = element_text(size =7)
  )
print(p4.5)
png(filename = "plots/Abrolhos/Abrolhos_BRUV_S_cyanolaemus.png", 
    
    
    width = 8, height = 4, res = 600, units = "in")        
p4.5# Change the dimensions here as necessary
dev.off()










#########BELOW HERE = OLD SCRIPT
# # Set cropping extent - larger than most zoomed out plot
# e <- ext(112, 116, -30, -26)
# 
# # Load necessary spatial files
# sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# # Australian outline and state and commonwealth marine parks
# aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
#   dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
# st_crs(aus) <- gdacrs
# ausc <- st_crop(aus, e)
# 
# # Commonwealth parks
# aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
# mpa <- st_crop(aumpa, e) %>% 
#   dplyr::mutate(Area_KM2 = as.character(Area_KM2)) %>%
#   dplyr::filter(!ResName %in% c("Perth Canyon"),
#                 !Area_KM2 %in% c("2107.97750059", "14448.0532117",
#                                  "22493.1162347", "14231.1456468"))
# # Reorder levels so everything plots nicely
# mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Recreational Use Zone",
#                                                 "Multiple Use Zone", 
#                                                 "Special Purpose Zone",
#                                                 "National Park Zone"))
# npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones
# 
# # State parks
# wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")                    # All aus mpas
# st_crs(wampa) <- st_crs(aumpa)
# wampa <- st_crop(wampa, e)                                                      # Crop to the study area
# # simplify zone names
# wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
# wampa$waname <- gsub(" [1-4]", "", wampa$waname)
# wampa$waname[wampa$NAME == "Ngari Capes"]     <- "General Use"
# wampa$waname <- dplyr::recode(wampa$waname, 
#                               "General Use" = "General Use Zone",
#                               # "MMA" = "Marine Management Area",
#                               # "Recreation Area" = "Recreation Zone",
#                               # "Conservation Area" = "Sanctuary Zone",
#                               "Special Purpose Zone (Shore Based Activities)" = 
#                                 "Special Purpose Zone\n(Shore Based Activities)")
# 
# unique(wampa$waname)
# sanc <- wampa %>%
#   dplyr::filter(waname %in% "Sanctuary Zone")
# 
# # Coastal waters limit
# cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
# cwatr <- st_crop(cwatr, e)  

# bathdf <- readRDS(paste0("data/spatial/rasters/", name, "_bathymetry.rds")) %>%
#   glimpse()

# resp.vars <- c(">Lm", "<Lm", "0.5-1Lm", "1-1.25Lm",
#                "1.25-1.50Lm", ">1.5Lm","Species.richness")
# 
# for (i in 1:length(resp.vars)) {
#   use.dat <- dat %>%
#     dplyr::select(x, y, resp.vars[i])
#   names(use.dat)[3] <- "Abundance"
  
  p <- ggplot() +
    geom_tile(data = use.dat, aes(x, y, fill = Abundance)) +
    scale_fill_viridis(option = "D", direction = -1) +
    labs(fill = resp.vars[i], x = NULL, y = NULL) +
    new_scale_fill() +                     
    geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
    geom_sf(data = npz, fill = NA, colour = "#7bbc63", size = 0.2) +                          # Add national park zones
    geom_sf(data = sanc, fill = NA, colour = "#bfd054", size = 0.1) +                         # Add national park zones
    geom_sf(data = cwatr, fill = NA, colour = "red", size = 0.3) +
    coord_sf(xlim = c(min(dat$x), max(dat$x)),
             ylim = c(min(dat$y), max(dat$y))) +
    theme_minimal()
  assign(paste0("gg_", resp.vars[i]), p)
}

gg_grid <- gg_Species.richness + `gg_>Lm` + `gg_<Lm` + `gg_0.5-1Lm` + `gg_1-1.25Lm` + 
  `gg_1.25-1.50Lm` + `gg_>1.5Lm` +
  plot_layout(ncol = 2, nrow = 4)

png(filename = paste(paste("plots/fish", name, sep = "/"),               
                     "fish-predictions.png", sep = "_"),
    width = 10, height = 12, res = 300, units = "in")                             # Change the dimensions here as necessary
gg_grid
dev.off()
  