###
# Project: Gabby PhD Ch3 Abrolhos  Habitat & Fish
# Data:    BRUV & BOSS fish MaxN
# Task:    Plot fish model predictions
# author:  Claude Spencer & Gabby Cummins
# date:    November 2023
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

# Set your study name
name <- "PtCloates"                                                      

dat <- readRDS(paste0("outputs/PtCloates/", name, "_predicted-fish.RDS")) %>%
  # dplyr::rename(species.richness = p_richness) %>%
  mutate(z = abs(z))%>%
  rename(BRUV.ta = p_BOSS_ta) %>%
  mutate(BRUV.ta = ifelse(BRUV.ta < 0, 0, BRUV.ta)) %>%
  rename(BOSS.ta =p_BRUV_ta) %>%
  mutate(BOSS.ta = ifelse(BOSS.ta < 0, 0, BOSS.ta)) %>%
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
e <- ext(112, 116, -30, -26)                                                 # Change your extent here
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


#Build elements for plot 1; BRUV TOTAL ABUNDANCE
p1 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 71 & z <=215), aes(x, y, fill = BRUV.ta)) +
  scale_fill_viridis(direction =-1)+
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.4, 113.8),                              # Set plot limits
           ylim = c(-22.85, -22.75)) +
  labs(x = NULL, y = NULL, fill = "ΣMaxN",                                    # Labels  
       colour = NULL, title = "BRUV predicted fish abundance at PtCloates, Ningaloo") +
  annotate("text", x = c(113.65, 113.57, 113.51),          # Add contour labels manually
           y = c(-22.75, -22.75, -22.75), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5)  # Center title horizontally
  )
print(p1)
png(filename = "plots/PtCloates/PtCloates_BRUV_ta.png", 
      
      
         width = 8, height = 4, res = 300, units = "in")                             # Change the dimensions here as necessary
dev.off()
  
  
#Build elements for plot 1; BOSS TOTAL ABUNDANCE
p2 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 71 & z <=215), aes(x, y, fill = BOSS.ta)) +
  scale_fill_viridis(direction = -1, na.value = "transparent", limits= c(0,160))+
  #scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"), na.value = "transparent") +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.4, 113.8),                              # Set plot limits
           ylim = c(-22.85, -22.75)) +
  labs(x = NULL, y = NULL, fill = "ΣMaxN",                                    # Labels  
       colour = NULL, title = "BOSS predicted fish abundance at Pt Cloates, Ningaloo") +
  annotate("text", x = c(113.65, 113.57, 113.51),          # Add contour labels manually
           y = c(-22.75, -22.75, -22.75), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5)  # Center title horizontally
  )
print(p2)
png(filename = "plots/PtCloates/PtCloates_BOSS_ta.png", 
    
    
    width = 10, height = 12, res = 300, units = "in")                             # Change the dimensions here as necessary
dev.off()
  
#Build elements for plot 3; BRUV SPECIES RICHNESS
p3 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 71 & z <=215), aes(x, y, fill = p_BRUV_richness)) +
  scale_fill_viridis(direction =-1, na.value = "transparent", limits= c(0,40)) +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.4, 113.8),                              # Set plot limits
           ylim = c(-22.85, -22.75)) +
  labs(x = NULL, y = NULL, fill = "N. fish species",                                    # Labels  
       colour = NULL, title = "BRUV predicted species richness at Point Cloates, Ningaloo") +
  annotate("text", x = c(113.65, 113.57, 113.51),          # Add contour labels manually
           y = c(-22.75, -22.75, -22.75), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5)  # Center title horizontally
  )
print(p3)
png(filename = "plots/PtCloates/PtCloates_BRUV_richness.png", 
    
    
    width = 8, height = 4, res = 300, units = "in")                             # Change the dimensions here as necessary
dev.off()  


#Build elements for plot 4; BRUV SPECIES RICHNESS
p4 <- ggplot() +
  geom_tile(data = dat %>% filter(z >= 71 & z <=215), aes(x, y, fill = p_BOSS_richness)) +
  scale_fill_viridis(direction =-1, na.value = "transparent", limits= c(0,40)) +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
               breaks = c(- 30, -70, - 200),                                 # Contour breaks - change to binwidth for regular contours
               colour = "#000000",
               alpha = 1, size = 0.5) + 
  
  geom_sf(data = npz, fill = NA, colour = "darkgreen", linewidth = 0.5) +  
  npz_cols+
  new_scale_colour() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +      # Add national park zones
  coord_sf(xlim = c(113.4, 113.8),                              # Set plot limits
           ylim = c(-22.85, -22.75)) +
    labs(x = NULL, y = NULL, fill = "N. fish species",                                    # Labels  
       colour = NULL, title = "BOSS predicted species richness at Point Cloates, Ningaloo") +
  annotate("text", x = c(113.65, 113.57, 113.51),          # Add contour labels manually
           y = c(-22.75, -22.75, -22.75), 
           label = c("30m", "70m", "200m"),
           size = 2, colour = "#000000") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5)  # Center title horizontally
  )
print(p4)
png(filename = "plots/PtCloates/PtCloates_BOSS_richness.png", 
    
    
    width = 8, height = 4, res = 300, units = "in")                             # Change the dimensions here as necessary
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
  