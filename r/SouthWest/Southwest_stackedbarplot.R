###
# Project: G Cummins FISH Paper Ch3 PHD
# Script: Making stacked bar plots w maxn for SOUTHWEST
# Task:    Making stacked bar plots with maxn data
# author:  G Cummins 
# date:    Aug 2024
##

rm(list=ls()) # Clear memory

# libraries----
#detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
# library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)
library(ggtext)
library(grid)
library(patchwork)

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "SouthWest"   # set study name

#MaxN
boss.maxn   <- read.csv("data/staging/SwC/2020-2021_south-west_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS",
                sample=as.character(sample))%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))%>%
  dplyr::mutate(name = paste(genus, species))%>%
  glimpse()


bruv.maxn <- read.csv("data/staging/SwC/2020_south-west_stereo-BRUVs.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  plyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))%>%
  dplyr::mutate(name = paste(genus, species))%>%
  glimpse()

unique(boss.maxn$id)

#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05) %>%
  glimpse()

swc_boss.maxn <- boss.maxn %>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05)

swc_bruv.maxn <- bruv.maxn %>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05)


#BOSS fish seen on how many samples
sw_samplefishBOSS <- swc_boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>% 
  dplyr::summarise(n =n())


#to get each MAXN sample on BOSS
sw_samplemaxnBOSS <- swc_boss.maxn %>%
  filter(maxn>0)

#BRUVS fish seen on how many samples
sw_samplefishBRUV <- swc_bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n())

#to get each MAXN sample on BRUVS
sw_samplemaxnBRUV <- swc_bruv.maxn %>%
  filter(maxn>0)

# look at all species ----
sw_fish.sp.maxn <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(method,scientific) %>%
  dplyr::summarise(maxn = sum(maxn))
# arrange(scientific)

#filter os sus sus and unknowns and boss #11
sw_fish.sp.maxn_filtered <- sw_fish.sp.maxn%>%
 # filter(scientific != "SUS sus") %>%
  #filter(scientific != "Cyprinocirrhites polyactis") %>%
  filter(!grepl("Unknown", scientific))    

# Filter the top 10 BOSS maxn values
sw_top_10_boss_maxn <- sw_fish.sp.maxn_filtered %>%
  filter(method == "BOSS") %>%
  top_n(10, maxn)

# Filter the top 10 BRUVS maxn values
sw_top_10_bruv_maxn <- sw_fish.sp.maxn_filtered %>%
  filter(method == "BRUV") %>%
  top_n(10, maxn)

# Create a bar graph for the top 10 BOSS maxn values
ggplot(sw_top_10_boss_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BOSS maxn by Scientific Species",
       x = "Scientific Species",
       y = "BOSS maxn") +
  theme_minimal() +
  theme(legend.position = "none")

# Create a bar graph for the top 10 BRUVS maxn values
ggplot(sw_top_10_bruv_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BRUVS maxn by Scientific Species",
       x = "Scientific Species",
       y = "BRUVS maxn") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine the top 10 BOSS and top 10 BRUVS DataFrames
sw_combined_top_10 <- rbind(sw_top_10_boss_maxn, sw_top_10_bruv_maxn)


# Create a complete data frame with all combinations of "scientific" and "method"
sw_all_combinations <- expand.grid(scientific = unique(sw_combined_top_10$scientific), method = c("BOSS", "BRUV"))
swc_all_combinations <- sw_all_combinations %>%
  left_join(sw_combined_top_10, by = c("scientific", "method")) %>%
  mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0

# Create the combined bar plot
Capesregion.barchart <- ggplot(swc_all_combinations, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(title = "South-west", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  #scale_y_discrete(labels = expression(italic(scientific))) +  # Italicize the y-axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 2000))

Capesregion.barchart

# ggsave("SouthwestAbundance.jpeg", Capesregion.barchart, width = 20, height = 14, units = "cm")

# Step 1: Extract the species from top_10_boss_maxn and top_10_bruv_maxn
capes_top_species <- unique(c(sw_top_10_boss_maxn$scientific, sw_top_10_bruv_maxn$scientific))

# Step 2: Filter fish.sp.maxn_filtered to include only these species
capes_filtered_maxn <- sw_fish.sp.maxn_filtered %>%
  filter(scientific %in% capes_top_species)

# Step 3: Create a complete data frame with all combinations of species and methods
capes_BOSSBRUV_topten <- expand.grid(scientific = capes_top_species, method = c("BOSS", "BRUV"))

# Step 4: Join this with the filtered_maxn to get the corresponding maxn values
capes_BOSSBRUV_stackedbar <- capes_BOSSBRUV_topten %>%
  left_join(capes_filtered_maxn, by = c("scientific", "method")) %>%
  mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0


# Create the combined bar plot
BOSSBRUV.capes.barchart <- ggplot(capes_BOSSBRUV_stackedbar, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(title = "South-west", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.line = element_line(color = "black", size = 0.5),  # Add black solid lines along x and y axes
        axis.ticks.x = element_line(color = "black", size = 0.5),  # Add tick marks along the x-axis
        axis.ticks.length.x = unit(0.3, "cm"),  # Set tick length
        panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_blank(),  # Ensure no background
        plot.background = element_blank()) +
  scale_y_continuous(limits = c(0, 2000), expand = c(0,0))

BOSSBRUV.capes.barchart

# ggsave("BOSSBRUVCapes_BarChart_complete.jpeg", BOSSBRUV.capes.barchart, width = 20, height = 14, units = "cm")


###HERE COMBINE ALL 3 BIOGEOGRAPHIC REGION ABUNDANCE PLOTS
##ABROLHOS STACKED BARPLOT INPUT
name <- "Abrolhos"   # set study name

# load and join datasets
#MaxN
ab.boss.maxn   <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  glimpse()
ab.bruv.maxn <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_stereo-BRUVs.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV")%>%
  glimpse()
#join
ab.maxn <- bind_rows(ab.boss.maxn, ab.bruv.maxn)%>%
  glimpse()

npz6maxn <- ab.maxn %>%
  dplyr::filter(location %in% "NPZ6")

# look at all species ----
ab.fish.sp.maxn <- npz6maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(method,scientific) %>%
  dplyr::summarise(maxn = sum(maxn))%>%
  arrange(scientific)

#filter os sus sus and unknowns
ab.fish.sp.maxn_filtered <- ab.fish.sp.maxn%>%
  filter(scientific != "SUS sus") %>%
  filter(!grepl("Unknown", scientific))    


# Filter the top 10 BOSS maxn values
ab.top_10_boss_maxn <- ab.fish.sp.maxn_filtered %>%
  filter(method == "BOSS") %>%
  top_n(10, maxn)


# #BOSS_barchart <- top_10_boss_maxn %>%
#   add_row(method = "BOSS", scientific = "Chrysophrys auratus", maxn = 0)+
#   add_row(method = "BOSS", scientific = "Lethrinus nebulosus", maxn = 0)+
#   add_row(method = "BOSS", scientific = "Diagramma pictum labiosum", maxn = 0)

# Filter the top 10 BRUVS maxn values
ab.top_10_bruv_maxn <- ab.fish.sp.maxn_filtered %>%
  filter(method == "BRUV") %>%
  top_n(10, maxn)

# Create a bar graph for the top 10 BOSS maxn values
ggplot(ab.top_10_boss_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BOSS maxn by Scientific Species",
       x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  theme_minimal() +
  theme(legend.position = "none")

# Create a bar graph for the top 10 BRUVS maxn values
ggplot(ab.top_10_bruv_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BRUVS maxn by Scientific Species",
       x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine the top 10 BOSS and top 10 BRUVS DataFrames
ab.combined_top_10 <- rbind(ab.top_10_boss_maxn, ab.top_10_bruv_maxn)

# Create a complete data frame with all combinations of "scientific" and "method"
ab.all_combinations <- expand.grid(scientific = unique(ab.combined_top_10$scientific), method = c("BOSS", "BRUV"))
ab.all_combinations <- ab.all_combinations %>%
  left_join(ab.combined_top_10, by = c("scientific", "method")) %>%
  mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0

# Create the combined bar plot
ab <- ggplot(ab.combined_top_10, aes(x = reorder(scientific, -maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 BOSS and BRUVS maxn by Scientific Species",
       x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  #scale_fill_manual(values = c("BOSS" = "blue", "BRUV" = "red")) +
  theme_minimal()
ab

# Create the combined bar plot
Abrol.barchart <- ggplot(ab.all_combinations, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(title = "Abrolhos", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  #scale_y_discrete(labels = expression(italic(scientific))) +  # Italicize the y-axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 650))

Abrol.barchart

##ABROLHOS
# Step 1: Extract the species from top_10_boss_maxn and top_10_bruv_maxn
ab.top_species <- unique(c(ab.top_10_boss_maxn$scientific, ab.top_10_bruv_maxn$scientific))

# Step 2: Filter fish.sp.maxn_filtered to include only these species
ab.filtered_maxn <- ab.fish.sp.maxn_filtered %>%
  filter(scientific %in% ab.top_species)

# Step 3: Create a complete data frame with all combinations of species and methods
ab.BOSSBRUV_topten <- expand.grid(scientific = ab.top_species, method = c("BOSS", "BRUV"))

# Step 4: Join this with the filtered_maxn to get the corresponding maxn values
ab.BOSSBRUV_stackedbar <- ab.BOSSBRUV_topten %>%
  left_join(ab.filtered_maxn, by = c("scientific", "method")) %>%
  mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0

# Create the combined bar plot
BOSSBRUV.abrol.barchart <- ggplot(ab.BOSSBRUV_stackedbar, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(title = "Abrolhos", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.line = element_line(color = "black", size = 0.5),  # Add black solid lines along x and y axes
        axis.ticks.x = element_line(color = "black", size = 0.5),  # Add tick marks along the x-axis
        axis.ticks.length.x = unit(0.3, "cm"),  # Set tick length
        panel.grid = element_blank(),  # Remove grid lines
        panel.background = element_blank(),  # Ensure no background
        plot.background = element_blank()) +
  scale_y_continuous(limits = c(0, 650), expand = c(0,0))
  
 BOSSBRUV.abrol.barchart

# ggsave("BOSSBRUVAbrolhos_BarChart_complete.jpeg", BOSSBRUV.abrol.barchart, width = 20, height = 14, units = "cm")
# plots PCO data

###POINT CLOATES BARCHART
name <- "PtCloates"   # set study name

# load and join datasets
#MaxN
pc.boss.maxn   <- read.csv("data/tidy/PtCloates/PtCloates_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(genus = ifelse(scientific == "Clupeidae Unknown spp", "Clupeidae", genus)) %>%
  dplyr::mutate(scientific = ifelse(scientific == "Clupeidae Unknown spp",
                                    "Clupeidae Clupeidae spp", scientific))%>%
  # unique(boss.maxn$sample)
  glimpse()
pc.bruv.maxn <- read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.maxn.csv")%>%
  #dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  dplyr::mutate(scientific = ifelse(scientific == "Sparidae Dentex spp",
                                    "Sparidae Dentex carpenteri", scientific))%>%
  dplyr::mutate(species = ifelse(scientific == "Sparidae Dentex carpenteri", "carpenteri", species)) %>%
  glimpse()

unique(pc.bruv.maxn$unique_id)
#join
pc.maxn <- bind_rows(pc.boss.maxn,pc.bruv.maxn)%>%
  glimpse()

#BOSS fish numbers seen on how many samples
pc.samplefishBOSS <- pc.boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>% 
  dplyr::summarise(n =n())

#to get each MAXN sample on BOSS
pc.samplemaxnBOSS <- boss.maxn %>%
  filter(maxn>0)

#BRUVS fish numbers seen on how many samples
pc.samplefishBRUV <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n())

#to get each MAXN sample on BRUVS
pc.samplemaxnBRUV <- bruv.maxn %>%
  filter(maxn>0)

# look at all species ----
pc.fish.sp.maxn <- pc.maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(method,scientific) %>%
  dplyr::summarise(maxn = sum(maxn))
# arrange(scientific)

#filter os sus sus and unknowns and boss #11
pc.fish.sp.maxn_filtered <- pc.fish.sp.maxn%>%
  filter(scientific != "SUS sus") %>%
  filter(scientific != "Cyprinocirrhites polyactis") %>%
  filter(!grepl("Unknown", scientific))    


# Filter the top 10 BOSS maxn values
pc.top_10_boss_maxn <- pc.fish.sp.maxn_filtered %>%
  filter(method == "BOSS") %>%
  top_n(10, maxn)

# Filter the top 10 BRUVS maxn values
pc.top_10_bruv_maxn <- pc.fish.sp.maxn_filtered %>%
  filter(method == "BRUV") %>%
  top_n(10, maxn)

# Create a bar graph for the top 10 BOSS maxn values
pcboss <- ggplot(pc.top_10_boss_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BOSS maxn by Scientific Species",
       x = "Scientific Species",
       y = "BOSS maxn") +
  theme_minimal() +
  theme(legend.position = "none")
pcboss

# Create a bar graph for the top 10 BRUVS maxn values
pcbruv <- ggplot(pc.top_10_bruv_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BRUVS maxn by Scientific Species",
       x = "Scientific Species",
       y = "BRUVS maxn") +
  theme_minimal() +
  theme(legend.position = "none")
pcbruv

# Combine the top 10 BOSS and top 10 BRUVS DataFrames
pc.combined_top_10 <- rbind(pc.top_10_boss_maxn, pc.top_10_bruv_maxn)

# Create a complete data frame with all combinations of "scientific" and "method"
pc.all_combinations <- expand.grid(scientific = unique(pc.combined_top_10$scientific), method = c("BOSS", "BRUV"))
PC.all_combinations <- pc.all_combinations %>%
  left_join(pc.combined_top_10, by = c("scientific", "method")) %>%
  mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0


# Create the combined bar plot
PtCloates.barchart <- ggplot(PC.all_combinations, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(title = "Ningaloo", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  #scale_y_discrete(labels = expression(italic(scientific))) +  # Italicize the y-axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 400))

PtCloates.barchart

##FIXING TOP 10 TO  include top ten plus the other method!
# Step 1: Extract the species from top_10_boss_maxn and top_10_bruv_maxn
pc.top_species <- unique(c(pc.top_10_boss_maxn$scientific, pc.top_10_bruv_maxn$scientific))

# Step 2: Filter fish.sp.maxn_filtered to include only these species
pc.filtered_maxn <- pc.fish.sp.maxn_filtered %>%
  filter(scientific %in% pc.top_species)

# Step 3: Create a complete data frame with all combinations of species and methods
pc.BOSSBRUV_topten <- expand.grid(scientific = pc.top_species, method = c("BOSS", "BRUV"))

# Step 4: Join this with the filtered_maxn to get the corresponding maxn values
pc.BOSSBRUV_stackedbar <- pc.BOSSBRUV_topten %>%
  left_join(pc.filtered_maxn, by = c("scientific", "method")) %>%
  mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0

# Create the combined bar plot
BOSSBRUV.ptcloates.barchart <- ggplot(pc.BOSSBRUV_stackedbar, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(title = "Ningaloo", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic"),
        axis.line = element_line(color = "black", size = 0.5),  # Add black solid lines along x and y axes
        axis.ticks.x = element_line(color = "black", size = 0.5),  # Add tick marks along the x-axis
        axis.ticks.length.x = unit(0.3, "cm"),  # Set tick length
        panel.grid = element_blank(),  # Remove grid lines
         panel.background = element_blank(),  # Ensure no background
        plot.background = element_blank()) +
  scale_y_continuous(limits = c(0, 400), expand = c(0,0))

BOSSBRUV.ptcloates.barchart

# ggsave("BOSSBRUVPtCloates_BarChart_complete.jpeg", BOSSBRUV.abrol.barchart, width = 20, height = 14, units = "cm")




##COMBINING PLOTS
BOSSBRUVbiogeographic_Barcharts <- BOSSBRUV.ptcloates.barchart + BOSSBRUV.abrol.barchart  + BOSSBRUV.capes.barchart +(plot_layout(ncol=1))
BOSSBRUVbiogeographic_Barcharts


ggsave("BOSSBRUVbiogeographicbarcharts_FINAL_withClupeidae.jpeg", BOSSBRUVbiogeographic_Barcharts, width = 20, height = 35, units = "cm")
# plots PCO data
