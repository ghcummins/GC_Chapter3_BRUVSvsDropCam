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
samplefishBOSS <- swc_boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>% 
  dplyr::summarise(n =n())


#to get each MAXN sample on BOSS
samplemaxnBOSS <- swc_boss.maxn %>%
  filter(maxn>0)

#BRUVS fish seen on how many samples
samplefishBRUV <- swc_bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n())

#to get each MAXN sample on BRUVS
samplemaxnBRUV <- swc_bruv.maxn %>%
  filter(maxn>0)

# look at all species ----
fish.sp.maxn <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(method,scientific) %>%
  dplyr::summarise(maxn = sum(maxn))
# arrange(scientific)

#filter os sus sus and unknowns and boss #11
fish.sp.maxn_filtered <- fish.sp.maxn%>%
 # filter(scientific != "SUS sus") %>%
  #filter(scientific != "Cyprinocirrhites polyactis") %>%
  filter(!grepl("Unknown", scientific))    

# Filter the top 10 BOSS maxn values
top_10_boss_maxn <- fish.sp.maxn_filtered %>%
  filter(method == "BOSS") %>%
  top_n(10, maxn)

# Filter the top 10 BRUVS maxn values
top_10_bruv_maxn <- fish.sp.maxn_filtered %>%
  filter(method == "BRUV") %>%
  top_n(10, maxn)

# Create a bar graph for the top 10 BOSS maxn values
ggplot(top_10_boss_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BOSS maxn by Scientific Species",
       x = "Scientific Species",
       y = "BOSS maxn") +
  theme_minimal() +
  theme(legend.position = "none")

# Create a bar graph for the top 10 BRUVS maxn values
ggplot(top_10_bruv_maxn, aes(x = reorder(scientific, maxn), y = maxn, fill = scientific)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 BRUVS maxn by Scientific Species",
       x = "Scientific Species",
       y = "BRUVS maxn") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine the top 10 BOSS and top 10 BRUVS DataFrames
combined_top_10 <- rbind(top_10_boss_maxn, top_10_bruv_maxn)


# Create a complete data frame with all combinations of "scientific" and "method"
all_combinations <- expand.grid(scientific = unique(combined_top_10$scientific), method = c("BOSS", "BRUV"))
all_combinations <- all_combinations %>%
  left_join(combined_top_10, by = c("scientific", "method")) %>%
  mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0

# Create the combined bar plot
Capesregion.barchart <- ggplot(all_combinations, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(title = "Capes region", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  #scale_y_discrete(labels = expression(italic(scientific))) +  # Italicize the y-axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 2000))

Capesregion.barchart

# ggsave("SouthwestAbundance.jpeg", Capesregion.barchart, width = 20, height = 14, units = "cm")

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
  labs(title = "Shallow Bank", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  #scale_y_discrete(labels = expression(italic(scientific))) +  # Italicize the y-axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 650))

Abrol.barchart

###POINT CLOATES BARCHART
name <- "PtCloates"   # set study name

# load and join datasets
#MaxN
pc.boss.maxn   <- read.csv("data/tidy/PtCloates/PtCloates_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  # unique(boss.maxn$sample)
  glimpse()
pc.bruv.maxn <- read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.maxn.csv")%>%
  #dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
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
  labs(title = "Point Cloates", x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  #scale_y_discrete(labels = expression(italic(scientific))) +  # Italicize the y-axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 850))

PtCloates.barchart






##COMBINING PLOTS
Biogeographic_Barchart_plots <- PtCloates.barchart + Abrol.barchart  + Capesregion.barchart +(plot_layout(ncol=1))
Biogeographic_Barchart_plots


ggsave("Biogeographicbarcharts1.jpeg", Biogeographic_Barchart_plots, width = 20, height = 35, units = "cm")
# plots PCO data
