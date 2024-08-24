###
# Project: G Cummins FISH Paper
# Script: Making stacked bar plots w maxn
# Task:    Making stacked bar plots with maxn data
# author:  G Cummins 
# date:    Nov 2023
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

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "Abrolhos"   # set study name

# load and join datasets
#MaxN
boss.maxn   <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  glimpse()
bruv.maxn <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_stereo-BRUVs.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV")%>%
  glimpse()
#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  glimpse()

npz6maxn <- maxn %>%
  dplyr::filter(location %in% "NPZ6")

# look at all species ----
fish.sp.maxn <- npz6maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(method,scientific) %>%
  dplyr::summarise(maxn = sum(maxn))
  arrange(scientific)

#filter os sus sus and unknowns
fish.sp.maxn_filtered <- fish.sp.maxn%>%
  filter(scientific != "SUS sus") %>%
  filter(!grepl("Unknown", scientific))    


# Filter the top 10 BOSS maxn values
top_10_boss_maxn <- fish.sp.maxn_filtered %>%
  filter(method == "BOSS") %>%
  top_n(10, maxn)
  
  
# #BOSS_barchart <- top_10_boss_maxn %>%
#   add_row(method = "BOSS", scientific = "Chrysophrys auratus", maxn = 0)+
#   add_row(method = "BOSS", scientific = "Lethrinus nebulosus", maxn = 0)+
#   add_row(method = "BOSS", scientific = "Diagramma pictum labiosum", maxn = 0)

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
ggplot(combined_top_10, aes(x = reorder(scientific, -maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 BOSS and BRUVS maxn by Scientific Species",
       x = "Scientific Species",
       y = "MaxN") +
  #scale_fill_manual(values = c("BOSS" = "blue", "BRUV" = "red")) +
  theme_minimal()




# Create the combined bar plot
Abrol.barchart <- ggplot(all_combinations, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
   #scale_y_discrete(labels = expression(italic(scientific))) +  # Italicize the y-axis labels
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 650))


Abrol.barchart

ggsave("AbrolhosAbund.jpeg", Abrol.barchart, width = 20, height = 14, units = "cm")
# plots PCO data


# Step 1: Extract the species from top_10_boss_maxn and top_10_bruv_maxn
top_species <- unique(c(top_10_boss_maxn$scientific, top_10_bruv_maxn$scientific))

# Step 2: Filter fish.sp.maxn_filtered to include only these species
filtered_maxn <- fish.sp.maxn_filtered %>%
  filter(scientific %in% top_species)

# Step 3: Create a complete data frame with all combinations of species and methods
BOSSBRUV_topten <- expand.grid(scientific = top_species, method = c("BOSS", "BRUV"))

# Step 4: Join this with the filtered_maxn to get the corresponding maxn values
BOSSBRUV_stackedbar <- BOSSBRUV_topten %>%
  left_join(filtered_maxn, by = c("scientific", "method")) %>%
   mutate(maxn = coalesce(maxn, 0))  # Replace missing maxn values with 0


# Create the combined bar plot
BOSSBRUV.abrol.barchart <- ggplot(BOSSBRUV_stackedbar, aes(x = reorder(scientific, maxn), y = maxn, fill = method)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6, color = "black") +  # Set the bar width and outline color
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray", size = 0.5) +  # Set the line size
  coord_flip() +
  labs(x = "Scientific name",
       y = "Overall abundance (ΣMaxN)") +
  scale_fill_manual(values = c("BOSS" = "white", "BRUV" = "dark grey"), name = "Method") +
  theme_minimal() +
  theme(axis.text.y = element_text(face = "italic")) +
  scale_y_continuous(limits = c(0, 650))

BOSSBRUV.abrol.barchart

ggsave("BOSSBRUVAbrolhos_BarChart_complete.jpeg", BOSSBRUV.abrol.barchart, width = 20, height = 14, units = "cm")
# plots PCO data


# Add custom italics labels for y-axis
#p + geom_text(aes(x = reorder(scientific, -maxn), y = maxn, label = scientific), vjust = -0.5, fontface = "italic")

###NOTE RELATIVE ABUNDANCE of fish was determined as the maximum number of individuals present of each species at a single point in time (MAxN; Cappo et al. 2004). 

# # # look at top species ----
# Bar <- fish.sp.maxn_filtered
#   mutate(scientific = paste(genus, species, sep = " ")) %>%
#   group_by(method,scientific) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   top_n(15)%>%
#   ungroup()

# # look at top species ----
# maxn.sum <- npz6maxn %>%
#   mutate(scientific = paste(genus, species, sep = " ")) %>%
#   group_by(method,scientific) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   top_n(15)%>%
#   ungroup()
