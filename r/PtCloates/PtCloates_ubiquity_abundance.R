###
# Project: G Cummins FISH Paper
# Script: Ningaloo Ubiquity and Abundance
# Task:   Plotting ubiquity and abundance
# author:  G Cummins 
# date:    SEPTEMBER 2024
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
library(stringr)
library(reshape2)
library(viridis)
library(terra)
library(patchwork)
library(sf)
library(sfheaders)
library(rgdal)
library(stars)
library(smoothr)
library(ggnewscale)
library(gridExtra)
library(patchwork)
library(metR)
library(vegan)
library(ggstance)
library(metR)
library(stringr)
library(VennDiagram)
library(ggVennDiagram)
library(ggvenn)

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "PtCloates"   # set study name

# load and join datasets
#MaxN
boss.maxn   <- read.csv("data/tidy/PtCloates/PtCloates_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))%>%
  glimpse()
bruv.maxn <- read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.maxn.csv")%>%
  #dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  dplyr::mutate(scientific = ifelse(scientific == "Apogonidae Apogon semilineatus",
                                    "Apogonidae Ostorhinchus semilineatus", scientific))%>%
  dplyr::mutate(genus = ifelse(genus == "Apogon", "Ostorhinchus", genus))%>%
  dplyr::mutate(scientific = ifelse(scientific == "Sparidae Dentex spp",
                                    "Sparidae Dentex carpenteri", scientific))%>%
  dplyr::mutate(species = ifelse(scientific == "Sparidae Dentex carpenteri", "carpenteri", species)) %>%
  glimpse()
#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  glimpse()



#BOSS fish species seen on how many samples
samplefishBOSS <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n()) 

sfboss <- boss.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species))%>%
  group_by(scientific) %>%
  dplyr::summarise(n = n()) 

sfboss_inds <- boss.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species))%>%
  group_by(scientific) %>%  
  dplyr::summarise(totalfish = sum(maxn))

sfboss_inds_n <- sfboss %>%
  left_join(sfboss_inds %>% dplyr::select(scientific, totalfish), by = "scientific")

# Creating new columns for fam, genus and sp
sfboss_allnames <- separate(sfboss_inds_n, scientific, into = c("family", "genus", "species"), sep = " ")

Ningaloo_boss_ind_n <- sfboss_inds_n %>%
  filter(scientific !="SUS SUS sus")

#save
# write.csv(sfboss_allnames, file = "outputs/PtCloates/PtCloatesBOSS_fishlist_final.csv", row.names = FALSE)

#sum of all individuals to see total fish seen on BOSS
total_sumindifishboss <- sum(Ningaloo_boss_ind_n$totalfish)
print(total_sumindifishboss)

#BRUVS fish numbers seen on how many samples
samplefishBRUV <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n())

sfbruv <- bruv.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species))%>%
  group_by(scientific) %>%
  dplyr::summarise(n = n()) 

sfbruv_inds <- bruv.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species))%>%
  group_by(scientific) %>%  
  dplyr::summarise(totalfish = sum(maxn))

sfbruv_inds_n <- sfbruv %>%
  left_join(sfbruv_inds %>% dplyr::select(scientific, totalfish), by = "scientific")

# Creating new columns for fam, genus and sp
sfbruv_allnames <- separate(sfbruv_inds_n, scientific, into = c("family", "genus", "species"), sep = " ")

totalbruvdrops <- 89 ##THIS IS FOR NINGALOO

Ningaloo_bruv_ind_n <- sfbruv_inds_n %>%
  filter(scientific !="SUS SUS sus") 

Ningaloo_bruv_ubiquity_tf <- Ningaloo_bruv_ind_n %>%
  mutate(ubiquity = ((Ningaloo_bruv_ind_n$n)/totalbruvdrops))%>%
  mutate(percent_ubiquity = (ubiquity *100))

#CALCULATE SCALED ABUNDANCE ON BRUV
bruv_min_abundance <- 0
bruv_max_abundance <-  max(Ningaloo_bruv_ubiquity_tf$totalfish)
bruv_max_abundance  ###CROSS CHECK WITH TABLE IN APPENDIX OF PAPER (ie 387 in this case)

#calculate scaled abundance
Ningaloo_bruv_scaled_abundance_ubiquity = Ningaloo_bruv_ubiquity_tf %>%
  mutate(bruvscaled_abund = (Ningaloo_bruv_ubiquity_tf$totalfish - bruv_min_abundance) / (bruv_max_abundance - bruv_min_abundance))

# Create the scatterplot
ggplot(Ningaloo_bruv_scaled_abundance_ubiquity, aes(x = bruvscaled_abund, y = percent_ubiquity)) +
  geom_point() +  # Add points for each row
  labs(x = "BRUV Scaled Abundance", y = "Percent Ubiquity") +  # Label the axes
  theme_minimal()  # Apply a minimal theme

# Create the scatterplot
ggplot(Ningaloo_bruv_scaled_abundance_ubiquity, aes(x = percent_ubiquity , y = bruvscaled_abund)) +
  geom_point() +  # Add points for each row
  labs(x = "Percent Ubiquity", y = "BRUV Scaled Abundance") +  # Label the axes
  theme_minimal()  # Apply a minimal theme


####SAME BUT FOR BOSS!!!!!!!
totalbossdrops <- 113 ##THIS IS FOR NINGALOO

Ningaloo_boss_ind_n <- sfboss_inds_n %>%
  filter(scientific !="SUS SUS sus") 

Ningaloo_boss_ubiquity_tf <- Ningaloo_boss_ind_n %>%
  mutate(ubiquity = ((Ningaloo_boss_ind_n$n)/totalbossdrops))%>%
  mutate(percent_ubiquity = (ubiquity *100))

#CALCULATE SCALED ABUNDANCE ON BRUV
boss_min_abundance <- 0
boss_max_abundance <-  max(Ningaloo_boss_ubiquity_tf$totalfish)
boss_max_abundance  ###CROSS CHECK WITH TABLE IN APPENDIX OF PAPER (ie 387 in this case)

#calculate scaled abundance
Ningaloo_boss_scaled_abundance_ubiquity = Ningaloo_boss_ubiquity_tf %>%
  mutate(bosscaled_abund = (Ningaloo_boss_ubiquity_tf$totalfish - boss_min_abundance) / (boss_max_abundance - boss_min_abundance))

# Create the scatterplot
ggplot(Ningaloo_boss_scaled_abundance_ubiquity, aes(x = bosscaled_abund, y = percent_ubiquity)) +
  geom_point() +  # Add points for each row
  labs(title = "Ningaloo", x = "BOSS Scaled Abundance", y = "Percent Ubiquity") +  # Label the axes
  theme_minimal()  # Apply a minimal theme

# Create the scatterplot
ggplot(Ningaloo_boss_scaled_abundance_ubiquity, aes(x = percent_ubiquity , y = bosscaled_abund)) +
  geom_point() +  # Add points for each row
  labs(title = "Ningaloo", x = "Percent Ubiquity", y = "BOSS Scaled Abundance") +  # Label the axes
  theme_minimal()  # Apply a minimal theme








