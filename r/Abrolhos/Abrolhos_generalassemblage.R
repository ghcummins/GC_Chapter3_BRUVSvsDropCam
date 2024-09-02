###
# Project: G Cummins FISH Paper
# Script: General assemblage Abrolhos Data
# Task:   Coming up with length and sample number for fish on stacked bar plots
# author:  G Cummins 
# date:    April 2024
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

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "Abrolhos"   # set study name

# load and join datasets
#MaxN I'm filteing by location NPZ6 because I only want Shallow Bank Data
boss.maxn   <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))%>%
  dplyr::mutate(name = paste(genus, species))%>%
  dplyr::filter(location == "NPZ6")%>%
  glimpse()

#MaxN BRUV
bruv.maxn <- read.csv("data/staging/Abrolhos/2021-05_Abrolhos_stereo-BRUVs.complete.maxn.csv")%>%
  #dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))%>%
  dplyr::mutate(name = paste(genus, species))%>%
  dplyr::filter(location == "NPZ6")%>%
  glimpse()

#BOSS fish species seen on how many samples
samplefishboss <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(n = n()) 

#BOSS individual fish ie MaxN
totalfishboss <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
dplyr::summarise(totalfish = sum(maxn))

#Shallow Bank Boss maxn column (total fish) and n (no. of drops)
SBboss_maxn_n <- samplefishboss %>%
  left_join(totalfishboss %>% dplyr::select(scientific, totalfish), by = "scientific")

#change columns so we have Family genus and species seperated - to be consistent w Pt CLoates list
SBboss_allnames <- separate(SBboss_maxn_n, scientific, into = c("family", "genus", "species"), sep = " ")

#save
# write.csv(SBboss_allnames, file = "outputs/Abrolhos/ShallowBankBOSS_fishlist.csv", row.names = FALSE)

total_indi_fish_BOSS <- SBboss_allnames%>%
  dplyr::summarise(totalfish = sum(totalfish))

sfboss_inds <- boss.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species))%>%
  group_by(scientific) %>%  
  dplyr::summarise(totalfish = sum(maxn))

fishfamiliesboss <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(family) %>%
  dplyr::summarise(totalfish = sum(maxn)) 

famBOSS <- unique(fishfamiliesboss$family)
famBOSS


#BRUV fish species seen on how many samples
samplefishbruv <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(n = n()) 

#BRUV individual fish ie MaxN
totalfishbruv <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(totalfish = sum(maxn))

total_indi_fish_BRUV <- SBbruv_allnames%>%
  dplyr::summarise(totalfish = sum(totalfish))

fishfamiliesbruv <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(family) %>%
  dplyr::summarise(totalfish = sum(maxn)) 

famBRUV <- unique(fishfamiliesbruv$family)
famBRUV

#Shallow Bank BRUV maxn column (total fish) and n (no. of drops)
SBbruv_maxn_n <- samplefishbruv %>%
  left_join(totalfishbruv %>% dplyr::select(scientific, totalfish), by = "scientific")

#change columns so we have Family genus and species seperated - to be consistent w Pt CLoates list
SBbruv_allnames <- separate(SBbruv_maxn_n, scientific, into = c("family", "genus", "species"), sep = " ")

#save
# write.csv(SBbruv_allnames, file = "outputs/Abrolhos/ShallowBankBRUV_fishlist.csv", row.names = FALSE)


##NEW WAY 2/09/2024 CALCULATING SPECIES
#calculating species unique to BOSS vs BRUV #1
# Remove row with "SUS SUS sus" from samplefishBOSS
samplefishBOSS_filtered <- samplefishboss%>%
  filter(scientific != "SUS SUS sus")

# Remove row with "SUS SUS sus" from samplefishBRUV
samplefishBRUV_filtered <- samplefishbruv %>%
  filter(scientific != "SUS SUS sus")

# Identify unique scientific names in each dataframe
unique_scientific_BOSS <- unique(samplefishBOSS_filtered$scientific)
unique_scientific_BOSS 
unique_scientific_BRUV <- unique(samplefishBRUV_filtered$scientific)
unique_scientific_BRUV

sp_BOSS <- samplefishBOSS_filtered %>% distinct(scientific)
sp_BRUV <- samplefishBRUV_filtered %>% distinct(scientific)

# unique_scientific_BOSS <- samplefishBOSS_filtered %>% distinct(scientific)
# unique_scientific_BRUV <- samplefishBRUV_filtered %>% distinct(scientific)

only_in_BRUV <- anti_join(unique_scientific_BRUV, unique_scientific_BOSS)

only_in_sp_BOSS <- anti_join(sp_BOSS, sp_BRUV)
only_in_sp_BRUV <- anti_join(sp_BRUV, sp_BOSS)


# CHECKS: Count the number of scientific names that are unique to each dataframe
unique_to_BOSS <- setdiff(unique_scientific_BOSS, unique_scientific_BRUV)
unique_to_BRUV <- setdiff(unique_scientific_BRUV, unique_scientific_BOSS)

species_unique_to_BOSS <- data.frame(scientific = unique_to_BOSS)
species_unique_to_BRUV <- data.frame(scientific = unique_to_BRUV)

# # Count the number of scientific names found in both dataframes
# common_names <- intersect(species_unique_to_BOSS, species_unique_to_BRUV)
# common_count <- length(common_names)

data_genus <- list(
  BRUV = sp_BRUV$scientific,
  BOSS = sp_BOSS$scientific
)

venn_plot_species <-ggvenn(data_genus, 
                           c("BRUV", "BOSS"), 
                           fill_color =c("gray38", "white"),
                           fill_alpha = 0.4,
                           show_percentage = F,
                           text_size=8)+
  # set_name_size = 8)+
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))
#ggtitle("Point Cloates\n")+
# theme(plot.title = element_text(size = 14))
venn_plot_species

ggsave("Abrolhos_SPECIES_venndiagramGRAY.jpeg", venn_plot_species, width = 15, height = 10, units = "cm")




# #Species unique to BOSS and BRUV [OLD WAY]
# speciesBOSS<- SBboss_allnames %>%
#   filter(name != "SUS sus")
#   # filter(species != "spp")
# 
# speciesBRUV <-SBbruv_allnames %>%
#   filter(name != "SUS sus")
# 
#  sp_BOSS <- speciesBOSS%>%distinct(name)
#  sp_BRUV <- speciesBRUV%>%distinct(name)
# 
# only_species_BOSS <- anti_join(sp_BOSS, sp_BRUV)
# only_species_BRUV <- anti_join(sp_BRUV, sp_BOSS)
# 
# data_genus <- list(
#   BRUV = g_BRUV$genus,
#   BOSS = g_BOSS$genus
# )
# 
# venn_plot_species <-ggvenn(data_genus, 
#                          c("BRUV", "BOSS"), 
#                          fill_color =c("gray38", "white"),
#                          fill_alpha = 0.4,
#                          show_percentage = F,
#                          text_size=8)+
#   # set_name_size = 8)+
#   theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))
# #ggtitle("Point Cloates\n")+
# # theme(plot.title = element_text(size = 14))
# venn_plot_species



#genera unique to BOSS and BRUV
generaBOSS <- unique(speciesBOSS$genus)
generaBOSS

generaBRUV <-unique(speciesBRUV$genus)
generaBRUV

g_BOSS <-data.frame(genus = generaBOSS)%>%
  filter(genus !="Unknown")
g_BRUV <-data.frame(genus = generaBRUV)%>%
  filter(genus !="Unknown")

only_in_g_BOSS <- anti_join(g_BOSS, g_BRUV)
only_in_g_bruv <- anti_join(g_BRUV, g_BOSS)

data_genus <- list(
  BRUV = g_BRUV$genus,
  BOSS = g_BOSS$genus
)

venn_plot_genus <-ggvenn(data_genus, 
                         c("BRUV", "BOSS"), 
                         fill_color =c("gray38", "white"),
                         fill_alpha = 0.4,
                         show_percentage = F,
                         text_size=8)+
  # set_name_size = 8)+
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))
#ggtitle("Point Cloates\n")+
# theme(plot.title = element_text(size = 14))
venn_plot_genus

# Create Venn diagram ##only done venn diagram for families... do for genera and species.


ggsave("Abrolhos_genera_venndiagram_GRAY.jpeg", venn_plot_genus, width = 15, height = 10, units = "cm")



#Families unique to BOSS and BRUV
familiesboss <- fishfamiliesboss %>%
  filter(family !="SUS")

familiesbruv <- fishfamiliesbruv %>%
  filter(family !="SUS")

f_BOSS <- familiesboss%>% distinct(family)
f_BRUV <- familiesbruv %>% distinct(family)

only_families_in_boss <- anti_join(f_BOSS, f_BRUV)
only_families_in_bruv <- anti_join(f_BRUV, f_BOSS)
