###
# Project: G Cummins FISH Paper
# Script: Making stacked bar plots w maxn
# Task:   Coming up with length and sample number for fish on stacked bar plots
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
  glimpse()
#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  glimpse()

apogonbruv <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific)

# apogonboss <- boss.maxn %>%
#   filter(maxn>0) %>%
#   group_by(scientific)

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
  left_join(sfboss_inds %>% select(scientific, totalfish), by = "scientific")

# Creating new columns for fam, genus and sp
sfboss_allnames <- separate(sfboss_inds_n, scientific, into = c("family", "genus", "species"), sep = " ")

#save
write.csv(sfboss_allnames, file = "outputs/PtCloates/PtCloatesBOSS_fishlist_final.csv", row.names = FALSE)


#to get each MAXN sample on BOSS
samplemaxnBOSS <- boss.maxn %>%
  filter(maxn>0)

# write.csv(samplemaxnBOSS, file = "data/samplemaxnBOSS.csv", row.names = FALSE)

#Total number of individual fish seen on BOSS and remove SUS
totalfishBOSS <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  filter(scientific !="SUS SUS sus")%>%
  dplyr::summarise(totalfish = sum(maxn))
#sum of all individuals to see total fish seen on BOSS
total_sumindifishboss <- sum(totalfishBOSS$totalfish)
print(total_sumindifishboss)
#calculate number of families on BOSS
fishfamiliesboss <- boss.maxn %>%
  group_by(family) %>%
  dplyr::summarise(totalfish = sum(maxn)) # don't forget to remove Sus from this

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
  left_join(sfbruv_inds %>% select(scientific, totalfish), by = "scientific")

# Creating new columns for fam, genus and sp
sfbruv_allnames <- separate(sfbruv_inds_n, scientific, into = c("family", "genus", "species"), sep = " ")

#save
write.csv(sfbruv_allnames, file = "outputs/PtCloates/PtCloatesBRUVS_fishlist_final.csv", row.names = FALSE)

#genera
genusbruv <- sfbruv_allnames%>%
  mutate(famgenus = str_c(family, genus, sep = "_"))%>%
  filter(species !="sus")%>%
  filter(genus !="Unknown")

genusboss <- sfboss_allnames%>%
  mutate(famgenus = str_c(family, genus, sep = "_"))%>%
  filter(species !="sus")%>%
  filter(genus !="Unknown")


genera_BOSS <- unique(genusboss$famgenus)
genera_BOSS 
genera_BRUV <- unique(genusbruv$famgenus)
genera_BRUV

g_BOSS <- genusboss %>% distinct(famgenus)
g_BRUV <- genusbruv %>% distinct(famgenus)

only_in_bruv <- anti_join(unique_scientific_BRUV, unique_scientific_BOSS)


#to get each MAXN sample on BRUVS
samplemaxnBRUV <- bruv.maxn %>%
  filter(maxn>0) 

BRUV.individual.fish <- sum(sfbruv_inds_n$totalfish)

#Total number of individual fish seen on BRUVS
totalfishBRUV <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  filter(scientific !="SUS SUS sus")%>%
  dplyr::summarise(totalfish = sum(maxn))
#sum of all individuals to see total fish seen on BRUVs
total_sumindifishbruv <- sum(totalfishBRUV$totalfish)
print(total_sumindifishbruv)
#calculate number of families on BRUVS
fishfamiliesbruv <- bruv.maxn %>%
  group_by(family) %>%
  dplyr::summarise(totalfish = sum(maxn))


##overallmaxn on BRUVS per fish species
overallmaxnBRUV <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(summaxn = sum(maxn))

overallmaxnBOSS <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(summaxn = sum(maxn))

allfishbruv <- sum(overallmaxnBRUV$summaxn, na.rm = TRUE)
allfishbruv
allfishboss <-sum(overallmaxnBOSS$summaxn, na.rm = TRUE)
allfishboss

#calculating species unique to BOSS vs BRUV ###NEEDS CHECKING AND FIXING
# Remove row with "SUS SUS sus" from samplefishBOSS
samplefishBOSS_filtered <- samplefishBOSS %>%
  filter(scientific != "SUS SUS sus")

# Remove row with "SUS SUS sus" from samplefishBRUV
samplefishBRUV_filtered <- samplefishBRUV %>%
  filter(scientific != "SUS SUS sus")

# Identify unique scientific names in each dataframe
unique_scientific_BOSS <- unique(samplefishBOSS_filtered$scientific)
unique_scientific_BRUV <- unique(samplefishBRUV_filtered$scientific)

unique_scientific_BOSS <- samplefishBOSS_filtered %>% distinct(scientific)
unique_scientific_BRUV <- samplefishBRUV_filtered %>% distinct(scientific)

only_in_bruv <- anti_join(unique_scientific_BRUV, unique_scientific_BOSS)



# Count the number of scientific names that are unique to each dataframe
unique_to_BOSS <- setdiff(unique_scientific_BOSS, unique_scientific_BRUV)
unique_to_BRUV <- setdiff(unique_scientific_BRUV, unique_scientific_BOSS)

species_unique_to_BOSS <- data.frame(scientific = unique_to_BOSS)
species_unique_to_BRUV <- data.frame(scientific = unique_to_BRUV)

# Count the number of scientific names found in both dataframes
common_names <- intersect(species_unique_to_BOSS, species_unique_to_BRUV)
common_count <- length(common_names)




####NEW ASPECT RATIO CALCS
#determine caudal aspect ratio for each species on bruvs
bruv_species <-bruv.maxn %>%
  filter(maxn>0) %>%
  distinct(genus, species) %>%
  dplyr::mutate(name = paste(genus, species))

validated <- rfishbase::validate_names(bruv_species$name)

## This is slow but essential for figuring out what fishbase name = CAAB
# # Remove the hashes to run again
code_crosswalk <- data.frame()

for (caab_name in unique(bruv_species$name)) {

  validated_name <-  rfishbase::validate_names(caab_name)

  message(paste("validating: ", caab_name))
  message(paste("validated name:", validated_name))

  temp_dat <- data.frame(fishbase_scientific = validated_name,
                         caab_scientific = caab_name)
  code_crosswalk <- bind_rows(code_crosswalk, temp_dat)

}


mismatches <- code_crosswalk %>%
  filter(!fishbase_scientific %in% caab_scientific)

morphometrics <- rfishbase::morphometrics(validated) %>%
  rename(fishbase_scientific = Species)
# names(test)
#calc mean se of aspect ratios for each species seen on bruvs
fbmorphometrics <- morphometrics %>%
  group_by(fishbase_scientific) %>%
  dplyr::summarise(armean = mean(AspectRatio), n = n(), sd = sd(AspectRatio)) %>%
  mutate(se = sd/ sqrt(n))
 
#joining back up fb and our bruvs species list
joiningbruvs <- fbmorphometrics %>%
  left_join(code_crosswalk, by = "fishbase_scientific")

#Final aspect ratio BRUV output to save (this is our BRUV species w aspect ratio)
PtCloates_aspectratio_bruvs <- joiningbruvs %>%
  select(-fishbase_scientific)%>%
  rename(name = caab_scientific)%>%
  select(name, everything())

#write.csv(PtCloates_aspectratio_bruvs, file = "outputs/PtCloates/PtCloatesaspectratioBRUVS.csv", row.names = FALSE)

##summary statistics for Pt Cloates aspect ratio BRUV (this is our BRUV species w aspect ratio)
#Note this is all species (not all individual fish)
mean_ptc_ar_bruv <- mean(PtCloates_aspectratio_bruvs$armean, na.rm = TRUE)
median_ptc_ar_bruv <- median(PtCloates_aspectratio_bruvs$armean, na.rm = TRUE)

#REPEAT ALL ABOVE BUT FOR BOSS:
#determine caudal aspect ratio for each species on bruvs
boss_species <-boss.maxn %>%
  filter(maxn>0) %>%
  distinct(genus, species) %>%
  dplyr::mutate(name = paste(genus, species))

validated <- rfishbase::validate_names(boss_species$name)

## This is slow but essential for figuring out what fishbase name = CAAB
# # Remove the hashes to run again
code_crosswalk_boss <- data.frame()

for (caab_name in unique(boss_species$name)) {
  
  validated_name <-  rfishbase::validate_names(caab_name)
  
  message(paste("validating: ", caab_name))
  message(paste("validated name:", validated_name))
  
  temp_dat <- data.frame(fishbase_scientific = validated_name,
                         caab_scientific = caab_name)
  code_crosswalk_boss <- bind_rows(code_crosswalk_boss, temp_dat)
  
}

mismatchesboss <- code_crosswalk_boss %>%
  filter(!fishbase_scientific %in% caab_scientific)

morphometricsboss <- rfishbase::morphometrics(validated) %>%
  rename(fishbase_scientific = Species)

#calc mean se of aspect ratios for each species seen on boss
fbmorphometricsboss <- morphometricsboss %>%
  group_by(fishbase_scientific) %>%
  dplyr::summarise(armean = mean(AspectRatio), n = n(), sd = sd(AspectRatio)) %>%
  mutate(se = sd/ sqrt(n))

#joining back up fb and our boss species
joiningboss <- fbmorphometricsboss %>%
  left_join(code_crosswalk_boss, by = "fishbase_scientific")

#Final aspect ratio BOSS output to save
PtCloates_aspectratio_boss <- joiningboss %>%
  select(-fishbase_scientific)%>%
  rename(name = caab_scientific)%>%
  select(name, everything())

# write.csv(PtCloates_aspectratio_boss, file = "outputs/PtCloates/PtCloatesaspectratioBOSS.csv", row.names = FALSE)

##summary statistics for Pt Cloates aspect ratio BOSS (this is our BOSS species w aspect ratio)
#Note this is all species (not all individual fish)
mean_ptc_ar_boss <- mean(PtCloates_aspectratio_boss$armean, na.rm = TRUE)
median_ptc_ar_boss <- median(PtCloates_aspectratio_boss$armean, na.rm = TRUE)

#Aspect ratio for individuals on bruvs (not just sp) - ALL FISH INDIVIDUALS SEEN
bruv_individuals <- bruv.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species)) %>%
  group_by(name)%>%
  dplyr::summarise(totalindividuals = sum(maxn))

# Duplicate rows based on the totalindividuals column
bruv_individuals_expanded <- bruv_individuals %>%
  uncount(totalindividuals)

# View the resulting dataframe
View(bruv_individuals_expanded)

#leftjoin w aspect ratio
bruv_individuals_aspectratio <- left_join(bruv_individuals_expanded, PtCloates_aspectratio_bruvs, by = "name")

#write.csv(bruv_individuals_aspectratio, file = "outputs/PtCloates/PtCindividualsaspratioBRUV.csv", row.names = FALSE)

#SUMMARY STATISTICS for BRUV all individuals and aspect ratio
mean_ptc_indi_ar_bruv <- mean(bruv_individuals_aspectratio$armean, na.rm = TRUE)
median_ptc_indi_ar_bruv <- median(bruv_individuals_aspectratio$armean, na.rm = TRUE)
perc25_ptc_indi_ar_bruv <- quantile(bruv_individuals_aspectratio$armean, probs = 0.25, na.rm = TRUE)
perc75_ptc_indi_ar_bruv <- quantile(bruv_individuals_aspectratio$armean, probs = 0.75, na.rm = TRUE)
IQR_indi_ar_bruv <- (perc75_ptc_indi_ar_bruv - perc25_ptc_indi_ar_bruv)
IQR1.5_indi_ar_bruv <- (IQR_indi_ar_bruv * 1.5)
summary(bruv_individuals_aspectratio$armean)
table(is.na(bruv_individuals_aspectratio$armean))


##REPEAT FOR BOSS
#attempt at aspect ratio for individuals on bruvs (not just sp)
boss_individuals <- boss.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species)) %>%
  group_by(name)%>%
  dplyr::summarise(totalindividuals = sum(maxn))

# Duplicate rows based on the totalindividuals column
boss_individuals_expanded <- boss_individuals %>%
  uncount(totalindividuals)

# View the resulting dataframe
View(boss_individuals_expanded)

#leftjoin w aspect ratio
boss_individuals_aspectratio <- left_join(boss_individuals_expanded, PtCloates_aspectratio_boss, by = "name")

#write.csv(boss_individuals_aspectratio, file = "outputs/PtCloates/PtCindividualsaspratioBOSS.csv", row.names = FALSE)

#SUMMARY STATISTICS for BOSS all individuals and aspect ratio
boss_ind_ar_mean<- mean(boss_individuals_aspectratio$armean, na.rm = TRUE)
boss_ind_ar_ste_mean <- mean(boss_individuals_aspectratio$se, na.rm = TRUE)
summary(boss_individuals_aspectratio$armean)
table(is.na(boss_individuals_aspectratio$armean))

#work out number of fish with length measurements on BRUVS
bruvcompletelengths <-  read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.length.csv") 

bruvlengths <- bruvcompletelengths %>%
  #dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))

bruvl <- bruvlengths %>%
  select(unique_id, scientific, length, number) 

#count number of lengths measured on BRUVs
bruvfishnolengths <- bruvl %>%
filter(!is.na(length)) %>%
filter(scientific !="SUS SUS sus")
  
###combine maxn and lengths for BRUVS
BRUVFISHES <- left_join(samplemaxnBRUV, bruvl, by = c("unique_id","scientific"))
#View(BRUVFISHES)

# meanlengthBRUV <- BRUVFISHES %>%
#   filter(!is.na(length)) %>%
#   group_by(scientific) %>%
#   dplyr::summarise(meanlength = mean(length))
  
medianlengthBRUV <- BRUVFISHES %>%
  filter(!is.na(length)) %>%
  group_by(scientific) %>%
  dplyr::summarise(medianlength = round(median(length), 0))

 # write.csv(BRUVFISHES, file = "data/BRUVFISHES.csv", row.names = FALSE)

#work out number of fish with length measurements on BOSS
bosscompletelengths <-  read.csv("data/tidy/PtCloates/PtCloates_BOSS.complete.length.csv") 

bosslengths <- bosscompletelengths %>%
  # dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))

bossl <- bosslengths %>%
  select(id, scientific, length, number)

###combine maxn and lengths for BOSS
BOSSFISHES <- left_join(samplemaxnBOSS, bossl, by = c("id","scientific"))
# View(BOSSFISHES)

#count number of lengths measured on BRUVs
bossfishnolengths <- bossl %>%
  filter(!is.na(length)) %>%
  filter(scientific !="SUS SUS sus")

# meanlengthBOSS <- BOSSFISHES %>%
#   filter(!is.na(length)) %>%
#   group_by(scientific) %>%
#   dplyr::summarise(meanlength = mean(length))

medianlengthBOSS <- BOSSFISHES %>%
  filter(!is.na(length)) %>%
  filter(scientific !="SUS SUS sus") %>%
  group_by(scientific) %>%
  dplyr::summarise(medianlength = round(median(length), 0))


#attempt at species accumulation curves
#load Pt Cloates species accum curve rds from script 01_FSSGAM (PCO modified)
PtCloatesspeciesaccum <- readRDS("data/staging/PtCloates/PtCloatesspeciesaccum.rds") 

#generate a BOSS only DF and a BRUV only DF
PtC_BOSS_SACdf <- PtCloatesspeciesaccum %>% 
  filter(method == "BOSS") %>%
  select(-method)

PtC_BRUV_SACdf <- PtCloatesspeciesaccum %>%
  filter(method =="BRUV") %>%
  select(-method)

#make each df Presence/Absence binomial matrix (ie replace any non zero number with 1) 
   ## Create a copy of the dataframe
  PtC_BOSS_SAC_PA <- PtC_BOSS_SACdf
# Exclude the first column
PtC_BOSS_SAC_PA[, -1][PtC_BOSS_SAC_PA[, -1] > 0] <- 1
# View the new dataframe
View(PtC_BOSS_SAC_PA)

##DO the same for BRUV 
PtC_BRUV_SAC_PA <- PtC_BRUV_SACdf
# Exclude the first column
PtC_BRUV_SAC_PA[, -1][PtC_BRUV_SAC_PA[, -1] > 0] <- 1
# View the new dataframe
View(PtC_BRUV_SAC_PA)

#edit so you can generate the Sp acc curve ie make x numeric
PtC_BOSS_spacc <- PtC_BOSS_SAC_PA %>%
  select(-matches("SUS SUS sus"))%>%
  mutate(dropnumber = row_number()) %>%
  select(dropnumber, everything()) %>%
  select(-unique_id) %>%
  select(where(~!all(. == 0)))

#DO the same for BRUV
PtC_BRUV_spacc <- PtC_BRUV_SAC_PA %>%
  mutate(dropnumber = row_number()) %>%
  select(-matches("SUS SUS sus"))%>%
  select(dropnumber, everything()) %>%
  select(-unique_id) %>%
  select(where(~!all(. == 0)))

#generate the species accumulation curves 
bosspaccum <- specaccum (PtC_BOSS_spacc) 

BOSS_SPA_Curve <-plot(bosspaccum, xlab ="Number of deployments", ylab ="Number of species") 
  
bruvspaccum <- specaccum(PtC_BRUV_spacc)

BRUV_SPA_Curve <-plot(bruvspaccum, xlab ="Number of deployments", ylab ="Number of species") 

# Set up the PNG device for plotting

# Set up a 1x2 layout for side-by-side plots
png(
  filename = paste("plots", name, "GABBY.png", sep = "_"),  # Specify the filename
  width = 10, height = 5, res = 300, units = "in"        # Set the width, height, resolution, and units
)

par(mfrow = c(1, 2))
# Plot BRUV_SPA_Curve
plot(bruvspaccum, xlab = "Number of BRUV deployments", ylab = "Number of species", ylim = c(0, 150))
# Plot BOSS_SPA_Curve
plot(bosspaccum, xlab = "Number of BOSS deployments", ylab = "", ylim = c(0, 150))


dev.off()  


#CALC SCALED UBIQUITY FOR THE PLOT 
#Assuming your dataframe is called 'samplefishBRUV' with columns 'species' and 'n'
# Calculate the minimum and maximum ubiquity values
min_ubiquity <- 0
# min_ubiquity <- min(samplefishBRUV$n)
max_ubiquity <- max(samplefishBRUV$n)

# Calculate the scaled ubiquity using the formula
ubiquityfishBRUV <- samplefishBRUV %>%
  dplyr::mutate(scaled_ubiquity = (samplefishBRUV$n - min_ubiquity) / (max_ubiquity - min_ubiquity))


#same but for BOSS
# Calculate the minimum and maximum ubiquity values
min_ubiquity <- 0
# min_ubiquity <- min(samplefishBRUV$n)
max_ubiquity <- max(samplefishBOSS$n)

# Calculate the scaled ubiquity using the formula
ubiquityfishBOSS <- samplefishBOSS %>%
  dplyr::mutate(scaled_ubiquity = (samplefishBOSS$n - min_ubiquity) / (max_ubiquity - min_ubiquity))

#change scientific so it reads name made up of genus and species
ubiquityfishBOSS <- separate(ubiquityfishBOSS, scientific, into = c("family", "genus", "species"), sep = " ") 
ubiquityfishBOSS$name <- paste(ubiquityfishBOSS$genus, ubiquityfishBOSS$species, sep = " ")

#new df with just the name and scaled ubiquity columns from ubiquityfishBOSS df
suBOSS <- select(ubiquityfishBOSS, name, scaled_ubiquity)

#new df just name and aspect ratio from PtCloates_aspectratio_bruvs
PtCloatesarBOSS <- select(PtCloates_aspectratio_boss, name, armean)

#scaled ubiquity and aspect ratio dataframe for BOSS
su_ar_df_BOSS<- left_join(suBOSS, PtCloatesarBOSS, by = "name")

#change scientific so it reads name made up of genus and species
ubiquityfishBRUV <- separate(ubiquityfishBRUV, scientific, into = c("family", "genus", "species"), sep = " ") 
  ubiquityfishBRUV$name <- paste(ubiquityfishBRUV$genus, ubiquityfishBRUV$species, sep = " ")

  #just the name and scaled ubiquity columns from ubiquityfishBRUV df
  suBRUV <- select(ubiquityfishBRUV, name, scaled_ubiquity)
  
  #just name and aspect ratio from PtCloates_aspectratio_bruvs
  PtCloatesarBRUV <- select(PtCloates_aspectratio_bruvs, name, armean)

#scaled ubiquity and aspect ratio dataframe for BRUVS  
  su_ar_df_BRUV<- left_join(suBRUV, PtCloatesarBRUV, by = "name")
  
  ######
  ###attempt to filter out bad data ie Unknown and Sus sus. Later will require further fixing ***check sp w Claude
  su_ar_BRUV <- su_ar_df_BRUV%>%
    filter(name != "Sus sus")%>%
    filter(name != "Unknown spp")%>%
    filter(name != "Unknown sp10")
   
  su_ar_BOSS <- su_ar_df_BOSS%>%
    filter(name != "Sus sus")%>%
    filter(name != "Unknown spp")
  
  #combine boss and bruv scaled ubiquity and aspect ratios to one DF
  all_su_ar <- merge(su_ar_BRUV, su_ar_BOSS, by = "name", all = TRUE)
  
  aspectratiomean <- coalesce(all_su_ar$armean.x, all_su_ar$armean.y)
  all_su_ar <- mutate(all_su_ar, armean = aspectratiomean)
  all_su_ar <- select(all_su_ar, -armean.x, -armean.y)
  all_su_ar <- all_su_ar %>%
    rename(scaledubiquitybruv = scaled_ubiquity.x)%>%
    rename(scaledubiquityboss = scaled_ubiquity.y)  ###here I have 153 species
  
  ###
  # Filter out rows with NA values in armean because we will exclude those from the plot
 ubiquity_aspectratio <- all_su_ar %>%
   filter(armean !="NA") %>%##now have 109 species
    mutate(scaledubiquityboss =scaledubiquityboss * -1) %>%#make BOSS scaled ubiquity negative
 mutate(
   scaledubiquityboss = ifelse(is.na(scaledubiquityboss), 0, scaledubiquityboss),  # Replace NA with 0 in scaledubiquityboss
   scaledubiquitybruv = ifelse(is.na(scaledubiquitybruv), 0, scaledubiquitybruv)  # Replace NA with 0 in scaledubiquitybruv
 )
 
 
  ubiquplot <- ggplot(ubiquity_aspectratio)+
    geom_linerangeh(mapping = aes(y=armean, xmin=scaledubiquityboss, xmax=scaledubiquitybruv), size = 1)+
    geom_vline(xintercept = 0)+
    xlab("Scaled ubiquity")+
    ylab("Aspect ratio of caudal fin")+
    coord_cartesian(xlim = c(-1,1))+
    theme_bw()
  
  print(ubiquplot)
 
 ggsave("ubiquplot.png", plot = ubiquplot, path = "plots/" , width = 8, height = 4, dpi = 300, units = "in")  
 
 
 ##BUBBLE PLOTS BY SPECIES
 L.miniatus <- samplemaxnBRUV %>%
   filter(scientific=="Lethrinidae Lethrinus miniatus")
 
 
 ## Species for bubble plots With the zeros left in and a new column to look at year if needed (year became irrelevant)
 L.miniatus.bruv <- bruv.maxn %>%
   filter(scientific=="Lethrinidae Lethrinus miniatus") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 Gymnocranius.bruv <- bruv.maxn %>%
   filter(scientific=="Lethrinidae Gymnocranius sp1") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 Rubrioperculatus.bruv <- bruv.maxn %>%
   filter(scientific=="Lethrinidae Lethrinus rubrioperculatus") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 C.chrysophrys.bruv <- bruv.maxn %>%
   filter(scientific=="Carangidae Carangoides chrysophrys") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 Pristipomoides.sp1.bruv <- bruv.maxn %>%
   filter(scientific=="Lutjanidae Pristipomoides sp1") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 P.nebulosa.boss <- boss.maxn %>%
   filter(scientific=="Pinguipedidae Parapercis nebulosa") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 L.miniatus.boss <- boss.maxn %>%
   filter(scientific=="Lethrinidae Lethrinus miniatus") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 Gymnocranius.boss <- boss.maxn %>%
   filter(scientific=="Lethrinidae Gymnocranius sp1") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 Pentapodus.boss <- boss.maxn %>%
   filter(scientific=="Nemipteridae Pentapodus nagasakiensis") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 L.sebae.boss <- boss.maxn %>%
   filter(scientific=="Lutjanidae Lutjanus sebae") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 L.rubrioperculatus.boss <- boss.maxn %>%
   filter(scientific=="Lethrinidae Lethrinus rubrioperculatus") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 C.chrysophrys.boss <- boss.maxn %>%
   filter(scientific=="Carangidae Carangoides chrysophrys") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 Pristipomoides.sp1.boss <- boss.maxn %>%
   filter(scientific=="Lutjanidae Pristipomoides sp1") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 P.nebulosa.bruv <- bruv.maxn %>%
   filter(scientific=="Pinguipedidae Parapercis nebulosa") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 Pentapodus.bruv <- bruv.maxn %>%
   filter(scientific=="Nemipteridae Pentapodus nagasakiensis") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 L.sebae.bruv <- bruv.maxn %>%
   filter(scientific=="Lutjanidae Lutjanus sebae") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 # ggplot(L.miniatus, aes(x=longitude, y=latitude))+
 #   geom_point(aes(size=maxn))
 
 # Bubble Plot with zeros
#  p2 <- ggplot()+
#          # geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Z), breaks = c(-30, -40, -50, -60, -70, -80, -90, -100, -110, -120, -130, -140, -150, -160, -170, -180, -200, -220, -240))+
#    geom_point(data=filter(L.miniatus.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape= 21, colour = "blue4", fill = "dodgerblue")+
#    geom_point(data=filter(L.miniatus.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size=0.5)+
#    # coord_sf(xlim = c(113.473, 113.567),                              # Set plot limits
#    #          ylim = c(-22.801, -22.660)) +
#    theme_classic()+
#    labs( x = "Longitude", y = "Latitude")+
#    scale_size(range = c(1,13), name = "Relative abundance")
# 
# print(p2)
 
###PLAIN MINIATUS BRUV
L.miniatus.bruv.bubble <- ggplot()+
  geom_point(data=filter(L.miniatus.bruv, maxn>0), aes(x=longitude, y=latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8) +
  geom_point(data= filter(L.miniatus.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size=0.5)+
  theme_classic()+
  labs(x = "Longitude", y = "Latitude")+
  scale_size_area(max_size=13, name = "Relative abundance", breaks = c(3, 6, 9, 12), labels = c(3, 6, 9, 12))+
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

print(L.miniatus.bruv.bubble)
#SAVE TO MEG DRIVE
ggsave("L.miniatus.bruv.png", plot = L.miniatus.bruv.bubble, path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
  
  ###ATTEMPT2 L.miniatus BRUV with depth plot
longitude_range <- range(L.miniatus.bruv$longitude, na.rm = TRUE)
latitude_range <- range(L.miniatus.bruv$latitude, na.rm = TRUE)

L.miniatus.bruv.Z <-ggplot() +
  geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Z), breaks = c(-300,-290, -280, -270, -260, -250, -240, -230, -220, -210, -200, -190, -180, -170, -160, -150, -140, -130, -120, -110, -100, -90, -80, -70, -60))+ # Add geom_contour
  geom_point(data = filter(L.miniatus.bruv, maxn > 0), aes(x = longitude, y = latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue") +
  geom_point(data = filter(L.miniatus.bruv, maxn == 0), aes(x = longitude, y = latitude), shape = 4, size = 0.5) +
  coord_cartesian(xlim = longitude_range, ylim = latitude_range) +  # Set plot limits
  theme_classic() +
  labs(x = "Longitude", y = "Latitude") +
  scale_size(range = c(1, 13), name = "Relative abundance")+
  # scale_fill_gradient(name = "Depth")+
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

print(L.miniatus.bruv.Z)
 ggsave("L.miniatus.bruv.Z.png", plot = L.miniatus.bruv.Z, path = "plots/BubblePlots/PtCloates" , width = 7, height = 4, dpi = 300, units = "in")

##GYMNOCRANIUS PLAIN PLOTS
 Gymnocranius.bruv.bubble <- ggplot()+
   geom_point(data=filter(Gymnocranius.bruv, maxn>0), aes(x=longitude, y=latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8) +
   geom_point(data=filter(Gymnocranius.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size=0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=10, name = "Relative abundance")+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
   # theme(text = element_text(family = "TimesNewRoman"))
 
 print(Gymnocranius.bruv.bubble)
 
 ggsave("Gymnocranius.bruv.png", plot = Gymnocranius.bruv.bubble, path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")
 
 
 #Gymnocranius BRUV by dominant habitat
 Gymnocranius.bruv.bubble.dom <- dominant_hab +
   geom_point(data=filter(Gymnocranius.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn) , shape = 21, colour = "blue4", fill = "dodgerblue") +
   geom_point(data=filter(Gymnocranius.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size=0.5)+
   theme_classic()
 
 print(Gymnocranius.bruv.bubble.dom)
   
 #RUBRIOPERCULATUS PLAIN BRUV PLOT
 Rubrioperculatus.bruv.bubble <-ggplot()+
   geom_point(data=filter(Rubrioperculatus.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(Rubrioperculatus.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=10, name = "Relative abundance", breaks = c(1, 3, 5, 7), labels = c(1, 3, 5, 7))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 
 #view
 print(Rubrioperculatus.bruv.bubble)
 
 #save
 ggsave("L.rubrioperculatus.bruv.png", plot = Rubrioperculatus.bruv.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
 
 # ##Rubrio + old coastline .shp (FOR 4th CHAPTER)
 # ocf <-st_read("data/spatial/shapefiles/line_recab_merge.shp")
 # plot(ocf)
 # 
 # Rubrioperculatus.bruv.bubble.ocf <-ggplot()+
 #   geom_point(data=filter(Rubrioperculatus.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
 #   geom_point(data=filter(Rubrioperculatus.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
 #   geom_sf(data = ocf) + 
 #   theme_classic()+
 #   labs(x = "Longitude", y = "Latitude")+
 #   scale_size_area(max_size=10, name = "Relative abundance", breaks = c(1, 3, 5, 7), labels = c(1, 3, 5, 7))+
 #   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
 #   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 # 
 # #view
 # print(Rubrioperculatus.bruv.bubble.ocf)
 
 #Carangoides chrysophrys plain bruv plot
 C.chrysophrys.bruv.bubble <- ggplot()+
   geom_point(data=filter(C.chrysophrys.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(C.chrysophrys.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=13, name = "Relative abundance", breaks = c(1, 5, 9, 13), labels = c(1, 5, 9, 13))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(C.chrysophrys.bruv.bubble)
 
 #save plain bubble plot
 ggsave("C.chrysophrys.bruv.png", plot = C.chrysophrys.bruv.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
 
 ##quick view at years
 YEAR.C.chrysophrys.bruv.bubble <- ggplot() +
   geom_point(data = filter(C.chrysophrys.bruv, maxn > 0), aes(x = longitude, y = latitude, size = maxn, color = factor(year == 2022)), shape = 21, alpha = 0.8) +
   geom_point(data = filter(C.chrysophrys.bruv, maxn == 0), aes(x = longitude, y = latitude, color = factor(year ==2022)), shape = 4, size = 0.5, alpha = 0.8) +
   theme_classic() +
   labs(x = "Longitude", y = "Latitude") +
   scale_size_area(max_size = 13, name = "Relative abundance", breaks = c(1, 5, 9, 13), labels = c(1, 5, 9, 13)) +
   scale_color_manual(values = c("green", "purple"), name = "Year", labels = c("2021", "2022")) +
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(YEAR.C.chrysophrys.bruv.bubble)
 
 # # Create a spatial object
 # C.c.spatial.points <- st_as_sf(C.chrysophrys.bruv, coords = c("longitude", "latitude"), crs = 4326)
 # 
 # # Save as shapefile
 # st_write(C.c.spatial.points, "C.chrysophrys_shapefile.shp")
 
 
 
 #Pristipomoides sp1 plain bruv plot
 Pristipomoides.sp1.bruv.bubble <- ggplot()+
   geom_point(data=filter(Pristipomoides.sp1.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(Pristipomoides.sp1.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=16, name = "Relative abundance", breaks = c(1,5, 9, 13, 17), labels = c(1, 5, 9, 13, 17))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(Pristipomoides.sp1.bruv.bubble)
 
 #save plain bubble plot
 ggsave("Pristipomoides.sp1.bruv.png", plot = Pristipomoides.sp1.bruv.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
 
 ##BRUV PLain drop for BOSS dominant species
 #Parapercis nebulosa BRUV PLAIN plot
 P.nebulosa.bruv.bubble <- ggplot()+
   geom_point(data=filter(P.nebulosa.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(P.nebulosa.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=10, name = "Relative abundance", breaks = c(1,2, 3, 4, 5), labels = c(1, 2, 3, 4, 5))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
print(P.nebulosa.bruv.bubble)

#save plain bubble plot
ggsave("P.nebulosa.bruv.png", plot = P.nebulosa.bruv.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  

 #Pentapodus nagasakiensis BRUV PLAIN plot   
 Pentapodus.bruv.bubble <- ggplot()+
   geom_point(data=filter(Pentapodus.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(Pentapodus.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=16, name = "Relative abundance", breaks = c(3,6, 9, 12, 15), labels = c(3, 6, 9, 12, 15))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(Pentapodus.bruv.bubble)
 
 #save plain bubble plot
 ggsave("Pentapodus.bruv.png", plot = Pentapodus.bruv.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
 
#Lutjanus sebae BRUV PLAIN plot   
 L.sebae.bruv.bubble <- ggplot()+
   geom_point(data=filter(Pentapodus.bruv, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(Pentapodus.bruv, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=12, name = "Relative abundance", breaks = c(1, 5, 10, 15), labels = c(1, 5, 10, 15))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(L.sebae.bruv.bubble)
 
 #save plain bubble plot
 ggsave("L.sebae.bruv.png", plot = L.sebae.bruv.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
 
 
 #BOSS PLOTS
 #BOSS plain P. nebulosa
 P.nebulosa.boss.bubble <-ggplot()+
   geom_point(data=filter(P.nebulosa.boss, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(P.nebulosa.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=6, name = "Relative abundance", breaks = c(1,2, 3), labels = c(1, 2, 3))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(P.nebulosa.boss.bubble)
 
 #save
 ggsave("P.nebulosa.boss.png", plot = P.nebulosa.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
 
 L.miniatus.boss.bubble <- ggplot()+
   geom_point(data=filter(L.miniatus.boss, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(L.miniatus.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=6, name = "Relative abundance", breaks = c(1,2), labels = c(1, 2))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(L.miniatus.boss.bubble)
 
 #save
 ggsave("L.miniatus.boss.png", plot = L.miniatus.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")  
 
 
 Gymnocranius.boss.bubble <- ggplot()+
   geom_point(data=filter(Gymnocranius.boss, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(Gymnocranius.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size=0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=8, name = "Relative abundance", breaks = c(1,2,4,5,6), labels = c(1, 2,4,5,6))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(Gymnocranius.boss.bubble)
 
 #save
 ggsave("Gymnocranius.boss.png", plot = Gymnocranius.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in") 
 
 # Gymnocranius.boss.bubble.domhab <- dominant_hab +
 #   geom_point(data=filter(Gymnocranius.boss, maxn>0), aes(x=longitude, y=latitude, size=maxn))+
 #   geom_point(data=filter(Gymnocranius.boss, maxn==0), aes(x=longitude, y=latitude), shape=4)+
 #   theme_classic()
 # 
 # print(Gymnocranius.boss.bubble.domhab)
 
 #Pentapodus nagasakiensis boss plain bubble plot
 P.nagasakiensis.boss.bubble <-ggplot()+
   geom_point(data=filter(Pentapodus.boss, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(Pentapodus.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=8, name = "Relative abundance", breaks = c(1,2,5), labels = c(1, 2,5))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(P.nagasakiensis.boss.bubble)
 
 #save
 ggsave("P.nagasakiensis.boss.png", plot = P.nagasakiensis.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in") 
 
 #L. sebae plain bubbleplot
 L.sebae.boss.bubble <- ggplot()+
   geom_point(data=filter(L.sebae.boss, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(L.sebae.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=8, name = "Relative abundance", breaks = c(1,2), labels = c(1, 2))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 ggplot() +
   geom_point(data = filter(L.sebae.boss, maxn > 0), aes(x = longitude, y = latitude, size = maxn, shape = "Non-zero"), colour = "blue4", fill = "dodgerblue", alpha = 0.8) +
   geom_point(data = filter(L.sebae.boss, maxn == 0), aes(x = longitude, y = latitude, shape = "Zero"), size = 3, colour = "black") +
   theme_classic() +
   labs(x = "Longitude", y = "Latitude") +
   scale_size_area(max_size = 8, name = "Relative abundance", breaks = c(1, 2), labels = c(1, 2)) +
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
   scale_shape_manual(values = c("Non-zero" = 21, "Zero" = 4), name = "Maxn", labels = c("Non-zero", "Zero")) +
   guides(size = guide_legend(override.aes = list(shape = 21)))  # Combine legends
 
 print(L.sebae.boss.bubble)
 
 #save
 ggsave("L.sebae.boss.png", plot = L.sebae.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in") 
 
 ##BOSS PLain drop for BRUV dominant species
 #BOSS L.rubrioperculatus plain bubble plot
 L.rubrioperculatus.boss.bubble <- ggplot()+
   geom_point(data=filter(L.rubrioperculatus.boss, maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(L.rubrioperculatus.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=8, name = "Relative abundance", breaks = c(1,2), labels = c(1, 2))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(L.rubrioperculatus.boss.bubble)
 
 #save
 ggsave("L.rubrioperculatus.boss.png", plot = L.rubrioperculatus.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in") 
 
  #BOSS C.chrysophrys plain bubble plot
 C.chrysophrys.boss.bubble <- ggplot()+
   geom_point(data=filter(C.chrysophrys.boss , maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(C.chrysophrys.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=8, name = "Relative abundance", breaks = c(1), labels = c(1))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(C.chrysophrys.boss.bubble)
 
 #save
 ggsave("C.chrysophrys.boss.png", plot =  C.chrysophrys.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in") 
 
  #BOSS Pristipomoides sp1 plain bubble plot
 Pristipomoides.sp1.boss.bubble <- ggplot()+
   geom_point(data=filter(Pristipomoides.sp1.boss , maxn>0), aes(x=longitude, y=latitude, size=maxn), shape = 21, colour = "blue4", fill = "dodgerblue", alpha = 0.8)+
   geom_point(data=filter(Pristipomoides.sp1.boss, maxn==0), aes(x=longitude, y=latitude), shape=4, size = 0.5)+
   theme_classic()+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=8, name = "Relative abundance", breaks = c( 2,3,5), labels = c( 2,3,5))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
 
 print(Pristipomoides.sp1.boss.bubble)
 
 #save
 ggsave("Pristipomoides.sp1.boss.png", plot =  Pristipomoides.sp1.boss.bubble , path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in") 
 
 
 
 ###LOADING HABITAT BACKGROUND
 # Set CRS for shapefiles
 wgscrs <- "+proj=longlat +datum=WGS84"                                    # Lat long projection
 
 # Bring in spatial layers
 # Load aus outline, state and commonwealth marine parks
 aus     <- st_read("data/spatial/shapefiles/cstauscd_r.mif")                    # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
 aus     <- aus[aus$FEAT_CODE == "mainland", ]                                   # Add islands here if needed
 aumpa   <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")   # All aus mpas
 st_crs(aus) <- st_crs(aumpa)                                                    # Set CRS to match - WGS84 and GDA94 effectively the same
 e <- ext(113, 114.5, -23, -21)                                                 # Change your extent here
 mpa <- st_crop(aumpa, e)                                                        # All commonwealth zones in the study area
 npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]  
 aus2 <- st_crop(aus, e)# Only National Park Zones in the study area
 
 # Load tidy habitat data from CHAPTER2: COPIED TO CH3 TIDY
 habi    <- readRDS('data/tidy/PtCloates/PtCloates_habitat-bathy-derivatives.rds') %>%
   dplyr::mutate(Z = abs(Z)) %>%  
   dplyr::filter(!is.na(TPI)) %>%
   glimpse()
 
 # Load the coastal waters boundary
 cwatr  <- st_read('data/spatial/shapefiles/amb_coastal_waters_limit.shp')  
 
 # Load bathymetry data 
 bathdf <- readRDS(paste(paste0('data/spatial/rasters/',                         
                                name), 'ga_bathy.rds', sep = "_"))
 
 spatialcovariates <- readRDS(paste(paste0('data/spatial/rasters/',                         
                                           name), 'spatial_covariates.rds', sep = "_"))
   plot(spatialcovariates)
   
   #convert spatraster to a raster
rasterspatialcov <- as(spatialcovariates, "Raster")

    # convert raster to a df
spatialcov_df <- as.data.frame(rasterspatialcov, xy =TRUE)
   
 # Load spatial predictions from  CHAPTER 2'R/05_habitat_model.R' COPIED TO DATA/TIDY CH3
 spreddf <- readRDS('data/tidy/PtCloates/PtCloates_spatial_habitat_predictions.rds') %>%
   dplyr::mutate(Z = abs(Z)) %>%  
   dplyr::filter(!is.na(TPI)) %>%
   dplyr::mutate(dom_tag = as.factor(dom_tag)) %>%                               # Factorise
   dplyr::mutate(dom_tag = dplyr::recode(dom_tag,                                # Tidy names for plot legend
                                         sand.fit = "Sand",
                                         inverts.fit = "Sessile invertebrates")) %>%
    glimpse()
 
 
 # Figure 1: Categorical habitat maps ----
 # Assign habitat class colours
 hab_cols <- scale_fill_manual(values = c(#"Macroalgae" = "#009E73",
   #"Rock" = "#D55E00",
   "Sand" = "lemonchiffon",
   "Sessile invertebrates" = "lightcyan1"
 ))
 
 npz_cols <- scale_colour_manual(values = c("National Park Zone" = "#7bbc63"),
                                 name = "Australian Marine Parks")
 
 
 
 
 
 #Build dom plot adapted for bubble plots
 dominant_hab <- ggplot() +
   geom_tile(data = spreddf, aes(x, y, fill = dom_tag)) +
   hab_cols +   # Class colours
   labs(fill = "Habitat") +
   new_scale_fill() +
   # new_scale_colour()+
   geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
                breaks = c( -30, -70, -130, - 200),                                 # Contour breaks - change to binwidth for regular contours
                colour = "grey80",
                alpha = 1, size = 0.5) +                                         # Transparency and linewidth
   geom_sf(data = npz, fill = NA, aes(colour = ZoneName), linewidth = 0.5) +  # Add national park zones
   npz_cols +
   new_scale_colour() +
   geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.5) +       #trying to add in AUSMAP
   coord_sf(xlim = c(113.47, 113.57),                              # Set plot limits
            ylim = c(-22.80, -22.66)) +
     labs(x = NULL, y = NULL,                                     # Labels  
        colour = NULL) +
   annotate("text", x = c(113.562, 113.52, 113.475),         # Add contour labels manually
            y = c(-22.75, -22.75, -22.75), 
            label = c("70m", "130m", "200m"),
            size = 2, colour = "#000000") +
   theme_minimal() +
   
   theme(panel.grid = element_blank())+
   theme(axis.ticks = element_line(size = 0.5, linetype = "dashed"))+
   theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
   theme(plot.margin = margin(0.5, 0.5, 0.5, 2, "cm"))
   
 
 print(dominant_hab)
 
 
 
 p3 <- dominant_hab +
   geom_point(data = filter(L.miniatus.bruv, maxn > 0), aes(x = longitude, y = latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue") +
   geom_point(data = filter(L.miniatus.bruv, maxn == 0), aes(x = longitude, y = latitude), shape = 4, size = 0.5) +
   scale_size(range = c(1, 13), name = "Relative abundance") +
   labs(x = "Longitude", y = "Latitude")

 print(p3) 
 
 
# CHARLOTTE PLOT### L.miniatus BRUV with depth plot
 longitude_range <- range(L.miniatus.bruv$longitude, na.rm = TRUE)
 latitude_range <- range(L.miniatus.bruv$latitude, na.rm = TRUE)
 
 L.miniatus.bruv.Z <-ggplot() +
   geom_contour_filled(data = bathdf, aes(x = x, y = y, z = Z, fill=after_stat(level_mid)), breaks = c(-300,-290, -280, -270, -260, -250, -240, -230, -220, -210, -200, -190, -180, -170, -160, -150, -140, -130, -120, -110, -100, -90, -80, -70, -60))+ # Add geom_contour
   geom_point(data = filter(L.miniatus.bruv, maxn > 0), aes(x = longitude, y = latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue") +
   geom_point(data = filter(L.miniatus.bruv, maxn == 0), aes(x = longitude, y = latitude), shape = 4, size = 0.5) +
   coord_cartesian(xlim = longitude_range, ylim = latitude_range) +  # Set plot limits
   theme_classic() +
   scale_fill_viridis(name="Depth", limits=c(-300, -50), labels=c(-300, -250, -200, -150, -100, -50))+
     # scale_fill_gradient(name = "Depth")+
   theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))+
   # scale_fill_distiller(super=metR::ScaleDiscretised, palette="Spectral")+
   labs(x = "Longitude", y = "Latitude")+
   scale_size_area(max_size=13, name = "Relative abundance", breaks = c(3, 6, 9, 12), labels = c(3, 6, 9, 12))+
   scale_x_continuous(labels = scales::number_format(accuracy = 0.01))+
   scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
   # metR::scale_fill_discretised(labels = c(-300,-290, -280, -270, -260, -250, -240, -230, -220, -210, -200, -190, -180, -170, -160, -150, -140, -130, -120, -110, -100, -90, -80, -70, -60), breaks= c(-300,-290, -280, -270, -260, -250, -240, -230, -220, -210, -200, -190, -180, -170, -160, -150, -140, -130, -120, -110, -100, -90, -80, -70, -60))
 
 print(L.miniatus.bruv.Z)
 
 plot(bathdf)
 depthplot<- ggplot() +
   geom_tile(data = bathdf, 
             aes(x, y, fill = Z)) 
 
 
 print(depthplot)
 
 
 ggsave("L.miniatus.bruv.Z.png", plot = L.miniatus.bruv.Z, path = "plots/BubblePlots/PtCloates" , width = 8, height = 4, dpi = 300, units = "in")
 
 ##Detrended creation
 detrended <- data.frame(
   x = spatialcov_df$x,
   y = spatialcov_df$y,
   detrended = spatialcov_df$detrended
 )
 
 # histo.detrended <- hist(detrended$detrended)
 
 L.miniatus.bruv.detrended <-ggplot() +
   geom_contour_filled(data = detrended, aes(x = x, y = y, z = detrended), breaks = c(-110, -100, -90, -80, -70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90))+ # Add geom_contour
   geom_point(data = filter(L.miniatus.bruv, maxn > 0), aes(x = longitude, y = latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue") +
   geom_point(data = filter(L.miniatus.bruv, maxn == 0), aes(x = longitude, y = latitude), shape = 4, size = 0.5) +
   coord_cartesian(xlim = longitude_range, ylim = latitude_range) +  # Set plot limits
   theme_classic() +
   labs(x = "Longitude", y = "Latitude") +
   scale_size(range = c(1, 13), name = "Relative abundance")+
   # scale_fill_gradient(name = "Depth")+
   theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
 
 print(L.miniatus.bruv.detrended)

 ##SLOPE creation
 slope <- data.frame(
   x = spatialcov_df$x,
   y = spatialcov_df$y,
   slope = spatialcov_df$slope
 )
 
 ##SLOPE fit
 L.miniatus.bruv.slope <-ggplot() +
   geom_contour_filled(data = slope, aes(x = x, y = y, z = slope), breaks = c(0, 1, 2, 3, 4, 5))+ # Add geom_contour
   geom_point(data = filter(L.miniatus.bruv, maxn > 0), aes(x = longitude, y = latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue") +
   geom_point(data = filter(L.miniatus.bruv, maxn == 0), aes(x = longitude, y = latitude), shape = 4, size = 0.5) +
   coord_cartesian(xlim = longitude_range, ylim = latitude_range) +  # Set plot limits
   theme_classic() +
   labs(x = "Longitude", y = "Latitude") +
   scale_size(range = c(1, 13), name = "Relative abundance")+
   # scale_fill_gradient(name = "Depth")+
   theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
 
 print(L.miniatus.bruv.slope)

 #LINEARTREND creation
 lineartrend <- data.frame(
   x = spatialcov_df$x,
   y = spatialcov_df$y,
   lineartrend = spatialcov_df$lineartrend
 )
 
 ##Lineartrend fit
 L.miniatus.bruv.lineartrend <-ggplot() +
   geom_contour_filled(data = lineartrend, aes(x = x, y = y, z = lineartrend), breaks = c(-280, -270, -260, -250, -240,-230, -220, -210, -200, -190, -180,-170, -160,-150, -140,-130, -120 ,-110, -100, -90, -80, -70, -60, -50, -40, -30, -20, -10, 0))+ # Add geom_contour
   geom_point(data = filter(L.miniatus.bruv, maxn > 0), aes(x = longitude, y = latitude, size = maxn), shape = 21, colour = "blue4", fill = "dodgerblue") +
   geom_point(data = filter(L.miniatus.bruv, maxn == 0), aes(x = longitude, y = latitude), shape = 4, size = 0.5) +
   coord_cartesian(xlim = longitude_range, ylim = latitude_range) +  # Set plot limits
   theme_classic() +
   labs(x = "Longitude", y = "Latitude") +
   scale_size(range = c(1, 13), name = "Relative abundance")
   # scale_fill_gradient(name = "Depth")+
   # theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))
 
 print(L.miniatus.bruv.lineartrend)
 