###
# Project: G Cummins FISH Paper
# Script: Making stacked bar plots w maxn
# Task:  FISH UBIQUITY PLOT
# author:  G Cummins 
# date:    August 2024
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
library(grid)

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
  dplyr::mutate(scientific = ifelse(scientific == "Lethrinidae Gymnocranius sp1",
                                    "Lethrinidae Gymnocranius grandoculis", scientific))%>%
  dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius grandoculis", "grandoculis", species)) %>%
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
  dplyr::mutate(scientific = ifelse(scientific == "Lethrinidae Gymnocranius sp1",
                                    "Lethrinidae Gymnocranius grandoculis", scientific))%>%
  dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius grandoculis", "grandoculis", species)) %>%
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

#to get each MAXN sample on BOSS
samplemaxnBOSS <- boss.maxn %>%
  filter(maxn>0)

#Total number of individual fish seen on BOSS and remove SUS
totalfishBOSS <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  filter(scientific !="SUS SUS sus")%>%
  dplyr::summarise(totalfish = sum(maxn))
#sum of all individuals to see total fish seen on BOSS
total_sumindifishboss <- sum(totalfishBOSS$totalfish)
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

#to get each MAXN sample on BRUVS
samplemaxnBRUV <- bruv.maxn %>%
  filter(maxn>0) 

BRUV.individual.fish <- sum(sfbruv_inds_n$totalfish)
BRUV.individual.fish

#Total number of individual fish seen on BRUVS
totalfishBRUV <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  filter(scientific !="SUS SUS sus")%>%
  dplyr::summarise(totalfish = sum(maxn))
#sum of all individuals to see total fish seen on BRUVs
total_sumindifishbruv <- sum(totalfishBRUV$totalfish)
print(total_sumindifishbruv)

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

####NEW ASPECT RATIO CALCS
#determine caudal aspect ratio for each species on bruvs
bruv_species <-bruv.maxn %>%
  filter(maxn>0) %>%
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
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
  dplyr::select(-fishbase_scientific)%>%
  rename(name = caab_scientific)%>%
  dplyr::select(name, everything()) %>%
  add_row(name = "Pristipomoides sp1", armean = 3.34, n = 4, sd = NA, se = NA) %>%
  add_row(name = "Seriola sp1", armean = 3.29, n = 3, sd = NA, se = NA)

 # write.csv(PtCloates_aspectratio_bruvs, file = "outputs/PtCloates/PtCloatesaspectratioBRUVS_20240827.csv", row.names = FALSE)

##summary statistics for Pt Cloates aspect ratio BRUV (this is our BRUV species w aspect ratio)
#Note this is all species (not all individual fish)
mean_ptc_ar_bruv <- mean(PtCloates_aspectratio_bruvs$armean, na.rm = TRUE)
median_ptc_ar_bruv <- median(PtCloates_aspectratio_bruvs$armean, na.rm = TRUE)

#REPEAT ALL ABOVE BUT FOR BOSS:
#determine caudal aspect ratio for each species on boss
boss_species <-boss.maxn %>%
  filter(maxn>0) %>%
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
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
  dplyr::select(-fishbase_scientific)%>%
  rename(name = caab_scientific)%>%
  dplyr::select(name, everything())%>%
  add_row(name = "Pristipomoides sp1", armean = 3.34, n = 4, sd = NA, se = NA) 

 # write.csv(PtCloates_aspectratio_boss, file = "outputs/PtCloates/PtCloatesaspectratioBOSS.csv", row.names = FALSE)

##summary statistics for Pt Cloates aspect ratio BOSS (this is our BOSS species w aspect ratio)
#Note this is all species (not all individual fish)
mean_ptc_ar_boss <- mean(PtCloates_aspectratio_boss$armean, na.rm = TRUE)
median_ptc_ar_boss <- median(PtCloates_aspectratio_boss$armean, na.rm = TRUE)

#Aspect ratio for individuals on bruvs (not just sp) - ALL FISH INDIVIDUALS SEEN
#Same df as sfbruv_inds but wth name instead of scientific
bruv_individuals <- bruv.maxn %>%
  filter(maxn>0) %>%
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
  dplyr::mutate(name = paste(genus, species)) %>%
  group_by(name)%>%
  dplyr::summarise(totalindividuals = sum(maxn))

# Duplicate rows based on the totalindividuals column
bruv_individuals_expanded <- bruv_individuals %>%
  uncount(totalindividuals)

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
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
  dplyr::mutate(name = paste(genus, species)) %>%
  group_by(name)%>%
  dplyr::summarise(totalindividuals = sum(maxn))

# Duplicate rows based on the totalindividuals column
boss_individuals_expanded <- boss_individuals %>%
  uncount(totalindividuals)

# View the resulting dataframe
# View(boss_individuals_expanded)

#leftjoin w aspect ratio
boss_individuals_aspectratio <- left_join(boss_individuals_expanded, PtCloates_aspectratio_boss, by = "name")

#write.csv(boss_individuals_aspectratio, file = "outputs/PtCloates/PtCindividualsaspratioBOSS.csv", row.names = FALSE)

#SUMMARY STATISTICS for BOSS all individuals and aspect ratio
boss_ind_ar_mean<- mean(boss_individuals_aspectratio$armean, na.rm = TRUE)
boss_ind_ar_ste_mean <- mean(boss_individuals_aspectratio$se, na.rm = TRUE)
summary(boss_individuals_aspectratio$armean)
table(is.na(boss_individuals_aspectratio$armean))

#CALC SCALED UBIQUITY FOR THE PLOT 
#Assuming your dataframe is called 'samplefishBRUV' with columns 'species' and 'n'

#BRUVS fish numbers seen on how many samples ##EDIT SO ITS AR ##THIS IS sfbruv
ar.samplefishBRUV <- bruv.maxn %>%
  filter(maxn>0) %>%
  # dplyr::mutate(scientific = ifelse(scientific == "Lethrinidae Gymnocranius sp1",
                                    # "Lethrinidae Gymnocranius grandoculis", scientific))%>%
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n())

BRUV_individuals_AR <- bruv.maxn %>%  ##THIS IS sfbruv_inds
  filter(maxn>0) %>%
  # dplyr::mutate(scientific = ifelse(scientific == "Lethrinidae Gymnocranius sp1",
                                    # "Lethrinidae Gymnocranius grandoculis", scientific))%>%
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
  group_by(scientific) %>%
  dplyr::summarise(totalindividuals = sum(maxn))

BRUV_individuals_AR_sep <- separate(BRUV_individuals_AR, scientific, into = c("family", "genus", "species"), sep = " ")%>%
  mutate(name = paste(genus, species, sep =" "))

# Calculate the minimum and maximum ubiquity values
min_ubiquity <- 0
# min_ubiquity <- min(samplefishBRUV$n)
max_ubiquity <- max(ar.samplefishBRUV$n)

# Calculate the scaled ubiquity using the formula
ubiquityfishBRUV1 <- ar.samplefishBRUV %>%
  dplyr::mutate(scaled_ubiquity = (ar.samplefishBRUV$n - min_ubiquity) / (max_ubiquity - min_ubiquity))

ubiquityfishBRUV <- ubiquityfishBRUV1 %>%
  left_join(BRUV_individuals_AR%>% dplyr::select(scientific,totalindividuals), by = "scientific")

#change scientific so it reads name made up of genus and species
ubiquityfishBRUV <- separate(ubiquityfishBRUV, scientific, into = c("family", "genus", "species"), sep = " ") 
ubiquityfishBRUV$name <- paste(ubiquityfishBRUV$genus, ubiquityfishBRUV$species, sep = " ")

#just the name and scaled ubiquity columns from ubiquityfishBRUV df
suBRUV <- dplyr::select(ubiquityfishBRUV, name, scaled_ubiquity, n, totalindividuals)

#just name and aspect ratio from PtCloates_aspectratio_bruvs
PtCloatesarBRUV <- dplyr::select(PtCloates_aspectratio_bruvs, name, armean)

#scaled ubiquity and aspect ratio dataframe for BRUVS  
su_ar_df_BRUV<- left_join(suBRUV, PtCloatesarBRUV, by = "name")%>%
  rename(n_BRUV = n)%>%
  rename(indi_BRUV = totalindividuals) %>%
  filter(!is.na(armean))

#BRUVsonly
plot(su_ar_df_BRUV$indi_BRUV,su_ar_df_BRUV$armean)
#TESTING INDIVIDUALS
# Create the new dataframe by repeating rows
INDIS_su_ar_BRUV <- su_ar_df_BRUV %>%
  uncount(weights = indi_BRUV)

# View the new dataframe
glimpse(INDIS_su_ar_BRUV)
#TESTCHECK
SUM <- su_ar_df_BRUV %>%
  summarise(total_indi_BRUV = sum(indi_BRUV, na.rm = TRUE))

BRUV_AR_INDIFISH <- INDIS_su_ar_BRUV %>%
  summarise(mean_ar_individuals_bruv = mean(armean, na.rm = TRUE))

# Create the bar plot
ggplot(su_ar_df_BRUV, aes(x = armean, y = indi_BRUV)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Number of Individuals by armean",
       x = "armean",
       y = "Number of Individuals (indi_BRUV)") +
  theme_minimal()

#same but for BOSS
#BOSS fish species seen on how many samples
ar.samplefishBOSS <- boss.maxn %>%
  filter(maxn>0) %>%
  # dplyr::mutate(scientific = ifelse(scientific == "Lethrinidae Gymnocranius sp1",
  #                                   "Lethrinidae Gymnocranius grandoculis", scientific))%>%
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n()) 

BOSS_individuals_AR <- boss.maxn %>%
  filter(maxn>0) %>%
  # dplyr::mutate(scientific = ifelse(scientific == "Lethrinidae Gymnocranius sp1",
  # "Lethrinidae Gymnocranius grandoculis", scientific))%>%
  # dplyr::mutate(species = ifelse(scientific == "Lethrinidae Gymnocranius sp1", "grandoculis", species)) %>%
  group_by(scientific) %>%
  dplyr::summarise(totalindividuals = sum(maxn))

BOSS_individuals_AR_sep <- separate(BOSS_individuals_AR, scientific, into = c("family", "genus", "species"), sep = " ") %>%
  mutate(name = paste(genus, species, sep = " "))

# Calculate the minimum and maximum ubiquity values
min_ubiquity <- 0
# min_ubiquity <- min(samplefishBRUV$n)
max_ubiquity <- max(ar.samplefishBOSS$n)

# Calculate the scaled ubiquity using the formula
ubiquityfishBOSS1 <- ar.samplefishBOSS %>%
  dplyr::mutate(scaled_ubiquity = (ar.samplefishBOSS$n - min_ubiquity) / (max_ubiquity - min_ubiquity))

ubiquityfishBOSS <- ubiquityfishBOSS1 %>%
  left_join(BOSS_individuals_AR%>% dplyr::select(scientific, totalindividuals), by = "scientific")

#change scientific so it reads name made up of genus and species
ubiquityfishBOSS <- separate(ubiquityfishBOSS, scientific, into = c("family", "genus", "species"), sep = " ") 
ubiquityfishBOSS$name <- paste(ubiquityfishBOSS$genus, ubiquityfishBOSS$species, sep = " ")

#new df with just the name and scaled ubiquity columns from ubiquityfishBOSS df
suBOSS <- dplyr::select(ubiquityfishBOSS, name, scaled_ubiquity, n, totalindividuals)

#new df just name and aspect ratio from PtCloates_aspectratio_bruvs
PtCloatesarBOSS <- dplyr::select(PtCloates_aspectratio_boss, name, armean)

#scaled ubiquity and aspect ratio dataframe for BOSS
su_ar_df_BOSS<- left_join(suBOSS, PtCloatesarBOSS, by = "name") %>%
  rename(n_BOSS = n)%>%
  rename(indi_BOSS = totalindividuals)%>%
  filter(!is.na(armean))

#TESTING INDIVIDUALS
# Create the new dataframe by repeating rows
INDIS_su_ar_BOSS <- su_ar_df_BOSS %>%
  uncount(weights = indi_BOSS)

# View the new dataframe
glimpse(INDIS_su_ar_BOSS)
#TESTCHECK
SUM <- su_ar_df_BOSS %>%
  summarise(total_indi_BOSS = sum(indi_BOSS, na.rm = TRUE))

BOSS_AR_INDIFISH <- INDIS_su_ar_BOSS %>%
  summarise(mean_ar_individuals_boss = mean(armean, na.rm = TRUE))


# Create the bar plot
ggplot(su_ar_df_BOSS, aes(x = armean, y = indi_BOSS)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Bar Plot of Number of Individuals by armean",
       x = "armean",
       y = "Number of Individuals (indi_BOSS)") +
  theme_minimal()

######
###attempt to filter out bad data ie Unknown and Sus sus. Later will require further fixing ***check sp w Claude
su_ar_BRUV <- su_ar_df_BRUV     ###rejigged
  # filter(name != "Sus sus")%>%
  # filter(name != "Unknown spp")%>%
  # filter(name != "Unknown sp10")

su_ar_BOSS <- su_ar_df_BOSS
  # filter(name != "Sus sus")%>%
  # filter(name != "Unknown spp")

#combine boss and bruv scaled ubiquity and aspect ratios to one DF
all_su_ar <- merge(su_ar_BRUV, su_ar_BOSS, by = "name", all = TRUE)

 aspectratiomean <- coalesce(all_su_ar$armean.x, all_su_ar$armean.y)
all_su_ar <- mutate(all_su_ar, armean = aspectratiomean)
all_su_ar <- dplyr::select(all_su_ar, -armean.x, -armean.y)
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
  )%>%
  mutate(n_BRUV = ifelse(is.na(n_BRUV), 0, n_BRUV), #replace NA with 0 in n_BRUV
         n_BOSS = ifelse(is.na(n_BOSS), 0, n_BOSS)) #replace NA with 0 in n_BOSS


write.csv(ubiquity_aspectratio, file = "outputs/PtCloates/ubiquity_aspectratio.csv", row.names = FALSE)
# summary(ubiquity_aspectratio$nBRUV_armean)
# summary(bruv_individuals_aspectratio$armean)

# BRUV <- textGrob(label = expression(bold("BRUV")), x = 0, y = 0, 
#                            just = "left", gp = gpar(col = "#000000", fontsize = 11))
# 
# BOSS <- textGrob(label = expression("BOSS"), x = 0, y = 0, 
#                  just = "left", gp = gpar(col = "#000000", fontsize = 11))
BOSS_AR_INDIFISH <- 2.500  # Replace with actual value
BRUV_AR_INDIFISH <- 2.875

ubiquplot <- ggplot(ubiquity_aspectratio)+
  geom_linerangeh(mapping = aes(y=armean, xmin=scaledubiquityboss, xmax=scaledubiquitybruv), size = 1.0)+
  geom_vline(xintercept = 0)+
  xlab("\nScaled ubiquity")+
  ylab("\nAspect ratio of caudal fin")+
  coord_cartesian(xlim = c(-1,1))+
  theme_bw()+
  scale_y_continuous(breaks = seq(floor(min(ubiquity_aspectratio$armean)), ceiling(max(ubiquity_aspectratio$armean)), by = 1)) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    axis.title.x = element_text(size = 14),  # Increase x-axis title size
    axis.text.y = element_text(size = 12),   # Increase y-axis text size
    axis.text.x = element_text(size = 12) 
  )+
  
  # Add arrows on the x-axis using geom_segment
  # geom_segment(aes(x = -0.05, y = -0.5, xend = -1, yend = -0.5),
  #              arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 0.6) +
  # geom_segment(aes(x = 0.05, y = -0.1, xend = 1, yend = -0.1),
  #              arrow = arrow(length = unit(0.2, "inches"), type = "closed"), size = 0.6) +
  # 
  # # Add labels for BRUV and BOSS
  # annotate("text", x = -0.9, y = 0.1, label = "BOSS", hjust = 0, vjust = 1) +
  # annotate("text", x = 0.9, y = 0.1, label = "BRUV", hjust = 1, vjust = 1)
  

  # Add labels for BRUV and BOSS
  # annotation_custom(BRUV, xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 0.5) +
  # annotation_custom(BOSS, xmin = -0.9, xmax = -0.5, ymin = -0.1, ymax = -0.1) +

theme(plot.margin = margin(t = 0.5, r = 2.5, b = 0.5, l = 0.5, unit = "cm")) +
  geom_segment(aes(x = -1.0, xend = 0.0, y = BOSS_AR_INDIFISH, yend = BOSS_AR_INDIFISH),
               linetype = "dashed", color = "blue", size = 1) +
  geom_segment(aes(x = 0.0, xend = 1.0, y = BRUV_AR_INDIFISH, yend = BRUV_AR_INDIFISH),
               linetype = "dashed", color = "blue", size = 1) 
  

print(ubiquplot)

ggsave("PtCloates_ubiquplot_complete6.png", plot = ubiquplot, path = "plots/" , width = 11, height = 15, dpi = 600, units = "in")  




BOSSBRUV_ubiquity_aspectratio <- ubiquity_aspectratio 


####GLM
GLM_BRUV = bruv.maxn %>%
  dplyr::select(unique_id, scientific, maxn, family, genus, species, method)%>%
  mutate(name = paste(genus, species, sep = " "))

# Perform the join and add the new columns
GLMBRUV_FINAL <- GLM_BRUV %>%
  left_join(su_ar_df_BRUV %>% dplyr::select(name, scaled_ubiquity, armean), by = "name")%>%
  filter(maxn>0)
