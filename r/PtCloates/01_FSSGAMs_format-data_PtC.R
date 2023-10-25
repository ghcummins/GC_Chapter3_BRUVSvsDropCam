###
# Project: Ch3 PhD Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Join Abrolhos BOSS and BRUV, format data for fssGAM
# author:  Gabby, Claude, Brooke, Kingsley
# date:    October 2023
##

rm(list=ls())

# libraries----
detach("package:plyr", unload=TRUE)#will error - don't worry
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

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "PtCloates"   # set study name ##

# load and join datasets
#MaxN
boss.maxn   <- read.csv("data/tidy/PtCloates/PtCloates_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS",
                sample=as.character(sample))%>%
    glimpse()
bruv.maxn <- read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  glimpse()
#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
    glimpse()

#length
boss.length <- read.csv("data/tidy/PtCloates/PtCloates_BOSS.complete.length.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  glimpse()
bruv.length <- read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.length.csv")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  glimpse()
#join
length <- bind_rows(boss.length,bruv.length)%>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  glimpse()



#habitat
allhab <- readRDS("data/staging/habitat/PtCloates_habitat-bathy-derivatives.rds")%>%
  # dplyr::select(-status) %>%
  ga.clean.names()%>%
  mutate(location = ifelse(is.na(location), "Point Cloates", location))%>%
  mutate(campaignid = ifelse(is.na(campaignid), 
                             ifelse(substr(date, 1, 4) == "2021", "2021-05_PtCloates_BOSS",
                            ifelse(substr(date, 1, 4) == "2022", "2022-05_PtCloates_Naked-BOSS", NA)), campaignid))%>%
  mutate(campaignid = ifelse(campaignid == "2021-05_PtCloates_stereo-BRUVS", "2021-05_PtCloates_BRUVS", campaignid))%>%
    glimpse()

# npz6hab <- readRDS("data/staging/Abrolhos/Abrolhos_habitat-bathy-derivatives.rds")%>%
#   #dplyr::select(-status) %>%
#   ga.clean.names()%>%
#   glimpse()

allhab <- allhab %>%
  #transform(kelps = kelps / broad.total.points.annotated) %>%
  #transform(macroalgae = macroalgae / broad.total.points.annotated) %>%
  transform(sand = sand / broad.total.points.annotated) %>%
  transform(rock = rock / broad.total.points.annotated) %>%
  transform(inverts = inverts / broad.total.points.annotated) %>%
  glimpse()

names(allhab)

metadata <- maxn %>%
  distinct(sample, method, campaignid, latitude, longitude, date, location, status, site, 
           depth, successful.count, successful.length)

# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  top_n(10)%>%
  ungroup()

## Total frequency of occurrence
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  #Theme1+
  theme(axis.text.y = element_text(face = "italic"))+
  #theme_collapse+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+

#DF for PRIMER
primerPtC <- maxn %>%
  dplyr::select(sample, scientific, maxn, 
                campaignid, latitude, longitude, status, depth, method) %>%
  left_join(allhab) %>%
  pivot_wider(
    names_from = scientific,
    values_from = maxn, 
    values_fill = 0
    ) %>%
  glimpse()

#savedata for PRIMER
saveRDS(primerPtC, "data/staging/PtCloates/PointCloatesprimer.rds")
write.csv(primerPtC, "data/staging/PtCloates/PointCloatesprimer.csv")

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample,method) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(total.abundance = rowSums(.[, 3:125], na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 3:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample, total.abundance, species.richness,method) %>%
  tidyr::gather(., "scientific", "maxn", 2:3) %>%
  dplyr::glimpse()

PtC.maxn <- ta.sr %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  glimpse()
  

testboss <- PtC.maxn %>%
  dplyr::filter(method%in%"BOSS")

testbruv <- PtC.maxn %>%
  dplyr::filter(method%in%"BRUV")

length(unique(testboss$sample))
46*2                  #was (75 x 2)
length(unique(testbruv$sample))
24*2                  #was (50 x 2)


unique(PtC.maxn$scientific)

# Set predictor variables---
names(PtC.maxn)
names(allhab)

pred.vars = c("z", 
              "sand", 
              "rock",
              "inverts", 
              "mean.relief",
              "sd.relief",
              "slope",
              "tpi",
              "roughness",
              "detrended") 

# predictor variables Removed at first pass---
# broad.Sponges and broad.Octocoral.Black and broad.Consolidated 

# Specify the columns you want to include in corr.pred.vars
selected_cols <- c("z", "sand", "rock", "inverts", 
                   "mean.relief", "sd.relief", "slope", 
                   "tpi", "roughness", "detrended")

# Create the new data frame corr.pred.vars
corr.pred.vars <- PtC.maxn %>%
  select(all_of(selected_cols))


# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(corr.pred.vars[,pred.vars]), 2)
# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow = c(3, 2))
for (i in pred.vars) {
  x <- PtC.maxn[ , i]
  x = as.numeric(unlist(x)) 
  hist((x)) #Looks best
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
}

#all looks fine
#write data to load in to next script
saveRDS(PtC.maxn, "data/staging/PtCloates/PtCloates.maxn.rds")
write.csv(PtC.maxn, "data/staging/PtCloates/PtCloates.maxn.csv")

#lengths
# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp","Lethrinidae Gymnocranius spp",
                                                       "Lethrinidae Lethrinus spp","Lethrinidae Unknown spp","Platycephalidae Platycephalus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))%>%    # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::filter(!species%in%c("albimarginatus","longimanus")) %>%
  dplyr::mutate(minlegal.wa = as.double(minlegal.wa)) %>%
  glimpse()

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) 

unique(without.min.length$scientific)

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

combined.length <- bind_rows(legal, sublegal) 

unique(combined.length$scientific)

complete.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number,method) %>%
  tidyr::complete(nesting(sample,method), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,allhab) %>%
  dplyr::filter(successful.length%in%c("Y")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

testboss <- complete.length %>%
  dplyr::filter(method%in%"BOSS")

testbruv <- complete.length %>%
  dplyr::filter(method%in%"BRUV")

length(unique(testboss$sample))
75*2
length(unique(testbruv$sample))
47*2

# Set predictor variables---
names(complete.length)
names(allhab)

pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "biog", 
              "mean.relief",
              "tpi",
              "roughness",
              "detrended") 

# predictor variables Removed at first pass---
# broad.Sponges and broad.Octocoral.Black and broad.Consolidated , "InPreds","BioTurb" are too rare

dat.length <- complete.length

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.length[,pred.vars]),2)
# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat.length[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#all looks fine
#write data to load in to next script
saveRDS(dat.length, "data/Tidy/dat.length.rds")
