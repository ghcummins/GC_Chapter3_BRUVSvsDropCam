###
# Project: Ch3 PhD Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Join PtCloates BOSS and BRUV, format data for fssGAM
# author:  Gabby, Claude, Brooke, Kingsley
# date:    May 2024
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
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)
library(tidyverse)
library(sf)
library(here)
library(leaflet)

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "Southwest"   # set study name ##

# load and join datasets
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

#join
bossbruvmaxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  glimpse()

maxn <- bossbruvmaxn %>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
         latitude >= -34.15 & latitude <= -34.05)

#Format data
dat.response <- maxn %>%
  filter(str_detect(scientific, "Labridae Pseudolabrus biserialis|Labridae Ophthalmolepis lineolatus|Labridae Coris auricularis|Scorpididae Neatypus obliquus"))%>%
  # select(-id)%>%
  group_by(sample,scientific,campaignid,latitude,longitude,method,unique_id) %>%
  summarise(number = sum(maxn))%>%
  ungroup()%>%
  mutate(response = paste(scientific, method, sep = "_")) %>%
  glimpse()

#load habitat
allhab <- readRDS("data/staging/habitat/SouthWest_habitat-bathy-derivatives.rds")%>%
  dplyr::select(-id) %>%
  ga.clean.names()%>%
  dplyr::select(-id)%>%
  # mutate(location = ifelse(is.na(location), "Point Cloates", location))%>%
  # mutate(campaignid = ifelse(is.na(campaignid), 
  #                            ifelse(substr(date, 1, 4) == "2021", "2021-05_PtCloates_BOSS",
  #                           ifelse(substr(date, 1, 4) == "2022", "2022-05_PtCloates_Naked-BOSS", NA)), campaignid))%>%
  # mutate(campaignid = ifelse(campaignid == "2021-05_PtCloates_stereo-BRUVS", "2021-05_PtCloates_BRUVS", campaignid))%>%
    glimpse()

allhab <- allhab %>%
  transform(seagrasses = seagrasses / broad.total.points.annotated) %>%
  transform(macroalgae = macroalgae / broad.total.points.annotated) %>%
  transform(sand = sand / broad.total.points.annotated) %>%
  transform(rock = rock / broad.total.points.annotated) %>%
  transform(inverts = inverts / broad.total.points.annotated) %>%
  transform(reef = reef / broad.total.points.annotated) %>%
  mutate(z = abs(z), sample = case_when(sample%in% "FHCO1"~"FHC01", sample%in% "FHCO2"~"FHC02", sample%in% "FHCO3"~"FHC03", .default = as.character(sample)))%>%
  mutate(unique_id = paste0(campaignid, sep="_", sample)) %>% 
  glimpse()

names(allhab)

metadata <- maxn %>%
  mutate(unique_id = paste0(campaignid, sep="_", sample)) %>% 
  mutate(date = as.character(date))%>%
  distinct(sample, method, campaignid, latitude, longitude, date, location, status, site, 
           depth, successful.count, successful.length, unique_id)

dat.maxn <- dat.response %>%
  left_join(allhab) %>%
  left_join(metadata) 
  # dplyr::mutate(reef =rock+inverts+macroalgae)

# # Specify the columns you want to include in corr.pred.vars
pred.vars <- c("z", "sand", "rock", "inverts", "macroalgae", "seagrasses",
                "sd.relief", "reef", "aspect",
               "tpi", "roughness", "detrended")

# Create the new data frame corr.pred.vars
corrtable <- as.data.frame(round(cor(dat.maxn[ , pred.vars]), 2))
##take out slope
## reef and mean.relief are too correlated. Which should I choose?

 write.csv(corrtable, file = "outputs/SwC/fish/correlation-table.csv")

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
# par(mfrow = c(3, 2))
# for (i in pred.vars) {
#   x <- dat.maxn[ , i]
#   x = as.numeric(unlist(x)) 
#   hist((x)) #Looks best
#   plot((x), main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x + 1))
#   plot(log(x + 1))
# }

#all looks fine
#write data to load in to next script
saveRDS(dat.maxn, "data/staging/SwC/Southwest.fish.dat.maxn.rds")
write.csv(dat.maxn, "data/staging/SwC/Southwest.fish.dat.maxn.csv")
