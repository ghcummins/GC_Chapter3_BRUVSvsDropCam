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
  glimpse()
bruv.maxn <- read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.maxn.csv")%>%
  #dplyr::mutate(method = "BRUV")%>%
  dplyr::mutate(method = "BRUV",
                sample=as.character(sample))%>%
  glimpse()
#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  glimpse()


#BOSS fish numbers seen on how many samples
samplefishBOSS <- boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n()) 

#to get each MAXN sample on BOSS
samplemaxnBOSS <- boss.maxn %>%
  filter(maxn>0)

# write.csv(samplemaxnBOSS, file = "data/samplemaxnBOSS.csv", row.names = FALSE)

#BRUVS fish numbers seen on how many samples
samplefishBRUV <- bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific) %>%
  dplyr::summarise(n = n())

#to get each MAXN sample on BRUVS
samplemaxnBRUV <- bruv.maxn %>%
  filter(maxn>0)

# write.csv(samplemaxnBRUV, file = "data/samplemaxnBRUV.csv", row.names = FALSE)


bruvcompletelengths <-  read.csv("data/tidy/PtCloates/PtCloates_BRUVS.complete.length.csv") 

bruvlengths <- bruvcompletelengths %>%
  dplyr::mutate(method = "BOSS")%>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  dplyr::mutate(unique_id = paste(campaignid,sample, sep ="_"))

bruvl <- bruvlengths %>%
  select(unique_id, scientific, length, number)


###combine attempt
BRUVFISHES <- left_join(samplemaxnBRUV, bruvl, by = c("unique_id","scientific"))
View(BRUVFISHES)

 write.csv(BRUVFISHES, file = "data/BRUVFISHES.csv", row.names = FALSE)

 str(L.miniatus)
 ##BUBBLE PLOTS
 L.miniatus <- samplemaxnBRUV %>%
   filter(scientific=="Lethrinidae Lethrinus miniatus")
 ## With the zeros left in
 L.miniatus.zero <- bruv.maxn %>%
   filter(scientific=="Lethrinidae Lethrinus miniatus") %>% 
   mutate(year = str_extract(campaignid, "^[:digit:]{4}"))
 
 
 ggplot(L.miniatus, aes(x=longitude, y=latitude))+
   geom_point(aes(size=maxn))
 
 # Plot with zeros
 ggplot()+
   geom_point(data=filter(L.miniatus.zero, maxn>0), aes(x=longitude, y=latitude, size=maxn))+
   geom_point(data=filter(L.miniatus.zero, maxn==0), aes(x=longitude, y=latitude), shape=4)+
   theme_classic()

   
   
 ggplot2::geom_point(L.miniatus, aes(x=longitude, y=latitude, size = maxn))
 