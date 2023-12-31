###
# Project: G Cummins FISH Paper
# Script: Cleaning SwC BOSS campaigns
# Task:    Merging BOSS campaigns and exporting .csv for PRIMER
# author:  G Cummins 
# date:    Oct 2023
##


rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to GitHub
library(RCurl)
library(R.utils)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
# to connect to googlesheets
library(googlesheets4)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"SwC_BOSS" 

## Set your working directory ----
working.dir<-getwd()

## Save these directory names to use later----
staging.dir<-paste(working.dir,"data/staging",sep="/") 
download.dir<-paste(working.dir,"data/raw/em export/SwC",sep="/")
tidy.dir<-paste(working.dir,"data/tidy/Southwest",sep="/")

setwd(working.dir)

# # Metadata1 ----
# metadata1 <-ga.list.files("_Metadata.csv")%>% # list all files ending in "_Metadata.csv"
#   purrr::map_df(~ga.read.files_em.csv(.))%>% # combine into dataframe
#   dplyr::select(campaignid,sample,latitude,longitude,date,location,status,depth,successful.count,successful.length)%>% # This line ONLY keep the 15 columns listed. Remove or turn this line off to keep all columns (Turn off with a # at the front).
#   #dplyr::filter(campaignid%in%c("2020-10_south-west_BOSS", "2021-03_West-Coast_BOSS"))%>%
#   glimpse()
# 
# unique(metadata1$campaignid) # check the number of campaigns in metadata, and the campaign name
# 
# setwd(staging.dir)
# write.csv(metadata1,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine Points and Count files into maxn ----
points.files <-ga.list.files("_Points.txt") # list all files ending in "Lengths.txt"
points.files$lines<-sapply(points.files,countLines) # Count lines in files (to avoid empty files breaking the script)
points<-as.data.frame(points.files)%>%
  dplyr::mutate(campaign=row.names(.))%>%
  filter(lines>1)%>% # filter out all empty text files
  dplyr::select(campaign)%>%
  as_vector(.)%>% # remove all empty files
  purrr::map_df(~ga.read.files_em.txt(.))%>%
  dplyr::filter(campaignid%in%c("2020-10_south-west_BOSS", 
                                "2021-03_West-Coast_BOSS"))

maxn <- points%>%
  dplyr::select(-c(sample)) %>%
  dplyr::rename(sample = period) %>%
  dplyr::group_by(campaignid,sample,filename,periodtime,frame,family,genus,species)%>%
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(campaignid,sample,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(maxn))%>%
  dplyr::select(-frame)%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::filter(maxn>0)%>%
  dplyr::inner_join(metadata1)%>%
  dplyr::filter(successful.count=="Yes")%>%
  dplyr::filter(maxn>0)%>%
  glimpse()

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

unique(maxn$sample)


# ## Combine Length, Lengths and 3D point files into length3dpoints----
length3dpoints<-ga.create.em.length3dpoints()%>%
  dplyr::select(-c(time,comment))%>% # take time out as there is also a time column in the metadata
  dplyr::select(-c(sample)) %>%
  dplyr::rename(sample=period)%>%
  dplyr::inner_join(metadata1)%>%
  dplyr::filter(successful.length=="Yes")%>%
  glimpse()
# 
# ## Save length files ----
 setwd(staging.dir)
 write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)
# 
# setwd(working.dir)

 
 

 