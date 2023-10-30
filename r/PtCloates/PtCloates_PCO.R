###
# Project: G Cummins FISH Paper
# Script: Making PCO plot for Pt Cloates
# Task:    Taking PRIMER data and making PCO plot
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
study<-"PtCloates" 

## Set your working directory ----
working.dir<-getwd()

## Save these directory names to use later----
staging.dir<-paste(working.dir,"data/staging/PtCloates",sep="/") 
raw.dir<-paste(working.dir,"data/raw",sep="/")
tidy.dir<-paste(working.dir,"data/tidy/PtCloates",sep="/")

setwd(working.dir)

#04.3.1 : Import data for PCO of method (using presence/absence data) ----
dat <- read.csv("data/raw/PtCloatesPCOdata.csv")  %>%  
  ga.clean.names()#import PCO.csv data sheet (manually exported from primer)
vectors <- read.csv("data/raw/PtCloatesPCOvector.csv")  %>% #import PCO.csv vectors sheet (manually exported from primer)
  ga.clean.names() %>%
  mutate(name = paste(genus, species, sep = " "))
  


ggplot()+
  geom_point(data=dat, aes(x = pco1, y=pco2, colour = method, shape = method, fill = method), size = 3) +
  scale_fill_manual(values=c("BOSS" = "#999999", "BRUV" = "#000000"))+
  scale_shape_manual(values = c("BOSS" = 2, "BRUV" = 6))+
  theme_classic() +
  labs(x = "PCO1", y ="PCO2")

ggplot()+
  geom_segment(data=filter(vectors, vectors=="Yes"),aes(x=0,y=0,xend=(pco1),yend=(pco2)),
               size = 0.5,colour="darkgrey",arrow = arrow(angle=25,length=unit(0.25,"cm"))) +
  geom_text(data= filter(vectors, vectors=="Yes"), aes(x=pco1, y = pco2, label = name))+
  theme_classic()+



# #04.3.2 : Set theme and palette for PCO plot ----
# Theme1 <-
#   theme( # use theme_get() to see available options
#     panel.grid.major = element_blank(), 
#     panel.grid.minor = element_blank(), 
#     legend.background = element_rect(fill="white"),
#     legend.key = element_blank(), # switch off the rectangle around symbols in the legend
#     # legend.position = ("bottom"),
#     legend.justification = ("center"),
#     # legend.direction=("horizontal"),
#     legend.position=c(.85, .1),
#     legend.text = element_text(size=15),
#     legend.title = element_blank(),
#     text=element_text(size=15),
#     strip.text.y = element_text(size = 15,angle = 0),
#     axis.title.x=element_text(vjust=0.3, size=17),
#     axis.title.y=element_text(vjust=0.6, angle=90, size=17),
#     axis.text.x=element_text(size=15),
#     axis.text.y=element_text(size=15),
#     axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
#     axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
#     strip.background = element_blank())
# Palette<- scale_fill_manual(values=custom_colors)
