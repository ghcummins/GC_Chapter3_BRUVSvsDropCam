###
# Project: G Cummins FISH Paper
# Script: Making PCO plot for Abrolhos
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
library(png)
library(cowplot)
library(gridExtra)
library(ggplot2)


## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"Abrolhos" 

## Set your working directory ----
working.dir<-getwd()

## Save these directory names to use later----
staging.dir<-paste(working.dir,"data/staging/Abrolhos",sep="/") 
raw.dir<-paste(working.dir,"data/raw",sep="/")
tidy.dir<-paste(working.dir,"data/tidy/Abrolhos",sep="/")
fish.pics<-paste(working.dir,"data/images", sep="/")

setwd(working.dir)

#04.3.1 : Import data for PCO of method (using presence/absence data) ----
dat <- read.csv("data/raw/AbrolhosPCOdata.csv")  %>%  
  ga.clean.names()  #import PCO.csv data sheet (manually exported from primer) 
vectors <- read.csv("data/raw/AbrolhosPCOvectors.csv")  %>% #import PCO.csv vectors sheet (manually exported from primer)
  ga.clean.names() %>%
  mutate(name = paste(genus, species, sep = " "))

dat <- dat %>%
  filter(!(sample == "npz6.12" & method == "BRUV"))

dat <- dat %>%
  mutate(zerofish = ifelse(
    sample %in% c("npz6.10", "npz6.12", "npz6.16", "npz6.23", "npz6.35", "npz6.4", "npz6.7") & method == "BOSS",
    "yes",
    "no"
  )) 
dat$zerofish <- factor(dat$zerofish, levels = c("yes", "no"))

# #load my fish pics
# #1Carangoides chrysophrys
# c.c <-readPNG("data/images/Carangoides chrysophrys.png")
# c.c <-as.raster(c.c)
# 
# #Gymnocranius sp1 (no pic, used grandoculis)
# g.s <-readPNG("data/images/Gymnocranius grandoculis.png")
# g.s <-as.raster(g.s)
# 
# #Lethrinus miniatus
# l.m <-readPNG("data/images/Lethrinus miniatus.png")
# l.m <-as.raster(l.m)
# 
# #Lethrinus rubrioperculatus
# l.r <-readPNG("data/images/Lethrinidae.png")
# l.r <-as.raster(l.r)
# 
# #1Pristipomoides sp1 no pic - used typus
# p.s <-readPNG("data/images/Pristipomoides typus.png")
# p.s <-as.raster(p.s)
# 
# #parapercis nebulosa
# p.n <-readPNG("data/images/Parapercis nebulosa.png")
# p.n <-as.raster(p.n)

Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    #legend.direction=("horizontal"),
    text=element_text(size=13),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=14, family = "TN"),
    axis.title.y=element_text(vjust=1.5, angle=90, size=14, family = "TN"),
    axis.text.x=element_text(size=13, family = "TN"),
    axis.text.y=element_text(size=13, family = "TN"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    legend.text = element_text(size=12,family="TN"),
    legend.title = element_blank(),
    strip.background = element_blank())
theme_get()



gg.dat<-ggplot(dat, aes(x=pco1, y=pco2))+
  geom_point(shape = 21, alpha=1, size=3, aes(x=pco1, y=pco2,fill=method))+
  #geom_point(data = filter(dat, zerofish == "yes"), shape = 22, size = 5, color = "blue") +
  #stat_ellipse(type = "norm", linetype = 2) +  #this makes the circle around the points ... I dont like it. 
  theme_bw() +  # Removes grey background
  Theme1+ 
  scale_shape_manual(values = c(21,25,21,25))+
  scale_fill_manual(values=c("white","darkgray"))+
  scale_x_continuous(limits = c(-0.6, 0.9), breaks = c(-0.5, 0, 0.5 ))+
  
  
  geom_segment(data = filter(vectors,r>=0.5), # this is the cut-off you choose
               aes(x=0,y=0, # this changes the start point of the vectors. You will notice sometimes that this is not always 0,0
                   xend=(pco1/1),yend=(pco2/1)), #this changes the length of your vecotrs relative to eachother and the entire plot
               size = 0.5,colour="black",arrow = arrow(angle=25,length=unit(0.25,"cm")))+
  
    #geom_text(data= filter(vectors, vectors=="Yes"), aes(x=pco1, y = pco2, label = name))+
   annotate("text", x = 0.17, y = 0.52, size = 3, label = "Chrysophrys auratus", parse=F,family="TN",fontface="italic")+  # this manually add the names of the species
  # # annotation_raster(p.s, xmin=-0.3, xmax=-0.1, ymin=0.33, ymax=0.2) + # this chucks in the fish or species pics. you need to play around with the numbers to get it in the right spot. 
  # 
   annotate("text", x = 0.53, y = 0.16, size = 3, label = "Chromis spp", parse=F,family="TN",fontface="italic")+  # this manually add the names of the species
  # #annotation_custom(Pristipomoides typus, xmin=-0.9, xmax=-0.6, ymin=-0.6, ymax=-0.4)  # this chucks in the fish or species pics. you need to play around with the numbers to get it in the right spot. 
  # 
   annotate("text", x = 0.78, y = 0.12, size = 3, label = "Choerodon rubescens", parse=F,family="TN",fontface="italic")+  # this manually add the names of the species
  # #annotation_custom(Pristipomoides typus, xmin=-0.9, xmax=-0.6, ymin=-0.6, ymax=-0.4)  # this chucks in the fish or species pics. you need to play around with the numbers to get it in the right spot. 
  # 
  annotate("text", x = 0.78, y = 0.065, size = 3, label = "Suezichthys cyanolaemus", parse=F,family="TN",fontface="italic")+  # this manually add the names of the species
  # #annotation_custom(Pristipomoides typus, xmin=-0.9, xmax=-0.6, ymin=-0.6, ymax=-0.4)  # this chucks in the fish or species pics. you need to play around with the numbers to get it in the right spot. 
  # 
  # annotate("text", x = -0.76, y = 0.06, size = 3, label = "Lethrinus miniatus", parse=F,family="TN",fontface="italic")+  # this manually add the names of the species
  # #annotation_custom(Pristipomoides typus, xmin=-0.9, xmax=-0.6, ymin=-0.6, ymax=-0.4)  # this chucks in the fish or species pics. you need to play around with the numbers to get it in the right spot. 
  # 
  # annotate("text", x = -0.53, y = -0.36, size = 3, label = "Parapercis nebulosa", parse=F,family="TN",fontface="italic")+  # this manually add the names of the species
  #annotation_custom(Pristipomoides typus, xmin=-0.9, xmax=-0.6, ymin=-0.6, ymax=-0.4)  # this chucks in the fish or species pics. you need to play around with the numbers to get it in the right spot. 
  
  #adding fish pic
  # annotation_raster(c.a, xmin=9.75,xmax=10.25,ymin=3300, ymax=4000)+            #1
  
  theme(legend.position = "right") +
  labs(x = "PCO1 (35.7% of total variation)", y = "PCO2 (7.3% of total variation)")
  
  #scale_y_continuous(limits = c(-0.6, 0.35))
  #labs(fill = "Method")
  # where you put you legend so it is out of the way
  # annotate("text", x = 0, y = 1.5, size = 6,  parse=F,family="TN")



gg.dat



ggsave("Abrolhos.PCO.jpeg", gg.dat, width = 23, height = 14, units = "cm")
# plots PCO data

ggplot()+
  geom_point(data=dat, aes(x = pco1, y=pco2, colour = method, shape = method, fill = method), size = 3) +
  scale_fill_manual(values=c("BOSS" = "#999999", "BRUV" = "#000000"))+
  scale_shape_manual(values = c("BOSS" = 2, "BRUV" = 6))+
  theme_classic() +
  labs(x = "PCO1", y ="PCO2")

# plots PCO vector
ggplot()+
  geom_segment(data=filter(vectors, vectors=="Yes"),aes(x=0,y=0,xend=(pco1),yend=(pco2)),
               size = 1,colour="darkgrey",arrow = arrow(angle=25,length=unit(0.25,"cm"))) +
  geom_text(data= filter(vectors, vectors=="Yes"), aes(x=pco1, y = pco2, label = name))+
  theme_classic()



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
