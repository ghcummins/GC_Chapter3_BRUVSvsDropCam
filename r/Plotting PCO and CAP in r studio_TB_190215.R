#### This script has been adapted by Todd Bond. Any errors are due to him and please email to todd.bond@uwa.edu.au ####

# You must have the CAP1 and CAP2 value from primer in a .xlsx file. 
# This is for each sample and for each determinant i.e. species or factors determining the dispersion of points
# When you make the cap or pco you must check the box that says "scores to worksheet". These values go in the PCO samples sheet of the excel workbook.
# When you create the vectors in Primer, you also have the option to add the scores to a worsheet. These are the scores that go into the vectos sheet of the excel workbook.
# the r is calculated in excel then you select the cut off below. 

# Libraries required ----
detach("package:plyr", unload=TRUE)#will error - no worries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(readxl)
library(grid) #needed for arrow function
library(png)
library(gridExtra)
library(mosaic)
library(extrafont)
library(RColorBrewer)
library(stringr)
loadfonts(device = "win")
windowsFonts(TN = windowsFont('Times'))
library(cowplot)

rm(list=ls())

# Where the data sits-----
data.dir=("C:/Users/00078110/Google Drive/R generic scripts/PCO scripts")
tidy.data=paste(data.dir,"Tidy data",sep="/")      # folder where tidy data sits
plots=paste(data.dir,"Plots",sep="/")              # where you want plots to be saved
fish.pics=paste(data.dir,"Fish Pics",sep="/")      # where species pics are if you want to add them to plots

## Fish Pics ----
setwd(fish.pics)
dir()

pic.G.B. <- readPNG("glaucosoma_buergeri_nb.png")%>%
  rasterGrob(interpolate=TRUE)

# Read in the combined data----
setwd(tidy.data)
dir()

pco.abundance <- read_excel("PCO and CAP coords.xlsx", sheet = "pco.samples")
head(pco.abundance)
str(pco.abundance)

pco.vectors <- read_excel("PCO and CAP coords.xlsx", sheet = "pco.vectors")
head(pco.vectors) 


## Theme and plotting----
setwd(plots)

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
    axis.title.y=element_text(vjust=0.6, angle=90, size=14, family = "TN"),
    axis.text.x=element_text(size=13, family = "TN"),
    axis.text.y=element_text(size=13, family = "TN"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    legend.text = element_text(size=12,family="TN"),
    legend.title = element_blank(),
    strip.background = element_blank())
theme_get()

# CAP_abundance----
setwd(plots)

gg.pco.abundance<-ggplot(pco.abundance, aes(x=pco1, y=pco2))+
  geom_point(alpha=1, size=2.5, aes(x=pco1, y=pco2,fill=factor1,shape=factor1))+
  #stat_ellipse(type = "norm", linetype = 2) +  #this makes the circle around the points ... I dont like it. 
  theme_bw() +  # Removes grey background
  Theme1+ 
  scale_shape_manual(values = c(21,25,21,25))+
  scale_fill_manual(values=c("darkblue","firebrick1", "cyan", "yellow"))+
  
  geom_segment(data=filter(pco.vectors,r>=0.7), # this is the cut-off you choose
               aes(x=0,y=0, # this changes the start point of the vectors. You will notice sometimes that this is not always 0,0
                                                   xend=(PCO1/1),yend=(PCO2/1)), #this changes the length of your vecotrs relative to eachother and the entire plot
               size = 0.5,colour="black",arrow = arrow(angle=25,length=unit(0.25,"cm")))+
  
  annotate("text", x = -0.8, y = -0.2, size = 5, label = "Arg. spi", parse=F,family="TN",fontface="italic")+  # this manually add the names of the species
  annotation_custom(pic.G.B., xmin=-0.9, xmax=-0.6, ymin=-0.6, ymax=-0.4)+  # this chucks in the fish or species pics. you need to play around with the numbers to get it in the right spot. 
  
  theme(legend.position = c(0.86,0.85),legend.background = element_blank())+  # where you put you legend so it is out of the way
  annotate("text", x = 0, y = 1.5, size = 6, label = "The title of your plot", parse=F,family="TN")  
  
gg.pco.abundance




#  only do this once you are happy with the plot.
ggsave("gg.pco.abundance.png", gg.pco.abundance,width = 16, height = 16,units = "cm")
























