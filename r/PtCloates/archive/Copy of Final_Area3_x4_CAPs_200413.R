
# 1. Make cap plots

# Naming conventions----
# data objects in lower case
# column names Capitalized


# Libraries required
detach("package:plyr", unload=TRUE)#will error - no worries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(readxl)
library(grid) #needed for arrow function
library(jpeg)
library(gridExtra)



# Where the data sits-----
rm(list=ls())

data.dir=("/Volumes/GoogleDrive/My Drive/meg_analysis/analysis-langlois-area3")

data=paste(data.dir,"Data",sep="/")
plots=paste(data.dir,"Plots",sep="/")
fish.pics=("/Volumes/GoogleDrive/My Drive/meg_graphics/Graphics_fish pictures")

# Read in the combined data----
setwd(data)
dir()
cap.abundance <- read_excel("PCO and CAP coords.xlsx", sheet = "cap.abundance")%>%
  glimpse()

cap.biomass <- read_excel("PCO and CAP coords.xlsx", sheet = "cap.biomass")%>%
  glimpse()


cap.abundance.vectors <- read_excel("PCO and CAP coords.xlsx", sheet = "cap.abundance.vectors")%>%
  glimpse()

# Re-name speciesp----
cap.abundance.vectors<-cap.abundance.vectors%>%
  mutate(taxa = recode_factor(taxa, `Argyrops spinifer` = "Argyrops notialis"),
         taxa.short = recode_factor(taxa.short, `A. spinifer` = "A. notialis"))%>%
  glimpse()

unique(cap.abundance.vectors$taxa.short)

cap.biomass.vectors <- read_excel("PCO and CAP coords.xlsx", sheet = "cap.biomass.vectors")%>%
  glimpse()

# Re-name speciesp----
cap.biomass.vectors<-cap.biomass.vectors%>%
  mutate(taxa = recode_factor(taxa, `Argyrops spinifer` = "Argyrops notialis"),
         taxa.short = recode_factor(taxa.short, `A. spinifer` = "A. notialis"))%>%
  glimpse()


## Theme and plotting----
setwd(plots)

# Theme1<-theme(panel.grid.major = element_blank(),   
#              legend.background = element_rect(fill="transparent"),
#              panel.grid.minor = element_blank(), 
#                           legend.position = ("bottom"),
#              #              legend.direction=("horizontal"),
#              strip.background = element_blank(),
#              legend.key = element_blank(),
#              legend.title = element_blank(),
#              panel.border = element_rect(colour = "black"),
#              legend.text = element_text(size=9),
#              axis.title.y =element_text(size=12),
#              axis.title.x=element_text(size=12),
#              axis.text.x=element_text(size=10),
#              axis.text.y=element_text(size=10),
#              strip.text=element_text(size=7))

Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    # legend.position = ("bottom"),
    legend.justification = ("center"),
    # legend.direction=("horizontal"),
    legend.position=c(.85, .1),
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    text=element_text(size=15),
    strip.text.y = element_text(size = 15,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=17),
    axis.title.y=element_text(vjust=0.6, angle=90, size=17),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Pallettes----
# head(ord)
# ord$YearStatus<-factor(ord$YearStatus, levels = c("2011 TFC","2014 TFC","2011 Fished","2014 Fished"))
# 
# ColourPalette <- scale_colour_manual(values=c("#33CC00","#336600","#FF6633","#990000"),breaks=c("2011 TFC","2014 TFC","2011 Fished","2014 Fished"), labels=c("TFC 2011","TFC 2014","Fished 2011","Fished 2014"))
# 
# FillPalette <- scale_fill_manual(values=c("#33CC00","#336600","#FF6633","#990000"),breaks=c("2011 TFC","2014 TFC","2011 Fished","2014 Fished"), labels=c("TFC 2011","TFC 2014","Fished 2011","Fished 2014"))
# 
# ShapePalette <- scale_shape_manual(values=c(21,22,23,24,25))


# cbbfillPalette <- scale_fill_manual(values=c("#CCCCCC","#000000"),breaks=c("Reserve","Fished"), labels=c("Sanctuary","Fished"))
Palette<- scale_fill_manual(values=c("#666666" ,"#CCCCCC","#FFFFFF"))
# cbbColourPalette <- scale_colour_manual(values=c("#CCCCCC","#000000"),breaks=c("Reserve","Fished"), labels=c("Sanctuary","Fished"))
# ShapePalette <- scale_shape_manual(values=c(21,22,17))
# 



## Importing picture of fish
setwd(fish.pics)
dir()

pic.l.s <- readJPEG("Lutjanus sebae 3cm.jpg")%>%
  rasterGrob(interpolate=TRUE)
pic.p.m <- readJPEG("Pristipomoides multidens 3cm.jpg")%>%
  rasterGrob(interpolate=TRUE)
pic.l.p <- readJPEG("Lethrinus punctulatus-3cmL.jpg")%>%
  rasterGrob(interpolate=TRUE)
pic.e.m <- readJPEG("Epinephelus multinotatus 3cm.jpg")%>%
  rasterGrob(interpolate=TRUE)



# CAP_abundance----

glimpse(cap.abundance.vectors)

setwd(plots)
gg.cap.abundance<-ggplot()+
  # TURN BACK ON FOR FINAL PLOT
  annotation_custom(pic.p.m, xmin=0.08, xmax=0.215, ymin=0.05, ymax=-0.05)+
  annotation_custom(pic.l.s, xmin=0.1, xmax=0.215, ymin=-0.12, ymax=-0.025)+
  annotation_custom(pic.e.m, xmin=0.075, xmax=0.205, ymin=0.04, ymax=0.125)+
  annotation_custom(pic.l.p, xmin=-0.05, xmax=-0.125, ymin=-0.075, ymax=-0.025)+
  
  geom_point(data=cap.abundance, aes(x=cap1, y=cap2, fill=status),alpha=1, size=5,shape=21)+#fill
  theme_bw() + 
  Theme1+
  guides(fill=guide_legend(override.aes = list(size = 4)))+
  guides(shape=guide_legend(override.aes = list(size = 4)))+
  # scale_x_reverse()+
  scale_x_reverse(limits=c(0.15,-0.2))+
  # scale_y_continuous(limits=c(-0.15,0.1))+
  # scale_y_reverse()+
  xlab (expression(paste("CAP axis 1 for fishing status,"  ,delta^{2},"=0.44")))+                 
  ylab (expression(paste("CAP axis 2 for fishing status,  "  ,delta^{2},"=0.57")))+
  annotate("text", x = Inf, y = Inf, size = 6, label = "(a)", parse=F,vjust = 1.5, hjust = -0.5)+
  annotate("text", x = Inf, y = Inf, size = 6, label = "Status***", parse=F,vjust = 3, hjust = -0.5)+
  annotate("text", x = -Inf, y = Inf, size = 6, label = "LoA = 76.7%", parse=F,vjust = 3, hjust = 1.5)+
  annotate("text", x = -Inf, y = Inf, size = 6, label = "m = 22", parse=F,vjust = 5, hjust = 2.5)+
#   annotate("text", x = -0.1, y = 0.1, size = 5, label = "LoA = 76.7%", parse=F,vjust = 0, hjust = 0)+
#   annotate("text", x = -0.1, y = 0.12, size = 5, label = "m = 22", parse=F,vjust = 0, hjust = 0)+
  Palette+
  geom_segment(data=filter(cap.abundance.vectors,vector=="yes"),aes(x=0,y=0,xend=(cap1/6),yend=(cap2/6)),size = 0.5,colour="darkgrey",arrow = arrow(angle=25,length=unit(0.25,"cm")))+
  geom_text(data=filter(cap.abundance.vectors,vector=="yes"),aes(x=(cap1/3.5),y=(cap2/3),label = taxa.short),size = 6,colour="black",fontface='italic',angle=0)
# +
# coord_equal()
  
  gg.cap.abundance
  ggsave("gg.cap.abundance.1.png", gg.cap.abundance,width = 20, height = 20,units = "cm")
  

  
  # CAP_biomass----
  setwd(plots)
  gg.cap.biomass<-ggplot()+
    annotation_custom(pic.p.m, xmin=-0.225, xmax=-0.05, ymin=-0.2, ymax=-0.1)+
    annotation_custom(pic.l.s, xmin=-0.25, xmax=-0.1, ymin=-0.15, ymax=-0.06)+
    annotation_custom(pic.e.m, xmin=-0.275, xmax=-0.1, ymin=-0.1, ymax=0.025)+

    geom_point(data=cap.biomass, aes(x=cap1, y=cap2, fill=status),alpha=1, size=5,shape=21,show.legend = FALSE)+#fill
    theme_bw() + 
    Theme1+
    guides(fill=guide_legend(override.aes = list(size = 4)))+
    guides(shape=guide_legend(override.aes = list(size = 4)))+
    scale_x_reverse(limits=c(0.25,-0.2))+
    scale_y_reverse(limits=c(0.18,-0.1))+
    xlab (expression(paste("CAP axis 1 for fishing status,"  ,delta^{2},"=0.51")))+                 
    ylab (expression(paste("CAP axis 2 for fishing status,  "  ,delta^{2},"=0.41")))+
    annotate("text", x = Inf, y = -Inf, size = 6, label = "(b)", parse=F,vjust = 1.5, hjust = -0.5)+
    annotate("text", x = Inf, y = -Inf, size = 6, label = "Status*", parse=F,vjust = 3, hjust = -0.5)+
    annotate("text", x = -Inf, y = -Inf, size = 6, label = "LoA = 66.3%", parse=F,vjust = 3, hjust = 1.5)+
    annotate("text", x = -Inf, y = -Inf, size = 6, label = "m = 15", parse=F,vjust = 5, hjust = 2.5)+
    Palette+
    geom_segment(data=filter(cap.biomass.vectors,vector=="yes"),aes(x=0,y=0,xend=(cap1/6),yend=(cap2/6)),size = 0.5,colour="darkgrey",arrow = arrow(angle=25,length=unit(0.25,"cm")))+
    geom_text(data=filter(cap.biomass.vectors,vector=="yes"),aes(x=(cap1/5),y=(cap2/4.5),label = taxa.short),size = 6,colour="black",fontface='italic',angle=0)
#   +
#     coord_equal()
  
  gg.cap.biomass
  ggsave("gg.cap.biomass.1.png", gg.cap.biomass,width = 20, height = 20,units = "cm")
  
  
  
  
  
  # Multi Plot----
  setwd(plots)
  library(gridExtra)
  
  # Main taxa
  # This just shows you on screen what plot will look like
  
  grid.arrange(gg.cap.abundance,gg.cap.biomass,ncol=1)
  
  # Use arrangeGrob ONLY - as we can pass this to ggsave! Note use of raw ggplot's
  Figure_3<-arrangeGrob(gg.cap.abundance,gg.cap.biomass,ncol=1)
  
  # Save plot as .png 
  ggsave(Figure_3,file=paste("ggmulti.cap.max.bio.status.png"),width = 20, height = 40,units = "cm")
  
  