###
# Project: G Cummins FISH Paper
# Script: Making PCO plot for Pt Cloates
# Task:    Taking PRIMER data and making PCO plot
# author:  G Cummins 
# date:    Oct 2023
##

#04.3.1 : Import data for PCO of method (using presence/absence data) ----
data <- read_excel(paste(wd, "04-primer/PCO-method/", "data.xlsx", sep="/"), sheet = "data")                 #import CAP.xlsx data sheet (manually exported from primer)
vectors <- read_excel(paste(wd, "04-primer/PCO-method/", "data.xlsx", sep="/"), sheet = "vectors") %>%  #import CAP.xlsx vectors sheet (manually exported from primer)
  mutate(taxa.short = sub("^(\\w)\\w+\\.", \\1. , taxa)) %>%
  mutate(genus = sub(\\..*, "", taxa),
         species = sub(".*\\.", "", taxa)) %>%
  left_join(species, by = c("genus", "species")) 
#04.3.2 : Set theme and palette for PCO plot ----
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
Palette<- scale_fill_manual(values=custom_colors)
