###
# Project: Abrolhos Shallow Bank Region - PhD Ch3 _ hyp2
# Data:    BOSS & BRUV fish, habitat
# Task:    Plotting fish importance scores
# author:  Gabby & Claude
# date:    July 2024
##

rm(list=ls())

# Plotting defaults----
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(patchwork)
library(cowplot)

name <- "Abrolhos"

#read in data - negative values manually added
dat.taxa <- read.csv(paste("outputs/Abrolhos/fish/Abrolhos_fish.all.var.imp.csv")) %>% 
  dplyr::filter(X != "Mullidae Parupeneus spilurus_BRUV") %>%
  dplyr::rename(resp.var= X) %>%
  gather(key=predictor,value=importance,2:ncol(.)) %>%
  #dplyr::mutate(importance = ifelse(predictor %in% c("detrended"), importance * -1, importance)) %>%
  glimpse()


# #Below creates X's for top model variables in importance score table
# dat.taxa <- dat.taxa %>%
#   mutate(label=NA)%>%
#   mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
#   mutate(label=ifelse(predictor=="macroalgae"&resp.var=="total.abundance","X",label))%>%
#   mutate(label=ifelse(predictor=="inverts"&resp.var=="species.richness","X",label))%>%
#   mutate(label=ifelse(predictor=="macroalgae"&resp.var=="species.richness","X",label))%>%
#   mutate(label=ifelse(predictor=="detrended"&resp.var=="greater than legal size","X",label))%>%
#   mutate(label=ifelse(predictor=="seagrass"&resp.var=="greater than legal size","X",label))%>%
#   mutate(label=ifelse(predictor=="detrended"&resp.var=="smaller than legal size","X",label))%>%
#   mutate(label=ifelse(predictor=="Z"&resp.var=="smaller than legal size","X",label))%>%
#   glimpse()

# Theme-makes it pretty?!
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# colour ramps- usually #(c("blue3", "white","red2"))(200) but lets try blue only.
re <- colorRampPalette(c("white","blue3"))(200)

# Labels-
legend_title<-"Importance"

imp.full <- ggplot(dat.taxa, #dplyr::filter(resp.var%in%c("total.abundance", "species.richness")), 
                   aes(x=predictor,y=resp.var,fill=importance)) +
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                       limits = c(0, 1))+
  # scale_y_discrete(labels=c("Species richness","Total abundance"))+
  # labs(x = NULL, y = NULL, title = "Whole assemblage") +
  theme_classic()+
  Theme1
# geom_text(aes(label=label)) +
# theme(axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank(), 
#       axis.line.x = element_blank(),
#       plot.title = element_text(hjust = -0.45, vjust = -15)) # Looks crap here but title comes back in exported version
imp.full


gg.importance <- imp.full 

#save output - changed dimensions for larger text in report
save_plot(paste0("plots/Abrolhos/", name, "_importance-scores_final.png"), 
          gg.importance,base_height = 4,base_width = 6.275)
