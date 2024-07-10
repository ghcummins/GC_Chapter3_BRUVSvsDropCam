###
# Project: Ningaloo, Point Cloates - PhD Ch3 _ hyp2
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
library(png)
library(patchwork)
library(grid)

name <- "Pt Cloates"


#read in data - negative values manually added
dat <- read.csv(paste("outputs/PtCloates/fish/PtCloates_fish.all.var.imp.csv")) %>% 
  dplyr::filter(X != "Nemipteridae Pentapodus nagasakiensis_BRUV")%>%
  dplyr::filter(X != "Lethrinidae Gymnocranius sp1_BRUV") %>%
  dplyr::rename(resp.var= X) %>%
  gather(key=predictor,value=importance,2:ncol(.)) %>%
  dplyr::mutate(
    #importance = ifelse(predictor %in% c("detrended"), importance * 0, importance),
    predictor = ifelse(predictor == "z", "depth", predictor)) %>% # Rename 'z' to 'depth') %>%
   glimpse()

#Below creates X's for top model variables in importance score table
dat.taxa <- dat %>%
  mutate(label=NA)%>%
  # mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(label=ifelse(predictor=="depth" &resp.var=="Pinguipedidae Parapercis nebulosa_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="reef"&resp.var=="Pinguipedidae Parapercis nebulosa_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="Pinguipedidae Parapercis nebulosa_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Lethrinidae Lethrinus miniatus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="reef"&resp.var=="Lethrinidae Lethrinus miniatus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="Lethrinidae Lethrinus miniatus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Lethrinidae Lethrinus miniatus_BOSS","X",label))%>%
  glimpse()

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
  labs(x = "Predictor", y = "Response Variable", title = "Point Cloates") +
    theme_classic()+
  Theme1+
  theme(
    plot.title = element_text(hjust = -0.25, face = "bold")) +
 geom_text(aes(label=label), colour = "white") +
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.line.x = element_blank(),
      plot.title = element_text(hjust = -0.35, vjust = -20)) # Looks crap here but title comes back in exported version
  imp.full


gg.importance <- imp.full 

# #save output - changed dimensions for larger text in report
# save_plot(paste0("plots/PtCloates/", name, "_importance-scores_final.png"), 
#           gg.importance,base_height = 4,base_width = 6.275)

##ABROLHOS
#read in data - negative values manually added
dat.abrolhos <- read.csv(paste("outputs/Abrolhos/fish/Abrolhos_fish.all.var.imp.csv")) %>% 
  #dplyr::filter(X != "Mullidae Parupeneus spilurus_BRUV") %>%
  dplyr::rename(resp.var= X) %>%
  gather(key=predictor,value=importance,2:ncol(.)) %>%
  dplyr::mutate(
    #importance = ifelse(predictor %in% c("detrended"), importance * -1, importance), 
  predictor = ifelse(predictor == "z", "depth", predictor)) %>%  # Rename 'z' to 'depth'
  glimpse()

#Below creates X's for top model variables in importance score table
dat.taxa.abrolhos <- dat.abrolhos %>%
  mutate(label=NA)%>%
  # mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(label=ifelse(predictor=="aspect" &resp.var=="Lethrinidae Lethrinus miniatus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Lethrinidae Lethrinus miniatus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="Labridae Suezichthys cyanolaemus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="reef"&resp.var=="Labridae Suezichthys cyanolaemus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Suezichthys cyanolaemus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="reef"&resp.var=="Labridae Coris auricularis_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Coris auricularis_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Choerodon rubescens_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="Labridae Choerodon rubescens_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Choerodon rubescens_BOSS","X",label))%>%
  glimpse()


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

abrolhos.imp.full <- ggplot(dat.taxa.abrolhos, #dplyr::filter(resp.var%in%c("total.abundance", "species.richness")), 
                   aes(x=predictor,y=resp.var,fill=importance)) +
  geom_tile(show.legend=F) +
  scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                       limits = c(0, 1))+
  # scale_y_discrete(labels=c("Species richness","Total abundance"))+
  labs(x = "Predictor", y = "Response Variable", title = "Shallow Bank") +
  Theme1+
  theme_classic()+
   theme(
    plot.title = element_text(hjust = -0.25, face = "bold"))+  # Centered and bold title
geom_text(aes(label=label), colour = "white") +
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.line.x = element_blank(),
      plot.title = element_text(hjust = -0.35, vjust = -0))# Looks crap here but title comes back in exported version
  
abrolhos.imp.full


abrolhos.gg.importance <- abrolhos.imp.full 

##SAME BUT FOR CAPES REGION IN SOUTHWEST
#read in data - negative values manually added
capesregion.dat.taxa <- read.csv(paste("outputs/SwC/fish/SouthWest_fish.all.var.imp.csv")) %>% 
  dplyr::rename(resp.var= X) %>%
  gather(key=predictor,value=importance,2:ncol(.)) %>%
  dplyr::mutate(
    #importance = ifelse(predictor %in% c("detrended"), importance * 0, importance),
    predictor = ifelse(predictor == "z", "depth", predictor)) %>%  # Rename 'z' to 'depth') %>%
  glimpse()

#Below creates X's for top model variables in importance score table
dat.taxa.capesregion <- capesregion.dat.taxa %>%
  mutate(label=NA)%>%
  # mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(label=ifelse(predictor=="tpi" &resp.var=="Scorpididae Neatypus obliquus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Scorpididae Neatypus obliquus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="Scorpididae Neatypus obliquus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="Scorpididae Neatypus obliquus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="Scorpididae Neatypus obliquus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Pseudolabrus biserialis_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Pseudolabrus biserialis_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="Labridae Ophthalmolepis lineolatus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Ophthalmolepis lineolatus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="Labridae Ophthalmolepis lineolatus_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="Labridae Ophthalmolepis lineolatus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="Labridae Ophthalmolepis lineolatus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="Labridae Ophthalmolepis lineolatus_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Coris auricularis_BRUV","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="Labridae Coris auricularis_BOSS","X",label))%>%
  mutate(label=ifelse(predictor=="aspect"&resp.var=="Labridae Coris auricularis_BOSS","X",label))%>%
  glimpse()

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

capesregion.imp.full <- ggplot(dat.taxa.capesregion, #dplyr::filter(resp.var%in%c("total.abundance", "species.richness")), 
                   aes(x=predictor,y=resp.var,fill=importance)) +
  geom_tile(show.legend=F) +
  scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                       limits = c(0, max(capesregion.dat.taxa$importance)))+
  # scale_y_discrete(labels=c("Species richness","Total abundance"))+
  labs(x = "Predictor", y = "Response Variable", title = "Capes Region") +
  theme_classic()+
    theme(
    plot.title = element_text(hjust = -0.25, face = "bold")) +
geom_text(aes(label=label), colour = "white") +
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.line.x = element_blank(),
      plot.title = element_text(hjust = -0.35, vjust = -0))+
  Theme1# Looks crap here but title comes back in exported version
capesregion.imp.full


capesregion.gg.importance <- capesregion.imp.full 

#join altogether
all_importancescores <- gg.importance + abrolhos.gg.importance + capesregion.gg.importance  + plot_layout(ncol = 1)
all_importancescores

#save
ggsave(filename = "plots/ALLimportancescores_final_covariateswithcrosses.png", 
       plot = all_importancescores, 
       width = 12, 
       height = 16, 
       dpi = 600, 
       units = "in")
