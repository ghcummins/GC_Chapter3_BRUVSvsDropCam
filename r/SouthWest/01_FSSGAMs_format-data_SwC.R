###
# Project: Ch3 PhD Southwest
# Data:    BOSS & BRUV fish, habitat
# Task:    Join SwC BOSS and BRUV, format data for fssGAM
# author:  Gabby, Claude, Brooke, Kingsley
# date:    October 2023
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
# library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)

 ## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
#setwd(working.dir)
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

unique(boss.maxn$id)

#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05) %>%
  glimpse()
   
# gab.maxn <- maxn %>%
# mutate(date = as.character(date))
  # filter(maxn>0)

# LT = leveneTest(maxn ~ method, gab.maxn)
# print(LT)

#figuring out BOSS samples and BOSS samples within wanted box dimensions
# BOSS.maxn <- boss.maxn %>%
#   filter(maxn>0)
# 
# unique(BOSS.maxn$id)

swc_boss.maxn <- boss.maxn %>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05)

# BOSS_swc_maxn <- swc_boss.maxn %>%
#   filter(maxn>0)
# 
# unique(BOSS_swc_maxn$id)
# BOSS_samples  <- unique(BOSS_swc_maxn$id)

unique(swc_boss.maxn$id)
#samples sites of BOSS
unique_swc_boss <- swc_boss.maxn %>%
  dplyr::select(id, sample, latitude, longitude) %>%
  distinct(id, .keep_all = TRUE)

#save sites this is BOSS sites in SWC in my campaign box
#ONLY SAVED OUT ONCE so hashed out
# write.csv(unique_swc_boss, file = "outputs/SwC/swcBOSS_uniquedeployments_campaignid.csv", row.names = FALSE)
# write.csv(unique_swc_boss, file = "data/staging/SwC/swcBOSS_uniquedeployments_campaignid.csv", row.names = FALSE)
# saveRDS(unique_swc_boss, "data/staging/SwC/swcBOSS_uniquedeployments_campaignid.rds")
  
 
# #BOSS fish species seen on how many samples ie number of drops
samplefishboss <- swc_boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(n = n()) 

#BOSS individual fish ie sum of MAXNs for that individual fish
totalfishboss <- swc_boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(totalfish = sum(maxn))

SwCBOSS_maxn_n <- samplefishboss %>%
  left_join(totalfishboss %>% dplyr::select(scientific, totalfish), by = "scientific")

#change columns so we have Family genus and species seperated - to be consistent w Pt CLoates list
SwCBOSS_allnames <- separate(SwCBOSS_maxn_n, scientific, into = c("family", "genus", "species"), sep = " ")

#save
#write.csv(SwCBOSS_allnames, file = "outputs/SwC/SWCBOSS_fishlist.csv", row.names = FALSE)

#BOSS summary stats
total_indi_fish_BOSS <- SwCBOSS_allnames%>%
  dplyr::summarise(totalfish = sum(totalfish))

sfboss_inds <- swc_boss.maxn %>%
  filter(maxn>0) %>%
  dplyr::mutate(name = paste(genus, species))%>%
  group_by(scientific) %>%  
  dplyr::summarise(totalfish = sum(maxn))

fishfamiliesboss <- swc_boss.maxn %>%
  filter(maxn>0) %>%
  group_by(family) %>%
  dplyr::summarise(totalfish = sum(maxn)) 

famBOSS <- unique(fishfamiliesboss$family)
famBOSS

#BRUV summary stats
swc_bruv.maxn <- bruv.maxn %>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05)
#BRUV fish species seen on how many samples
samplefishbruv <- swc_bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(n = n()) 

#BRUV individual fish ie MaxN
totalfishbruv <- swc_bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(totalfish = sum(maxn))

fishfamiliesbruv <- swc_bruv.maxn %>%
  filter(maxn>0) %>%
  group_by(family) %>%
  dplyr::summarise(totalfish = sum(maxn)) 

famBRUV <- unique(fishfamiliesbruv$family)
famBRUV

#Shallow Bank BRUV maxn column (total fish) and n (no. of drops)
swcbruv_maxn_n <- samplefishbruv %>%
  left_join(totalfishbruv %>% dplyr::select(scientific, totalfish), by = "scientific")

#change columns so we have Family genus and species seperated - to be consistent w Pt CLoates list
swcbruv_allnames <- separate(swcbruv_maxn_n, scientific, into = c("family", "genus", "species"), sep = " ")

total_indi_fish_BRUV <- swcbruv_allnames%>%
  dplyr::summarise(totalfish = sum(totalfish))
#save
write.csv(swcbruv_allnames, file = "outputs/SwC/SWCBRUV_fishlist.csv", row.names = FALSE)

#Species unique to BOSS and BRUV
 speciesBOSS<- SwCBOSS_allnames %>%
  filter(name != "SUS sus")
  # filter(species != spp) ###what are our choices taxonomically
 
 speciesBRUV <-swcbruv_allnames %>%
   filter(name != "SUS sus")
   # filter(species !=spp)
 
sp_BOSS <- speciesBOSS%>%distinct(name)
sp_BRUV <- speciesBRUV%>%distinct(name)
 
 only_species_BOSS <- anti_join(sp_BOSS, sp_BRUV)
 only_species_BRUV <- anti_join(sp_BRUV, sp_BOSS)

#Genera unique to BOSS and BRUV
generaBOSS <- unique(speciesBOSS$genus)
generaBOSS

generaBRUV <-unique(speciesBRUV$genus)
generaBRUV

g_BOSS <-data.frame(genus = generaBOSS)%>%
filter(genus !="Unknown")

g_BRUV <-data.frame(genus = generaBRUV)%>%
filter(genus !="Unknown")
 
only_in_g_BOSS <- anti_join(g_BOSS, g_BRUV)
only_in_g_bruv <- anti_join(g_BRUV, g_BOSS)

data_genus <- list(
  BRUV = g_BRUV$genus,
  BOSS = g_BOSS$genus
)

venn_plot_genus <-ggvenn(data_genus, 
                         c("BRUV", "BOSS"), 
                         fill_color =c("dark grey", "white"),
                         fill_alpha = 0.4,
                         show_percentage = F,
                         text_size=8)+
  # set_name_size = 8)+
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))
#ggtitle("Point Cloates\n")+
# theme(plot.title = element_text(size = 14))
venn_plot_genus

# Create Venn diagram ##only done venn diagram for families... do for genera and species.


# ggsave("Capesregion_genera_venndiagramgreys.jpeg", venn_plot_genus, width = 15, height = 10, units = "cm")

#Families unique to BOSS and BRUV
familiesboss <- fishfamiliesboss %>%
  filter(family !="SUS")

familiesbruv <- fishfamiliesbruv %>%
  filter(family !="SUS")

f_BOSS <- familiesboss%>% distinct(family)
f_BRUV <- familiesbruv %>% distinct(family)

only_families_in_boss <- anti_join(f_BOSS, f_BRUV)
only_families_in_bruv <- anti_join(f_BRUV, f_BOSS)

#load habitat
allhab <- readRDS("data/staging/habitat/SouthWest_habitat-bathy-derivatives.rds")%>%
  ga.clean.names()%>%
  dplyr::select(-id) %>%
  #dplyr::select(-sample) %>%
  dplyr::select(-planned.or.exploratory)%>%
  dplyr::select(-date)%>%
  dplyr::select(-time)%>%
  dplyr::select(-location)%>%
  dplyr::select(-site)%>%
  dplyr::select(-depth)%>%
  dplyr::select(-successful.count)%>%
  dplyr::select(-successful.length)%>%
  dplyr::select(-commonwealth.zone)%>%
  dplyr::select(-state.zone)%>%
  dplyr::select(-status)%>%
  filter(longitude >= 114.72 & longitude <= 114.95 &
           latitude >= -34.15 & latitude <= -34.05) %>%
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
  #mutate(z = abs(z), sample = case_when(sample%in% "FHCO1"~"FHC01", sample%in% "FHCO2"~"FHC02", sample%in% "FHCO3"~"FHC03", .default = as.character(sample)))%>%
  mutate(unique_id = paste0(campaignid, sep="_", sample)) %>% 
  glimpse()

names(allhab)

metadata <- maxn %>%
  mutate(unique_id = paste0(campaignid, sep="_", sample)) %>% 
  mutate(date = as.character(date))%>%
  distinct(sample, method, campaignid, latitude, longitude, date, location, status, site, 
           depth, successful.count, successful.length, unique_id)

names(maxn)
#Format data for PRIMER
primerSWC <- maxn %>%
  #filter(family !="SUS")%>%
  dplyr::select(unique_id, sample, scientific, maxn, 
                campaignid, latitude, longitude, status, depth, method) %>%
  left_join(allhab) %>%
  pivot_wider(
    names_from = scientific,
    values_from = maxn, 
    values_fill = 0
  ) %>%
  glimpse()

#savedata for PRIMER
saveRDS(primerSWC, "data/staging/SwC/ForPRIMER/CapesRegionprimer.rds")
write.csv(primerSWC, "data/staging/SwC/ForPRIMER/CapesRegionprimer.csv")


#Format data
dat.response <- maxn %>%
  filter(str_detect(scientific, "Labridae Pseudolabrus biserialis|Labridae Ophthalmolepis lineolatus|Labridae Coris auricularis|Scorpididae Neatypus obliquus"))%>%
  # select(-id)%>%
  group_by(sample,scientific,campaignid,latitude,longitude,method,unique_id) %>%
  summarise(number = sum(maxn))%>%
  ungroup()%>%
  mutate(response = paste(scientific, method, sep = "_")) %>%
  glimpse()

dat.maxn <- dat.response %>%
  left_join(allhab) %>%
  left_join(metadata) 

#Format data
anova.response <- gab.maxn %>%
  #filter(str_detect(scientific, "Labridae Pseudolabrus biserialis|Labridae Ophthalmolepis lineolatus|Labridae Coris auricularis|Scorpididae Neatypus obliquus"))%>%
  # select(-id)%>%
  group_by(sample,scientific,campaignid,latitude,longitude,method,unique_id) %>%
  summarise(number = sum(maxn))%>%
  ungroup()%>%
  mutate(response = paste(scientific, method, sep = "_")) %>%
  glimpse()

anova.maxn <- anova.response %>%
  left_join(allhab) %>%
  left_join(metadata)


# Add a colum that categorises the dominant habitat class
anova.maxn$dom_tag <- apply(anova.maxn%>%dplyr::select(reef, sand), 1, # Set columns manually here only 12 to 14 for Pt Cloates
                         FUN = function(x){names(which.max(x))})
# spreddf$dom_tag <- sub('.', '', spreddf$dom_tag)                                # Removes the p but not really sure why haha
  
#two-way ANOVA
anova_swc <- aov(number ~ method * dom_tag, data = anova.maxn)
summary(anova_swc)

anova2 <- anova.maxn %>%
  filter(number>0)

anova2_swc <- aov(number ~ method * dom_tag, data = anova2)
summary(anova2_swc)

# Sum the numbers for the specified response of Coris auricularis_BRUV
sum_numbers <- dat.maxn %>%
  filter(response == "Labridae Coris auricularis_BRUV") %>%
  filter(number>0)%>%
 dplyr::summarise(n=n())

# Sum the numbers for the specified response of Coris auricularis_BRUV
no_sum_numbers <- dat.maxn %>%
  filter(response == "Scorpididae Neatypus obliquus_BRUV") %>%
  filter(number>0)%>%
  dplyr::summarise(n=n())

##above is testing number of samples Coris auricularis is on on BRUV dpeloyments

# Sum the numbers for the specified response of Coris auricularis_BRUV
reef.c.a <- dat.maxn %>%
  filter(response == "Labridae Coris auricularis_BRUV") %>%
  filter(number>0)%>%
  dplyr::summarise(mean=mean(reef))

# Sum the numbers for the specified response of Coris auricularis_BRUV
reef.means <- dat.maxn %>%
  filter(number>0)%>%
  group_by(response) %>%
  dplyr::summarise(mean=mean(reef))

maxn.means <- dat.maxn %>%
  filter(number>0)%>%
  group_by(response) %>%
  dplyr::summarise(
    mean=mean(number),
    se = sd(number) / sqrt(n()))

# Create the bar plot with error bars
ggplot(maxn.means, aes(x = response, y = mean, fill = response)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.7)) +
  labs(x = "Response", y = "Mean Number", title = "Mean Number with Standard Error") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )



# #BOSS fish species seen on how many samples ie number of drops
samplefishboss <- swc_boss.maxn %>%
  filter(maxn>0) %>%
  group_by(scientific, name) %>%
  dplyr::summarise(n = n()) 

# #habitat
# allhab <- readRDS("data/staging/habitat/PtCloates_habitat-bathy-derivatives.rds")%>%
#   # dplyr::select(-status) %>%
#   ga.clean.names()%>%
#   mutate(location = ifelse(is.na(location), "Point Cloates", location))%>%
#   mutate(campaignid = ifelse(is.na(campaignid), 
#                              ifelse(substr(date, 1, 4) == "2021", "2021-05_PtCloates_BOSS",
#                             ifelse(substr(date, 1, 4) == "2022", "2022-05_PtCloates_Naked-BOSS", NA)), campaignid))%>%
#   mutate(campaignid = ifelse(campaignid == "2021-05_PtCloates_stereo-BRUVS", "2021-05_PtCloates_BRUVS", campaignid))%>%
#     glimpse()
# 
# 
# allhab <- allhab %>%
#   #transform(kelps = kelps / broad.total.points.annotated) %>%
#   #transform(macroalgae = macroalgae / broad.total.points.annotated) %>%
#   transform(sand = sand / broad.total.points.annotated) %>%
#   transform(rock = rock / broad.total.points.annotated) %>%
#   transform(inverts = inverts / broad.total.points.annotated) %>%
#   glimpse()
# 
# names(allhab)
# 
# metadata <- maxn %>%
#   distinct(sample, method, campaignid, latitude, longitude, date, location, status, site, 
#            depth, successful.count, successful.length)
# 
# # look at top species ----
# maxn.sum <- maxn %>%
#   mutate(scientific = paste(genus, species, sep = " ")) %>%
#   group_by(scientific) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   top_n(10)%>%
#   ungroup()
# 
# ## Total frequency of occurrence
# ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
#   geom_bar(stat="identity",position = position_dodge()) +
#   coord_flip() +
#   xlab("Species") +
#   ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
#   #Theme1+
#   theme(axis.text.y = element_text(face = "italic"))+
#   #theme_collapse+
#   scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+
# 
# #DF for PRIMER
# primerPtC <- maxn %>%
#   dplyr::select(sample, scientific, maxn, 
#                 campaignid, latitude, longitude, status, depth, method) %>%
#   left_join(allhab) %>%
#   pivot_wider(
#     names_from = scientific,
#     values_from = maxn, 
#     values_fill = 0
#     ) %>%
#   glimpse()
# 
# #savedata for PRIMER
# saveRDS(primerPtC, "data/staging/PtCloates/PointCloatesprimer.rds")
# write.csv(primerPtC, "data/staging/PtCloates/PointCloatesprimer.csv")
# 
# # Create total abundance and species richness ----
# ta.sr <- maxn %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(scientific,sample,method) %>%
#   dplyr::summarise(maxn = sum(maxn)) %>%
#   tidyr::spread(scientific, maxn, fill = 0) %>% 
#   dplyr::ungroup() %>%
#   dplyr::mutate(total.abundance = rowSums(.[, 3:125], na.rm = TRUE )) %>% #Add in Totals
#   dplyr::mutate(species.richness = rowSums(.[, 3:(ncol(.))] > 0)) %>% # double check these
#   dplyr::select(sample, total.abundance, species.richness,method) %>%
#   tidyr::gather(., "scientific", "maxn", 2:3) %>%
#   dplyr::glimpse()
# 
# PtC.maxn <- ta.sr %>%
#   left_join(allhab) %>%
#   left_join(metadata) %>%
#   glimpse()
#   
# 
# testboss <- PtC.maxn %>%
#   dplyr::filter(method%in%"BOSS")
# 
# testbruv <- PtC.maxn %>%
#   dplyr::filter(method%in%"BRUV")
# 
# length(unique(testboss$sample))
# 46*2                  #was (75 x 2)
# length(unique(testbruv$sample))
# 24*2                  #was (50 x 2)
# 
# 
# unique(PtC.maxn$scientific)
# 
# # Set predictor variables---
# names(PtC.maxn)
# names(allhab)
# 
# pred.vars = c("z", 
#               "sand", 
#               "rock",
#               "inverts", 
#               "mean.relief",
#               "sd.relief",
#               "slope",
#               "tpi",
#               "roughness",
#               "detrended") 
# 
# # predictor variables Removed at first pass---
# # broad.Sponges and broad.Octocoral.Black and broad.Consolidated 
# 
# # Specify the columns you want to include in corr.pred.vars
# selected_cols <- c("z", "sand", "rock", "inverts", 
#                    "mean.relief", "sd.relief", "slope", 
#                    "tpi", "roughness", "detrended")
# 
# # Create the new data frame corr.pred.vars
# corr.pred.vars <- PtC.maxn %>%
#   select(all_of(selected_cols))
# 
# 
# # Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
# round(cor(corr.pred.vars[,pred.vars]), 2)
# # nothing is highly correlated 
# 
# # Plot of likely transformations - thanks to Anna Cresswell for this loop!
# par(mfrow = c(3, 2))
# for (i in pred.vars) {
#   x <- PtC.maxn[ , i]
#   x = as.numeric(unlist(x)) 
#   hist((x)) #Looks best
#   plot((x), main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x + 1))
#   plot(log(x + 1))
# }
# 
# #all looks fine
# #write data to load in to next script
# saveRDS(PtC.maxn, "data/staging/PtCloates/PtCloates.maxn.rds")
# write.csv(PtC.maxn, "data/staging/PtCloates/PtCloates.maxn.csv")
# 
# #lengths
# # Create abundance of all recreational fished species ----
# url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"
# 
# master<-googlesheets4::read_sheet(url)%>%
#   ga.clean.names()%>%
#   filter(grepl('Australia', global.region))%>% # Change country here
#   dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
#   distinct()%>%
#   glimpse()
# 
# unique(master$fishing.type)
# 
# fished.species <- length %>%
#   dplyr::left_join(master) %>%
#   dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp","Lethrinidae Gymnocranius spp",
#                                                        "Lethrinidae Lethrinus spp","Lethrinidae Unknown spp","Platycephalidae Platycephalus spp")
#                                       ,"R",fishing.type))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
#   dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
#   dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))%>%    # Brooke removed leatherjackets, sea sweeps and goat fish
#   dplyr::filter(!species%in%c("albimarginatus","longimanus")) %>%
#   dplyr::mutate(minlegal.wa = as.double(minlegal.wa)) %>%
#   glimpse()
# 
# without.min.length <- fished.species %>%
#   filter(is.na(minlegal.wa))%>%
#   distinct(scientific) 
# 
# unique(without.min.length$scientific)
# 
# legal <- fished.species %>%
#   tidyr::replace_na(list(minlegal.wa=0)) %>%
#   dplyr::filter(length>minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "greater than legal size") %>%
#   dplyr::glimpse()
# 
# sublegal <- fished.species %>%
#   dplyr::filter(length<minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "smaller than legal size") %>%
#   dplyr::glimpse()
# 
# combined.length <- bind_rows(legal, sublegal) 
# 
# unique(combined.length$scientific)
# 
# complete.length <- combined.length %>%
#   dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
#   dplyr::select(sample,scientific,number,method) %>%
#   tidyr::complete(nesting(sample,method), scientific) %>%
#   replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
#   dplyr::ungroup()%>%
#   dplyr::filter(!is.na(scientific)) %>% # this should not do anything
#   dplyr::left_join(.,metadata) %>%
#   dplyr::left_join(.,allhab) %>%
#   dplyr::filter(successful.length%in%c("Y")) %>%
#   dplyr::mutate(scientific=as.character(scientific)) %>%
#   dplyr::glimpse()
# 
# testboss <- complete.length %>%
#   dplyr::filter(method%in%"BOSS")
# 
# testbruv <- complete.length %>%
#   dplyr::filter(method%in%"BRUV")
# 
# length(unique(testboss$sample))
# 75*2
# length(unique(testbruv$sample))
# 47*2
# 
# # Set predictor variables---
# names(complete.length)
# names(allhab)
# 
# pred.vars = c("depth", 
#               "macroalgae", 
#               "sand", 
#               "biog", 
#               "mean.relief",
#               "tpi",
#               "roughness",
#               "detrended") 
# 
# # predictor variables Removed at first pass---
# # broad.Sponges and broad.Octocoral.Black and broad.Consolidated , "InPreds","BioTurb" are too rare
# 
# dat.length <- complete.length
# 
# # Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
# round(cor(dat.length[,pred.vars]),2)
# # nothing is highly correlated 
# 
# # Plot of likely transformations - thanks to Anna Cresswell for this loop!
# par(mfrow=c(3,2))
# for (i in pred.vars) {
#   x<-dat.length[ ,i]
#   x = as.numeric(unlist(x))
#   hist((x))#Looks best
#   plot((x),main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x+1))
#   plot(log(x+1))
# }
# 
# #all looks fine
# #write data to load in to next script
# saveRDS(dat.length, "data/Tidy/dat.length.rds")
