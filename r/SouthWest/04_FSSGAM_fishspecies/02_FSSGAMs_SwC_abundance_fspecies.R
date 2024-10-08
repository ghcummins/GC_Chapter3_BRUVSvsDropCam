###
# Project: Parks - Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Modelling fish abundance w/ FSSGAM
# author:  Claude, Brooke, Kingsley, Gabby
# date:    July 2024
##

rm(list=ls())

# libraries----
# detach("package:plyr", unload=TRUE)#will error - don't worry
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
library(purrr)

#Set study name
name <- "SouthWest"

# read in
dat <- readRDS("data/staging/SwC/Southwest.fish.dat.maxn.rds")%>%
  #dplyr::mutate(reef =rock+inverts)%>%
  mutate(z = abs(z), name = scientific, scientific = paste(method,scientific,sep=".")) %>%
  #mutate(status = ifelse(is.na(status), "No-take", status)) %>%
  glimpse()

# # Re-set the predictors for modeling----
pred.vars <- c("z", "reef", "tpi", "aspect", 
               "roughness","detrended") 

# # Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(as.character(dat$response))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- dat[which(dat$response == unique.vars[i]), ]
  if(length(which(temp.dat$number == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars

# Run the full subset model selection----
savedir <- "outputs/SwC/fish"
use.dat <- as.data.frame(dat)
str(use.dat)

is.na(dat$status) #no trues so all good :)

# factor.vars <- c("status")# Status as a factors with 2 levels
out.all     <- list()
var.imp     <- list()

str(use.dat)

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- as.data.frame(dat[which(dat$response == resp.vars[i]), ])
  use.dat$method <- as.factor(use.dat$method)
  use.dat$location <- as.factor(use.dat$location)
  Model1  <- gam(number ~ s(depth, k = 3, bs='cr'),
                 family = tw(),  data = use.dat)
  #Check with claude if this should be z and not depth
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  cyclic.vars = "aspect",
                                  # pred.vars.fact = factor.vars,
                                  # linear.vars = "depth",
                                  k = 3,
                                  factor.smooth.interactions = F
                                  
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i   <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all <- c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp <- c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model,all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Save model fits, data, and importance scores---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
fish.all.mod.fits   <- do.call("rbind",out.all)
#all.mod.fits <- list_rbind(out.all, names_to = "response")
fish.all.var.imp    <- do.call("rbind",var.imp)
write.csv(fish.all.mod.fits[ , -2], file = paste(savedir, paste(name, "fish.all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(fish.all.var.imp, file = paste(savedir, paste(name, "fish.all.var.imp.csv", sep = "_"), sep = "/"))
