###
# Project: Parks - Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Modelling fish abundance w/ FSSGAM
# author:  Claude, Brooke, Kingsley
# date:    Nov-Dec 2021
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

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "Abrolhos"  # set study name

dat <- readRDS("data/staging/Abrolhos/ShallowBank.maxn.rds")%>%
  dplyr::mutate(reef = macroalgae+rock+inverts)%>%
  mutate(z = abs(z), scientific = paste(method,scientific,sep=".")) %>%
    glimpse()
    unique(dat$scientific)

# # Re-set the predictors for modeling----
pred.vars <- c("z", "reef",
               "mean.relief","roughness","detrended") 

# Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(as.character(dat$scientific))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- dat[which(dat$scientific == unique.vars[i]), ]
  if(length(which(temp.dat$maxn == 0)) / nrow(temp.dat) < 0.9){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars   

# Run the full subset model selection----
savedir <- "outputs/Abrolhos"
use.dat <- as.data.frame(dat) 
str(use.dat)

is.na(dat$status) 

# factor.vars <- c("status")# Status as a factors with 2 levels
out.all     <- list()
var.imp     <- list()

str(use.dat)

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- as.data.frame(dat[which(dat$scientific == resp.vars[i]), ])
  use.dat$method <- as.factor(use.dat$method)
  use.dat$location <- as.factor(use.dat$location)
  Model1  <- gam(maxn ~ s(depth, k = 3, bs='cr'),
                 family = gaussian(link="identity"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
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
all.mod.fits   <- do.call("rbind",out.all)
all.var.imp    <- do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))


