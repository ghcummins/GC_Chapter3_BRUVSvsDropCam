###
# Project: Gabby 3rd chapter of PhD
# Data:    BOSS & BRUV fish, habitat
# Task:    Modelling fish abundance w/ FSSGAM
# author:  Claude, Brooke, Kingsley & Gabby
# date:    Nov 2023
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

  # read in
  dat1 <- readRDS("data/tidy/Abrolhos/C.auricularis.rds")%>%
  dplyr::rename(number=maxn)%>%
  glimpse()

dat1$method_fac<-factor(dat1$method)

# dat2 <- dat1 %>%
#   mutate(method = as.factor(method))
# 
# 
# mod <- gam(number ~ s(reef, k=3, bs = 'cr') + s(z, k=3, bs='cr') + method, family=tw(), data =dat2)
# summary(mod)

Model1  <- gam(number ~ s(reef,k=3,bs='cr', by=method_fac) + s(z,k=3,bs='cr', by=method_fac),
               #s(tpi, method_fac, k = 3, bs='fs'),
               family = tw(),  data = dat1)
summary(Model1)

testdata_reef <- expand.grid(reef=seq(min(Model1$model$reef), max(Model1$model$reef), by = 0.1),
                        z = mean(Model1$model$z),
                        method_fac = c("BOSS","BRUV"))

testdata$preds

testdata_reef$preds<-predict.gam(Model1, newdata=testdata_reef, type='response', se.fit=F)
testdata_reef$preds_se<-predict.gam(Model1, newdata=testdata_reef, type='response', se.fit=T)[[2]]

testdata_reef %>%
  ggplot(.) + geom_line(aes(x=reef, y =preds, colour = method_fac))


testdata_z <- expand.grid(reef = mean(Model1$model$reef),
                             z = seq(min(Model1$model$z), max(Model1$model$z), by = 0.1),
                             method_fac = c("BOSS","BRUV"))

testdata_z$preds

testdata_z$preds<-predict.gam(Model1, newdata=testdata_z, type='response', se.fit=F)
testdata_z$preds_se<-predict.gam(Model1, newdata=testdata_z, type='response', se.fit=T)[[2]]

testdata_z %>%
  ggplot(.) + geom_line(aes(x=z, y =preds, colour = method_fac))


testdata_reef %>%
ggplot(.) + geom_line(aes(x=reef, y =preds, colour = method_fac))
test[[2]]
                          
         
                          
  #                         mean(Model1$model$reef),
  #                       z=mean(Model1$model$z),
  #                       method_fac = c("BOSS","BRUV"))%>%
  # distinct()%>%
  # glimpse()

fits <- predict.gam(Model1, newdata=testdata, type='response', se.fit=T)

summary(Model1)

# + s(tpi, method, k = 3, bs='fs') + s(reef, method, k = 3, bs='fs')
test1<-gam(number ~ s(z, k = 3, bs = "cr") + method_fac, dat1, family = tw())




par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
best.model = out.list$success.models[[best.model.name]]
plot(Model1,all.terms = T, pages = 1, residuals = T, pch = 16)
mtext(side = 2, text = resp.vars[i], outer = F)


##make effects plots outside FFSGAM
# Assuming Model1 is your GAM model
library(mgcv)

# Create partial effect plots (plus effects plots) for the smooth terms
par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))  # Set up a multi-panel plot

# Create plus effects plots for all terms in the model
plot(Model1, all.terms = TRUE, pages = 1, residuals = TRUE, pch = 16)

# You can adjust the number of rows and columns in the plot grid as needed
# (e.g., mfrow = c(2, 2) for a 2x2 grid)


