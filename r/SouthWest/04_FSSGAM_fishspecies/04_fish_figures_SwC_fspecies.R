###
# Project: Gabby PhD Ch3 Capes Region Southwest  Habitat & Fish
# Data:    BRUV & BOSS fish MaxN
# Task:    Plot fish model predictions
# author:  Claude Spencer & Gabby Cummins
# date:    May 2024
##

# Clear your environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(scales)
library(dismo)
library(viridis)
library(patchwork)
library(png)
library(grid)

# Set your study name
name <- "SouthWest"     