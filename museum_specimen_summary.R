# museum mouse data comparisons 
# KDavidson
# 2015 - cleaned 2020

library(tidyverse)

setwd("~/UVic/`Hakai 100 Islands 2015/`DATA/Isotopes")

data.raw <- read.csv("museum_mice_only.csv")

#########################################################################################################################################################

#                                                               CLEANED

data <- data.raw %>% 
  filter(Spp=="Mouse") %>%
  print()

#########################################################################################################################################################

#                                                               SUMMARY

museum_sum <- data %>% 
  group_by(loc) %>% 
  summarize(d13C_mean=mean(d13C), d13C_sd=sd(d13C)/sqrt(length(d13C)), d15N_mean=mean(d15N), d15N_sd=sd(d15N)/sqrt(length(d15N))) %>% 
  print()


