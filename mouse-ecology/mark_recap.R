
# home range calculation revision
# removed using convex hull at recommendation of anomymous reviewer and replace with adehabitatHR 

# jun 2021

#####################################################################################################################################################

setwd("~/zzzpersonal")

library(tidyverse)
library(readxl)
library(sp)   # for spatial points dataframe 
library(adehabitatHR)     # for home range size calc

mr.raw <- read_excel("MR.xlsx", sheet="Sheet1")

#####################################################################################################################################################

#                                                                     CLEAN

mr.dat <- mr.raw %>%
  rename(mouse_id = Mouse_ID,
         trap_id = Trap_name,
         date=Date,
         site=Site,
         dist_shore=Distshore,
         weight=Weight,
         foot_lgth=Foot,
         condition_factor=CF) %>%
  print()

#####################################################################################################################################################

#                                                                 EXTRACT >= 3 recaps

recap_names <- mr.dat %>%
  group_by(mouse_id) %>%
  summarize(n=n()) %>% 
  filter(n>=3) %>%
  pull(mouse_id) %>%
  print()


# Extract from dataframe 
recaps <- mr.dat %>% 
  filter(mouse_id%in%recap_names) %>% 
  filter(mouse_id != "GOS-1Br") %>%
  dplyr::select(mouse_id, x_dd, y_dd) %>%
  mutate_at(c("x_dd", "y_dd"), as.numeric) %>%
  print()
# GOS 1-Br removed over note about concerns over duplicated entry 

#####################################################################################################################################################
  
#                                                                        SPATIAL 

# prepare coordinates, data, and proj4string
coords <- recaps[,c("x_dd", "y_dd")]   
data <- recaps[,1]         
crs <- CRS("+init=epsg:28992") # proj4string of coords

# make the SpatialPointsDataFrame object
mouse.sp <- SpatialPointsDataFrame(coords=coords, data=data, proj4string=crs)

# homerange 
kernel.ref <- kernelUD(mouse.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref) 



