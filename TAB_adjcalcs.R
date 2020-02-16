

#### Calculating adjacent TERRESTRIAL ARTHROPOD BIOMASS values for the GLMM values (trap + adjacent buffer)


setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

TAB_raw <- read.csv("TAB_species-level.csv")

library(dplyr)
library(ggplot2)
library(magrittr)

# STEP 1: FILTER
  # To start, want to isolate only the "true" biomass estimates as some were "derived" by applying, e.g., weight of small spider to weight of unk beetle
    # And remove slugs as they are unlikely prey
TAB_true <- TAB_raw %>% 
  filter(biom_stat != "der", ID != "slug") %>%
  write.csv("TAB_true.csv")

# STEP 2: SUM 
  # Now, sum all species-level biomasses to create a total trap biomass (this dataframe will be used to calculate adjacent buffer biomasses)
TAB_trap <- TAB_true %>% 
  group_by(site, Sample) %>% 
  summarize(total_adjbiomass_mg = sum(adj.biomass.mg)) %>% 
  print(TAB_trap)


# STEP 3: BIND
  # Bind site + Sample columns to make one ID "GF-A1" to make below code work 
  # but first re-name "GS-S" to "GS"
levels(TAB_trap$site)[levels(TAB_trap$site)=="GS-S"] <- "GS"

TAB_trap$name <- paste(TAB_trap$site, TAB_trap$Sample, sep="-")



############################################################################################################################################
##
##                            GRIEF BAY 
##
############################################################################################################################################

GFA1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-B1", "GF-B2")) %>% 
  summarize(GFA1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-A3", "GF-B1", "GF-B2", "GF-B3")) %>% 
  summarize(GFA2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A2", "GF-A3", "GF-A4", "GF-B2", "GF-B3", "GF-B4")) %>% 
  summarize(GFA3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A3", "GF-A4", "GF-A5", "GF-B3", "GF-B4", "GF-B5")) %>% 
  summarize(GFA4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A4", "GF-A5", "GF-A6", "GF-B4", "GF-B5", "GF-B7(2)")) %>%      # B7(2) assigned to be B6 by random draw
  summarize(GFA5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A5", "GF-A6", "GF-A7", "GF-B5", "GF-B7(2)", "GF-B7")) %>%      # B7(2) assigned to be B6 by random draw
  summarize(GFA6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA7 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A6", "GF-A7", "GF-A8", "GF-B7(2)", "GF-B7", "GF-B8")) %>%      # B7(2) assigned to be B6 by random draw
  summarize(GFA7_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA8 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A7", "GF-A8", "GF-A9", "GF-B7", "GF-B8", "GF-B9")) %>% 
  summarize(GFA8_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA8 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A7", "GF-A8", "GF-A9", "GF-B7", "GF-B8", "GF-B9")) %>% 
  summarize(GFA8_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFA9 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A8", "GF-A9", "GF-B8", "GF-B9")) %>% 
  summarize(GFA9_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-B1", "GF-B2", "GF-X1", "GF-X2")) %>% 
  summarize(GFB1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-A3", "GF-B1", "GF-B2", "GF-B3", "GF-X1", "GF-X2", "GF-X3")) %>% 
  summarize(GFB2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A2", "GF-A3", "GF-A4", "GF-B2", "GF-B3", "GF-B4", "GF-X2", "GF-X3", "GF-X4")) %>% 
  summarize(GFB3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A3", "GF-A4", "GF-A5", "GF-B3", "GF-B4", "GF-B5", "GF-X3", "GF-X4", "GF-X5")) %>% 
  summarize(GFB4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB5 <- TAB_trap %>%   
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A4", "GF-A5", "GF-A6", "GF-B4", "GF-B5", "GF-B7(2)", "GF-X4", "GF-X5", "GF-X6")) %>%      # B7(2) assigned to be B6 by random draw
  summarize(GFB5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A5", "GF-A6", "GF-A7", "GF-B5", "GF-B7(2)", "GF-B7", "GF-X5", "GF-X6", "GF-X7")) %>%     # B7(2) assigned to be B6 by random draw
  summarize(GFB6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB7 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A6", "GF-A7", "GF-A8", "GF-B7(2)", "GF-B7", "GF-B8", "GF-X6", "GF-X7", "GF-X8")) %>%     # B7(2) assigned to be B6 by random draw
  summarize(GFB7_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB8 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A7", "GF-A8", "GF-A9", "GF-B7", "GF-B8", "GF-B9", "GF-X7", "GF-X8", "GF-X9")) %>% 
  summarize(GFB8_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFB9 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-A8", "GF-A9", "GF-B8", "GF-B9", "GF-X8", "GF-X9")) %>% 
  summarize(GFB9_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFX1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B1", "GF-B2", "GF-X1", "GF-X2", "GF-Y1", "GF-Y2")) %>% 
  summarize(GFX1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GFX2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B1", "GF-B2", "GF-B3", "GF-X1", "GF-X2", "GF-X3", "GF-Y1", "GF-Y2", "GF-Y3")) %>% 
  summarize(GFX2_buff = mean(total_adjbiomass_mg, na.rm=T))

GFX3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B2", "GF-B3", "GF-B4", "GF-X2", "GF-X3", "GF-X4", "GF-Y2", "GF-Y3", "GF-Y4")) %>% 
  summarize(GFX3_buff = mean(total_adjbiomass_mg, na.rm=T))

GFX4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B3", "GF-B4", "GF-B5", "GF-X3", "GF-X4", "GF-X5", "GF-Y3", "GF-Y4", "GF-Y5")) %>% 
  summarize(GFX4_buff = mean(total_adjbiomass_mg, na.rm=T))

GFX5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B4", "GF-B5", "GF-B7(2)", "GF-X4", "GF-X5", "GF-X6", "GF-Y4", "GF-Y5", "GF-Y6")) %>%      # B7(2) assigned to be B6 by random draw
  summarize(GFX5_buff = mean(total_adjbiomass_mg, na.rm=T))

GFX6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B5", "GF-B7(2)", "GF-B7", "GF-X5", "GF-X6", "GF-X7", "GF-Y5", "GF-Y6", "GF-Y7")) %>%      # B7(2) assigned to be B6 by random draw
  summarize(GFX6_buff = mean(total_adjbiomass_mg, na.rm=T))

GFX7 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B7(2)", "GF-B7", "GF-B8", "GF-X6", "GF-X7", "GF-X8", "GF-Y6", "GF-Y7", "GF-Y8")) %>%      # B7(2) assigned to be B6 by random draw
  summarize(GFX7_buff = mean(total_adjbiomass_mg, na.rm=T))

GFX8 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B7", "GF-B8", "GF-B9", "GF-X7", "GF-X8", "GF-X9", "GF-Y7", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFX8_buff = mean(total_adjbiomass_mg, na.rm=T))

GFX9 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-B8", "GF-B9", "GF-X8", "GF-X9", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFX9_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X1", "GF-X2", "GF-Y1", "GF-Y2")) %>% 
  summarize(GFY1_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X1", "GF-X2", "GF-X3", "GF-Y1", "GF-Y2", "GF-Y3")) %>% 
  summarize(GFY2_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X2", "GF-X3", "GF-X4", "GF-Y2", "GF-Y3", "GF-Y4")) %>% 
  summarize(GFY3_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X3", "GF-X4", "GF-X5", "GF-Y3", "GF-Y4", "GF-Y5")) %>% 
  summarize(GFY4_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X4", "GF-X5", "GF-X6", "GF-Y4", "GF-Y5", "GF-Y6")) %>% 
  summarize(GFY5_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X5", "GF-X6", "GF-X7", "GF-Y5", "GF-Y6", "GF-Y7")) %>% 
  summarize(GFY6_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY7 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X6", "GF-X7", "GF-X8", "GF-Y6", "GF-Y7", "GF-Y8")) %>% 
  summarize(GFY7_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY8 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X7", "GF-X8", "GF-X9", "GF-Y7", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFY8_buff = mean(total_adjbiomass_mg, na.rm=T))

GFY9 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GF-X8", "GF-X9", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFY9_buff = mean(total_adjbiomass_mg, na.rm=T))



############################################################################################################################################
##
##                            GOSLING 
##
############################################################################################################################################


GOSA1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-B1", "GOS-B2")) %>% 
  summarize(GOSA1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSA2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-A3", "GOS-B1", "GOS-B2", "GOS-B3")) %>% 
  summarize(GOSA2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSA3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A2", "GOS-A3", "GOS-A4", "GOS-B2", "GOS-B3", "GOS-B4")) %>% 
  summarize(GOSA3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSA4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A3", "GOS-A4", "GOS-A5", "GOS-B3", "GOS-B4", "GOS-B5")) %>% 
  summarize(GOSA4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSA5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A4", "GOS-A5", "GOS-A6", "GOS-B4", "GOS-B5", "GOS-B6")) %>% 
  summarize(GOSA5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSA6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A5", "GOS-A6", "GOS-B5", "GOS-B6")) %>% 
  summarize(GOSA6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSB1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-B1", "GOS-B2", "GOS-C1", "GOS-C2")) %>% 
  summarize(GOSB1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSB2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-A3", "GOS-B1", "GOS-B2", "GOS-B3", "GOS-C1", "GOS-C2", "GOS-C3")) %>% 
  summarize(GOSB2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSB3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A2", "GOS-A3", "GOS-A4", "GOS-B2", "GOS-B3", "GOS-B4", "GOS-C2", "GOS-C3", "GOS-C4")) %>% 
  summarize(GOSB3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSB4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A3", "GOS-A4", "GOS-A5", "GOS-B3", "GOS-B4", "GOS-B5", "GOS-C3", "GOS-C4", "GOS-C5")) %>% 
  summarize(GOSB4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSB5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A4", "GOS-A5", "GOS-A6", "GOS-B4", "GOS-B5", "GOS-B6", "GOS-C4", "GOS-C5", "GOS-C6")) %>% 
  summarize(GOSB5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSB6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-A5", "GOS-A6", "GOS-B5", "GOS-B6", "GOS-C5", "GOS-C6")) %>% 
  summarize(GOSB6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSC1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-B1", "GOS-B2", "GOS-C1", "GOS-C2", "GOS-D1", "GOS-D2")) %>% 
  summarize(GOSC1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GOSC2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-B1", "GOS-B2", "GOS-B3", "GOS-C1", "GOS-C2", "GOS-C3", "GOS-D1", "GOS-D2", "GOS-D3")) %>% 
  summarize(GOSC2_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSC3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-B2", "GOS-B3", "GOS-B4", "GOS-C2", "GOS-C3", "GOS-C4", "GOS-D2", "GOS-D3", "GOS-D4")) %>% 
  summarize(GOSC3_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSC4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-B3", "GOS-B4", "GOS-B5", "GOS-C3", "GOS-C4", "GOS-C5", "GOS-D3", "GOS-D4", "GOS-D5")) %>% 
  summarize(GOSC4_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSC5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-B4", "GOS-B5", "GOS-B6", "GOS-C4", "GOS-C5", "GOS-C6", "GOS-D4", "GOS-D5", "GOS-D6")) %>% 
  summarize(GOSC5_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSC6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-B5", "GOS-B6", "GOS-C5", "GOS-C6", "GOS-D5", "GOS-D6")) %>% 
  summarize(GOSC6_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSD1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-C1", "GOS-C2", "GOS-D1", "GOS-D2", "GOS-E1", "GOS-E2")) %>% 
  summarize(GOSD1_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSD2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-C1", "GOS-C2", "GOS-C3", "GOS-D1", "GOS-D2", "GOS-D3", "GOS-E1", "GOS-E2", "GOS-E3")) %>% 
  summarize(GOSD2_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSD3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-C2", "GOS-C3", "GOS-C4", "GOS-D2", "GOS-D3", "GOS-D4", "GOS-E2", "GOS-E3", "GOS-E4")) %>% 
  summarize(GOSD3_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSD4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-C3", "GOS-C4", "GOS-C5", "GOS-D3", "GOS-D4", "GOS-D5", "GOS-E3", "GOS-E4", "GOS-E5")) %>% 
  summarize(GOSD4_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSD5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-C4", "GOS-C5", "GOS-C6", "GOS-D4", "GOS-D5", "GOS-D6", "GOS-E4", "GOS-E5", "GOS-E6")) %>% 
  summarize(GOSD5_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSD6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-C5", "GOS-C6", "GOS-D5", "GOS-D6", "GOS-E5", "GOS-E6")) %>% 
  summarize(GOSD6_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSE1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-D1", "GOS-D2", "GOS-E1", "GOS-E2", "GOS-F1", "GOS-F2")) %>% 
  summarize(GOSE1_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSE2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-D1", "GOS-D2", "GOS-D3", "GOS-E1", "GOS-E2", "GOS-E3", "GOS-F1", "GOS-F2", "GOS-F3")) %>% 
  summarize(GOSE2_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSE3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-D2", "GOS-D3", "GOS-D4", "GOS-E2", "GOS-E3", "GOS-E4", "GOS-F2", "GOS-F3", "GOS-F4")) %>% 
  summarize(GOSE3_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSE4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-D3", "GOS-D4", "GOS-D5", "GOS-E3", "GOS-E4", "GOS-E5", "GOS-F3", "GOS-F4", "GOS-F5")) %>% 
  summarize(GOSE4_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSE5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-D4", "GOS-D5", "GOS-D6", "GOS-E4", "GOS-E5", "GOS-E6", "GOS-F4", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSE5_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSE6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-D5", "GOS-D6", "GOS-E5", "GOS-E6", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSE6_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSF1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-E1", "GOS-E2", "GOS-F1", "GOS-F2")) %>% 
  summarize(GOSF1_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSF2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-E1", "GOS-E2", "GOS-E3", "GOS-F1", "GOS-F2", "GOS-F3")) %>% 
  summarize(GOSF2_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSF3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-E2", "GOS-E3", "GOS-E4", "GOS-F2", "GOS-F3", "GOS-F4")) %>% 
  summarize(GOSF3_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSF4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-E3", "GOS-E4", "GOS-E5", "GOS-F3", "GOS-F4", "GOS-F5")) %>% 
  summarize(GOSF4_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSF5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-E4", "GOS-E5", "GOS-E6", "GOS-F4", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSF5_buff = mean(total_adjbiomass_mg, na.rm=T))

GOSF6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GOS-E5", "GOS-E6", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSF6_buff = mean(total_adjbiomass_mg, na.rm=T))





############################################################################################################################################
##
##                            GOOSE 
##
############################################################################################################################################


GSA1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-B1", "GS-B2")) %>% 
  summarize(GSA1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSA2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-A3", "GS-B1", "GS-B2", "GS-B3")) %>% 
  summarize(GSA2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSA3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A2", "GS-A3", "GS-A4", "GS-B2", "GS-B3", "GS-B4")) %>% 
  summarize(GSA3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSA4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A3", "GS-A4", "GS-A5", "GS-B3", "GS-B4", "GS-B5")) %>% 
  summarize(GSA4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSA5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A4", "GS-A5", "GS-A6", "GS-B4", "GS-B5", "GS-B6")) %>% 
  summarize(GSA5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSA6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A5", "GS-A6", "GS-B5", "GS-B6")) %>% 
  summarize(GSA6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSB1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-B1", "GS-B2", "GS-C1", "GS-C2")) %>% 
  summarize(GSB1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSB2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-A3", "GS-B1", "GS-B2", "GS-B3", "GS-C1", "GS-C2", "GS-C3")) %>% 
  summarize(GSB2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSB3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A2", "GS-A3", "GS-A4", "GS-B2", "GS-B3", "GS-B4", "GS-C2", "GS-C3", "GS-C4")) %>% 
  summarize(GSB3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSB4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A3", "GS-A4", "GS-A5", "GS-B3", "GS-B4", "GS-B5", "GS-C3", "GS-C4", "GS-C5")) %>% 
  summarize(GSB4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSB5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A4", "GS-A5", "GS-A6", "GS-B4", "GS-B5", "GS-B6", "GS-C4", "GS-C5", "GS-C6")) %>% 
  summarize(GSB5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSB6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-A5", "GS-A6", "GS-B5", "GS-B6", "GS-C5", "GS-C6")) %>% 
  summarize(GSB6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSC1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-B1", "GS-B2", "GS-C1", "GS-C2", "GS-D1", "GS-D2")) %>% 
  summarize(GSC1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

GSC2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-B1", "GS-B2", "GS-B3", "GS-C1", "GS-C2", "GS-C3", "GS-D1", "GS-D2", "GS-D3")) %>% 
  summarize(GSC2_buff = mean(total_adjbiomass_mg, na.rm=T))

GSC3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-B2", "GS-B3", "GS-B4", "GS-C2", "GS-C3", "GS-C4", "GS-D2", "GS-D3", "GS-D4")) %>% 
  summarize(GSC3_buff = mean(total_adjbiomass_mg, na.rm=T))

GSC4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-B3", "GS-B4", "GS-B5", "GS-C3", "GS-C4", "GS-C5", "GS-D3", "GS-D4", "GS-D5")) %>% 
  summarize(GSC4_buff = mean(total_adjbiomass_mg, na.rm=T))

GSC5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-B4", "GS-B5", "GS-B6", "GS-C4", "GS-C5", "GS-C6", "GS-D4", "GS-D5", "GS-D6")) %>% 
  summarize(GSC5_buff = mean(total_adjbiomass_mg, na.rm=T))

GSC6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-B5", "GS-B6", "GS-C5", "GS-C6", "GS-D5", "GS-D6")) %>% 
  summarize(GSC6_buff = mean(total_adjbiomass_mg, na.rm=T))

GSD1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-C1", "GS-C2", "GS-D1", "GS-D2", "GS-E1", "GS-E2")) %>% 
  summarize(GSD1_buff = mean(total_adjbiomass_mg, na.rm=T))

GSD2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-C1", "GS-C2", "GS-C3", "GS-D1", "GS-D2", "GS-D3", "GS-E1", "GS-E2", "GS-E3")) %>% 
  summarize(GSD2_buff = mean(total_adjbiomass_mg, na.rm=T))

GSD3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-C2", "GS-C3", "GS-C4", "GS-D2", "GS-D3", "GS-D4", "GS-E2", "GS-E3", "GS-E4")) %>% 
  summarize(GSD3_buff = mean(total_adjbiomass_mg, na.rm=T))

GSD4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-C3", "GS-C4", "GS-C5", "GS-D3", "GS-D4", "GS-D5", "GS-E3", "GS-E4", "GS-E5")) %>% 
  summarize(GSD4_buff = mean(total_adjbiomass_mg, na.rm=T))

GSD5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-C4", "GS-C5", "GS-C6", "GS-D4", "GS-D5", "GS-D6", "GS-E4", "GS-E5", "GS-E6")) %>% 
  summarize(GSD5_buff = mean(total_adjbiomass_mg, na.rm=T))

GSD6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-C5", "GS-C6", "GS-D5", "GS-D6", "GS-E5", "GS-E6")) %>% 
  summarize(GSD6_buff = mean(total_adjbiomass_mg, na.rm=T))

GSE1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-D1", "GS-D2", "GS-E1", "GS-E2", "GS-F1", "GS-F2")) %>% 
  summarize(GSE1_buff = mean(total_adjbiomass_mg, na.rm=T))

GSE2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-D1", "GS-D2", "GS-D3", "GS-E1", "GS-E2", "GS-E3", "GS-F1", "GS-F2", "GS-F3")) %>% 
  summarize(GSE2_buff = mean(total_adjbiomass_mg, na.rm=T))

GSE3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-D2", "GS-D3", "GS-D4", "GS-E2", "GS-E3", "GS-E4", "GS-F2", "GS-F3", "GS-F4")) %>% 
  summarize(GSE3_buff = mean(total_adjbiomass_mg, na.rm=T))

GSE4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-D3", "GS-D4", "GS-D5", "GS-E3", "GS-E4", "GS-E5", "GS-F3", "GS-F4", "GS-F5")) %>% 
  summarize(GSE4_buff = mean(total_adjbiomass_mg, na.rm=T))

GSE5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-D4", "GS-D5", "GS-D6", "GS-E4", "GS-E5", "GS-E6", "GS-F4", "GS-F5", "GS-F6")) %>% 
  summarize(GSE5_buff = mean(total_adjbiomass_mg, na.rm=T))

GSE6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-D5", "GS-D6", "GS-E5", "GS-E6", "GS-F5", "GS-F6")) %>% 
  summarize(GSE6_buff = mean(total_adjbiomass_mg, na.rm=T))

GSF1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-E1", "GS-E2", "GS-F1", "GS-F2")) %>% 
  summarize(GSF1_buff = mean(total_adjbiomass_mg, na.rm=T))

GSF2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-E1", "GS-E2", "GS-E3", "GS-F1", "GS-F2", "GS-F3")) %>% 
  summarize(GSF2_buff = mean(total_adjbiomass_mg, na.rm=T))

GSF3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-E2", "GS-E3", "GS-E4", "GS-F2", "GS-F3", "GS-F4")) %>% 
  summarize(GSF3_buff = mean(total_adjbiomass_mg, na.rm=T))

GSF4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-E3", "GS-E4", "GS-E5", "GS-F3", "GS-F4", "GS-F5")) %>% 
  summarize(GSF4_buff = mean(total_adjbiomass_mg, na.rm=T))

GSF5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-E4", "GS-E5", "GS-E6", "GS-F4", "GS-F5", "GS-F6")) %>% 
  summarize(GSF5_buff = mean(total_adjbiomass_mg, na.rm=T))

GSF6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("GS-E5", "GS-E6", "GS-F5", "GS-F6")) %>% 
  summarize(GSF6_buff = mean(total_adjbiomass_mg, na.rm=T))







############################################################################################################################################
##
##                            NORTH BEACH 
##
############################################################################################################################################


NBA1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-B1", "NB-B2")) %>% 
  summarize(NBA1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBA2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-A3", "NB-B1", "NB-B2", "NB-B3")) %>% 
  summarize(NBA2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBA3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A2", "NB-A3", "NB-A4", "NB-B2", "NB-B3", "NB-B4")) %>% 
  summarize(NBA3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBA4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A3", "NB-A4", "NB-A5", "NB-B3", "NB-B4", "NB-B5")) %>% 
  summarize(NBA4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBA5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A4", "NB-A5", "NB-A6", "NB-B4", "NB-B5", "NB-B6")) %>% 
  summarize(NBA5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBA6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A5", "NB-A6", "NB-B5", "NB-B6")) %>% 
  summarize(NBA6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBB1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-B1", "NB-B2", "NB-C1", "NB-C2")) %>% 
  summarize(NBB1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBB2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-A3", "NB-B1", "NB-B2", "NB-B3", "NB-C1", "NB-C2", "NB-C3")) %>% 
  summarize(NBB2_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBB3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A2", "NB-A3", "NB-A4", "NB-B2", "NB-B3", "NB-B4", "NB-C2", "NB-C3", "NB-C4")) %>% 
  summarize(NBB3_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBB4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A3", "NB-A4", "NB-A5", "NB-B3", "NB-B4", "NB-B5", "NB-C3", "NB-C4", "NB-C5")) %>% 
  summarize(NBB4_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBB5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A4", "NB-A5", "NB-A6", "NB-B4", "NB-B5", "NB-B6", "NB-C4", "NB-C5", "NB-C6")) %>% 
  summarize(NBB5_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBB6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-A5", "NB-A6", "NB-B5", "NB-B6", "NB-C5", "NB-C6")) %>% 
  summarize(NBB6_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBC1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-B1", "NB-B2", "NB-C1", "NB-C2", "NB-D1", "NB-D2")) %>% 
  summarize(NBC1_buff = mean(total_adjbiomass_mg, na.rm=T)) 

NBC2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-B1", "NB-B2", "NB-B3", "NB-C1", "NB-C2", "NB-C3", "NB-D1", "NB-D2", "NB-D3")) %>% 
  summarize(NBC2_buff = mean(total_adjbiomass_mg, na.rm=T))

NBC3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-B2", "NB-B3", "NB-B4", "NB-C2", "NB-C3", "NB-C4", "NB-D2", "NB-D3", "NB-D4")) %>% 
  summarize(NBC3_buff = mean(total_adjbiomass_mg, na.rm=T))

NBC4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-B3", "NB-B4", "NB-B5", "NB-C3", "NB-C4", "NB-C5", "NB-D3", "NB-D4", "NB-D5")) %>% 
  summarize(NBC4_buff = mean(total_adjbiomass_mg, na.rm=T))

NBC5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-B4", "NB-B5", "NB-B6", "NB-C4", "NB-C5", "NB-C6", "NB-D4", "NB-D5", "NB-D6")) %>% 
  summarize(NBC5_buff = mean(total_adjbiomass_mg, na.rm=T))

NBC6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-B5", "NB-B6", "NB-C5", "NB-C6", "NB-D5", "NB-D6")) %>% 
  summarize(NBC6_buff = mean(total_adjbiomass_mg, na.rm=T))

NBD1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-C1", "NB-C2", "NB-D1", "NB-D2", "NB-E1", "NB-E2")) %>% 
  summarize(NBD1_buff = mean(total_adjbiomass_mg, na.rm=T))

NBD2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-C1", "NB-C2", "NB-C3", "NB-D1", "NB-D2", "NB-D3", "NB-E1", "NB-E2", "NB-E3")) %>% 
  summarize(NBD2_buff = mean(total_adjbiomass_mg, na.rm=T))

NBD3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-C2", "NB-C3", "NB-C4", "NB-D2", "NB-D3", "NB-D4", "NB-E2", "NB-E3", "NB-E4")) %>% 
  summarize(NBD3_buff = mean(total_adjbiomass_mg, na.rm=T))

NBD4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-C3", "NB-C4", "NB-C5", "NB-D3", "NB-D4", "NB-D5", "NB-E3", "NB-E4", "NB-E5")) %>% 
  summarize(NBD4_buff = mean(total_adjbiomass_mg, na.rm=T))

NBD5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-C4", "NB-C5", "NB-C6", "NB-D4", "NB-D5", "NB-D6", "NB-E4", "NB-E5", "NB-E6")) %>% 
  summarize(NBD5_buff = mean(total_adjbiomass_mg, na.rm=T))

NBD6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-C5", "NB-C6", "NB-D5", "NB-D6", "NB-E5", "NB-E6")) %>% 
  summarize(NBD6_buff = mean(total_adjbiomass_mg, na.rm=T))

NBE1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-D1", "NB-D2", "NB-E1", "NB-E2", "NB-F1", "NB-F2")) %>% 
  summarize(NBE1_buff = mean(total_adjbiomass_mg, na.rm=T))

NBE2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-D1", "NB-D2", "NB-D3", "NB-E1", "NB-E2", "NB-E3", "NB-F1", "NB-F2", "NB-F3")) %>% 
  summarize(NBE2_buff = mean(total_adjbiomass_mg, na.rm=T))

NBE3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-D2", "NB-D3", "NB-D4", "NB-E2", "NB-E3", "NB-E4", "NB-F2", "NB-F3", "NB-F4")) %>% 
  summarize(NBE3_buff = mean(total_adjbiomass_mg, na.rm=T))

NBE4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-D3", "NB-D4", "NB-D5", "NB-E3", "NB-E4", "NB-E5", "NB-F3", "NB-F4", "NB-F5")) %>% 
  summarize(NBE4_buff = mean(total_adjbiomass_mg, na.rm=T))

NBE5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-D4", "NB-D5", "NB-D6", "NB-E4", "NB-E5", "NB-E6", "NB-F4", "NB-F5", "NB-F6")) %>% 
  summarize(NBE5_buff = mean(total_adjbiomass_mg, na.rm=T))

NBE6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-D5", "NB-D6", "NB-E5", "NB-E6", "NB-F5", "NB-F6")) %>% 
  summarize(NBE6_buff = mean(total_adjbiomass_mg, na.rm=T))

NBF1 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-E1", "NB-E2", "NB-F1", "NB-F2")) %>% 
  summarize(NBF1_buff = mean(total_adjbiomass_mg, na.rm=T))

NBF2 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-E1", "NB-E2", "NB-E3", "NB-F1", "NB-F2", "NB-F3")) %>% 
  summarize(NBF2_buff = mean(total_adjbiomass_mg, na.rm=T))

NBF3 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-E2", "NB-E3", "NB-E4", "NB-F2", "NB-F3", "NB-F4")) %>% 
  summarize(NBF3_buff = mean(total_adjbiomass_mg, na.rm=T))

NBF4 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-E3", "NB-E4", "NB-E5", "NB-F3", "NB-F4", "NB-F5")) %>% 
  summarize(NBF4_buff = mean(total_adjbiomass_mg, na.rm=T))

NBF5 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-E4", "NB-E5", "NB-E6", "NB-F4", "NB-F5", "NB-F6")) %>% 
  summarize(NBF5_buff = mean(total_adjbiomass_mg, na.rm=T))

NBF6 <- TAB_trap %>% 
  select(name, site, total_adjbiomass_mg) %>%
  filter(name %in% c("NB-E5", "NB-E6", "NB-F5", "NB-F6")) %>% 
  summarize(NBF6_buff = mean(total_adjbiomass_mg, na.rm=T))






### BIND ALL VALUES INTO ONE DATAFRAME

TAB_buffadj <- bind_cols(GFA1, GFA2, GFA3, GFA4, GFA5, GFA6, GFA7, GFA8, GFA9,
                         GFB1, GFB2, GFB3, GFB4, GFB5, GFB6, GFB7, GFB8, GFB9,
                         GFX1, GFX2, GFX3, GFX4, GFX5, GFX6, GFX7, GFX8, GFX9,
                         GFY1, GFY2, GFY3, GFY4, GFY5, GFY6, GFY7, GFY8, GFY9,
                         GOSA1, GOSA2, GOSA3, GOSA4, GOSA5, GOSA6,
                         GOSB1, GOSB2, GOSB3, GOSB4, GOSB5, GOSB6,
                         GOSC1, GOSC2, GOSC3, GOSC4, GOSC5, GOSC6,
                         GOSD1, GOSD2, GOSD3, GOSD4, GOSD5, GOSD6,
                         GOSE1, GOSE2, GOSE3, GOSE4, GOSE5, GOSE6,
                         GOSF1, GOSF2, GOSF3, GOSF4, GOSF5, GOSF6,
                         GSA1, GSA2, GSA3, GSA4, GSA5, GSA6,
                         GSB1, GSB2, GSB3, GSB4, GSB5, GSB6,
                         GSC1, GSC2, GSC3, GSC4, GSC5, GSC6,
                         GSD1, GSD2, GSD3, GSD4, GSD5, GSD6,
                         GSE1, GSE2, GSE3, GSE4, GSE5, GSE6,
                         GSF1, GSF2, GSF3, GSF4, GSF5, GSF6,
                         NBA1, NBA2, NBA3, NBA4, NBA5, NBA6,
                         NBB1, NBB2, NBB3, NBB4, NBB5, NBB6,
                         NBC1, NBC2, NBC3, NBC4, NBC5, NBC6,
                         NBD1, NBD2, NBD3, NBD4, NBD5, NBD6,
                         NBE1, NBE2, NBE3, NBE4, NBE5, NBE6,
                         NBF1, NBF2, NBF3, NBF4, NBF5, NBF6)
write.csv(TAB_buffadj, "TAB_buffadj.csv")       # unfortunately is in column-based format so need to transpose for ease of use

# transpose dataframe
TAB_buffadj <- read.csv("TAB_buffadj.csv")
transposed_TAB_buffadj <- t(TAB_buffadj)
write.csv(transposed_TAB_buffadj, "TAB_buffadj.csv")    # re-write old untransposed file into row-based format






################################################################################################################################################################

# Calculate SITE level TAB

TAB_raw <- read.csv("TAB_species-level.csv")


# STEP 1: FILTER
# To start, want to isolate only the "true" biomass estimates as some were "derived" by applying, e.g., weight of small spider to weight of unk beetle
# And remove slugs as they are unlikely prey
TAB_true <- TAB_raw %>% 
  select(region, site, Sample, dist_group, ID, ID2, Guild, Order, count, avg_ind_biomass, biomass, adj.biomass.mg, adj.biomass.g, biom_stat) %>%
  filter(biom_stat != "der", ID != "slug") 

# STEP 2: SUM 
# Now, sum all species-level biomasses to create a total trap biomass (this dataframe will be used to calculate adjacent buffer biomasses)
TAB_trap <- TAB_true %>% 
  group_by(site, Sample) %>% 
  summarize(total_adjbiomass_mg = sum(adj.biomass.mg)) %>% 
  print(TAB_trap)

# STEP 3: AVERAGE 
  # Take average across all trap biomassess for each site 
TAB_site <- TAB_trap %>% 
  group_by(site) %>%
  summarize(site_mean = mean(total_adjbiomass_mg), sd=sd(total_adjbiomass_mg)) %>% 
  print(TAB_site)



















































