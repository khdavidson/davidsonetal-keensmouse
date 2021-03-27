

#### Calculating adjacent NDVI values for the GLMM values (trap + adjacent buffer) 


setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

NDVI_raw <- read.csv("NDVI_traps.csv")

library(dplyr)


############################################################################################################################################
##
##                            GRIEF BAY 
##
############################################################################################################################################

GFA1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-B1", "GF-B2")) %>% 
  summarize(GFA1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-A3", "GF-B1", "GF-B2", "GF-B3")) %>% 
  summarize(GFA2_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A2", "GF-A3", "GF-A4", "GF-B2", "GF-B3", "GF-B4")) %>% 
  summarize(GFA3_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A3", "GF-A4", "GF-A5", "GF-B3", "GF-B4", "GF-B5")) %>% 
  summarize(GFA4_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A4", "GF-A5", "GF-A6", "GF-B4", "GF-B5", "GF-B6")) %>% 
  summarize(GFA5_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A5", "GF-A6", "GF-A7", "GF-B5", "GF-B6", "GF-B7")) %>% 
  summarize(GFA6_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA7 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A6", "GF-A7", "GF-A8", "GF-B6", "GF-B7", "GF-B8")) %>% 
  summarize(GFA7_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA8 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A7", "GF-A8", "GF-A9", "GF-B7", "GF-B8", "GF-B9")) %>% 
  summarize(GFA8_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA8 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A7", "GF-A8", "GF-A9", "GF-B7", "GF-B8", "GF-B9")) %>% 
  summarize(GFA8_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFA9 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A8", "GF-A9", "GF-B8", "GF-B9")) %>% 
  summarize(GFA9_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-B1", "GF-B2", "GF-X1", "GF-X2")) %>% 
  summarize(GFB1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A1", "GF-A2", "GF-A3", "GF-B1", "GF-B2", "GF-B3", "GF-X1", "GF-X2", "GF-X3")) %>% 
  summarize(GFB2_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A2", "GF-A3", "GF-A4", "GF-B2", "GF-B3", "GF-B4", "GF-X2", "GF-X3", "GF-X4")) %>% 
  summarize(GFB3_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A3", "GF-A4", "GF-A5", "GF-B3", "GF-B4", "GF-B5", "GF-X3", "GF-X4", "GF-X5")) %>% 
  summarize(GFB4_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A4", "GF-A5", "GF-A6", "GF-B4", "GF-B5", "GF-B6", "GF-X4", "GF-X5", "GF-X6")) %>% 
  summarize(GFB5_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A5", "GF-A6", "GF-A7", "GF-B5", "GF-B6", "GF-B7", "GF-X5", "GF-X6", "GF-X7")) %>% 
  summarize(GFB6_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB7 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A6", "GF-A7", "GF-A8", "GF-B6", "GF-B7", "GF-B8", "GF-X6", "GF-X7", "GF-X8")) %>% 
  summarize(GFB7_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB8 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A7", "GF-A8", "GF-A9", "GF-B7", "GF-B8", "GF-B9", "GF-X7", "GF-X8", "GF-X9")) %>% 
  summarize(GFB8_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFB9 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-A8", "GF-A9", "GF-B8", "GF-B9", "GF-X8", "GF-X9")) %>% 
  summarize(GFB9_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFX1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B1", "GF-B2", "GF-X1", "GF-X2", "GF-Y1", "GF-Y2")) %>% 
  summarize(GFX1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GFX2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B1", "GF-B2", "GF-B3", "GF-X1", "GF-X2", "GF-X3", "GF-Y1", "GF-Y2", "GF-Y3")) %>% 
  summarize(GFX2_buff = mean(NDVI_5m_mean, na.rm=T))

GFX3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B2", "GF-B3", "GF-B4", "GF-X2", "GF-X3", "GF-X4", "GF-Y2", "GF-Y3", "GF-Y4")) %>% 
  summarize(GFX3_buff = mean(NDVI_5m_mean, na.rm=T))

GFX4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B3", "GF-B4", "GF-B5", "GF-X3", "GF-X4", "GF-X5", "GF-Y3", "GF-Y4", "GF-Y5")) %>% 
  summarize(GFX4_buff = mean(NDVI_5m_mean, na.rm=T))

GFX5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B4", "GF-B5", "GF-B6", "GF-X4", "GF-X5", "GF-X6", "GF-Y4", "GF-Y5", "GF-Y6")) %>% 
  summarize(GFX5_buff = mean(NDVI_5m_mean, na.rm=T))

GFX6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B5", "GF-B6", "GF-B7", "GF-X5", "GF-X6", "GF-X7", "GF-Y5", "GF-Y6", "GF-Y7")) %>% 
  summarize(GFX6_buff = mean(NDVI_5m_mean, na.rm=T))

GFX7 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B6", "GF-B7", "GF-B8", "GF-X6", "GF-X7", "GF-X8", "GF-Y6", "GF-Y7", "GF-Y8")) %>% 
  summarize(GFX7_buff = mean(NDVI_5m_mean, na.rm=T))

GFX8 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B7", "GF-B8", "GF-B9", "GF-X7", "GF-X8", "GF-X9", "GF-Y7", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFX8_buff = mean(NDVI_5m_mean, na.rm=T))

GFX9 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-B8", "GF-B9", "GF-X8", "GF-X9", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFX9_buff = mean(NDVI_5m_mean, na.rm=T))

GFY1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X1", "GF-X2", "GF-Y1", "GF-Y2")) %>% 
  summarize(GFY1_buff = mean(NDVI_5m_mean, na.rm=T))

GFY2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X1", "GF-X2", "GF-X3", "GF-Y1", "GF-Y2", "GF-Y3")) %>% 
  summarize(GFY2_buff = mean(NDVI_5m_mean, na.rm=T))

GFY3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X2", "GF-X3", "GF-X4", "GF-Y2", "GF-Y3", "GF-Y4")) %>% 
  summarize(GFY3_buff = mean(NDVI_5m_mean, na.rm=T))

GFY4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X3", "GF-X4", "GF-X5", "GF-Y3", "GF-Y4", "GF-Y5")) %>% 
  summarize(GFY4_buff = mean(NDVI_5m_mean, na.rm=T))

GFY5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X4", "GF-X5", "GF-X6", "GF-Y4", "GF-Y5", "GF-Y6")) %>% 
  summarize(GFY5_buff = mean(NDVI_5m_mean, na.rm=T))

GFY6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X5", "GF-X6", "GF-X7", "GF-Y5", "GF-Y6", "GF-Y7")) %>% 
  summarize(GFY6_buff = mean(NDVI_5m_mean, na.rm=T))

GFY7 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X6", "GF-X7", "GF-X8", "GF-Y6", "GF-Y7", "GF-Y8")) %>% 
  summarize(GFY7_buff = mean(NDVI_5m_mean, na.rm=T))

GFY8 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X7", "GF-X8", "GF-X9", "GF-Y7", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFY8_buff = mean(NDVI_5m_mean, na.rm=T))

GFY9 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GF-X8", "GF-X9", "GF-Y8", "GF-Y9")) %>% 
  summarize(GFY9_buff = mean(NDVI_5m_mean, na.rm=T))



############################################################################################################################################
##
##                            GOSLING 
##
############################################################################################################################################


GOSA1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-B1", "GOS-B2")) %>% 
  summarize(GOSA1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSA2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-A3", "GOS-B1", "GOS-B2", "GOS-B3")) %>% 
  summarize(GOSA2_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSA3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A2", "GOS-A3", "GOS-A4", "GOS-B2", "GOS-B3", "GOS-B4")) %>% 
  summarize(GOSA3_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSA4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A3", "GOS-A4", "GOS-A5", "GOS-B3", "GOS-B4", "GOS-B5")) %>% 
  summarize(GOSA4_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSA5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A4", "GOS-A5", "GOS-A6", "GOS-B4", "GOS-B5", "GOS-B6")) %>% 
  summarize(GOSA5_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSA6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A5", "GOS-A6", "GOS-B5", "GOS-B6")) %>% 
  summarize(GOSA6_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSB1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-B1", "GOS-B2", "GOS-C1", "GOS-C2")) %>% 
  summarize(GOSB1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSB2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A1", "GOS-A2", "GOS-A3", "GOS-B1", "GOS-B2", "GOS-B3", "GOS-C1", "GOS-C2", "GOS-C3")) %>% 
  summarize(GOSB2_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSB3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A2", "GOS-A3", "GOS-A4", "GOS-B2", "GOS-B3", "GOS-B4", "GOS-C2", "GOS-C3", "GOS-C4")) %>% 
  summarize(GOSB3_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSB4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A3", "GOS-A4", "GOS-A5", "GOS-B3", "GOS-B4", "GOS-B5", "GOS-C3", "GOS-C4", "GOS-C5")) %>% 
  summarize(GOSB4_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSB5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A4", "GOS-A5", "GOS-A6", "GOS-B4", "GOS-B5", "GOS-B6", "GOS-C4", "GOS-C5", "GOS-C6")) %>% 
  summarize(GOSB5_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSB6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-A5", "GOS-A6", "GOS-B5", "GOS-B6", "GOS-C5", "GOS-C6")) %>% 
  summarize(GOSB6_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSC1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-B1", "GOS-B2", "GOS-C1", "GOS-C2", "GOS-D1", "GOS-D2")) %>% 
  summarize(GOSC1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GOSC2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-B1", "GOS-B2", "GOS-B3", "GOS-C1", "GOS-C2", "GOS-C3", "GOS-D1", "GOS-D2", "GOS-D3")) %>% 
  summarize(GOSC2_buff = mean(NDVI_5m_mean, na.rm=T))

GOSC3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-B2", "GOS-B3", "GOS-B4", "GOS-C2", "GOS-C3", "GOS-C4", "GOS-D2", "GOS-D3", "GOS-D4")) %>% 
  summarize(GOSC3_buff = mean(NDVI_5m_mean, na.rm=T))

GOSC4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-B3", "GOS-B4", "GOS-B5", "GOS-C3", "GOS-C4", "GOS-C5", "GOS-D3", "GOS-D4", "GOS-D5")) %>% 
  summarize(GOSC4_buff = mean(NDVI_5m_mean, na.rm=T))

GOSC5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-B4", "GOS-B5", "GOS-B6", "GOS-C4", "GOS-C5", "GOS-C6", "GOS-D4", "GOS-D5", "GOS-D6")) %>% 
  summarize(GOSC5_buff = mean(NDVI_5m_mean, na.rm=T))

GOSC6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-B5", "GOS-B6", "GOS-C5", "GOS-C6", "GOS-D5", "GOS-D6")) %>% 
  summarize(GOSC6_buff = mean(NDVI_5m_mean, na.rm=T))

GOSD1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-C1", "GOS-C2", "GOS-D1", "GOS-D2", "GOS-E1", "GOS-E2")) %>% 
  summarize(GOSD1_buff = mean(NDVI_5m_mean, na.rm=T))

GOSD2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-C1", "GOS-C2", "GOS-C3", "GOS-D1", "GOS-D2", "GOS-D3", "GOS-E1", "GOS-E2", "GOS-E3")) %>% 
  summarize(GOSD2_buff = mean(NDVI_5m_mean, na.rm=T))

GOSD3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-C2", "GOS-C3", "GOS-C4", "GOS-D2", "GOS-D3", "GOS-D4", "GOS-E2", "GOS-E3", "GOS-E4")) %>% 
  summarize(GOSD3_buff = mean(NDVI_5m_mean, na.rm=T))

GOSD4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-C3", "GOS-C4", "GOS-C5", "GOS-D3", "GOS-D4", "GOS-D5", "GOS-E3", "GOS-E4", "GOS-E5")) %>% 
  summarize(GOSD4_buff = mean(NDVI_5m_mean, na.rm=T))

GOSD5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-C4", "GOS-C5", "GOS-C6", "GOS-D4", "GOS-D5", "GOS-D6", "GOS-E4", "GOS-E5", "GOS-E6")) %>% 
  summarize(GOSD5_buff = mean(NDVI_5m_mean, na.rm=T))

GOSD6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-C5", "GOS-C6", "GOS-D5", "GOS-D6", "GOS-E5", "GOS-E6")) %>% 
  summarize(GOSD6_buff = mean(NDVI_5m_mean, na.rm=T))

GOSE1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-D1", "GOS-D2", "GOS-E1", "GOS-E2", "GOS-F1", "GOS-F2")) %>% 
  summarize(GOSE1_buff = mean(NDVI_5m_mean, na.rm=T))

GOSE2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-D1", "GOS-D2", "GOS-D3", "GOS-E1", "GOS-E2", "GOS-E3", "GOS-F1", "GOS-F2", "GOS-F3")) %>% 
  summarize(GOSE2_buff = mean(NDVI_5m_mean, na.rm=T))

GOSE3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-D2", "GOS-D3", "GOS-D4", "GOS-E2", "GOS-E3", "GOS-E4", "GOS-F2", "GOS-F3", "GOS-F4")) %>% 
  summarize(GOSE3_buff = mean(NDVI_5m_mean, na.rm=T))

GOSE4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-D3", "GOS-D4", "GOS-D5", "GOS-E3", "GOS-E4", "GOS-E5", "GOS-F3", "GOS-F4", "GOS-F5")) %>% 
  summarize(GOSE4_buff = mean(NDVI_5m_mean, na.rm=T))

GOSE5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-D4", "GOS-D5", "GOS-D6", "GOS-E4", "GOS-E5", "GOS-E6", "GOS-F4", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSE5_buff = mean(NDVI_5m_mean, na.rm=T))

GOSE6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-D5", "GOS-D6", "GOS-E5", "GOS-E6", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSE6_buff = mean(NDVI_5m_mean, na.rm=T))

GOSF1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-E1", "GOS-E2", "GOS-F1", "GOS-F2")) %>% 
  summarize(GOSF1_buff = mean(NDVI_5m_mean, na.rm=T))

GOSF2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-E1", "GOS-E2", "GOS-E3", "GOS-F1", "GOS-F2", "GOS-F3")) %>% 
  summarize(GOSF2_buff = mean(NDVI_5m_mean, na.rm=T))

GOSF3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-E2", "GOS-E3", "GOS-E4", "GOS-F2", "GOS-F3", "GOS-F4")) %>% 
  summarize(GOSF3_buff = mean(NDVI_5m_mean, na.rm=T))

GOSF4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-E3", "GOS-E4", "GOS-E5", "GOS-F3", "GOS-F4", "GOS-F5")) %>% 
  summarize(GOSF4_buff = mean(NDVI_5m_mean, na.rm=T))

GOSF5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-E4", "GOS-E5", "GOS-E6", "GOS-F4", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSF5_buff = mean(NDVI_5m_mean, na.rm=T))

GOSF6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GOS-E5", "GOS-E6", "GOS-F5", "GOS-F6")) %>% 
  summarize(GOSF6_buff = mean(NDVI_5m_mean, na.rm=T))





############################################################################################################################################
##
##                            GOOSE 
##
############################################################################################################################################


GSA1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-B1", "GS-B2")) %>% 
  summarize(GSA1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSA2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-A3", "GS-B1", "GS-B2", "GS-B3")) %>% 
  summarize(GSA2_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSA3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A2", "GS-A3", "GS-A4", "GS-B2", "GS-B3", "GS-B4")) %>% 
  summarize(GSA3_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSA4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A3", "GS-A4", "GS-A5", "GS-B3", "GS-B4", "GS-B5")) %>% 
  summarize(GSA4_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSA5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A4", "GS-A5", "GS-A6", "GS-B4", "GS-B5", "GS-B6")) %>% 
  summarize(GSA5_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSA6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A5", "GS-A6", "GS-B5", "GS-B6")) %>% 
  summarize(GSA6_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSB1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-B1", "GS-B2", "GS-C1", "GS-C2")) %>% 
  summarize(GSB1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSB2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A1", "GS-A2", "GS-A3", "GS-B1", "GS-B2", "GS-B3", "GS-C1", "GS-C2", "GS-C3")) %>% 
  summarize(GSB2_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSB3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A2", "GS-A3", "GS-A4", "GS-B2", "GS-B3", "GS-B4", "GS-C2", "GS-C3", "GS-C4")) %>% 
  summarize(GSB3_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSB4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A3", "GS-A4", "GS-A5", "GS-B3", "GS-B4", "GS-B5", "GS-C3", "GS-C4", "GS-C5")) %>% 
  summarize(GSB4_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSB5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A4", "GS-A5", "GS-A6", "GS-B4", "GS-B5", "GS-B6", "GS-C4", "GS-C5", "GS-C6")) %>% 
  summarize(GSB5_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSB6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-A5", "GS-A6", "GS-B5", "GS-B6", "GS-C5", "GS-C6")) %>% 
  summarize(GSB6_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSC1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-B1", "GS-B2", "GS-C1", "GS-C2", "GS-D1", "GS-D2")) %>% 
  summarize(GSC1_buff = mean(NDVI_5m_mean, na.rm=T)) 

GSC2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-B1", "GS-B2", "GS-B3", "GS-C1", "GS-C2", "GS-C3", "GS-D1", "GS-D2", "GS-D3")) %>% 
  summarize(GSC2_buff = mean(NDVI_5m_mean, na.rm=T))

GSC3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-B2", "GS-B3", "GS-B4", "GS-C2", "GS-C3", "GS-C4", "GS-D2", "GS-D3", "GS-D4")) %>% 
  summarize(GSC3_buff = mean(NDVI_5m_mean, na.rm=T))

GSC4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-B3", "GS-B4", "GS-B5", "GS-C3", "GS-C4", "GS-C5", "GS-D3", "GS-D4", "GS-D5")) %>% 
  summarize(GSC4_buff = mean(NDVI_5m_mean, na.rm=T))

GSC5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-B4", "GS-B5", "GS-B6", "GS-C4", "GS-C5", "GS-C6", "GS-D4", "GS-D5", "GS-D6")) %>% 
  summarize(GSC5_buff = mean(NDVI_5m_mean, na.rm=T))

GSC6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-B5", "GS-B6", "GS-C5", "GS-C6", "GS-D5", "GS-D6")) %>% 
  summarize(GSC6_buff = mean(NDVI_5m_mean, na.rm=T))

GSD1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-C1", "GS-C2", "GS-D1", "GS-D2", "GS-E1", "GS-E2")) %>% 
  summarize(GSD1_buff = mean(NDVI_5m_mean, na.rm=T))

GSD2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-C1", "GS-C2", "GS-C3", "GS-D1", "GS-D2", "GS-D3", "GS-E1", "GS-E2", "GS-E3")) %>% 
  summarize(GSD2_buff = mean(NDVI_5m_mean, na.rm=T))

GSD3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-C2", "GS-C3", "GS-C4", "GS-D2", "GS-D3", "GS-D4", "GS-E2", "GS-E3", "GS-E4")) %>% 
  summarize(GSD3_buff = mean(NDVI_5m_mean, na.rm=T))

GSD4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-C3", "GS-C4", "GS-C5", "GS-D3", "GS-D4", "GS-D5", "GS-E3", "GS-E4", "GS-E5")) %>% 
  summarize(GSD4_buff = mean(NDVI_5m_mean, na.rm=T))

GSD5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-C4", "GS-C5", "GS-C6", "GS-D4", "GS-D5", "GS-D6", "GS-E4", "GS-E5", "GS-E6")) %>% 
  summarize(GSD5_buff = mean(NDVI_5m_mean, na.rm=T))

GSD6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-C5", "GS-C6", "GS-D5", "GS-D6", "GS-E5", "GS-E6")) %>% 
  summarize(GSD6_buff = mean(NDVI_5m_mean, na.rm=T))

GSE1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-D1", "GS-D2", "GS-E1", "GS-E2", "GS-F1", "GS-F2")) %>% 
  summarize(GSE1_buff = mean(NDVI_5m_mean, na.rm=T))

GSE2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-D1", "GS-D2", "GS-D3", "GS-E1", "GS-E2", "GS-E3", "GS-F1", "GS-F2", "GS-F3")) %>% 
  summarize(GSE2_buff = mean(NDVI_5m_mean, na.rm=T))

GSE3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-D2", "GS-D3", "GS-D4", "GS-E2", "GS-E3", "GS-E4", "GS-F2", "GS-F3", "GS-F4")) %>% 
  summarize(GSE3_buff = mean(NDVI_5m_mean, na.rm=T))

GSE4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-D3", "GS-D4", "GS-D5", "GS-E3", "GS-E4", "GS-E5", "GS-F3", "GS-F4", "GS-F5")) %>% 
  summarize(GSE4_buff = mean(NDVI_5m_mean, na.rm=T))

GSE5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-D4", "GS-D5", "GS-D6", "GS-E4", "GS-E5", "GS-E6", "GS-F4", "GS-F5", "GS-F6")) %>% 
  summarize(GSE5_buff = mean(NDVI_5m_mean, na.rm=T))

GSE6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-D5", "GS-D6", "GS-E5", "GS-E6", "GS-F5", "GS-F6")) %>% 
  summarize(GSE6_buff = mean(NDVI_5m_mean, na.rm=T))

GSF1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-E1", "GS-E2", "GS-F1", "GS-F2")) %>% 
  summarize(GSF1_buff = mean(NDVI_5m_mean, na.rm=T))

GSF2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-E1", "GS-E2", "GS-E3", "GS-F1", "GS-F2", "GS-F3")) %>% 
  summarize(GSF2_buff = mean(NDVI_5m_mean, na.rm=T))

GSF3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-E2", "GS-E3", "GS-E4", "GS-F2", "GS-F3", "GS-F4")) %>% 
  summarize(GSF3_buff = mean(NDVI_5m_mean, na.rm=T))

GSF4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-E3", "GS-E4", "GS-E5", "GS-F3", "GS-F4", "GS-F5")) %>% 
  summarize(GSF4_buff = mean(NDVI_5m_mean, na.rm=T))

GSF5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-E4", "GS-E5", "GS-E6", "GS-F4", "GS-F5", "GS-F6")) %>% 
  summarize(GSF5_buff = mean(NDVI_5m_mean, na.rm=T))

GSF6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("GS-E5", "GS-E6", "GS-F5", "GS-F6")) %>% 
  summarize(GSF6_buff = mean(NDVI_5m_mean, na.rm=T))







############################################################################################################################################
##
##                            NORTH BEACH 
##
############################################################################################################################################


NBA1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-B1", "NB-B2")) %>% 
  summarize(NBA1_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBA2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-A3", "NB-B1", "NB-B2", "NB-B3")) %>% 
  summarize(NBA2_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBA3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A2", "NB-A3", "NB-A4", "NB-B2", "NB-B3", "NB-B4")) %>% 
  summarize(NBA3_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBA4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A3", "NB-A4", "NB-A5", "NB-B3", "NB-B4", "NB-B5")) %>% 
  summarize(NBA4_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBA5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A4", "NB-A5", "NB-A6", "NB-B4", "NB-B5", "NB-B6")) %>% 
  summarize(NBA5_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBA6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A5", "NB-A6", "NB-B5", "NB-B6")) %>% 
  summarize(NBA6_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBB1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-B1", "NB-B2", "NB-C1", "NB-C2")) %>% 
  summarize(NBB1_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBB2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A1", "NB-A2", "NB-A3", "NB-B1", "NB-B2", "NB-B3", "NB-C1", "NB-C2", "NB-C3")) %>% 
  summarize(NBB2_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBB3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A2", "NB-A3", "NB-A4", "NB-B2", "NB-B3", "NB-B4", "NB-C2", "NB-C3", "NB-C4")) %>% 
  summarize(NBB3_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBB4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A3", "NB-A4", "NB-A5", "NB-B3", "NB-B4", "NB-B5", "NB-C3", "NB-C4", "NB-C5")) %>% 
  summarize(NBB4_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBB5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A4", "NB-A5", "NB-A6", "NB-B4", "NB-B5", "NB-B6", "NB-C4", "NB-C5", "NB-C6")) %>% 
  summarize(NBB5_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBB6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-A5", "NB-A6", "NB-B5", "NB-B6", "NB-C5", "NB-C6")) %>% 
  summarize(NBB6_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBC1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-B1", "NB-B2", "NB-C1", "NB-C2", "NB-D1", "NB-D2")) %>% 
  summarize(NBC1_buff = mean(NDVI_5m_mean, na.rm=T)) 

NBC2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-B1", "NB-B2", "NB-B3", "NB-C1", "NB-C2", "NB-C3", "NB-D1", "NB-D2", "NB-D3")) %>% 
  summarize(NBC2_buff = mean(NDVI_5m_mean, na.rm=T))

NBC3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-B2", "NB-B3", "NB-B4", "NB-C2", "NB-C3", "NB-C4", "NB-D2", "NB-D3", "NB-D4")) %>% 
  summarize(NBC3_buff = mean(NDVI_5m_mean, na.rm=T))

NBC4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-B3", "NB-B4", "NB-B5", "NB-C3", "NB-C4", "NB-C5", "NB-D3", "NB-D4", "NB-D5")) %>% 
  summarize(NBC4_buff = mean(NDVI_5m_mean, na.rm=T))

NBC5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-B4", "NB-B5", "NB-B6", "NB-C4", "NB-C5", "NB-C6", "NB-D4", "NB-D5", "NB-D6")) %>% 
  summarize(NBC5_buff = mean(NDVI_5m_mean, na.rm=T))

NBC6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-B5", "NB-B6", "NB-C5", "NB-C6", "NB-D5", "NB-D6")) %>% 
  summarize(NBC6_buff = mean(NDVI_5m_mean, na.rm=T))

NBD1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-C1", "NB-C2", "NB-D1", "NB-D2", "NB-E1", "NB-E2")) %>% 
  summarize(NBD1_buff = mean(NDVI_5m_mean, na.rm=T))

NBD2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-C1", "NB-C2", "NB-C3", "NB-D1", "NB-D2", "NB-D3", "NB-E1", "NB-E2", "NB-E3")) %>% 
  summarize(NBD2_buff = mean(NDVI_5m_mean, na.rm=T))

NBD3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-C2", "NB-C3", "NB-C4", "NB-D2", "NB-D3", "NB-D4", "NB-E2", "NB-E3", "NB-E4")) %>% 
  summarize(NBD3_buff = mean(NDVI_5m_mean, na.rm=T))

NBD4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-C3", "NB-C4", "NB-C5", "NB-D3", "NB-D4", "NB-D5", "NB-E3", "NB-E4", "NB-E5")) %>% 
  summarize(NBD4_buff = mean(NDVI_5m_mean, na.rm=T))

NBD5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-C4", "NB-C5", "NB-C6", "NB-D4", "NB-D5", "NB-D6", "NB-E4", "NB-E5", "NB-E6")) %>% 
  summarize(NBD5_buff = mean(NDVI_5m_mean, na.rm=T))

NBD6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-C5", "NB-C6", "NB-D5", "NB-D6", "NB-E5", "NB-E6")) %>% 
  summarize(NBD6_buff = mean(NDVI_5m_mean, na.rm=T))

NBE1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-D1", "NB-D2", "NB-E1", "NB-E2", "NB-F1", "NB-F2")) %>% 
  summarize(NBE1_buff = mean(NDVI_5m_mean, na.rm=T))

NBE2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-D1", "NB-D2", "NB-D3", "NB-E1", "NB-E2", "NB-E3", "NB-F1", "NB-F2", "NB-F3")) %>% 
  summarize(NBE2_buff = mean(NDVI_5m_mean, na.rm=T))

NBE3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-D2", "NB-D3", "NB-D4", "NB-E2", "NB-E3", "NB-E4", "NB-F2", "NB-F3", "NB-F4")) %>% 
  summarize(NBE3_buff = mean(NDVI_5m_mean, na.rm=T))

NBE4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-D3", "NB-D4", "NB-D5", "NB-E3", "NB-E4", "NB-E5", "NB-F3", "NB-F4", "NB-F5")) %>% 
  summarize(NBE4_buff = mean(NDVI_5m_mean, na.rm=T))

NBE5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-D4", "NB-D5", "NB-D6", "NB-E4", "NB-E5", "NB-E6", "NB-F4", "NB-F5", "NB-F6")) %>% 
  summarize(NBE5_buff = mean(NDVI_5m_mean, na.rm=T))

NBE6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-D5", "NB-D6", "NB-E5", "NB-E6", "NB-F5", "NB-F6")) %>% 
  summarize(NBE6_buff = mean(NDVI_5m_mean, na.rm=T))

NBF1 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-E1", "NB-E2", "NB-F1", "NB-F2")) %>% 
  summarize(NBF1_buff = mean(NDVI_5m_mean, na.rm=T))

NBF2 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-E1", "NB-E2", "NB-E3", "NB-F1", "NB-F2", "NB-F3")) %>% 
  summarize(NBF2_buff = mean(NDVI_5m_mean, na.rm=T))

NBF3 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-E2", "NB-E3", "NB-E4", "NB-F2", "NB-F3", "NB-F4")) %>% 
  summarize(NBF3_buff = mean(NDVI_5m_mean, na.rm=T))

NBF4 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-E3", "NB-E4", "NB-E5", "NB-F3", "NB-F4", "NB-F5")) %>% 
  summarize(NBF4_buff = mean(NDVI_5m_mean, na.rm=T))

NBF5 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-E4", "NB-E5", "NB-E6", "NB-F4", "NB-F5", "NB-F6")) %>% 
  summarize(NBF5_buff = mean(NDVI_5m_mean, na.rm=T))

NBF6 <- NDVI_raw %>% 
  select(name, site, NDVI_5m_mean) %>%
  filter(name %in% c("NB-E5", "NB-E6", "NB-F5", "NB-F6")) %>% 
  summarize(NBF6_buff = mean(NDVI_5m_mean, na.rm=T))






### BIND ALL VALUES INTO ONE DATAFRAME

NDVI_buffadj <- bind_cols(GFA1, GFA2, GFA3, GFA4, GFA5, GFA6, GFA7, GFA8, GFA9,
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
write.csv(NDVI_buffadj, "NDVI_buffadj.csv")       # unfortunately is in column-based format so need to transpose for ease of use

# transpose dataframe
NDVI_buffadj <- read.csv("NDVI_buffadj.csv")
transposed_NDVI_buffadj <- t(NDVI_buffadj)
write.csv(transposed_NDVI_buffadj, "NDVI_buffadj.csv")    # re-write old untransposed file into row-based format







#################################################################################################################################################################

# Calculate SITE level NDVI 

NDVI_raw <- read.csv("NDVI_traps.csv")

# Recall: 5m buffer placed around each trap to assign a trap-level NDVI score 

# STEP 1: AVERAGE 
  # Average trap-level NDVI scores at each site 
NDVI_site <- NDVI_raw %>% 
  group_by(site) %>% 
  summarize(site_mean = mean(NDVI_5m_mean), se = sd(NDVI_5m_mean)/sqrt(length(NDVI_5m_mean))) %>% 
  filter(site != "IP") %>%
  print(NDVI_site)

# STEP 2: STATS (remove IP though as this is for the GLMM results)
  NDVI_glmm <- NDVI_raw %>% 
    select(site, NDVI_5m_mean) %>% 
    filter(site != "IP") 

lm1 <- lm(NDVI_glmm$NDVI_5m_mean ~ NDVI_glmm$site)
r1 <- resid(lm1)
plot(r1)
plot(lm1)
qqnorm(r1)
qqline(r1)
hist(r1)

kruskal.test(NDVI_glmm$NDVI_5m_mean ~ NDVI_glmm$site)
dunnTest(NDVI_glmm$NDVI_5m_mean ~ NDVI_glmm$site, method="bh")


# plot NDVI ~ dist
plot(NDVI_raw$NDVI_5m_mean ~ NDVI_raw$dist)



#################################################################################################################################################################

# Quick test for seasonality effects of NDVI 

NDVI_raw <- read.csv("NDVI_traps.csv")


# STEP 1: SEASON COLUMN 
  library(dplyr)
  library(tidyr)
  library(FSA)
  
  # remove IP, create dates   
  NDVI_raw <- NDVI_raw %>%
    filter(site != "IP") %>%
    mutate(date = case_when(.$site == "GF" ~ "04-06-2015", 
                            .$site == "GOS" ~ "14-08-2014", 
                            .$site == "NB" ~ "04-08-2014",
                            .$site == "GS" ~ "14-08-2014",
                              TRUE ~ "foreign"))

 # separate 
  NDVI_raw$date <- as.Date(NDVI_raw$date,"%d-%m-%Y")
  
  NDVI_raw$day <- as.numeric(format(NDVI_raw$date, format = "%d"))
  NDVI_raw$month <- as.numeric(format(NDVI_raw$date, format = "%m"))
  NDVI_raw$year <- as.numeric(format(NDVI_raw$date, format = "%Y"))
  
  NDVI_raw$month_year <- paste(NDVI_raw$month, NDVI_raw$year, sep = "-")
  
  
# STEP 2: ASSUMPTIONS  
  date.lm <- lm(NDVI_raw$NDVI_5m_mean ~ NDVI_raw$year)
  r.lm <- resid(date.lm)

  qqnorm(r.lm)
  qqline(r.lm)
  hist(r.lm)
  plot(r.lm)
  plot(date.lm)
  
  # non-normal, use non-parametric
  NDVI_raw$date <- as.factor(NDVI_raw$date)
  kruskal.test(NDVI_raw$NDVI_5m_mean ~ NDVI_raw$date)
  dunnTest(NDVI_raw$NDVI_5m_mean ~ NDVI_raw$date, method="bh")
  
  
  kruskal.test(NDVI_raw$NDVI_5m_mean ~ NDVI_raw$year)
  kruskal.test(NDVI_raw$NDVI_5m_mean ~ NDVI_raw$month)
  kruskal.test(NDVI_raw$NDVI_5m_mean ~ NDVI_raw$day)



## 































