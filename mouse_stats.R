
# Basic mouse ecology comparisons - some of this used in MS and some not. 
# slightly revised Aug 2020

########################################################################################################################################################

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

ltdat <- read.csv("Livetraps_subset.csv")

library(tidyverse)
library(broom)
library(dunn.test)

########################################################################################################################################################

#                                                          DATA CLEANING

mice <- ltdat %>% 
  filter(Spp != "SORE", Age == "A") %>%
  mutate(dist_group = ifelse(Dist_Shore==0 | Dist_Shore==25, "0-25",
                             ifelse(Dist_Shore==50 | Dist_Shore==75, "50-75",
                                    ifelse(Dist_Shore==100 | Dist_Shore==125, "100-125", "150-200")))) %>%
  print()
  
########################################################################################################################################################

#                                                          BODY MASS COMPARISONS

# Change in body mass moving inland for males and females 


#---------------- Males 
mice.m <- mice %>% 
  filter(Sex=="M") %>% 
  #summarize(n=n()) %>%   # n = 35
  print()

lm.bm <- lm(log10(mice.m$Final_wgt) ~ mice.m$dist_group)
r.bm <- resid(lm.bm)
hist(r.bm)
plot(r.bm)
qqnorm(r.bm)
qqline(r.bm)
plot(lm.bm)
# non-normal even with log10 and log transformation, will use non-parametric

kruskal.test(mice.m$Final_wgt ~ mice.m$dist_group)



#---------------- Females 
mice.f <- mice %>% 
  filter(Sex=="F") %>% 
  #summarize(n=n()) %>%   n=19
  print()

lm.bf <- lm(mice.f$Final_wgt ~ mice.f$dist_group)
r.bf <- resid(lm.bf)
hist(r.bf)
plot(r.bf)
qqnorm(r.bf)
qqline(r.bf)
plot(lm.bf)
# non-normal even with log10 and log transformation, will use non-parametric

kruskal.test(mice.f$Final_wgt ~ mice.f$dist_group)


########################################################################################################################################################

# OLD STUFF BELOW






## just going to do proportions for now - averaged across sites within regions 

############################################ gender proportions
gender <- ltdat %>%
  group_by(Region,Site,Sex) %>% 
  filter(Spp != "SORE") %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  print(gender)

genreg <- gender %>%
  group_by(Region,Sex) %>%
  filter(Sex =="F") %>%
  summarize(mean = mean(freq), se = (sd(freq)/sqrt(length(freq)))) %>%
    print(genreg)

# create new df without shrews for chisq
newgend <- ltdat %>% 
  group_by(Region, Site, Sex) %>%
  filter(Spp != "SORE") %>% 
  print(newgend)

write.csv(newgend, "newgend.csv")
newgend <- read.csv("newgend.csv")

chisq.test(table(newgend$Sex, newgend$Region))
fisher.test(table(newgend$Sex, newgend$Region))


########################################### age proportions
age <- ltdat %>%
  group_by(Region,Site,Age) %>% 
  filter(Spp != "SORE") %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  print(age)

agereg <- age %>%
  group_by(Region,Age) %>%
  filter(Age =="A") %>%
  summarize(mean = mean(freq), se = (sd(freq)/sqrt(length(freq)))) %>%
  print(agereg)


# breeding status proportions 
breed <- ltdat %>%
  group_by(Region,Site,bs_cat) %>% 
  filter(Spp != "SORE") %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  print(breed)

breedreg <- breed %>%
  group_by(Region,bs_cat) %>%
  filter(bs_cat != "nb") %>%
  summarize(mean = mean(freq), se = (sd(freq)/sqrt(length(freq)))) %>%
  print(breedreg)



######################################################## body mass (OLD)
# use bcsub subset of just non-reproductive adults (previously subsetted in "bodycondition" script)
bcsub <- read.csv("bcsub.csv")
# by site 
mass <- bcsub %>% 
  group_by(region, site) %>% 
  summarize(mean(Final_wgt), se = (sd(Final_wgt)/sqrt(length(Final_wgt)))) %>% 
  print(mass)

#by region
mass <- bcsub %>% 
  group_by(region) %>% 
  summarize(mean(Final_wgt), se = (sd(Final_wgt)/sqrt(length(Final_wgt)))) %>% 
  print(mass)

#### anovaaaaa
# create new DF with no shrews 
mass2 <- ltdat %>% 
  group_by(Region, Site, Final_wgt) %>% 
  filter(Spp != "SORE", ID != "IP-B1") %>% 
  print(mass)

# test for normality by REGION
wgtmod <- lm(mass2$Final_wgt ~ mass2$Region)
wgtresid <- residuals(wgtmod)
hist(wgtresid)
shapiro.test(wgtresid)   # can't reject null thr NORMAL

# test for var by REGION
bartlett.test(mass2$Final_wgt ~ mass2$Region)   # can barely just reject null 
plot(wgtmod)

# aov test by REGION since normal and equal var 
massaov <- aov(mass2$Final_wgt ~ mass2$Region)    # p = 0.06
summary(massaov)

#wilcox by REGION just in case 
wilcox.test(mass2$Final_wgt ~ mass2$Region)      # p = 0.11






# test for normality by SITE 
wgtmod2 <- lm(mass2$Final_wgt ~ mass2$Site)
wgtresid2 <- residuals(wgtmod2)
hist(wgtresid2)
shapiro.test(wgtresid2)   # can't reject null thr NORMAL

# test for var by SITE
bartlett.test(mass2$Final_wgt ~ mass2$Site)   # can't reject null thr equal var
plot(wgtmod2)

# aov test by SITE 
siteaov <- aov(mass2$Final_wgt ~ mass2$Site)    #  p = 0.044
summary(siteaov)
TukeyHSD(siteaov)

# wilcox by SITE 
wilcox.test(mass2$Final_wgt ~ mass2$Site)

