setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

## anova to figure out if sig differences between regions for inverts and plants 
iso.dat <- read.csv("invert_veg_mouse_iso.csv")

library(dplyr)

########################################################### forest inverts CARNIVORES first by REGION

# pull out forest invert carns 
fic <- iso.dat %>%
  group_by(taxa,guild) %>%
  filter(taxa=="invert", region != "all", location == "forest", ID != "mouse", guild =="carnivore") %>%
  print(fic)

# create linear model to extract residuals - C and N
lm_fic_regC <- lm(fic$d13C ~ fic$region)
summary(lm_fic_regC)

lm_fic_regN <- lm(fic$d15N ~ fic$region)
summary(lm_fic_regN)

# extract an analyze residuals - C and N
res_fic_regC <- resid(lm_fic_regC)
res_fic_regN <- resid(lm_fic_regN)

# qqplot - C and N 
qqnorm(res_fic_regC)
qqline(res_fic_regC, col=2)

qqnorm(res_fic_regN)
qqline(res_fic_regN, col=2)

# histogram - C and N 
hist(res_fic_regC)   # pretty damn close to normal
hist(res_fic_regN)   # pretty damn close to normal 

# Boxplot - maybe tiny bit of heteroscedasticity 
plot(res_fic_regC)
plot(res_fic_regN)

# F-test to test if variances are unequal
var.test(fic$d13C ~ fic$region)         
## p << 0.001
## H0: ratio of variances is equal - reject null hypothesis therefore unequal variance  
var.test(fic$d15N ~ fic$region)
## p < 0.001 therefore do not reject null and equal var 


# can't do any transformations due to negative values, so will stick with non-parametric 


# wilcox test for forest invert carnivores  
wilcox.test(fic$d13C ~ fic$region)
# p = 8.337 e-05

wilcox.test(fic$d15N ~ fic$region)
# p = 0.004 



############################################################################ forest invertebrate herbivores


# pull out forest invert herbs/detri 
fih <- iso.dat %>%
  group_by(taxa,guild) %>%
  filter(taxa=="invert", region != "all", location == "forest", ID != "mouse", guild2 =="TI herbivore/detritivore") %>%
  print(fic)

# create linear model to extract residuals - C and N
lm_fih_regC <- lm(fih$d13C ~ fih$region)
summary(lm_fih_regC)

lm_fih_regN <- lm(fih$d15N ~ fih$region)
summary(lm_fih_regN)

# extract an analyze residuals - C and N
res_fih_regC <- resid(lm_fih_regC)
res_fih_regN <- resid(lm_fih_regN)

# qqplot - C and N 
qqnorm(res_fih_regC)
qqline(res_fih_regC, col=2)

qqnorm(res_fih_regN)
qqline(res_fih_regN, col=2)

# histogram - C and N 
hist(res_fih_regC)   # pretty damn close to normal
hist(res_fih_regN)   # pretty damn close to normal 

# Boxplot - lots of heteroscedasticity 
plot(res_fih_regC)
plot(res_fih_regN)

# F-test to test if variances are unequal
var.test(fih$d13C ~ fih$region)         
## p = 0.04
## H0: ratio of variances is equal - reject null hypothesis therefore unequal variance  
var.test(fih$d15N ~ fih$region)
## p = 0.16 therefore do not reject null and equal var 


# can't do any transformations due to negative values, so will stick with non-parametric 


# wilcox test for forest invert carnivores  
wilcox.test(fih$d13C ~ fih$region)
# p = 0.07

wilcox.test(fih$d15N ~ fih$region)
# p = 0.08


# what was the mean C and N signatures? 
fih_means <- iso.dat %>%
  group_by(region,guild2) %>%
  filter(taxa=="invert", region != "all", location == "forest", ID != "mouse", guild2 =="TI herbivore/detritivore") %>%
  summarize(meanC = mean(d13C), meanN = mean(d15N)) %>% 
  print(fih_means)



count(fih$guild2=="TI herbivore/detritivore", region=="GS")

############################################################### now plants ###############################################################

# pull out plants for anova 
plant <- iso.dat %>%
  group_by(taxa) %>%
  filter(taxa=="plant", region != "all", ID != "mushroom", ID != "lichen/fungus", ID != "grass", ID != "sedge", species != "myanthemum",
         species != "crab apple", species != "crowberry", species != "bunchberry") %>%
  print(plant)

write.csv(plant, "plant.csv")

# create lm to extract residuals 
plant <- read.csv("plant.csv")
plantC_lm <- lm(plant$d13c ~ plant$region)
plantN_lm <- lm(plant$d15N ~ plant$region)

# extract residuals 
plant_C_resid <- resid(plantC_lm)
plant_N_resid <- resid(plantN_lm)

# qqplot 
qqnorm(plant_C_resid)
qqline(plant_C_resid, col=2)

qqnorm(plant_N_resid)
qqline(plant_N_resid, col=2)
# def not normally distributed 

# histogram 
hist(plant_C_resid)   # pretty damn close 
hist(plant_N_resid)   # pretty damn close 

# Boxplot - shows similar problem
plot(plant_C_resid)
plot(plant_N_resid)


## super non-normal, heteroscedastic  


# wilcox test for plants 
wilcox.test(plant$d13C ~ plant$region)
# p = 0.009 therefore sig difference
wilcox.test(plant$d15N ~ plant$region)
# p = 6.1e-05 therefore SIG difference 


count(plant, region=="GS")



################################################################ beach inverts #########################################################


######################################## pull out beach invert carnivores for anova 
bic <- iso.dat %>%
  group_by(taxa) %>%
  filter(taxa=="invert", region != "all", location == "intertidal", guild =="carnivore") %>%
  print(bic)

bic_mean <- bic %>% 
  group_by(region) %>%
  summarize(meanN = mean(d15N), meanC = mean(d13C)) %>% 
  print(bic_mean)

write.csv(bic, "bic.csv")

# make lm
bic_lm_C <- lm(bic$d13C ~ bic$region)
bic_lm_N <- lm(bic$d15N ~ bic$region)

#extract residuals 
resid_bic_lm_C <- resid(bic_lm_C)
resid_bic_lm_N <- resid(bic_lm_N)

# qqplot 
qqnorm(resid_bic_lm_C)
qqline(resid_bic_lm_C, col=2)

qqnorm(resid_bic_lm_N)
qqline(resid_bic_lm_N, col=2)
# def not normally distributed 

# histogram 
hist(resid_bic_lm_C)   
hist(resid_bic_lm_N)    

# plotty plot plot 
plot(resid_bic_lm_C)
plot(resid_bic_lm_N)


# wilcox
wilcox.test(bic$d13C ~ bic$region)
wilcox.test(bic$d15N ~ bic$region)


count(bic, region=="GS")




######################################## pull out beach invert carnivores for anova 
hic <- iso.dat %>%
  group_by(taxa) %>%
  filter(taxa=="invert", region != "all", location == "intertidal", guild =="herbivore") %>%
  print(hic)

write.csv(hic, "hic.csv")

# make lm
hic_lm_C <- lm(hic$d13C ~ hic$region)
hic_lm_N <- lm(hic$d15N ~ hic$region)

# extract residuals 
resid_hic_lm_C <- resid(hic_lm_C)
resid_hic_lm_N <- resid(hic_lm_N)

# qqplot 
qqnorm(resid_hic_lm_C)
qqline(resid_hic_lm_N, col=2)

qqnorm(resid_hic_lm_C)
qqline(resid_hic_lm_N, col=2)
# def not normally distributed 

# histogram 
hist(resid_hic_lm_C)   
hist(resid_hic_lm_N)    

# plotty plot plot 
plot(resid_hic_lm_C)
plot(resid_hic_lm_N)

# wilcox 
wilcox.test(hic$d13C ~ hic$region)
wilcox.test(hic$d15N ~ hic$region)


count(hic, region=="GS")




############################################################################## summary plot ##########################################
iso.dat <- read.csv("invert_veg_mouse_iso.csv")

#create subset database for values with NO fractionation
summary_food_nofrac <- iso.dat %>%
  group_by(region,taxa,location,guild2) %>% 
  filter(region != "all", ID != "mushroom", ID != "lichen/fungus", ID != "sedge", ID != "grass", species != "myanthemum",
         species != "crab apple", species != "crowberry", species != "bunchberry", species != "mouse") %>%
  summarize(mean(d13C), mean(d15N), seC=(sd(d13C)/sqrt(length(d13C))), seN=(sd(d15N)/sqrt(length(d15N)))) %>% 
  print(summary_food)

write.csv(summary_food_nofrac, "summary_food_nofrac.csv")


# create subset database for values WITH factionation
summary_food <- iso.dat %>%
  group_by(region,taxa,location,guild2) %>% 
  filter(region != "all", ID != "mushroom", ID != "lichen/fungus", ID != "sedge", ID != "grass", ID != "grass", species != "myanthemum",
         species != "crab apple", species != "crowberry", species != "bunchberry", species != "mouse") %>%
  summarize(mean(d13Cfrac), mean(d15Nfrac), seC=(sd(d13Cfrac)/sqrt(length(d13Cfrac))), seN=(sd(d15Nfrac)/sqrt(length(d15Nfrac)))) %>% 
  print(summary_food)

write.csv(summary_food, "summary_food.csv")

## plot means +- SE 
library(ggplot2)
summary_food_frac <- read.csv("summary_food.csv")
summary_food_nofrac <- read.csv("summary_food_nofrac.csv")
# new column for combined location/taxa
summary_food$combo <- c("forest invert", "beach invert", "forest plant", "forest invert", "beach invert", "forest plant")
summary_food$combo_guild <- c("carnivore", "herbivore", "herbivore", "carnivore", "herbivore","veg","carnivore", "herbivore", "herbivore", "carnivore", "herbivore","veg")

scaleFUN <- function(x) sprintf("%.1f", x)


ggplot(summary_food_nofrac, aes(x=mean.d13C., y=mean.d15N.)) +
  xlim(-35,-10) +
  ylim(-12,17) +
  labs(x = "",
       y = "") +
 # geom_point(data=subset(summary_food_frac, region=="GS"), aes(colour=location), fill="transparent",size=15,stroke=2) +
  geom_point(aes(colour=location, fill=location),size=9,stroke=2) +
 # scale_shape_manual(values=c(24, 21)) +
  scale_color_manual(breaks = c("forest", "intertidal"), values=c("#19c740","#333ccc")) +
  scale_fill_manual(breaks = c("forest", "intertidal","consumer"), values=c("black","#19c740","#333ccc")) +
  geom_errorbar(data = summary_food_nofrac, aes(ymin = mean.d15N.-seN,
                                         ymax = mean.d15N.+seN),width=.2,size=1) + 
  geom_errorbarh(data = summary_food_nofrac, aes(xmin = mean.d13C.-seC,
                                          xmax = mean.d13C.+seC),height=.3,size=1) +
  scale_y_continuous(labels=scaleFUN) + 
  scale_x_continuous(labels=scaleFUN) + 
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  # theme(panel.grid.minor = element_line(colour = "transparent")) + 
  # theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = " black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  ylab(expression(paste(delta^{15},'N (\211)'))) 

  

### mouse 

mouse <- iso.dat %>%
  group_by(region) %>%
  filter(species=="mouse") %>%
  summarize(meanN = mean(d15N), meanC = mean(d13C), n = n()) %>%
  print(mouse)

count(mouse, region=="GS")














