# regional test between reduced food groups 

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

# create separate data frames based on regions 
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)

iso_db <- read.csv("invert_veg_iso_mouse_6nov17.csv")



# The following code was taken from the "mixsiar_new_6nov17" file 



#####
# SIMPLIFIED SOURCES - CJZ rejection said should try grouping herbs+carns to basically just have 3 sources: veg, terrestrial inverts, intertidal inverts.
# here are the simplified sources, done on Nov 2, 2019
#####

# create new column for easy simplification 
iso_db <- iso_db %>% 
  mutate(simple_source = paste(paste(gsub("-", "-", taxa)), location, sep="-")) 


##############
# VEGETATION #
##############
veg <- iso_db %>% 
  filter(simple_source =="plant-forest", ID %in% c("salal", "huckleberry")) %>%
  print()

        # n = 19 from CV, and n = 81 from GS

#####
# Test for normality
#####

# create linear model to extract residuals - C and N
lm.v.c <- lm(veg$d13C ~ veg$region)
summary(lm.v.c)

lm.v.n <- lm(veg$d15N ~ veg$region)
summary(lm.v.n)

# extract an analyze residuals - C and N
res.v.c <- resid(lm.v.c)
res.v.n <- resid(lm.v.n)

# qqplot - C and N 
qqnorm(res.v.c)
qqline(res.v.c, col=2)

qqnorm(res.v.n)
qqline(res.v.n, col=2)

# histogram - C and N 
hist(res.v.c)             # kinda normal
hist(res.v.n)             # kinda normal 

# Boxplot 
plot(res.v.c)             # bit of heteroscedasticity 
plot(res.v.n)             # bit of heteroscedasticity 

# F-test to test if variances are unequal
var.test(veg$d13C ~ veg$region)         
## p = 0.09
## H0: ratio of variances is equal - reject null hypothesis therefore unequal variance  
var.test(veg$d15N ~ veg$region)
## p < 0.05 therefore do not reject null and equal var 


# can't do any transformations due to negative values, so will stick with non-parametric 


# wilcox test for forest invert carnivores  
wilcox.test(veg$d13C ~ veg$region)
# p = 0.125

wilcox.test(veg$d15N ~ veg$region)
# p = 0.0003




##################
# FOREST INVERTS #
##################
fi <- iso_db %>% 
  filter(simple_source =="invert-forest", region != "all") %>%
  print()

        # n = 76 from CV, and n = 68 from GS

#####
# Test for normality
#####

# create linear model to extract residuals - C and N
lm.fi.c <- lm(fi$d13C ~ fi$region)
summary(lm.fi.c)

lm.fi.n <- lm(fi$d15N ~ fi$region)
summary(lm.fi.n)

# extract an analyze residuals - C and N
res.fi.c <- resid(lm.fi.c)
res.fi.n <- resid(lm.fi.n)

# qqplot - C and N 
qqnorm(res.fi.c)
qqline(res.fi.c, col=2)

qqnorm(res.fi.n)
qqline(res.fi.n, col=2)

# histogram - C and N 
hist(res.fi.c)             # kinda normal
hist(res.fi.n)             # kinda normal 

# Boxplot 
plot(res.fi.c)              
plot(res.fi.n)             # bit of heteroscedasticity 

# F-test to test if variances are unequal
var.test(fi$d13C ~ fi$region)         
## p = 0.02 therefore do not reject null and equal var
## H0: ratio of variances is equal  
var.test(fi$d15N ~ fi$region)
## p = 0.58 therefore do not reject null and equal var 


# can't do any transformations due to negative values, so will stick with non-parametric 


# wilcox test for forest invert carnivores  
wilcox.test(fi$d13C ~ fi$region)
# p = 0.01

wilcox.test(fi$d15N ~ fi$region)
# p < 0.001




#################
# BEACH INVERTS #
#################
ii <- iso_db %>% 
  filter(simple_source =="invert-intertidal") %>%
  print()

        # n = 47 from CV, and n = 54 from GS

#####
# Test for normality
#####

# create linear model to extract residuals - C and N
lm.ii.c <- lm(ii$d13C ~ ii$region)
summary(lm.ii.c)

lm.ii.n <- lm(ii$d15N ~ ii$region)
summary(lm.ii.n)

# extract an analyze residuals - C and N
res.ii.c <- resid(lm.ii.c)
res.ii.n <- resid(lm.ii.n)

# qqplot - C and N 
qqnorm(res.ii.c)
qqline(res.ii.c, col=2)

qqnorm(res.ii.n)
qqline(res.ii.n, col=2)

# histogram - C and N 
hist(res.ii.c)             # not normal
hist(res.ii.n)             # not normal 

# Boxplot 
plot(res.ii.c)             # bit of heteroscedasticity  
plot(res.ii.n)             # bit of heteroscedasticity 

# F-test to test if variances are unequal
var.test(ii$d13C ~ ii$region)         
## p = 0.003 
## H0: ratio of variances is equal- reject H0 variance ratio is not equal
var.test(ii$d15N ~ ii$region)
## p = 0.79 therefore do not reject null and equal var 


# can't do any transformations due to negative values, so will stick with non-parametric 


# wilcox test for forest invert carnivores  
wilcox.test(ii$d13C ~ ii$region)
# p < 0.001

wilcox.test(ii$d15N ~ ii$region)
# p = 0.13












