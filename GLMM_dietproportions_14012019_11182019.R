# This GLMM code was taken from 'GLMM_dietproportions_14042019.R' and only changed for the amended analysis in light of reviewers from CJZ. 
# The response variable was made from reduced prey groupings (plant, terr invert, int invert), so the proportions haven't changed a ton, but a lil bit! 

  
                                ## MAY ALSO WANT TO CONSIDER REDUCING MODEL SCOPE TO TEST SPECIFIC HYPOTHESES! ##


#install.packages("glmmADMB", 
#                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
#                         getOption("repos")),
#                 type="source")

library(dplyr)
library(devtools)
library(glmmADMB)
library(lme4)
library(AICcmodavg)
library(bbmle)
library(MuMIn)


setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")


#### STEP 1: CREATE RESPONSE VARIABLE (% intertidal invertebrates)

    # This step is not required just a placeholder from residual script. % int invert in diet was manually transferred from MixSIAR output to GLMM database




#### STEP 2: ESTABLISH PREDICTOR VARIABLES (site-level)

# Terrestrial arthropod biomass (site): see "TAB_adjcalcs" R script 

# Terrestrial arthropod biomass (trap): see "TAB_adjcalcs" R script

# NDVI (site): see "NDVI_adjcalcs" R script

# NDVI (trap): see "NDVI_adjcalcs" R script

# Intertidal arthropod biomass (site)
beach_biom <- read.csv("beach_invert_BIOMASS.csv")
# summarize all the different species biomasses per trap to get total biomass per trap 
trapsum <- beach_biom %>% 
  group_by(Region, Trap_no, Site) %>% 
  summarize(sum = sum(Biomass)) %>%        # note: just using "biomass" here as it doesn't need to be adjusted for effort (all traps out for 1 night)
  print(trapsum)
# calculate average biomass per trap at each site
avg_trap <- trapsum %>% 
  group_by(Site) %>% 
  summarize(mean = mean(sum), se = sd(sum)/(sqrt(length(sum)))) %>%
  print(avg_trap)






#### STEP 3: TEST COLLINEARITY OF PREDICTORS

data = read.csv("GLMM_database_corrected_23012019.csv")

z <- cbind(data$PRNINT, data$NDVI_site_rs, data$NDVI_trap_rs, data$ti_biom_site_rs, data$ti_biom_trap_rs, data$ii_biom_rs, data$gn, data$bs)
colnames(z) <- c("proportion","NDVI site", "NDVI trap", "TI Biom site", "TI Biom trap", "II", "gender", "rep stat")
cor(z)
pairs(z)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = cor(x, y,use="na.or.complete")
  txt = format(c(r, 0.123456789), digits=digits)[1]
  txt = paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor = 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)#cex.cor * r)
}
pairs(z,
      upper.panel = panel.cor,
      cex=1,
      pch=16)


# LM with all predictors
library(glmmADMB)
full.mm <- glmmadmb(PRNINT_simp ~ NDVI_site_rs + NDVI_trap_rs + ti_biom_site_rs + ti_biom_trap_rs + ii_biom_rs + gn*bs + (1|site), family="beta", link="logit", data=data)
summary(full.mm)
resid(full.mm)
hist(resid(full.mm))
plot(resid(full.mm))
qqnorm(resid(full.mm))
qqline(resid(full.mm))

library(car)
vif(full.mm)




                                                                      # editing got to here



#### STEP 4: MODELS 

# Based on correlation analysis, can't have the following variables together: 
# NDVI-site - TAB-site (-0.73)
# NDVI-trap - TAB-trap (-0.70)
# TAB-site - TAB-trap (0.71)
# TAB-site - BAB (-0.64)

#################################################### ADD 
# I want to run: 

#1. location ~ 1  (null)
#2: location ~ NDVI_t
#3. location ~ NDVI_s
#4. location ~ TI_t   
#5. location ~ TI_s   
#6. location ~ II
#7. location ~ gender
#8. location ~ breed
#9. location ~ breed*gender

#10. ndvi trap + ti site
#11. ndvi trap + ii
#12. ndvi trap + gender
#13. ndvi trap + breeding
#14. ndvi trap + gender*breeding

#15. ndvi trap + ti site + gender
#16. ndvi trap + ti site + breeding
#17. ndvi trap + ti site + gender*breeding 
#18. ndvi trap + ti site + gender + breeding

#19. ndvi trap + ii + gender
#20. ndvi trap + ii + breeding
#21. ndvi trap + ii + gender*breeding
#22. ndvi trap + ii + gender + breeding

#23. ndvi trap + gender + breeding

#24. ndvi site + ii
#25. ndvi site + gender
#26. ndvi site + breeding
#27. ndvi site + gender*breeding
#28. ndvi site + gender + breeding

#29. ndvi site + ii + gender 
#30. ndvi site + ii + breeding
#31. ndvi site + ii + gender*breeding
#32. ndvi site + ii + gender + breeding

#33. ti trap + gender
#34. ti trap + breeding
#35. ti trap + gender*breeding
#36. ti trap + gender + breeding

#37. ti trap + ii
#38. ti trap + ii + gender
#39. ti trap + ii + breeding
#40. ti trap + ii + gender*breeding
#41. ti trap + ii + gender + breeding

#42. ti site + gender
#43. ti site + breeding
#44. ti site + gender*breeding
#45. ti site + gender + breeding

#46. ii + gender
#47. ii + breeding
#48. ii + gender*breeding
#49. ii + gender + breeding

#50. gender + breeding


################################################# % PRNINT from MIXSIAR ########################################################################################


#data = read.csv("GLMM_database_FINAL_abbrv.csv")
data = read.csv("GLMM_database_corrected_23012019.csv")

library(glmmADMB)

prop_m1 = glmmadmb(PRNINT_simp ~ 1 + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m2 = glmmadmb(PRNINT_simp ~  ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m3 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m4 = glmmadmb(PRNINT_simp ~  ti_biom_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m5 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m6 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m7 = glmmadmb(PRNINT_simp ~  gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m8 = glmmadmb(PRNINT_simp ~  bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m9 = glmmadmb(PRNINT_simp ~  gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m10 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ti_biom_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m11 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m12 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m13 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m14 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m15 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ti_biom_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m16 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ti_biom_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m17 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ti_biom_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m18 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ti_biom_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m19 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m20 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m21 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m22 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m23 = glmmadmb(PRNINT_simp ~  NDVI_trap_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m24 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m25 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m26 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m27 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m28 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m29 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m30 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m31 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m32 = glmmadmb(PRNINT_simp ~  NDVI_site_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m33 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m34 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m35 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m36 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m37 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m38 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m39 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m40 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_m41 = glmmadmb(PRNINT_simp ~  ti_biom_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m42 = glmmadmb(PRNINT_simp ~  ti_biom_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m43 = glmmadmb(PRNINT_simp ~  ti_biom_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m44 = glmmadmb(PRNINT_simp ~  ti_biom_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m45 = glmmadmb(PRNINT_simp ~  ti_biom_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m46 = glmmadmb(PRNINT_simp ~  ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m47 = glmmadmb(PRNINT_simp ~  ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m48 = glmmadmb(PRNINT_simp ~  ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_m49 = glmmadmb(PRNINT_simp ~  ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_m50 = glmmadmb(PRNINT_simp ~  gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)




#### STEP 5: COMPETE MODELS 

library(AICcmodavg)
library(bbmle)
library(MuMIn)

# models 17,18,21,22,31,32,40,41 removed
AICc(prop_m1, prop_m2, prop_m3, prop_m4, prop_m5, prop_m6, prop_m7, prop_m8, prop_m9, prop_m10,
     prop_m11, prop_m12, prop_m13, prop_m14, prop_m15, prop_m16, prop_m19, prop_m20,
     prop_m23, prop_m24, prop_m25, prop_m26, prop_m27, prop_m28, prop_m29, prop_m30, 
     prop_m33, prop_m34, prop_m35, prop_m36, prop_m37, prop_m38, prop_m39,
     prop_m42, prop_m43, prop_m44, prop_m45, prop_m46, prop_m47, prop_m48, prop_m49, prop_m50)
anova(prop_m1, prop_m2, prop_m3, prop_m4, prop_m5, prop_m6, prop_m7, prop_m8, prop_m9, prop_m10,
      prop_m11, prop_m12, prop_m13, prop_m14, prop_m15, prop_m16, prop_m19, prop_m20,
      prop_m23, prop_m24, prop_m25, prop_m26, prop_m27, prop_m28, prop_m29, prop_m30, 
      prop_m33, prop_m34, prop_m35, prop_m36, prop_m37, prop_m38, prop_m39,
      prop_m42, prop_m43, prop_m44, prop_m45, prop_m46, prop_m47, prop_m48, prop_m49, prop_m50)

# AIC comparison table - models 17,18,21,22,31,32,40,41 removed
cand.models <- list(prop_m1, prop_m2, prop_m3, prop_m4, prop_m5, prop_m6, prop_m7, prop_m8, prop_m9, prop_m10,
                    prop_m11, prop_m12, prop_m13, prop_m14, prop_m15, prop_m16, prop_m19, prop_m20,
                    prop_m23, prop_m24, prop_m25, prop_m26, prop_m27, prop_m28, prop_m29, prop_m30, 
                    prop_m33, prop_m34, prop_m35, prop_m36, prop_m37, prop_m38, prop_m39,
                    prop_m42, prop_m43, prop_m44, prop_m45, prop_m46, prop_m47, prop_m48, prop_m49, prop_m50)
cand.names <- c("prop_m1", "prop_m2", "prop_m3", "prop_m4", "prop_m5", "prop_m6", "prop_m7", "prop_m8", "prop_m9", "prop_m10",
                "prop_m11", "prop_m12", "prop_m13", "prop_m14", "prop_m15", "prop_m16", "prop_m19", "prop_m20",
                "prop_m23", "prop_m24", "prop_m25", "prop_m26", "prop_m27", "prop_m28", "prop_m29", "prop_m30", 
                "prop_m33", "prop_m34", "prop_m35", "prop_m36", "prop_m37", "prop_m38", "prop_m39",
                "prop_m42", "prop_m43", "prop_m44", "prop_m45", "prop_m46", "prop_m47", "prop_m48", "prop_m49", "prop_m50")

# Make a table of all AIC vals w weights - models 17,18,21,22,31,32,40,41 removed
t <- AICctab(prop_m1, prop_m2, prop_m3, prop_m4, prop_m5, prop_m6, prop_m7, prop_m8, prop_m9, prop_m10,
                    prop_m11, prop_m12, prop_m13, prop_m14, prop_m15, prop_m16, prop_m19, prop_m20,
                    prop_m23, prop_m24, prop_m25, prop_m26, prop_m27, prop_m28, prop_m29, prop_m30, 
                    prop_m33, prop_m34, prop_m35, prop_m36, prop_m37, prop_m38, prop_m39,
                    prop_m42, prop_m43, prop_m44, prop_m45, prop_m46, prop_m47, prop_m48, prop_m49, prop_m50, 
                    nobs=44,logLik=T, base=T, weights=T, delta=T, sort=T)
print(t)
class(t) <- "data.frame"
write.csv(t, "AIC_table_dec72019_propnmods.csv", row.names = T)

# Select top models 95% model weight
top.set <- model.sel(cand.models)
top.comp.models.95 <- get.models(top.set, cumsum(weight)<=0.95)
modavg.95 <- model.avg(top.comp.models.95)
summary(modavg.95)

write.csv(top.set, "AICc_model_selection_table_dec72019_propnmods.csv")

# Select top models dAIC <2
#top.comp.models.d2 <- get.models(top.set, delta<2)
#modavg.d2 <- model.avg(top.comp.models.d2)
#summary(modavg.d2)






## SUMMARY STATS ###############################################################################################################################################

# Some extra summary statistics, figures to help with interpretation and writing this section for manuscript 

library(ggplot2)

# % marine food ~ distance from shore 
data$dist_class <- factor(data$dist_class, levels=c("0-25", "50-75", "100-125", "150-200", ordered=T))

ggplot(data=data, aes(x=dist_class, y=PRNINT)) +
  geom_point()


# Average +/- SE % food in diet, range, etc. 
mean(data$PRNINT)
sd(data$PRNINT)/sqrt(length(data$PRNINT))
min(data$PRNINT)
max(data$PRNINT)
median(data$PRNINT)


# % BAB in diets 
perc <- data %>% 
  group_by(region,gn) %>% 
  summarize(avg = mean(PRNINT_simp), sd=sd(PRNINT_simp)) %>% 
  print()

##############################################################################################################################################################





######################################################### C13 MODELS ########################################################################################



#data = read.csv("GLMM_database_FINAL_abbrv.csv")
data = read.csv("GLMM_database_corrected_23012019.csv")



C_m1 = lmer(mouse_d13C ~ 1 + (1|site), data=data)
C_m2 = lmer(mouse_d13C ~  ii_biom_rs + (1|site), data=data)
r2 <- resid(C_m2)
hist(r2)
plot(r2)
qqnorm(r2)
qqline(r2)

C_m3 = lmer(mouse_d13C ~  ti_biom_trap_rs + (1|site), data=data)
C_m4 = lmer(mouse_d13C ~  ti_biom_site_rs + (1|site), data=data)
C_m5 = lmer(mouse_d13C ~  NDVI_trap_rs + (1|site), data=data)
C_m6 = lmer(mouse_d13C ~  NDVI_site_rs + (1|site), data=data)
C_m7 = lmer(mouse_d13C ~  gn + (1|site), data=data)
C_m8 = lmer(mouse_d13C ~  bs + (1|site), data=data)
C_m9 = lmer(mouse_d13C ~  gn*bs + (1|site), data=data)

C_m10 = lmer(mouse_d13C ~  NDVI_trap_rs + ti_biom_site_rs + (1|site), data=data)
C_m11 = lmer(mouse_d13C ~  NDVI_trap_rs + ii_biom_rs + (1|site), data=data)
C_m12 = lmer(mouse_d13C ~  NDVI_trap_rs + gn + (1|site), data=data)
C_m13 = lmer(mouse_d13C ~  NDVI_trap_rs + bs + (1|site), data=data)
C_m14 = lmer(mouse_d13C ~  NDVI_trap_rs + gn*bs + (1|site), data=data)

C_m15 = lmer(mouse_d13C ~  NDVI_trap_rs + ti_biom_site_rs + gn + (1|site), data=data)
C_m16 = lmer(mouse_d13C ~  NDVI_trap_rs + ti_biom_site_rs + bs + (1|site), data=data)
#C_m17 = lmer(mouse_d13C ~  NDVI_trap_rs + ti_biom_site_rs + gn*bs + (1|site), data=data)
#C_m18 = lmer(mouse_d13C ~  NDVI_trap_rs + ti_biom_site_rs + gn + bs + (1|site), data=data)

C_m19 = lmer(mouse_d13C ~  NDVI_trap_rs + ii_biom_rs + gn + (1|site), data=data)
C_m20 = lmer(mouse_d13C ~  NDVI_trap_rs + ii_biom_rs + bs + (1|site), data=data)
#C_m21 = lmer(mouse_d13C ~  NDVI_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data)
#C_m22 = lmer(mouse_d13C ~  NDVI_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data)

C_m23 = lmer(mouse_d13C ~  NDVI_trap_rs + gn + bs + (1|site), data=data)

C_m24 = lmer(mouse_d13C ~  NDVI_site_rs + ii_biom_rs + (1|site), data=data)
C_m25 = lmer(mouse_d13C ~  NDVI_site_rs + gn + (1|site), data=data)
C_m26 = lmer(mouse_d13C ~  NDVI_site_rs + bs + (1|site), data=data)
C_m27 = lmer(mouse_d13C ~  NDVI_site_rs + gn*bs + (1|site), data=data)
C_m28 = lmer(mouse_d13C ~  NDVI_site_rs + gn + bs + (1|site), data=data)

C_m29 = lmer(mouse_d13C ~  NDVI_site_rs + ii_biom_rs + gn + (1|site), data=data)
C_m30 = lmer(mouse_d13C ~  NDVI_site_rs + ii_biom_rs + bs + (1|site), data=data)
#C_m31 = lmer(mouse_d13C ~  NDVI_site_rs + ii_biom_rs + gn*bs + (1|site), data=data)
#C_m32 = lmer(mouse_d13C ~  NDVI_site_rs + ii_biom_rs + gn + bs + (1|site), data=data)

C_m33 = lmer(mouse_d13C ~  ti_biom_trap_rs + gn + (1|site), data=data)
C_m34 = lmer(mouse_d13C ~  ti_biom_trap_rs + bs + (1|site), data=data)
C_m35 = lmer(mouse_d13C ~  ti_biom_trap_rs + gn*bs + (1|site), data=data)
C_m36 = lmer(mouse_d13C ~  ti_biom_trap_rs + gn + bs + (1|site), data=data)

C_m37 = lmer(mouse_d13C ~  ti_biom_trap_rs + ii_biom_rs + (1|site), data=data)
C_m38 = lmer(mouse_d13C ~  ti_biom_trap_rs + ii_biom_rs + gn + (1|site), data=data)
C_m39 = lmer(mouse_d13C ~  ti_biom_trap_rs + ii_biom_rs + bs + (1|site), data=data)
#C_m40 = lmer(mouse_d13C ~  ti_biom_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data)
#C_m41 = lmer(mouse_d13C ~  ti_biom_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data)

C_m42 = lmer(mouse_d13C ~  ti_biom_site_rs + gn + (1|site), data=data)
C_m43 = lmer(mouse_d13C ~  ti_biom_site_rs + bs + (1|site), data=data)
C_m44 = lmer(mouse_d13C ~  ti_biom_site_rs + gn*bs + (1|site), data=data)
C_m45 = lmer(mouse_d13C ~  ti_biom_site_rs + gn + bs + (1|site), data=data)

C_m46 = lmer(mouse_d13C ~  ii_biom_rs + gn + (1|site), data=data)
C_m47 = lmer(mouse_d13C ~  ii_biom_rs + bs + (1|site), data=data)
C_m48 = lmer(mouse_d13C ~  ii_biom_rs + gn*bs + (1|site), data=data)
C_m49 = lmer(mouse_d13C ~  ii_biom_rs + gn + bs + (1|site), data=data)

C_m50 = lmer(mouse_d13C ~  gn + bs + (1|site), data=data)




#### STEP 5: COMPETE MODELS 

# models 17,18,21,22,31,32,40,41 removed
AICc(C_m1, C_m2, C_m3, C_m4, C_m5, C_m6, C_m7, C_m8, C_m9, C_m10,
     C_m11, C_m12, C_m13, C_m14, C_m15, C_m16, C_m19, C_m20,
     C_m23, C_m24, C_m25, C_m26, C_m27, C_m28, C_m29, C_m30, 
     C_m33, C_m34, C_m35, C_m36, C_m37, C_m38, C_m39,
     C_m42, C_m43, C_m44, C_m45, C_m46, C_m47, C_m48, C_m49, C_m50)
anova(C_m1, C_m2, C_m3, C_m4, C_m5, C_m6, C_m7, C_m8, C_m9, C_m10,
      C_m11, C_m12, C_m13, C_m14, C_m15, C_m16, C_m19, C_m20,
      C_m23, C_m24, C_m25, C_m26, C_m27, C_m28, C_m29, C_m30, 
      C_m33, C_m34, C_m35, C_m36, C_m37, C_m38, C_m39,
      C_m42, C_m43, C_m44, C_m45, C_m46, C_m47, C_m48, C_m49, C_m50)

# AIC comparison table - models 17,18,21,22,31,32,40,41 removed
cand.models.C <- list(C_m1, C_m2, C_m3, C_m4, C_m5, C_m6, C_m7, C_m8, C_m9, C_m10,
                    C_m11, C_m12, C_m13, C_m14, C_m15, C_m16, C_m19, C_m20,
                    C_m23, C_m24, C_m25, C_m26, C_m27, C_m28, C_m29, C_m30, 
                    C_m33, C_m34, C_m35, C_m36, C_m37, C_m38, C_m39,
                    C_m42, C_m43, C_m44, C_m45, C_m46, C_m47, C_m48, C_m49, C_m50)
cand.names.C <- c("C_m1", "C_m2", "C_m3", "C_m4", "C_m5", "C_m6", "C_m7", "C_m8", "C_m9", "C_m10",
                "C_m11", "C_m12", "C_m13", "C_m14", "C_m15", "C_m16", "C_m19", "C_m20",
                "C_m23", "C_m24", "C_m25", "C_m26", "C_m27", "C_m28", "C_m29", "C_m30", 
                "C_m33", "C_m34", "C_m35", "C_m36", "C_m37", "C_m38", "C_m39",
                "C_m42", "C_m43", "C_m44", "C_m45", "C_m46", "C_m47", "C_m48", "C_m49", "C_m50")

# Make a table of all AIC vals w weights - models 17,18,21,22,31,32,40,41 removed
t.C <- AICctab(C_m1, C_m2, C_m3, C_m4, C_m5, C_m6, C_m7, C_m8, C_m9, C_m10,
             C_m11, C_m12, C_m13, C_m14, C_m15, C_m16, C_m19, C_m20,
             C_m23, C_m24, C_m25, C_m26, C_m27, C_m28, C_m29, C_m30, 
             C_m33, C_m34, C_m35, C_m36, C_m37, C_m38, C_m39,
             C_m42, C_m43, C_m44, C_m45, C_m46, C_m47, C_m48, C_m49, C_m50, 
             nobs=44,logLik=T, base=T, weights=T, delta=T, sort=T)
print(t.C)
class(t.C) <- "data.frame"
write.csv(t.C, "AIC_table_dec72019_Cmodels.csv", row.names = F)

# Select top models 95% model weight
top.set.C <- model.sel(cand.models.C)
top.comp.models.95.C <- get.models(top.set.C, cumsum(weight)<=0.95)
modavg.95.C <- model.avg(top.comp.models.95.C)
summary(modavg.95.C)

write.csv(top.set.C, "AICc_model_selection_table_dec72019_Cmodels.csv")



######################################################### N15 MODELS ########################################################################################



#data = read.csv("GLMM_database_FINAL_abbrv.csv")
data = read.csv("GLMM_database_corrected_23012019.csv")



N_m1 = lmer(mouse_d15N ~ 1 + (1|site), data=data)
N_m2 = lmer(mouse_d15N ~  ii_biom_rs + (1|site), data=data)
r2 <- resid(N_m2)
hist(r2)
plot(r2)
qqnorm(r2)
qqline(r2)

N_m3 = lmer(mouse_d15N ~  ti_biom_trap_rs + (1|site), data=data)
N_m4 = lmer(mouse_d15N ~  ti_biom_site_rs + (1|site), data=data)
N_m5 = lmer(mouse_d15N ~  NDVI_trap_rs + (1|site), data=data)
N_m6 = lmer(mouse_d15N ~  NDVI_site_rs + (1|site), data=data)
N_m7 = lmer(mouse_d15N ~  gn + (1|site), data=data)
N_m8 = lmer(mouse_d15N ~  bs + (1|site), data=data)
N_m9 = lmer(mouse_d15N ~  gn*bs + (1|site), data=data)

N_m10 = lmer(mouse_d15N ~  NDVI_trap_rs + ti_biom_site_rs + (1|site), data=data)
N_m11 = lmer(mouse_d15N ~  NDVI_trap_rs + ii_biom_rs + (1|site), data=data)
N_m12 = lmer(mouse_d15N ~  NDVI_trap_rs + gn + (1|site), data=data)
N_m13 = lmer(mouse_d15N ~  NDVI_trap_rs + bs + (1|site), data=data)
N_m14 = lmer(mouse_d15N ~  NDVI_trap_rs + gn*bs + (1|site), data=data)

N_m15 = lmer(mouse_d15N ~  NDVI_trap_rs + ti_biom_site_rs + gn + (1|site), data=data)
N_m16 = lmer(mouse_d15N ~  NDVI_trap_rs + ti_biom_site_rs + bs + (1|site), data=data)
#N_m17 = lmer(mouse_d15N ~  NDVI_trap_rs + ti_biom_site_rs + gn*bs + (1|site), data=data)
#N_m18 = lmer(mouse_d15N ~  NDVI_trap_rs + ti_biom_site_rs + gn + bs + (1|site), data=data)

N_m19 = lmer(mouse_d15N ~  NDVI_trap_rs + ii_biom_rs + gn + (1|site), data=data)
N_m20 = lmer(mouse_d15N ~  NDVI_trap_rs + ii_biom_rs + bs + (1|site), data=data)
#N_m21 = lmer(mouse_d15N ~  NDVI_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data)
#N_m22 = lmer(mouse_d15N ~  NDVI_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data)

N_m23 = lmer(mouse_d15N ~  NDVI_trap_rs + gn + bs + (1|site), data=data)

N_m24 = lmer(mouse_d15N ~  NDVI_site_rs + ii_biom_rs + (1|site), data=data)
N_m25 = lmer(mouse_d15N ~  NDVI_site_rs + gn + (1|site), data=data)
N_m26 = lmer(mouse_d15N ~  NDVI_site_rs + bs + (1|site), data=data)
N_m27 = lmer(mouse_d15N ~  NDVI_site_rs + gn*bs + (1|site), data=data)
N_m28 = lmer(mouse_d15N ~  NDVI_site_rs + gn + bs + (1|site), data=data)

N_m29 = lmer(mouse_d15N ~  NDVI_site_rs + ii_biom_rs + gn + (1|site), data=data)
N_m30 = lmer(mouse_d15N ~  NDVI_site_rs + ii_biom_rs + bs + (1|site), data=data)
#N_m31 = lmer(mouse_d15N ~  NDVI_site_rs + ii_biom_rs + gn*bs + (1|site), data=data)
#N_m32 = lmer(mouse_d15N ~  NDVI_site_rs + ii_biom_rs + gn + bs + (1|site), data=data)

N_m33 = lmer(mouse_d15N ~  ti_biom_trap_rs + gn + (1|site), data=data)
N_m34 = lmer(mouse_d15N ~  ti_biom_trap_rs + bs + (1|site), data=data)
N_m35 = lmer(mouse_d15N ~  ti_biom_trap_rs + gn*bs + (1|site), data=data)
N_m36 = lmer(mouse_d15N ~  ti_biom_trap_rs + gn + bs + (1|site), data=data)

N_m37 = lmer(mouse_d15N ~  ti_biom_trap_rs + ii_biom_rs + (1|site), data=data)
N_m38 = lmer(mouse_d15N ~  ti_biom_trap_rs + ii_biom_rs + gn + (1|site), data=data)
N_m39 = lmer(mouse_d15N ~  ti_biom_trap_rs + ii_biom_rs + bs + (1|site), data=data)
#N_m40 = lmer(mouse_d15N ~  ti_biom_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data)
#N_m41 = lmer(mouse_d15N ~  ti_biom_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data)

N_m42 = lmer(mouse_d15N ~  ti_biom_site_rs + gn + (1|site), data=data)
N_m43 = lmer(mouse_d15N ~  ti_biom_site_rs + bs + (1|site), data=data)
N_m44 = lmer(mouse_d15N ~  ti_biom_site_rs + gn*bs + (1|site), data=data)
N_m45 = lmer(mouse_d15N ~  ti_biom_site_rs + gn + bs + (1|site), data=data)

N_m46 = lmer(mouse_d15N ~  ii_biom_rs + gn + (1|site), data=data)
N_m47 = lmer(mouse_d15N ~  ii_biom_rs + bs + (1|site), data=data)
N_m48 = lmer(mouse_d15N ~  ii_biom_rs + gn*bs + (1|site), data=data)
N_m49 = lmer(mouse_d15N ~  ii_biom_rs + gn + bs + (1|site), data=data)

N_m50 = lmer(mouse_d15N ~  gn + bs + (1|site), data=data)




#### STEP 5: COMPETE MODELS 

# models 17,18,21,22,31,32,40,41 removed
AICc(N_m1, N_m2, N_m3, N_m4, N_m5, N_m6, N_m7, N_m8, N_m9, N_m10,
     N_m11, N_m12, N_m13, N_m14, N_m15, N_m16, N_m19, N_m20,
     N_m23, N_m24, N_m25, N_m26, N_m27, N_m28, N_m29, N_m30, 
     N_m33, N_m34, N_m35, N_m36, N_m37, N_m38, N_m39,
     N_m42, N_m43, N_m44, N_m45, N_m46, N_m47, N_m48, N_m49, N_m50)
anova(N_m1, N_m2, N_m3, N_m4, N_m5, N_m6, N_m7, N_m8, N_m9, N_m10,
      N_m11, N_m12, N_m13, N_m14, N_m15, N_m16, N_m19, N_m20,
      N_m23, N_m24, N_m25, N_m26, N_m27, N_m28, N_m29, N_m30, 
      N_m33, N_m34, N_m35, N_m36, N_m37, N_m38, N_m39,
      N_m42, N_m43, N_m44, N_m45, N_m46, N_m47, N_m48, N_m49, N_m50)

# AIC comparison table - models 17,18,21,22,31,32,40,41 removed
cand.models.N <- list(N_m1, N_m2, N_m3, N_m4, N_m5, N_m6, N_m7, N_m8, N_m9, N_m10,
                      N_m11, N_m12, N_m13, N_m14, N_m15, N_m16, N_m19, N_m20,
                      N_m23, N_m24, N_m25, N_m26, N_m27, N_m28, N_m29, N_m30, 
                      N_m33, N_m34, N_m35, N_m36, N_m37, N_m38, N_m39,
                      N_m42, N_m43, N_m44, N_m45, N_m46, N_m47, N_m48, N_m49, N_m50)
cand.names.N <- c("N_m1", "N_m2", "N_m3", "N_m4", "N_m5", "N_m6", "N_m7", "N_m8", "N_m9", "N_m10",
                  "N_m11", "N_m12", "N_m13", "N_m14", "N_m15", "N_m16", "N_m19", "N_m20",
                  "N_m23", "N_m24", "N_m25", "N_m26", "N_m27", "N_m28", "N_m29", "N_m30", 
                  "N_m33", "N_m34", "N_m35", "N_m36", "N_m37", "N_m38", "N_m39",
                  "N_m42", "N_m43", "N_m44", "N_m45", "N_m46", "N_m47", "N_m48", "N_m49", "N_m50")

# Make a table of all AIC vals w weights - models 17,18,21,22,31,32,40,41 removed
t.N <- AICctab(N_m1, N_m2, N_m3, N_m4, N_m5, N_m6, N_m7, N_m8, N_m9, N_m10,
               N_m11, N_m12, N_m13, N_m14, N_m15, N_m16, N_m19, N_m20,
               N_m23, N_m24, N_m25, N_m26, N_m27, N_m28, N_m29, N_m30, 
               N_m33, N_m34, N_m35, N_m36, N_m37, N_m38, N_m39,
               N_m42, N_m43, N_m44, N_m45, N_m46, N_m47, N_m48, N_m49, N_m50, 
               nobs=44,logLik=T, base=T, weights=T, delta=T, sort=T)
print(t.N)
class(t.N) <- "data.frame"
write.csv(t.N, "AIC_table_dec72019_Nmodels.csv", row.names = F)

# Select top models 95% model weight
top.set.N <- model.sel(cand.models.N)
top.Nomp.models.95.N <- get.models(top.set.N, cumsum(weight)<=0.95)
modavg.95.N <- model.avg(top.Nomp.models.95.N)
summary(modavg.95.N)

write.csv(top.set.N, "AICc_model_selection_table_dec72019_Nmodels.csv")

























#### NEW PREDICTIVE FIGURE #### 
# CODE FROM CHRISTINA!  (Jan 29, 2019) 


#females
logistic = function(x){1/(1+exp(-x))}

datf = data[data$gn=='F',][1,]
BAB.dat = seq(-0.4,1,0.01)
datf = datf[rep(1,length(BAB.dat)),]
datf$ii_biom_rs = BAB.dat

fem<- predict(modavg.95, newdata = datf, se=TRUE)$fit
fem<-as.numeric(fem)

femse<-predict(modavg.95, newdata = datf, se.fit=TRUE)$se.fit
femse<-as.numeric(femse)

f.pred<-list(mean=matrix(fem, length(BAB.dat), 2), 
             lci=matrix(fem-1.96*femse, length(BAB.dat), 2), 
             uci=matrix(fem+1.96*femse, length(BAB.dat), 2))
dff<-data.frame(f.pred)



#males
logistic = function(x){1/(1+exp(-x))}

datm = data[data$gn=='M',][1,]
#BAB.dat = seq(-3, 1, 1)
datm = datm[rep(1,length(BAB.dat)),]
datm$ii_biom_rs = BAB.dat

mal<- predict (modavg.95, newdata = datm, se=TRUE)$fit
mal<-as.numeric(mal)

malse<-predict (modavg.95, newdata = datm, se.fit=TRUE)$se.fit
malse<-as.numeric(malse)

m.pred<-list(mean=matrix(mal, length(BAB.dat), 2), 
             lci=matrix(mal-1.96*femse, length(BAB.dat), 2), 
             uci=matrix(mal+1.96*femse, length(BAB.dat), 2))
dfm<-data.frame(m.pred)



# plot
#determine sd and mean for raw values 
sd(data$ii_biom_rs)
mean(data$ii_biom_rs)

# plot parameters (same for M and F)
plot(-100,-100, xlim=range(data$ii_biom_rs), ylim=c(0,1), ylab="", xlab="",axes=FALSE,cex=5)
#mtext("(a)")
mtext(text="BAB", side=1, line=2, cex=1)
mtext(text="Proportion of beach arthropods in diet", side=2, line=2, cex=1.5)
BAB.axis.values<-c(
  BAB.0.54 <- (0.54-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
  BAB.0.56 <- (0.56-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
  BAB.0.58 <- (0.58-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
  BAB.0.60 <- (0.60-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
  BAB.0.616 <- (0.616-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)))
axis.values <- BAB.axis.values
axis.labels<-c(0.54,0.56,0.58,0.60,0.62)
axis(1, at=axis.values, labels=axis.labels, cex=4)
axis(2, cex=4)
box()

#mal
mcol <- rgb(23,190,187, max = 255, alpha = 75)       # set alpha for male polygon colour
points(data$NDVI_site_rs[as.character(data$gn)=='M'], 
       (data$PRNINT[as.character(data$gn)=='M']), 
       col='#17bebb', xlim=range(data$NDVI_site_rs), pch=16, cex=1.2)
lines(NDVI.dat, logistic(m.pred$mean[,1]), col='#17bebb', lwd=2)
polygon(c(NDVI.dat,rev(NDVI.dat)), 
        c(logistic(m.pred$lci[,1]),rev(logistic(m.pred$uci[,1]))),
        col=mcol, border=NA)

#fem
fcol <- rgb(255,125,88, max = 255, alpha = 80)         # set alpha for female polygon colour
points(data$ii_biom_rs[as.character(data$gn)=='F'], 
       (data$PRNINT_simp[as.character(data$gn)=='F']), 
       col='#ff7d58', xlim=range(data$NDVI_site_rs), pch=19, cex=1)
lines(BAB.dat, logistic(f.pred$mean[,1]), col='#ff7d58', lwd=2)
polygon(c(BAB.dat,rev(BAB.dat)), 
        c(logistic(f.pred$lci[,1]),rev(logistic(f.pred$uci[,1]))),
        col=fcol, border=NA)

legend("topleft", inset=.02, legend=c("Males", "Females"), col=c("turquoise4","salmon"), pch = 19)


# same but in ggplot for formatting - COLOUR
ggplot() + 
  ylim(0,1) + 
  scale_x_continuous(breaks=c(
    (300-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (544-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (788-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (1032-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (1275-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg))), labels=c(300,544,788,1030,1275)) +
  geom_point(data=data, aes(x=ii_biom_rs, y=PRNINT_simp, fill=gn, colour=gn), shape=21, size=3, stroke=1.5, alpha=0.7) + 
  geom_ribbon(aes(x=BAB.dat, ymin = logistic(f.pred$lci[,1]), ymax = rev(logistic(f.pred$uci[,1]))), alpha = 0.2, fill="#ff7d58") +
  geom_ribbon(aes(x=BAB.dat, ymin = logistic(m.pred$lci[,1]), ymax = rev(logistic(m.pred$uci[,1]))), alpha = 0.2, fill="#17bebb") + 
  geom_line(aes(x=BAB.dat,y=logistic(m.pred$mean[,1])), colour="#17bebb", size=1.5, alpha=0.6) +
  geom_line(aes(x=BAB.dat,y=logistic(f.pred$mean[,1])), colour="#ff7d58", size=1.5, alpha=0.6) +
  scale_fill_discrete(name="Sex", labels=c("Female", "Male")) +
  scale_colour_discrete(name="Sex", labels=c("Female", "Male")) +
  labs(x = "Beach arthropod biomass (mg)", y = "% Beach arthropods in diet") +
  theme_bw() +
  theme(text = element_text(size=12),
        panel.border = element_rect(size=1.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black", size=14),
        axis.title = element_text(colour = "black", size=18),
        axis.ticks = element_line(size = 1, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        #plot.margin=unit(c(0.5,0.2,0.2,0.2),"cm"),
        legend.position = c(0.87,0.87),
        legend.background = element_rect(colour="black"),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        legend.margin = margin(t=0.1,b=0.1,l=0.1,r=0.1, unit="cm")) 




# same but in ggplot for formatting - BW
data$gn <- factor(data$gn, levels=c("M", "F"), ordered=T)

ggplot() + 
  ylim(0,1) + 
  scale_x_continuous(breaks=c(
    (300-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (544-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (788-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (1032-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg)),
    (1275-mean(data$ii_biom_mg)) /(2*sd(data$ii_biom_mg))), labels=c(300,544,788,1030,1275)) +
  scale_fill_manual(name="Sex", labels=c("Male", "Female"), values=c("gray20", "gray60")) +
  scale_colour_manual(name="Sex", labels=c("Male", "Female"), values=c("gray20", "gray60")) +
  geom_point(data=data, aes(x=ii_biom_rs, y=PRNINT_simp, fill=gn, colour=gn), shape=21, size=3, stroke=1.5, alpha=0.7) + 
  geom_ribbon(aes(x=BAB.dat, ymin = logistic(m.pred$lci[,1]), ymax = rev(logistic(m.pred$uci[,1]))), alpha = 0.2, fill="gray20") + 
  geom_ribbon(aes(x=BAB.dat, ymin = logistic(f.pred$lci[,1]), ymax = rev(logistic(f.pred$uci[,1]))), alpha = 0.2, fill="gray60") +
  geom_line(aes(x=BAB.dat,y=logistic(m.pred$mean[,1])), colour="gray20", size=1.2, alpha=0.6) +
  geom_line(aes(x=BAB.dat,y=logistic(f.pred$mean[,1])), colour="gray60", size=1.2, alpha=0.6) +
  labs(x = "Beach arthropod biomass (mg)", y = "Beach arthropods in diet (%)") +
  theme_bw() +
  theme(text = element_text(size=12),
        panel.border = element_rect(size=1.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black", size=14),
        axis.title = element_text(colour = "black", size=18),
        axis.ticks = element_line(size = 1, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        #plot.margin=unit(c(0.5,0.2,0.2,0.2),"cm"),
        legend.position = c(0.87,0.86),
        legend.background = element_rect(colour="black"),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        legend.margin = margin(t=0.1,b=0.1,l=0.1,r=0.1, unit="cm")) 
