# sensitivity GLMMs:
## PART I: 5% and 95% beta-GLMMS 
## PART II: d13C and d15N isotope Gaussian-LMMs

# June-Aug 2020 

# All candidate model outlines, correlation analyses, etc. in GLMM_dietproportions_14012019_11182019.R

########################################################################################################################################################

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

library(tidyverse)
library(devtools)
library(glmmADMB)
library(lme4)
library(AICcmodavg)
library(bbmle)
library(MuMIn)
library(dotwhisker)   # for parameter plot

data = read.csv("GLMM_database_corrected_23012019.csv")


########################################################################################################################################################
############################################################### PART I: 5% and 95% beta-GLMMS ##########################################################
########################################################################################################################################################

# Sensitivity analysis using 5% and 95% credible interval proportion results from MixSIAR result of % beach inverts in diet 


#--------------------------- 5% CI models 

prop_5ci_m1 = glmmadmb(PRNINT_simp_5lci ~ 1 + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m2 = glmmadmb(PRNINT_simp_5lci ~  ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m3 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m4 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m5 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m6 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m7 = glmmadmb(PRNINT_simp_5lci ~  gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m8 = glmmadmb(PRNINT_simp_5lci ~  bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m9 = glmmadmb(PRNINT_simp_5lci ~  gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m10 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ti_biom_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m11 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m12 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m13 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m14 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m15 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ti_biom_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m16 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ti_biom_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m17 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ti_biom_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m18 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ti_biom_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m19 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m20 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m21 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m22 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m23 = glmmadmb(PRNINT_simp_5lci ~  NDVI_trap_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m24 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m25 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m26 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m27 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m28 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m29 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m30 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m31 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m32 = glmmadmb(PRNINT_simp_5lci ~  NDVI_site_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m33 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m34 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m35 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m36 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m37 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m38 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m39 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m40 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_5ci_m41 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m42 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m43 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m44 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m45 = glmmadmb(PRNINT_simp_5lci ~  ti_biom_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m46 = glmmadmb(PRNINT_simp_5lci ~  ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m47 = glmmadmb(PRNINT_simp_5lci ~  ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m48 = glmmadmb(PRNINT_simp_5lci ~  ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_5ci_m49 = glmmadmb(PRNINT_simp_5lci ~  ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_5ci_m50 = glmmadmb(PRNINT_simp_5lci ~  gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)



#--------------------------- COMPETE 5% CI models 


# models 17,18,21,22,31,32,40,41 removed
AICc(prop_5ci_m1, prop_5ci_m2, prop_5ci_m3, prop_5ci_m4, prop_5ci_m5, prop_5ci_m6, prop_5ci_m7, prop_5ci_m8, prop_5ci_m9, prop_5ci_m10,
     prop_5ci_m11, prop_5ci_m12, prop_5ci_m13, prop_5ci_m14, prop_5ci_m15, prop_5ci_m16, prop_5ci_m19, prop_5ci_m20,
     prop_5ci_m23, prop_5ci_m24, prop_5ci_m25, prop_5ci_m26, prop_5ci_m27, prop_5ci_m28, prop_5ci_m29, prop_5ci_m30, 
     prop_5ci_m33, prop_5ci_m34, prop_5ci_m35, prop_5ci_m36, prop_5ci_m37, prop_5ci_m38, prop_5ci_m39,
     prop_5ci_m42, prop_5ci_m43, prop_5ci_m44, prop_5ci_m45, prop_5ci_m46, prop_5ci_m47, prop_5ci_m48, prop_5ci_m49, prop_5ci_m50)
anova(prop_5ci_m1, prop_5ci_m2, prop_5ci_m3, prop_5ci_m4, prop_5ci_m5, prop_5ci_m6, prop_5ci_m7, prop_5ci_m8, prop_5ci_m9, prop_5ci_m10,
      prop_5ci_m11, prop_5ci_m12, prop_5ci_m13, prop_5ci_m14, prop_5ci_m15, prop_5ci_m16, prop_5ci_m19, prop_5ci_m20,
      prop_5ci_m23, prop_5ci_m24, prop_5ci_m25, prop_5ci_m26, prop_5ci_m27, prop_5ci_m28, prop_5ci_m29, prop_5ci_m30, 
      prop_5ci_m33, prop_5ci_m34, prop_5ci_m35, prop_5ci_m36, prop_5ci_m37, prop_5ci_m38, prop_5ci_m39,
      prop_5ci_m42, prop_5ci_m43, prop_5ci_m44, prop_5ci_m45, prop_5ci_m46, prop_5ci_m47, prop_5ci_m48, prop_5ci_m49, prop_5ci_m50)

# AIC comparison table - models 17,18,21,22,31,32,40,41 removed
cand.models.5ci <- list(prop_5ci_m1, prop_5ci_m2, prop_5ci_m3, prop_5ci_m4, prop_5ci_m5, prop_5ci_m6, prop_5ci_m7, prop_5ci_m8, prop_5ci_m9, prop_5ci_m10,
                    prop_5ci_m11, prop_5ci_m12, prop_5ci_m13, prop_5ci_m14, prop_5ci_m15, prop_5ci_m16, prop_5ci_m19, prop_5ci_m20,
                    prop_5ci_m23, prop_5ci_m24, prop_5ci_m25, prop_5ci_m26, prop_5ci_m27, prop_5ci_m28, prop_5ci_m29, prop_5ci_m30, 
                    prop_5ci_m33, prop_5ci_m34, prop_5ci_m35, prop_5ci_m36, prop_5ci_m37, prop_5ci_m38, prop_5ci_m39,
                    prop_5ci_m42, prop_5ci_m43, prop_5ci_m44, prop_5ci_m45, prop_5ci_m46, prop_5ci_m47, prop_5ci_m48, prop_5ci_m49, prop_5ci_m50)
cand.names.5ci <- c("prop_5ci_m1", "prop_5ci_m2", "prop_5ci_m3", "prop_5ci_m4", "prop_5ci_m5", "prop_5ci_m6", "prop_5ci_m7", "prop_5ci_m8", "prop_5ci_m9", "prop_5ci_m10",
                "prop_5ci_m11", "prop_5ci_m12", "prop_5ci_m13", "prop_5ci_m14", "prop_5ci_m15", "prop_5ci_m16", "prop_5ci_m19", "prop_5ci_m20",
                "prop_5ci_m23", "prop_5ci_m24", "prop_5ci_m25", "prop_5ci_m26", "prop_5ci_m27", "prop_5ci_m28", "prop_5ci_m29", "prop_5ci_m30", 
                "prop_5ci_m33", "prop_5ci_m34", "prop_5ci_m35", "prop_5ci_m36", "prop_5ci_m37", "prop_5ci_m38", "prop_5ci_m39",
                "prop_5ci_m42", "prop_5ci_m43", "prop_5ci_m44", "prop_5ci_m45", "prop_5ci_m46", "prop_5ci_m47", "prop_5ci_m48", "prop_5ci_m49", "prop_5ci_m50")

# Make a table of all AIC vals w weights - models 17,18,21,22,31,32,40,41 removed
t.5ci <- AICctab(prop_5ci_m1, prop_5ci_m2, prop_5ci_m3, prop_5ci_m4, prop_5ci_m5, prop_5ci_m6, prop_5ci_m7, prop_5ci_m8, prop_5ci_m9, prop_5ci_m10,
             prop_5ci_m11, prop_5ci_m12, prop_5ci_m13, prop_5ci_m14, prop_5ci_m15, prop_5ci_m16, prop_5ci_m19, prop_5ci_m20,
             prop_5ci_m23, prop_5ci_m24, prop_5ci_m25, prop_5ci_m26, prop_5ci_m27, prop_5ci_m28, prop_5ci_m29, prop_5ci_m30, 
             prop_5ci_m33, prop_5ci_m34, prop_5ci_m35, prop_5ci_m36, prop_5ci_m37, prop_5ci_m38, prop_5ci_m39,
             prop_5ci_m42, prop_5ci_m43, prop_5ci_m44, prop_5ci_m45, prop_5ci_m46, prop_5ci_m47, prop_5ci_m48, prop_5ci_m49, prop_5ci_m50, 
             nobs=44,logLik=T, base=T, weights=T, delta=T, sort=T)
print(t.5ci)
class(t.5ci) <- "data.frame"
write.csv(t.5ci, "AIC_table_dec72019_propnmods_5ci.csv", row.names = T)

# Select top models 95% model weight
top.set.5ci <- model.sel(cand.models.5ci)
top.comp.models.95.5ci <- get.models(top.set.5ci, cumsum(weight)<=0.95)
modavg.95.5ci <- model.avg(top.comp.models.95.5ci)
summary(modavg.95.5ci)

# all model parameter estimates 
write.csv(top.set.5ci, "AICc_model_selection_table_dec72019_propnmods_5ci.csv")

# model-averaged parameter estimates
modavg.95.5ci.table <- coefTable(modavg.95.5ci, full=TRUE)
modavg.95.5ci.df <- data.frame(modavg.95.5ci.table)

write.csv(modavg.95.5ci.df, "params_full_model_averaged_5ci_models.csv")



########################################################################################################################################################



#--------------------------- 95% CI models 

prop_95ci_m1 = glmmadmb(PRNINT_simp_95uci ~ 1 + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m2 = glmmadmb(PRNINT_simp_95uci ~  ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m3 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m4 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m5 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m6 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m7 = glmmadmb(PRNINT_simp_95uci ~  gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m8 = glmmadmb(PRNINT_simp_95uci ~  bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m9 = glmmadmb(PRNINT_simp_95uci ~  gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m10 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ti_biom_site_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m11 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m12 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m13 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m14 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m15 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ti_biom_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m16 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ti_biom_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m17 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ti_biom_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m18 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ti_biom_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m19 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m20 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m21 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m22 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m23 = glmmadmb(PRNINT_simp_95uci ~  NDVI_trap_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m24 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m25 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m26 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m27 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m28 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m29 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m30 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m31 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m32 = glmmadmb(PRNINT_simp_95uci ~  NDVI_site_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m33 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m34 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m35 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m36 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m37 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + ii_biom_rs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m38 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m39 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m40 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
#prop_95ci_m41 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_trap_rs + ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m42 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_site_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m43 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_site_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m44 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_site_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m45 = glmmadmb(PRNINT_simp_95uci ~  ti_biom_site_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m46 = glmmadmb(PRNINT_simp_95uci ~  ii_biom_rs + gn + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m47 = glmmadmb(PRNINT_simp_95uci ~  ii_biom_rs + bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m48 = glmmadmb(PRNINT_simp_95uci ~  ii_biom_rs + gn*bs + (1|site), data=data, family="beta", link = "logit", debug=F)
prop_95ci_m49 = glmmadmb(PRNINT_simp_95uci ~  ii_biom_rs + gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)

prop_95ci_m50 = glmmadmb(PRNINT_simp_95uci ~  gn + bs + (1|site), data=data, family="beta", link = "logit", debug=F)



#--------------------------- COMPETE 95% CI models 


# models 17,18,21,22,31,32,40,41 removed
AICc(prop_95ci_m1, prop_95ci_m2, prop_95ci_m3, prop_95ci_m4, prop_95ci_m5, prop_95ci_m6, prop_95ci_m7, prop_95ci_m8, prop_95ci_m9, prop_95ci_m10,
     prop_95ci_m11, prop_95ci_m12, prop_95ci_m13, prop_95ci_m14, prop_95ci_m15, prop_95ci_m16, prop_95ci_m19, prop_95ci_m20,
     prop_95ci_m23, prop_95ci_m24, prop_95ci_m25, prop_95ci_m26, prop_95ci_m27, prop_95ci_m28, prop_95ci_m29, prop_95ci_m30, 
     prop_95ci_m33, prop_95ci_m34, prop_95ci_m35, prop_95ci_m36, prop_95ci_m37, prop_95ci_m38, prop_95ci_m39,
     prop_95ci_m42, prop_95ci_m43, prop_95ci_m44, prop_95ci_m45, prop_95ci_m46, prop_95ci_m47, prop_95ci_m48, prop_95ci_m49, prop_95ci_m50)
anova(prop_95ci_m1, prop_95ci_m2, prop_95ci_m3, prop_95ci_m4, prop_95ci_m5, prop_95ci_m6, prop_95ci_m7, prop_95ci_m8, prop_95ci_m9, prop_95ci_m10,
      prop_95ci_m11, prop_95ci_m12, prop_95ci_m13, prop_95ci_m14, prop_95ci_m15, prop_95ci_m16, prop_95ci_m19, prop_95ci_m20,
      prop_95ci_m23, prop_95ci_m24, prop_95ci_m25, prop_95ci_m26, prop_95ci_m27, prop_95ci_m28, prop_95ci_m29, prop_95ci_m30, 
      prop_95ci_m33, prop_95ci_m34, prop_95ci_m35, prop_95ci_m36, prop_95ci_m37, prop_95ci_m38, prop_95ci_m39,
      prop_95ci_m42, prop_95ci_m43, prop_95ci_m44, prop_95ci_m45, prop_95ci_m46, prop_95ci_m47, prop_95ci_m48, prop_95ci_m49, prop_95ci_m50)

# AIC comparison table - models 17,18,21,22,31,32,40,41 removed
cand.models.95ci <- list(prop_95ci_m1, prop_95ci_m2, prop_95ci_m3, prop_95ci_m4, prop_95ci_m5, prop_95ci_m6, prop_95ci_m7, prop_95ci_m8, prop_95ci_m9, prop_95ci_m10,
                        prop_95ci_m11, prop_95ci_m12, prop_95ci_m13, prop_95ci_m14, prop_95ci_m15, prop_95ci_m16, prop_95ci_m19, prop_95ci_m20,
                        prop_95ci_m23, prop_95ci_m24, prop_95ci_m25, prop_95ci_m26, prop_95ci_m27, prop_95ci_m28, prop_95ci_m29, prop_95ci_m30, 
                        prop_95ci_m33, prop_95ci_m34, prop_95ci_m35, prop_95ci_m36, prop_95ci_m37, prop_95ci_m38, prop_95ci_m39,
                        prop_95ci_m42, prop_95ci_m43, prop_95ci_m44, prop_95ci_m45, prop_95ci_m46, prop_95ci_m47, prop_95ci_m48, prop_95ci_m49, prop_95ci_m50)
cand.names.95ci <- c("prop_95ci_m1", "prop_95ci_m2", "prop_95ci_m3", "prop_95ci_m4", "prop_95ci_m5", "prop_95ci_m6", "prop_95ci_m7", "prop_95ci_m8", "prop_95ci_m9", "prop_95ci_m10",
                    "prop_95ci_m11", "prop_95ci_m12", "prop_95ci_m13", "prop_95ci_m14", "prop_95ci_m15", "prop_95ci_m16", "prop_95ci_m19", "prop_95ci_m20",
                    "prop_95ci_m23", "prop_95ci_m24", "prop_95ci_m25", "prop_95ci_m26", "prop_95ci_m27", "prop_95ci_m28", "prop_95ci_m29", "prop_95ci_m30", 
                    "prop_95ci_m33", "prop_95ci_m34", "prop_95ci_m35", "prop_95ci_m36", "prop_95ci_m37", "prop_95ci_m38", "prop_95ci_m39",
                    "prop_95ci_m42", "prop_95ci_m43", "prop_95ci_m44", "prop_95ci_m45", "prop_95ci_m46", "prop_95ci_m47", "prop_95ci_m48", "prop_95ci_m49", "prop_95ci_m50")

# Make a table of all AIC vals w weights - models 17,18,21,22,31,32,40,41 removed
t.95ci <- AICctab(prop_95ci_m1, prop_95ci_m2, prop_95ci_m3, prop_95ci_m4, prop_95ci_m5, prop_95ci_m6, prop_95ci_m7, prop_95ci_m8, prop_95ci_m9, prop_95ci_m10,
                 prop_95ci_m11, prop_95ci_m12, prop_95ci_m13, prop_95ci_m14, prop_95ci_m15, prop_95ci_m16, prop_95ci_m19, prop_95ci_m20,
                 prop_95ci_m23, prop_95ci_m24, prop_95ci_m25, prop_95ci_m26, prop_95ci_m27, prop_95ci_m28, prop_95ci_m29, prop_95ci_m30, 
                 prop_95ci_m33, prop_95ci_m34, prop_95ci_m35, prop_95ci_m36, prop_95ci_m37, prop_95ci_m38, prop_95ci_m39,
                 prop_95ci_m42, prop_95ci_m43, prop_95ci_m44, prop_95ci_m45, prop_95ci_m46, prop_95ci_m47, prop_95ci_m48, prop_95ci_m49, prop_95ci_m50, 
                 nobs=44,logLik=T, base=T, weights=T, delta=T, sort=T)
print(t.95ci)
class(t.95ci) <- "data.frame"
write.csv(t.95ci, "AIC_table_dec72019_propnmods_95ci.csv", row.names = T)

# Select top models 95% model weight
top.set.95ci <- model.sel(cand.models.95ci)
top.comp.models.95.95ci <- get.models(top.set.95ci, cumsum(weight)<=0.95)
modavg.95.95ci <- model.avg(top.comp.models.95.95ci)
summary(modavg.95.95ci)

# all model parameter estimates 
write.csv(top.set.95ci, "AICc_model_selection_table_dec72019_propnmods_95ci.csv")

# model-averaged parameter estimates
modavg.95.95ci.table <- coefTable(modavg.95.95ci, full=TRUE)
modavg.95.95ci.df <- data.frame(modavg.95.95ci.table)

write.csv(modavg.95.95ci.df, "params_full_model_averaged_95ci_models.csv")







########################################################################################################################################################

####################################################### PART II: d13C and d15N isotope Gaussian-LMMs ###################################################

########################################################################################################################################################


#--------------------------- d13C models 

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



#--------------------------- Compete models 


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
write.csv(t.C, "AIC_table_dec72019_Cmodels.csv", row.names = T)

# Select top models 95% model weight
top.set.C <- model.sel(cand.models.C)
top.comp.models.95.C <- get.models(top.set.C, cumsum(weight)<=0.95)
modavg.95.C <- model.avg(top.comp.models.95.C)
summary(modavg.95.C)

# all model parameter estimates 
write.csv(top.set.C, "AICc_model_selection_table_dec72019_Cmodels.csv")

# model-averaged parameter estimates
modavg.95.C.table <- coefTable(modavg.95.C, full=TRUE)
modavg.95.C.df <- data.frame(modavg.95.C.table)

write.csv(modavg.95.C.df, "params_full_model_averaged_Cmodels.csv")




#######################################################################################################################################################



#--------------------------- d15N models

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



#--------------------------- Compete models


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
write.csv(t.N, "AIC_table_dec72019_Nmodels.csv", row.names = T)

# Select top models 95% model weight
top.set.N <- model.sel(cand.models.N)
top.Nomp.models.95.N <- get.models(top.set.N, cumsum(weight)<=0.95)
modavg.95.N <- model.avg(top.Nomp.models.95.N)
summary(modavg.95.N)

# all model parameter estimates 
write.csv(top.set.N, "AICc_model_selection_table_dec72019_Nmodels.csv")

# model-averaged parameter estimates
modavg.95.N.table <- coefTable(modavg.95.N, full=TRUE)
modavg.95.N.df <- data.frame(modavg.95.N.table)

write.csv(modavg.95.N.df, "params_full_model_averaged_Nmodels.csv")





#######################################################################################################################################################
#######################################################################################################################################################
#######################################################################################################################################################


# COMBINED COEFFICIENT PLOT FROM ALL MODELS

# All .csv's below are saved outputs at the end of each model-fitting exercise in this script and in GLMM_dietproportions_14012019_11182019.R


#######################################################################################################################################################

#                                                                 LOAD DATA

# Median proportion parameter estimates:
params.dat <- read.csv("params_full_model_averaged_propnmods.csv")

# CI sensitivity test: 5% CI parameter estimates:
s_5ci.dat <- read.csv("params_full_model_averaged_5ci_models.csv")
# CI sensitivity test: 95% CI parameter estimates:
s_95ci.dat <- read.csv("params_full_model_averaged_95ci_models.csv")

# Isotope sensitivity test: d13C parameter estimates: 
s_d13c.dat <- read.csv("params_full_model_averaged_Cmodels.csv")
# Isotope sensitivity test: d15N parameter estimates: 
s_d15n.dat <- read.csv("params_full_model_averaged_Nmodels.csv")

#######################################################################################################################################################

#                                                                 CLEAN DATA

# Median proportion parameter estimates:
params <- params.dat %>% 
   rename(parameter = X,
          estimate = Estimate,
          se = `Std..Error`) %>%
   mutate(`Response variable` = "MixSIAR median (50%)") %>%
   print()

# CI sensitivity test: 5% CI parameter estimates:
s_5ci <- s_5ci.dat %>% 
   rename(parameter = X,
          estimate = Estimate,
          se = `Std..Error`) %>%
   mutate(`Response variable` = "Sensitivity: MixSIAR 5% CI") %>%
   print()
# CI sensitivity test: 95% CI parameter estimates:
s_95ci <- s_95ci.dat %>% 
   rename(parameter = X,
          estimate = Estimate,
          se = `Std..Error`) %>%
   mutate(`Response variable` = "Sensitivity: MixSIAR 95% CI") %>%
   print()

# Isotope sensitivity test: d13C parameter estimates: 
s_d13c <- s_d13c.dat %>% 
   rename(parameter = X,
          estimate = Estimate,
          se = `Std..Error`) %>%
   mutate(`Response variable` = "Sensitivity: Raw d13C") %>%
   print()
# Isotope sensitivity test: d15N parameter estimates: 
s_d15n <- s_d15n.dat %>% 
   rename(parameter = X,
          estimate = Estimate,
          se = `Std..Error`) %>%
   mutate(`Response variable` = "Sensitivity: Raw d15N") %>%
   print()

# Merge into one DF 
master_df <- Reduce(function(...) merge(..., all=TRUE), list(params, s_5ci, s_95ci, s_d13c, s_d15n))


#######################################################################################################################################################


#                                                                  PLOT 
dodge <- position_dodge(width=1)

# coefficient plot with intercept
ggplot(master_df, aes(x=parameter, y=estimate, group=`Response variable`, fill=`Response variable`, 
                                                             colour=`Response variable`)) +
   geom_hline(yintercept=0, linetype="dashed", size=1.2) + 
   geom_errorbar(data=master_df, aes(x=parameter, ymin=estimate-se, ymax=estimate+se), 
                 width=0, size=1.5, position=position_dodge(width=0.7)) +
   geom_point(size=5, stroke=2, shape=21, alpha=0.6, position=position_dodge(width=0.7)) +
   scale_x_discrete(labels=c("ti_biom_trap_rs"="FAB-trap", "ti_biom_site_rs"="FAB-site", "NDVI_trap_rs"="NDVI-trap", "NDVI_site_rs"="NDVI-site",
                             "ii_biom_rs"="BAB", "gnM"="male", "bsNB:gnM"="nr*male", "bsNB"="nr", "(Intercept)"="(intercept)")) +
   scale_y_continuous(limits=c(-30,10), labels=seq(-30,10,by=10)) +
   labs(x="", y="Estimate") +
   coord_flip() +
   theme_bw() +
   theme(axis.title = element_text(size=23, face = "bold"),
         axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
         axis.text = element_text(size=20, colour="black"),
         axis.ticks = element_line(size=1),
         axis.ticks.length = unit(1.5, "mm"),
         panel.grid.major = element_line(colour="gray80", size=0.6),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(size=1.1),
         legend.position = c(0.18,0.83),
         legend.background = element_rect(fill="white", colour="black"),
         legend.title = element_text(size=19, face="bold"),
         legend.text = element_text(size=15))



# coefficient plot without intercept
ggplot(master_df %>% filter(parameter != "(Intercept)"), aes(x=parameter, y=estimate, group=`Response variable`, fill=`Response variable`, 
                                                             colour=`Response variable`)) +
   geom_hline(yintercept=0, linetype="dashed", size=1.2) + 
   geom_errorbar(data=master_df %>% filter(parameter != "(Intercept)"), aes(x=parameter, ymin=estimate-se, ymax=estimate+se), 
                 width=0, size=1.5, position=position_dodge(width=0.7)) +
   geom_point(size=5, stroke=2, shape=21, alpha=0.6, position=position_dodge(width=0.7)) +
   scale_x_discrete(labels=c("ti_biom_trap_rs"="FAB-trap", "ti_biom_site_rs"="FAB-site", "NDVI_trap_rs"="NDVI-trap", "NDVI_site_rs"="NDVI-site",
                             "ii_biom_rs"="BAB", "gnM"="male", "bsNB:gnM"="nr*male", "bsNB"="nr")) +
   scale_y_continuous(limits=c(-5,5)) +
   labs(x="", y="Estimate") +
   coord_flip() +
   theme_bw() +
   theme(axis.title = element_text(size=23, face = "bold"),
         axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
         axis.text = element_text(size=20, colour="black"),
         axis.ticks = element_line(size=1),
         axis.ticks.length = unit(1.5, "mm"),
         panel.grid.major = element_line(colour="gray80", size=0.6),
         panel.grid.minor = element_blank(),
         panel.border = element_rect(size=1.1),
         legend.position = c(0.82,0.83),
         legend.background = element_rect(fill="white", colour="black"),
         legend.title = element_text(size=19, face="bold"),
         legend.text = element_text(size=15)) 

































