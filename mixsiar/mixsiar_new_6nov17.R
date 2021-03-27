############# MixSIAR separate region models 

# am running separate models for each region (CV,GS) due to significant differences in prey signatures between GS and CV 

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

# create separate data frames based on regions 
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)

iso_db <- read.csv("invert_veg_iso_mouse_6nov17.csv")

#############
# CONSUMERS #
#############

# CV consumer
mouse_CV <- iso_db %>% 
  group_by(region) %>% 
  filter(ID == "mouse", region == "CV") %>% 
  print(mouse_CV) %T>%
  write.csv("mouse_CV.csv")

# GS consumer
mouse_GS <- iso_db %>% 
  group_by(region) %>% 
  filter(ID == "mouse",  region == "GS") %>% 
  print(mouse_GS) %T>%
  write.csv("mouse_GS.csv")


###########
# SOURCES #
###########

# CV source 
iso_CV <- iso_db %>% 
  group_by(region, guild2) %>% 
  filter(region =="CV", ID != "mouse", ID != "mushroom", ID != "lichen/fungus", ID != "slug", ID != "myanthemum") %>%
  summarize(Meand13C = mean(d13C), Meand15N = mean(d15N), SDd13C = sd(d13C), SDd15N = sd(d15N), n = length(d13C_FINAL)) %>%
  print(iso_CV) %T>%
  write.csv("CV_preygroups.csv")

# GS source 
iso_GS <- iso_db %>% 
  group_by(region, guild2) %>% 
  filter(region =="GS", ID != "mouse", ID != "mushroom", ID != "grass", ID !="sedge", ID != "slug", ID != "bunchberry") %>% 
  summarize(Meand13C = mean(d13C), Meand15N = mean(d15N), SDd13C = sd(d13C), SDd15N = sd(d15N), n = length(d13C_FINAL)) %>%
  print(iso_GS) %T>%
  write.csv("GS_preygroups.csv")

#####
# SIMPLIFIED SOURCES - CJZ rejection said should try grouping herbs+carns to basically just have 3 sources: veg, terrestrial inverts, intertidal inverts.
# here are the simplified sources, done on Nov 2, 2019
#####

# create new column for easy simplification 
iso_db <- iso_db %>% 
  mutate(simple_source = paste(paste(gsub("-", "-", taxa)), location, sep="-")) 

# CV source
iso_CV_s <- iso_db %>% 
  group_by(region, simple_source) %>% 
  filter(region =="CV", ID != "mouse", ID != "mushroom", ID != "lichen/fungus", ID != "slug", ID != "myanthemum") %>%
  summarize(Meand13C = mean(d13C), Meand15N = mean(d15N), SDd13C = sd(d13C), SDd15N = sd(d15N), n = length(d13C_FINAL)) %>%
  print(iso_CV) %T>%
  write.csv("CV_preygroups_simple.csv", row.names = F)

ggplot(iso_CV, aes(x=meand13C, y=meand15N, colour=simple_source)) +
  geom_point() + 
  geom_errorbar(data=iso_CV, aes(ymin=meand15N-sdd15N, ymax=meand15N+sdd15N, width=0.2)) +
  geom_errorbarh(data=iso_CV, aes(xmin=meand13C-sdd13C, xmax=meand13C+sdd13C, height=0.2))


# GS source 
iso_GS_s <- iso_db %>% 
  group_by(region, simple_source) %>% 
  filter(region =="GS", ID != "mouse", ID != "mushroom", ID != "grass", ID !="sedge", ID != "slug", ID != "bunchberry") %>% 
  summarize(Meand13C = mean(d13C), Meand15N = mean(d15N), SDd13C = sd(d13C), SDd15N = sd(d15N), n = length(d13C_FINAL)) %>%
  print(iso_GS) %T>%
  write.csv("GS_preygroups_simple.csv", row.names = F)

ggplot(iso_GS, aes(x=Meand13C, y=Meand15N, colour=simple_source)) +
  geom_point() + 
  geom_errorbar(data=iso_GS, aes(ymin=Meand15N-SDd15N, ymax=Meand15N+SDd15N, width=0.2)) +
  geom_errorbarh(data=iso_GS, aes(xmin=Meand13C-SDd13C, xmax=Meand13C+SDd13C, height=0.2))


##################
# DISCRIMINATION #
##################

# manually put all files into C:\Users\katie\R\win-library\3.3\MixSIAR\files\rodent_prey_FIELD



################################################################################## MIXSIAR ###################################################################

######
# CV #
######

library(MixSIAR)
mixsiar.dir <- find.package("MixSIAR")
paste0(mixsiar.dir)

# CV CONSUMER
CV.mix <- system.file("files","mouse_CV.csv", package = "MixSIAR")

# Load the mixture/consumer data
CVmix = load_mix_data(filename=CV.mix, 
                    iso_names=c("d13C","d15N"), 
                    factors=c(NULL),                     # set to "Sample ID" for individual estimates. NULL for CV vs. GS overall %
                    fac_random=c(NULL),                     # set to TRUE for individual estimates. NULL for CV vs GS overall %s
                    fac_nested=c(NULL), 
                    cont_effects=NULL)

# CV SOURCES
# Replace the system.file call with the path to your file
CVsource.filename <- system.file("files", "CV_preygroups_simple.csv", package = "MixSIAR")
CVsource.filename2 <- system.file("files", "CV_preygroups.csv", package="MixSIAR")

# Load the source data
CVsource <- load_source_data(filename=CVsource.filename,
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           CVmix)
CVsource2 <- load_source_data(filename=CVsource.filename2,
                            source_factors=NULL,
                            conc_dep = FALSE,
                            data_type="means", 
                            CVmix)

# DISCRIMINATION
# Replace the system.file call with the path to your file
discr.filename <- system.file("files", "mouse_discrimination_simple.csv", package = "MixSIAR")
discr.filename2 <- system.file("files", "mouse_discrimination.csv", package = "MixSIAR")

# Load the discrimination/TDF data
discr <- load_discr_data(filename=discr.filename, CVmix)
discr2 <- load_discr_data(filename=discr.filename2, CVmix)

###################
# isospace plot 
plot_data(filename="isospace_plot_prelim", plot_save_pdf=FALSE, plot_save_png=FALSE, CVmix,CVsource,discr)
plot_data(filename="isospace_plot_prelim", plot_save_pdf=FALSE, plot_save_png=FALSE, CVmix,CVsource2,discr2)

# Calculate the convex hull area, standardized by source variance
library(splancs)
calc_area(source=source,mix=mix,discr=discr)


###################
# priors
# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
plot_prior(alpha.prior=1,source)


###################
# Write the JAGS model file
CV_model <- "CV_regional_simplemodel_verylong.txt"     # Name of the JAGS model file
resid_err <- T
process_err <- TRUE
write_JAGS_model(CV_model, resid_err, process_err, CVmix, CVsource)

run <- list(chainLength=3000000, burn=1500000, thin=500, chains=3, calcDIC=TRUE)     # can be used in place of run="very short" in run_model() below. This is just a manual version of the pre-loaded options. 


###################
# RUN MODELS!

jags_CV_model <- run_model(run="very long", CVmix, CVsource, discr, CV_model,
                          alpha.prior = 1, resid_err, process_err)


###################
# Output 

output_options <- list(summary_save = TRUE,
                       summary_name = "mixsiar_CV_regional_simplemodel_verylong_summary",
                       sup_post = TRUE,
                       plot_post_save_pdf = F,
                       plot_post_name = "mixsiar_CV_regional_simplemodel_verylong_postplot",
                       sup_pairs = TRUE,
                       plot_pairs_save_pdf = F,
                       plot_pairs_name = "mixsiar_CV_regional_simplemodel_verylong_pairsplot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = F,
                       plot_xy_name = "mixsiar_CV_regional_simplemodel_verylong_xyplot",
                       gelman = TRUE,
                       heidel = TRUE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "mixsiar_CV_regional_simplemodel_verylong_diagnostics",
                       indiv_effect = F,
                       plot_post_save_png = F,
                       plot_pairs_save_png = F,
                       plot_xy_save_png = F)

options(max.print=99999)
output_JAGS(jags_CV_model, CVmix, CVsource, output_options)







######
# GS #
######

library(MixSIAR)
mixsiar.dir <- find.package("MixSIAR")
paste0(mixsiar.dir)

# GS consumer
GS.mix <- system.file("files","mouse_GS.csv", package = "MixSIAR")

# Load the mixture/consumer data
GSmix = load_mix_data(filename=GS.mix, 
                    iso_names=c("d13C","d15N"), 
                    factors=c(NULL), 
                    fac_random=c(NULL), 
                    fac_nested=c(NULL), 
                    cont_effects=NULL)

# GS sources
# Replace the system.file call with the path to your file
GSsource.filename <- system.file("files", "GS_preygroups_simple.csv", package = "MixSIAR")
GSsource.filename2 <- system.file("files", "GS_preygroups.csv", package = "MixSIAR")

# Load the source data
GSsource <- load_source_data(filename=GSsource.filename,
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           GSmix)
GSsource2 <- load_source_data(filename=GSsource.filename2,
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           GSmix)

# Discrimination
# Replace the system.file call with the path to your file
discr.filename <- system.file("files", "mouse_discrimination_simple.csv", package = "MixSIAR")
discr.filename2 <- system.file("files", "mouse_discrimination.csv", package = "MixSIAR")

# Load the discrimination/TDF data
discr <- load_discr_data(filename=discr.filename, GSmix)
discr2 <- load_discr_data(filename=discr.filename2, GSmix)


###################
## isospace plot 
plot_data(filename="isospace_plot_prelim", plot_save_pdf=FALSE, plot_save_png=FALSE, GSmix,GSsource,discr)
plot_data(filename="isospace_plot_prelim", plot_save_pdf=FALSE, plot_save_png=FALSE, GSmix,GSsource2,discr2)

# Calculate the convex hull area, standardized by source variance
library(splancs)
calc_area(source=source,mix=mix,discr=discr)


###################
# priors
# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
plot_prior(alpha.prior=1,source)


###################
# Write the JAGS model file
GS_model <- "GS_regional_simplemodel_verylong.txt"     # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(GS_model, resid_err, process_err, GSmix, GSsource)

run <- list(chainLength=3000000, burn=1500000, thin=500, chains=3, calcDIC=TRUE)


###################
# RUN MODELS!

jags_GS_model <- run_model(run="very long", GSmix, GSsource, discr, GS_model,
                          alpha.prior = 1, resid_err, process_err)


###################
# Output 

GSoutput_options <- list(summary_save = TRUE,
                       summary_name = "mixsiar_GS_regional_simplemodel_verylong_summary",
                       sup_post = T,
                       plot_post_save_pdf = F,
                       plot_post_name = "mixsiar_GS_regional_simplemodel_verylong_postplot",
                       sup_pairs = T,
                       plot_pairs_save_pdf = F,
                       plot_pairs_name = "mixsiar_GS_regional_simplemodel_verylong_pairsplot",
                       sup_xy = T,
                       plot_xy_save_pdf = F,
                       plot_xy_name = "mixsiar_GS_regional_simplemodel_verylong_xyplot",
                       gelman = TRUE,
                       heidel = TRUE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "mixsiar_GS_regional_simplemodel_verylong_diagnostics",
                       indiv_effect = F,
                       plot_post_save_png = F,
                       plot_pairs_save_png = F,
                       plot_xy_save_png = F)

options(max.print=99999)
output_JAGS(jags_GS_model, GSmix, GSsource, GSoutput_options)












