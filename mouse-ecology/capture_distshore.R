# Mouse capture distance from shore relative abundance comparisons
# cleaned May 2020
# k davidson

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

library(tidyverse)
library(car)      # for Anova()
library(ggpubr)   # for geom_bracket


# some minor updates Mar2021

########################################################################################################################################################

#                                                               DATA CLEANING & CALCULATIONS


########################################
# CREATE AVG +/- SE CPTURE PROPORTIONS #
########################################
# Excel file has raw number of captures (both location of first capture and including recaptures) per phase, per site and per distance group.
# The corresponding effort parameters (p, i, n, tu, s - see Nelson and Clark 1973) are also listed.
# Phase just corresponds to the fact that at some sites, due to late arrival/logistics, only one trapline was laid. The following nights, the rest of the
# grid was laid. To correct for the fact that some traplines had more effort (i.e., an extra  night of trapping), the corresponding captures are split
# by these two phases, and the relevant start dates are listed. 
# First, relative abundance (CPUE) will be calculated for each phase, site and distance group, and then added together and eventually averaged across
# sites. This is essentially all done in Stapp and Polis 2000a and b, with the exception of the phased trapping component specific to this analysis. 

#phase_data <- read.csv("CPUE_phases.csv") old
phase_data <- read.csv("CPUE_phases_genrep_08102019.csv")

phase_data <- phase_data %>% rename(sex=gender, s=s_mis)

# Remove Excel column calculations and re-organize males and females for R use
phase_data_R <- phase_data %>%
  select(site:n, s) %>% 
  #gather("sex", "a_sex", 7:8) %>%
  #arrange(site, group, phase) %>%
  #select(site:a_frstcap, sex, a_sex, p:s) %>%
  print()
# did not bother calculating CPUE to include recaps as that isn't want I'm interested in, but to do so, replace 'a_frstcap' with 'a_recap'


###################################
# CALCULATE CPUE FOR ALL CAPTURES #
###################################

# summarize as unique values because the split by sex creates duplicates 
CPUE_all <- phase_data_R %>% 
  group_by(site, phase, phase_start_date, group) %>% 
  summarize(a_frstcap = unique(a_frstcap), p=unique(p), i=unique(i), n=unique(n), s=unique(s)) %>% 
  arrange(site, group, phase) %>%
  mutate(CPUE_firstcap_ALL = a_frstcap*100 / ((p*i*n) - i*s/2) ) %>%       # equation 2 Nelson & Clarke 1973, where TU = p*i*n 
  print()
  
# Sum the CPUE across each phase to have total CPUE at each site and distance group (accounting for changing effort over time)
CPUE_groups_all <- CPUE_all %>%
  group_by(site, group) %>%
  summarize(first_caps_sum_ALL = sum(CPUE_firstcap_ALL)) %>%
  print
# this is index of abundance at each distance group for each site based on location of first capture


# Calculate proportion of captures at each distance location for each site 
# in a previous version, this was made in Excel first and then put into R. This has been re-created in R, but it corresponds to the old file
# titled 'CPUE_propns.csv'
propn_groups_all <- CPUE_groups_all %>% 
  mutate(site_n = sum(first_caps_sum_ALL)) %>% 
  group_by(site, group) %>% 
  summarize(propn = first_caps_sum_ALL/site_n) %>%
  print()

# Average proportions (+/- SE) at each distance location (for plotting only, not stats)
avg_propn_groups_all <- propn_groups_all %>%
  group_by(group) %>%
  summarize(mean_CPUEfrst = mean(propn), se_frst = (sd(propn)/sqrt(length(propn)))) %>%
  print()

# NOTE: these files and data are done by phases, taking into account the fact that we only opened 2/4 transects the first
# nights at GF and IP. These are only ADULT mice, and include location of first capture. recaptures are included initially but omitted as only the
# first capture location was of interest (Stapp and Polis 2000a and b)



#########################
# CALCULATE CPUE BY SEX #
#########################

# Calculate CPUE for each sex
CPUE_sex <- phase_data_R %>%
  mutate(CPUE_firstcap_sex = a_sex*100 / ((p*i*n) - i*s/2) ) %>%       # equation 2 Nelson & Clarke 1973, where TU = p*i*n
  print()

# Sum the CPUE across each phase to have total CPUE at each site and distance group for each sex (accounting for changing effort over time)
CPUE_groups_sex <- CPUE_sex %>%
  group_by(site, group, sex) %>%
  summarize(first_caps_sum_sex = sum(CPUE_firstcap_sex)) %>%
  print
# this is index of abundance at each distance group for each site based on location of first capture


# Calculate proportion of captures at each distance location for each site and sex
propn_groups_sex <- CPUE_groups_sex %>% 
  group_by(site) %>%
  mutate(site_n = sum(first_caps_sum_sex)) %>% 
  ungroup() %>%
  group_by(site, group, sex) %>% 
  summarize(propn = first_caps_sum_sex/site_n) %>%
  print()

# Average proportions (+/- SE) across sites at each distance location (for plotting only, not stats)
avg_propn_groups_sex <- propn_groups_sex %>%
  group_by(group, sex) %>%
  summarize(mean_propn_sex = mean(propn), se_propn_sex = (sd(propn)/sqrt(length(propn)))) %>%
  print()



########################################################################################################################################################

#                                                               STATISTICAL COMPARISONS 

# old file read in: cpuepropn <- read.csv("CPUE_propns.csv")
# new dataframe to use is: 'propn_groups' 


#------------------------------------------------- ONE-WAY ANOVAS 

####################
# PROPORTION TESTS #
####################

#####
# TEST ASSUMPTIONS - normality and equal variance for first captures
# here propn_groups_all$propn is based on first captures 
#####
shapiro.test(propn_groups_all$propn)                                  # p = 0.55 therefore normal
bartlett.test(propn_groups_all$propn ~ propn_groups_all$group)        # p = 0.26 therefore equal variances 

res1<-resid(lm(propn_groups_all$propn ~ propn_groups_all$group))
plot(res1)  # possible pattern to residuals
hist(res1)  # fairly normal
qqnorm(res1)
qqline(res1)
shapiro.test(res1)
plot(lm(propn_groups_all$propn ~ propn_groups_all$group))

# arcsine transform and assess if it is better (Stapp and Polis 2000a and b performed arcsine transformation to proportions)
propn_groups_all$asin_propn <- asin(propn_groups_all$propn)
res2<-resid(lm(propn_groups_all$asin_propn ~ propn_groups_all$group))
plot(res2)
hist(res2)
qqnorm(res2)
qqline(res2)
shapiro.test(res2)
plot(lm(propn_groups_all$asin_propn ~ propn_groups_all$group))
# arcsine doesn't really change anything and original raw proportion data were OK, so maintain that

#####
# One-way Anova based on location of first capture
##### 
frstcap_aov <- aov(propn_groups_all$propn ~ propn_groups_all$group)
summary(frstcap_aov)     # p = 0.0496 
TukeyHSD(frstcap_aov)
# 0-25 vs. 150-200         p = 0.0498

#####
# Two-way Anova taking site into account - just for interest
#####
frstcap_2aov <- aov(propn_groups_all$propn ~ propn_groups_all$group + propn_groups_all$site)
summary(frstcap_2aov) 
TukeyHSD(frstcap_2aov)


##############
# CPUE TESTS #
##############

#####
# TEST ASSUMPTIONS - normality and equal variance for first captures
# here propn_groups$propn is based on first captures 
#####
shapiro.test(CPUE_groups_all$first_caps_sum_ALL)                                   
bartlett.test(CPUE_groups_all$first_caps_sum_ALL ~ CPUE_groups_all$group)         

res1<-resid(lm(CPUE_groups_all$first_caps_sum_ALL ~ CPUE_groups_all$group))
plot(res1)  # possible pattern to residuals
hist(res1)  # not normal
qqnorm(res1)
qqline(res1)
shapiro.test(res1)    
plot(lm(CPUE_groups_all$first_caps_sum_ALL ~ CPUE_groups_all$group))

# log transform and assess if it is better (Stapp and Polis 2000a and b performed arcsine transformation to proportions)
CPUE_groups_all$log_cpue <- log(CPUE_groups_all$first_caps_sum_ALL+1)
res2<-resid(lm(CPUE_groups_all$log_cpue ~ CPUE_groups_all$group))
plot(res2)
hist(res2)
qqnorm(res2)
qqline(res2)
shapiro.test(res2)   # p = 0.07 improved but not awesome 
plot(lm(CPUE_groups_all$log_cpue ~ CPUE_groups_all$group))

#####
# Anova based on location of first capture
##### 
frstcap_cpue_aov <- aov(CPUE_groups_all$log_cpue ~ CPUE_groups_all$group)
summary(frstcap_cpue_aov)    # p = 0.11 
TukeyHSD(frstcap_cpue_aov)
# 0-25 vs. 150-200         p = 0.10



#------------------------------------------------- TWO-WAY ANOVAS - including sex


####################
# PROPORTION TESTS #
####################

#####
# TEST ASSUMPTIONS - normality and equal variance for first captures
# here propn_groups_sex$propn is based on first captures 
#####
res1<-resid(lm(propn_groups_sex$propn ~ propn_groups_sex$group + propn_groups_sex$sex))
plot(res1)  # possible pattern to residuals
hist(res1)  # fairly normal
qqnorm(res1)
qqline(res1)
shapiro.test(res1)    # p = 0.45 therefore normal
plot(lm(propn_groups_sex$propn ~ propn_groups_sex$group + propn_groups_sex$sex))

# arcsine transform and assess if it is better (Stapp and Polis 2000a and b performed arcsine transformation to proportions)
propn_groups_sex$asin_propn <- asin(propn_groups_sex$propn)
res2<-resid(lm(propn_groups_sex$asin_propn ~ propn_groups_sex$group + propn_groups_sex$sex))
plot(res2)
hist(res2)   # possibly more normal
qqnorm(res2)
qqline(res2)
shapiro.test(res2)   # p = 0.42 therefore normal but no real improvement
plot(lm(propn_groups_sex$asin_propn ~ propn_groups_sex$group + propn_groups_sex$sex))
# arcsine doesn't really change anything and original raw proportion data were OK, so maintain that

#####
# Two-way Anova based on location of first capture  **MS stats**
##### 
options(contrasts = c("contr.sum","contr.poly"))
propn_groups_sex$sex <- as.factor(propn_groups_sex$sex)
summary(frstcapsex_2aov <- aov(propn_groups_sex$propn ~ propn_groups_sex$group + propn_groups_sex$sex))
Anova(lm(propn_groups_sex$propn ~ propn_groups_sex$group + propn_groups_sex$sex), type = "II")
TukeyHSD(frstcapsex_2aov)


##############
# CPUE TESTS #
##############

#####
# TEST ASSUMPTIONS - normality and equal variance for first captures
# here CPUE_groups_sex$propn is based on first captures 
#####
shapiro.test(CPUE_groups_sex$first_caps_sum_sex + CPUE_groups_sex$sex)                              # p = 0.09 therefore normal
bartlett.test(CPUE_groups_sex$first_caps_sum_sex ~ CPUE_groups_sex$group + CPUE_groups_sex$sex)         # p = 0.31 therefore equal variances 

res1<-resid(lm(CPUE_groups_sex$first_caps_sum_sex ~ CPUE_groups_sex$group + CPUE_groups_sex$sex))
plot(res1)  # possible pattern to residuals
hist(res1)  # not normal
qqnorm(res1)
qqline(res1)
shapiro.test(res1)    
plot(lm(CPUE_groups_sex$first_caps_sum_sex ~ CPUE_groups_sex$group))

# log transform and assess if it is better 
CPUE_groups_sex$log_cpue <- log(CPUE_groups_sex$first_caps_sum_sex+1)
res2<-resid(lm(CPUE_groups_sex$log_cpue ~ CPUE_groups_sex$group))
plot(res2)
hist(res2)
qqnorm(res2)
qqline(res2)
shapiro.test(res2)   # p = 0.07 improved but not awesome 
plot(lm(CPUE_groups_sex$log_cpue ~ CPUE_groups_sex$group))

#####
# Anova based on location of first capture 
##### 
options(contrasts = c("contr.sum","contr.poly"))
frstcap_cpue_aov <- aov(CPUE_groups_sex$log_cpue ~ CPUE_groups_sex$group + CPUE_groups_sex$sex)
aov_unb <- Anova(lm(CPUE_groups_sex$log_cpue ~ CPUE_groups_sex$group + CPUE_groups_sex$sex), type=2)   # no difference between types 2 and 3
summary(aov_unb)     
TukeyHSD(aov_unb)
 








#######################################################################################################################################################

#                                                               PLOTS

#######
# ALL #
#######

#----- PROPORTIONS #

# data file: 'avg_propn_groups' created in last pipe loop above
avg_propn_groups_all$stat <- c("a","ab","b","ab")
avg_propn_groups_all$group <- factor(avg_propn_groups_all$group, levels = c("0-25", "50-75", "100-125", "150-200"), ordered=T)

# FIRST CAPTURE plot - white for MS 
ggplot(avg_propn_groups_all, aes(x=group, y=mean_CPUEfrst)) + 
  labs(x="Distance from beach (m)", y="Proportion of captures") +
  scale_y_continuous(limits=c(0, 1.0), breaks=seq(0, 1.0, by = 0.2))  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  geom_bar(stat = "identity", fill = "gray") +
  #geom_text(aes(label=stat), vjust=-4,size=12) +
  geom_errorbar(data = avg_propn_groups, aes(ymin = mean_CPUEfrst - se_frst,
                                ymax = mean_CPUEfrst + se_frst), width=.09, size=1, colour="black")  +
  #annotate(geom="text", x=0.52, y=1.1, label="A", color="black", size=8, fontface=2) +
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", size=1.2))+
    theme(panel.grid.minor = element_blank()) + 
    theme(panel.grid.major = element_blank()) + 
    theme(axis.text = element_text(colour = "black", size=30)) + 
    theme(axis.title.x = element_text(margin = margin(t=6, r=0, b=0, l=0), colour = "black", size=34)) +
    theme(axis.title.y = element_text(margin = margin(t=0, r=7, b=0, l=0), colour = "black", size=34)) +
    theme(axis.ticks = element_line(size = 1, colour = "black")) +
    theme(axis.ticks.length = unit(0.2, "cm")) +
    theme(legend.position=c(0.87,0.85))+ 
    theme(legend.background = element_rect(colour="black")) +
    theme(legend.spacing.y = unit(0.1, "cm")) 


#----- RAW CPUE #

# data file: 'avg_propn_groups' created in last pipe loop above
CPUE_raw_all <- CPUE_groups_all %>% 
  group_by(group) %>%
  summarize(CPUE_mean_all = sum(first_caps_sum_ALL), se = (sd(first_caps_sum_ALL)/sqrt(length(first_caps_sum_ALL)))) %>%
  print()

CPUE_raw_all$stat <- c("a","ab","b","ab")
CPUE_raw_all$group <- factor(CPUE_raw_all$group, levels = c("0-25", "50-75", "100-125", "150-200"), ordered=T)

# FIRST CAPTURE plot - white for MS 
ggplot(CPUE_raw_all, aes(x=group, y=CPUE_mean_all)) + 
  labs(x="Distance from beach (m)", y="Relative abundance (CPUE)") +
  #scale_y_continuous(limits=c(0, 1.0), breaks=seq(0, 1.0, by = 0.2))  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  geom_bar(stat = "identity", fill = "gray") +
  #geom_text(aes(label=stat), vjust=-4,size=12) +
  geom_errorbar(data = CPUE_raw_all, aes(ymin = CPUE_mean_all - se,
                                             ymax = CPUE_mean_all + se), width=.09, size=1, colour="black")  +
  #annotate(geom="text", x=0.52, y=1.1, label="A", color="black", size=8, fontface=2) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1.2))+
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(axis.text = element_text(colour = "black", size=30)) + 
  theme(axis.title.x = element_text(margin = margin(t=6, r=0, b=0, l=0), colour = "black", size=34)) +
  theme(axis.title.y = element_text(margin = margin(t=0, r=7, b=0, l=0), colour = "black", size=34)) +
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(legend.position=c(0.87,0.85))+ 
  theme(legend.background = element_rect(colour="black")) +
  theme(legend.spacing.y = unit(0.1, "cm")) 


#######
# SEX #
#######

#----- PROPORTIONS #

# data file: 'avg_propn_groups_sex' created in last pipe loop above
#avg_propn_groups_sex2 <- avg_propn_groups_sex %>% 
#  mutate(sex = ifelse(sex=="a_frstcap_females", "Females", "Males")) %>%
#  rename(Sex = sex) %>%
#  print()
# ****** FIGURE 2: 
avg_propn_groups_sex$group <- factor(avg_propn_groups_sex$group, levels = c("0-25", "50-75", "100-125", "150-200"), ordered=T)
#avg_propn_groups_sex$stat <- c("a", "ab","b","ab")

# FIRST CAPTURE plot - white for MS 
ggplot(avg_propn_groups_sex, aes(x=group, y=mean_propn_sex, group=sex)) + 
  geom_bar(aes(fill=Sex), stat="identity", position="dodge", colour="black", size=1.5) + 
  geom_bracket(xmin=0.5, xmax=1.5, y.position=0.45, label="a", size=0.6, label.size=8, tip.length=0.01) +
  geom_bracket(xmin=1.6, xmax=2.52, y.position=0.35, label="ab", size=0.6, label.size=8, tip.length=0.01) +
  geom_bracket(xmin=2.59, xmax=3.5, y.position=0.35, label="ab", size=0.6, label.size=8, tip.length=0.01) +
  geom_bracket(xmin=3.6, xmax=4.5, y.position=0.3, label="b", size=0.6, label.size=8, tip.length=0.01) +  
  scale_y_continuous(limits=c(0, 0.5), breaks=seq(0, 1.0, by = 0.1))  +
  scale_fill_manual(values=c("gray20", "gray80")) +
  geom_errorbar(data = avg_propn_groups_sex, aes(ymin = mean_propn_sex - se_propn_sex,
                                             ymax = mean_propn_sex + se_propn_sex), width=0, size=1, colour="black", position=position_dodge(width=0.9))  +
  labs(x="Distance from beach (m)", y="Proportion of captures") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black", size=30),
        axis.title.x = element_text(margin = margin(t=6, r=0, b=0, l=0), colour = "black", face="bold", size=34),
        axis.title.y = element_text(margin = margin(t=0, r=7, b=0, l=0), colour = "black", face="bold", size=34),
        axis.ticks = element_line(size = 1, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.text = element_text(size=30),
        legend.title = element_blank(),
        legend.position=c(0.86,0.85),
        legend.background = element_rect(colour="black", size=0.8),
        legend.spacing.y = unit(0.1, "cm")) 


#----- RAW CPUE #

# data file: 'CPUE_groups_sex' created in last pipe loop above
CPUE_raw_sex <- CPUE_groups_sex %>% 
  group_by(group, sex) %>%
  summarize(CPUE_mean_sex = sum(first_caps_sum_sex), se_sex = (sd(first_caps_sum_sex)/sqrt(length(first_caps_sum_sex)))) %>%
  print()

#CPUE_raw_all$stat <- c("a","ab","b","ab")
CPUE_raw_sex$group <- factor(CPUE_raw_sex$group, levels = c("0-25", "50-75", "100-125", "150-200"), ordered=T)

# FIRST CAPTURE plot - white for MS 
ggplot(CPUE_raw_sex, aes(x=group, y=CPUE_mean_sex, group=sex)) + 
  labs(x="Distance from beach (m)", y="Relative abundance (CPUE)") +
  geom_bar(aes(fill=sex), position = "dodge", stat="identity", colour="black", size=1.5) + 
  #geom_text(aes(label=stat), vjust=-4,size=12) +
  geom_errorbar(data = CPUE_raw_sex, aes(ymin = CPUE_mean_sex - se_sex,
                                         ymax = CPUE_mean_sex + se_sex), width=.09, size=1, colour="black", position = position_dodge(width=0.9))  +
  #annotate(geom="text", x=0.52, y=1.1, label="A", color="black", size=8, fontface=2) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1.2))+
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(axis.text = element_text(colour = "black", size=30)) + 
  theme(axis.title.x = element_text(margin = margin(t=6, r=0, b=0, l=0), colour = "black", size=34)) +
  theme(axis.title.y = element_text(margin = margin(t=0, r=7, b=0, l=0), colour = "black", size=34)) +
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(legend.position=c(0.87,0.85))+ 
  theme(legend.background = element_rect(colour="black")) +
  theme(legend.spacing.y = unit(0.1, "cm")) 


#



















####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### ####### 

####################
# MALES VS FEMALES #
###################






# remake figure 7 from thesis for MS submission jun-2020

# raw trapping data
trapdat <- read.csv("Livetraps_subset_no100isls_locnfirstcapture.csv")

trapdat$Cap_date <- as.Date(trapdat$Cap_date, format = "%d-%b-%y")

genprn <- trapdat %>%
  mutate(dist_class = ifelse(Dist_Shore == "0", "0-25", 
                             ifelse(Dist_Shore == "25", "0-25",
                                    ifelse(Dist_Shore == "50", "50-75",
                                           ifelse(Dist_Shore == "75", "50-75",
                                                  ifelse(Dist_Shore == "100", "100-125",
                                                         ifelse(Dist_Shore == "125", "100-125", 
                                                                ifelse(Dist_Shore == "150", "150-200",
                                                                       ifelse(Dist_Shore == "175", "150-200", "150-200"))))))))) %>%
  filter(ID != "no mark-esc") %>%
  group_by(dist_class, Sex) %>% 
  summarize(n=n()) %>% 
  ungroup() %>%
  mutate(propn=n/sum(n)) %>%
  print()

genprn$dist_class <- factor(genprn$dist_class, levels = c("0-25", "50-75", "100-125", "150-200", ordered=T))

ggplot(genprn, aes(x=dist_class, y=propn, fill=Sex)) + 
  geom_bar(stat="identity", colour="black", size=0.8) +
  scale_fill_manual(values = c("gray80", "gray20"), name = "Sex", labels = c("Female", "Male")) +
  #annotate(geom="text", x=0.55, y=1, label="B", color="black", size=6, fontface=2) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.2)) +
  labs(x="Distance from beach (m)", y="Proportion of captures") +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1.2))+
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(axis.text = element_text(colour = "black", size=30)) + 
  theme(axis.title.x = element_text(margin = margin(t=6, r=0, b=0, l=0), colour = "black", size=34)) +
  theme(axis.title.y = element_text(margin = margin(t=0, r=7, b=0, l=0), colour = "black", size=34)) +
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(legend.position=c(0.86,0.85))+ 
  theme(legend.background = element_rect(colour="black", size=1)) +
  theme(legend.spacing.y = unit(0.1, "cm"),
        legend.text = element_text(size=30),
        legend.title = element_text(size=34)) 






