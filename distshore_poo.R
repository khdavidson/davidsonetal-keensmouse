# Mouse faecal trends from shoreline to interior
# k davidson
# building on 2015 data may 2020
# archival script: distancefromshore_isopoo.R

library(tidyverse)
library(broom)

setwd("~/UVic/`Hakai 100 Islands 2015/`DATA/Isotopes")

poo.data = read.csv("Tray70bRversion.csv")

########################################################################################################################################################

#                                                             DATA CLEANING 


# Although the C:N and other C normalization values were calculated in Excel, I want to re-do here for transparency & reproducibility. 
# Information used here relies on Post et al 2007 Oecologia 152(1): 179-189

# Clean column names, re-calculate
poo.data <- poo.data %>% 
  rename(sample_ID = Sample.ID,
         C_amount = C.Amount..ug.,
         N_amount = N.Amount..ug.,
         CN_ratio = C.N) %>%
  mutate(CN_ratio = C_amount/N_amount) %>% 
  mutate(dist_group = ifelse(dist_beach =="0"|dist_beach=="25", "0-25", 
                             ifelse(dist_beach=="50"|dist_beach=="75", "50-75",
                                    ifelse(dist_beach=="100"|dist_beach=="125", "100-125", "150-200")))) %>%
  print()


########################################################################################################################################################

#                                                             PILOT STATISTICAL COMPARISONS

###################
# SUBSTRATE TESTS #
###################

# Faecal samples were part of a pilot project, so they were collected at S. Gosling Isl. (what ended up being one of the main sites) and a cobble beach on
# the east side of Gosling Isl. 
# Increasing sample size to include both sites would be great - provided there are not significant differences as one beach was sandy and the other was
# cobble. 

# See what the mean +/- SD signatures are 
substrate <- poo.data %>% 
  group_by(substrate) %>% 
  summarize(meanC = mean(d13C), sdC = sd(d13C), meanN = mean(d15N), sdN = (sd(d15N))) %>% 
  print()
# cobble C values are more enriched than sand, but N values are more depleted than sand. Significant? 

# linear models 
lm1 <- lm(poo.data$d13C ~ poo.data$substrate)
r1 <- resid(lm1)
hist(r1)
qqnorm(r1)
qqline(r1)
summary(lm1)

lm2 <- lm(poo.data$d15N ~ poo.data$substrate)
r2 <- resid(lm2)
hist(r2)
qqnorm(r2)
qqline(r2)
summary(lm2)
# both models indicate significant difference in the collective signature, but it's possible it's an artifact of where the samples were collected because
# we know shoreline is more enriched - so if one site has more/less shoreline or inland samples, there would be a significant difference. Really need to
# test each distance interval against each other 

# run lms over each distance grouping - omitted 0, 25 and 50 as they didn't have enough observations (1 level factor throws error)
C_lms <- poo.data %>%
  filter(dist_beach != "0", dist_beach != "25", dist_beach != "50") %>%
  group_by(dist_beach) %>%
  do(glance(lm(d13C ~ substrate, data = .))) %>%
  print()

# there is a significant difference at 75, 175 and 200m, but there is still an obvious declining trend overall, and due to low sample size it isn't clear
# what is artifact and what could be important. 

# therefore I suggest proceeding with both sites included but with separate symbols for each beach and clear explanation that as part of a pilot study 
# this extra site was included. 



##################
# C:N over space #
##################

ggplot(poo.data, aes(x=dist_beach, y=CN_ratio)) +
  geom_point()

lm_cn <- lm(poo.data$CN_ratio ~ poo.data$dist_beach)
rcn <- resid(lm_cn)
hist(rcn)
qqnorm(rcn)
qqline(rcn)
aov_cn <- aov(poo.data$CN_ratio ~ poo.data$dist_beach)
summary(aov_cn)
# no pattern/significance really 



#######################################################################################################################################################

#                                                           STATISTICAL COMPARISONS

# Stats for overall trend (pooled, not separated by site i.e. substrate) of [isotope] ~ distance from shore

# ----------------------------- NITROGEN 

lmN <- lm(poo.data$d15N ~ poo.data$dist_group)
aovN <- aov(poo.data$d15N ~ poo.data$dist_group)
rN <- resid(lmN)
hist(rN)
qqnorm(rN)
qqline(rN)
summary(lmN)
summary(aovN)
TukeyHSD(aovN)


# ----------------------------- CARBON 

lmC <- lm(poo.data$d13C ~ poo.data$dist_group)
aovC <- aov(poo.data$d13C ~ poo.data$dist_group)
rC <- resid(lmC)
hist(rC)
qqnorm(rC)
qqline(rC)
summary(lmC)
summary(aovC)


#######################################################################################################################################################

#                                                           PLOTS



# Summarize mean +/- SE for each site (i.e., substrate) -- for plotting only 
poo.summary <- poo.data %>% 
  group_by(dist_group) %>% 
  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), 
            meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N)),
            n=n()) %>% 
 #mutate(label = ifelse(n==1, "*", "")) %>%
 #ungroup() %>%
 #mutate(site = ifelse(substrate=="Sand", "GOS", "GOS-E")) %>%
  print()
poo.summary$site <- factor(poo.summary$site, levels=c("GOS", "GOS-E"), ordered=T)
poo.summary$dist_group <- factor(poo.summary$dist_group, levels=c("0-25", "50-75", "100-125", "150-200"), ordered=T)


# ----------------------------- NITROGEN 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=poo.summary, aes(x=dist_group, y=meanN)) + 
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  geom_errorbar(aes(ymin = meanN-seN, ymax = meanN+seN), width=0, size=1.5) + 
  geom_point(colour="black", size=7, stroke=1.3) +
 # scale_shape_manual(values=c(21, 24)) +
 # scale_fill_manual(values=c("gray10", "gray60")) +
#  scale_colour_manual(values=c("gray10", "gray60")) +
  scale_y_continuous(breaks=seq(-1,10,by=2), labels=scaleFUN) +
  theme_bw() +
  theme(axis.title = element_text(size=30, face = "bold"),
        axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
        axis.text = element_text(size=27, colour="black"),
        axis.ticks = element_line(size=1),
        axis.ticks.length = unit(1.5, "mm"),
        panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.1),
        legend.text = element_text(size=27), 
        legend.title = element_blank(),
        legend.position = c(0.85,0.86),
        legend.background = element_rect(colour="black", size=0.8),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(t=-2,b=4,l=5,r=8)) #+
  #geom_text(data=summary %>% filter(label=="*"), aes(label=label), nudge_x=0.1, nudge_y=-0.15, colour="white", size=13)


# ----------------------------- CARBON 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=poo.summary, aes(x=dist_group, y=meanC)) + 
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  geom_errorbar(aes(ymin = meanC-seC, ymax = meanC+seC), width=0, size=1.5, alpha=0.7) + 
  geom_point(colour="black", size=7, stroke=1.3) +
  #scale_shape_manual(values=c(21, 24)) +
  #scale_fill_manual(values=c("gray10", "gray60")) +
  #scale_colour_manual(values=c("gray10", "gray60")) +
  scale_y_continuous(limits=c(-29, -26.5), labels=scaleFUN) +
  theme_bw() +
  theme(axis.title = element_text(size=30, face = "bold"),
        axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
        axis.text = element_text(size=27, colour="black"),
        axis.ticks = element_line(size=1),
        axis.ticks.length = unit(1.5, "mm"),
        panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.1),
        legend.text = element_text(size=27), 
        legend.title = element_blank(),
        legend.position = c(0.88,0.88),
        legend.background = element_rect(colour="black", size=0.8),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(t=-2,b=4,l=5,r=8)) #+
  #geom_text(data=summary %>% filter(label=="*"), aes(label=label), nudge_x=0, nudge_y=-0.032, colour="white", size=13)
  



#=========================================================================================== COMBO PLOTS

# Combo plots of HAIR + POO
# must have distshore_hair script open and have ran all of the intermediate tables prior to re-making these plots
# data to use for hair: hair.summary     for poo: poo.summary
# this is also duplicatedin the distshore_poo script

poo.summary <- poo.summary %>%
  select(dist_group:seN) %>%
  mutate(source="Faeces") %>%
  print()

hair.summary <- hair.summary %>%
  mutate(source="Hair") %>%
  print()

combo.summary <- rbind(poo.summary, hair.summary)
combo.summary <- combo.summary %>%
  arrange(desc(source)) %>%
  print
combo.summary$source <- factor(combo.summary$source, levels=c("Faeces", "Hair"), ordered=T)


# ----------------------------- NITROGEN 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=combo.summary, aes(x=dist_group, y=meanN)) + 
  geom_errorbar(aes(ymin=meanN-seN, ymax=meanN+seN, colour=source), width=0, size=1.5) + 
  geom_point(aes(fill=source), colour="black", size=7, stroke=1.3, shape=21) +
  scale_fill_manual(values=c("gray80", "gray20")) +
  scale_colour_manual(values=c("gray80", "gray20")) +
  scale_y_continuous(labels=scaleFUN) +
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme_bw() +
  theme(axis.title = element_text(size=30, face = "bold"),
        axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
        axis.text = element_text(size=27, colour="black"),
        axis.ticks = element_line(size=1),
        axis.ticks.length = unit(1.5, "mm"),
        panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.1),
        legend.position = "none") 


# ----------------------------- CARBON 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=combo.summary, aes(x=dist_group, y=meanC)) + 
  geom_errorbar(aes(ymin=meanC-seC, ymax=meanC+seC, colour=source), width=0, size=1.5) + 
  geom_point(aes(fill=source), colour="black", size=7, stroke=1.3, shape=21) +
  scale_fill_manual(values=c("gray80", "gray20")) +
  scale_colour_manual(values=c("gray80", "gray20")) +
  scale_y_continuous(labels=scaleFUN) +
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme_bw() +
  theme(axis.title = element_text(size=30, face = "bold"),
        axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
        axis.text = element_text(size=27, colour="black"),
        axis.ticks = element_line(size=1),
        axis.ticks.length = unit(1.5, "mm"),
        panel.grid.major = element_blank(),#line(colour="gray80", size=0.8),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.1),
        legend.text = element_text(size=27), 
        legend.title = element_blank(),
        legend.position = c(0.15,0.86),
        legend.background = element_rect(colour="black", size=0.8),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(t=-2,b=4,l=5,r=8)) 
































