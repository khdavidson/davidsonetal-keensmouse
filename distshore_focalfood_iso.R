# isotope signatures of representative food items (ground beetles, weevils, salal)
# cleaned june 2020

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

library(tidyverse)
library(FSA)   # for Dunn test

iso.dat <- read.csv("invert_veg_iso.csv")


########################################################################################################################################################

#                                                             DATA CLEANING 

# Select focal food items:

# Salal
iso.salal.dat <- iso.dat %>% 
  filter(ID == "salal", dist_group != "UNK") %>%
  print()

# Weevil
iso.weevil.dat <- iso.dat %>% 
  filter(ID == "weevil", dist_group != "INT") %>%
  print()

# Ground beetle
iso.gbeetle.dat <- iso.dat %>% 
  filter(ID == "ground beetle") %>%
  print()


########################################################################################################################################################


#                                                           STATISTICAL COMPARISONS

# Stats for each focal food item, ordered by isotopes


# ----------------------------- NITROGEN 

#####
# Salal
#####
lmNS <- lm(iso.salal.dat$d15N ~ iso.salal.dat$dist_group)
rNS <- resid(lmNS)
plot(rNS)
hist(rNS)
qqnorm(rNS)
qqline(rNS)
# not quite normal, and difficult to transform due to positive and negative values, so will use non-parametric test
kruskal.test(iso.salal.dat$d15N ~ iso.salal.dat$dist_group)
dunnTest(iso.salal.dat$d15N ~ iso.salal.dat$dist_group)

#####
# Weevil
#####
# will continue as above with non-parametric test for equivalent/consistent results
# Dunn test not needed because only 2 groups
kruskal.test(iso.weevil.dat$d15N ~ iso.weevil.dat$dist_group)

#####
# Ground beetle
#####
# will continue as above with non-parametric test for equivalent/consistent results
kruskal.test(iso.gbeetle.dat$d15N ~ iso.gbeetle.dat$dist_group)
dunnTest(iso.gbeetle.dat$d15N ~ iso.gbeetle.dat$dist_group)



# ----------------------------- CARBON 

#####
# Salal
#####
# will continue as above with non-parametric test for equivalent/consistent results
kruskal.test(iso.salal.dat$d13C ~ iso.salal.dat$dist_group)
dunnTest(iso.salal.dat$d13C ~ iso.salal.dat$dist_group)

#####
# Weevil
#####
# will continue as above with non-parametric test for equivalent/consistent results
# Dunn test not needed because only 2 groups
kruskal.test(iso.weevil.dat$d13C ~ iso.weevil.dat$dist_group)

#####
# Ground beetle
#####
# will continue as above with non-parametric test for equivalent/consistent results
kruskal.test(iso.gbeetle.dat$d13C ~ iso.gbeetle.dat$dist_group)
dunnTest(iso.gbeetle.dat$d13C ~ iso.gbeetle.dat$dist_group)



#######################################################################################################################################################

#                                                           PLOTS

# combine into one df for plotting

combo.df <- rbind(iso.salal.dat, iso.weevil.dat, iso.gbeetle.dat)

# Summarize mean +/- SE 
combo.summary <- combo.df %>% 
  group_by(ID, dist_group) %>% 
  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print()

# manually edit weevil intervals for plotting
combo.summary <- combo.summary %>% 
  mutate_at(vars(c(2)), funs(as.character)) %>%
  mutate(dist_group = ifelse(dist_group=="0-75", "50-75", 
                             ifelse(dist_group=="100-200", "150-200", dist_group))) %>% 
  ungroup() %>%
  mutate(ID = ifelse(ID=="weevil", "Weevil", 
                     ifelse(ID=="salal", "Salal", "Ground beetle"))) %>%
  print()

combo.summary$dist_group <- factor(combo.summary$dist_group, levels=c("0-25", "50-75", "100-125", "150-200"), ordered=T)
combo.summary$ID <- factor(combo.summary$ID, levels=c("Salal", "Weevil", "Ground beetle"), ordered=T)


# ----------------------------- NITROGEN 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=combo.summary, aes(x=dist_group, y=meanN, group=ID)) + 
  geom_line(size=1.1, colour="gray80") +
  geom_errorbar(aes(ymin = meanN-seN, ymax = meanN+seN), colour="black", width=0, size=1.5) + 
  geom_point(aes(shape=ID), fill="gray80", colour="black", size=7, stroke=1.3) +
  scale_shape_manual(values=c("Salal"=23, "Weevil"=22, "Ground beetle"=24)) +
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  scale_y_continuous(labels=scaleFUN) +
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


# ----------------------------- NITROGEN 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=combo.summary, aes(x=dist_group, y=meanC, group=ID)) + 
  geom_errorbar(aes(ymin = meanC-seC, ymax = meanC+seC), colour="black", width=0, size=1.5) + 
  geom_line(size=1.1, colour="gray80") +
  geom_point(aes(shape=ID), fill="gray80", colour="black", size=7, stroke=1.3) +
  scale_shape_manual(values=c("Salal"=23, "Weevil"=22, "Ground beetle"=24)) +
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  scale_y_continuous(labels=scaleFUN) +
  theme_bw() +
  theme(axis.title = element_text(size=30, face = "bold"),
        axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
        axis.text = element_text(size=27, colour="black"),
        axis.ticks = element_line(size=1),
        axis.ticks.length = unit(1.5, "mm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.1),
        legend.text = element_text(size=27), 
        legend.title = element_blank(),
        legend.position = c(0.8,0.5),
        legend.background = element_rect(colour="black", size=0.8),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(t=-2,b=4,l=5,r=8))























