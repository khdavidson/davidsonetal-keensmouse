# isotope signatures of representative food items (ground beetles, weevils, salal)
# cleaned june 2020

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

library(tidyverse)
library(FSA)   # for Dunn test

iso.dat <- read.csv("invert_veg_iso.csv")


########################################################################################################################################################

#                                                             DATA CLEANING 

# cleaning main database 
iso.dat <- iso.dat %>% 
  rename(CN_ratio = C.N) %>%
  print

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

#                                                           LIPID CORRECTION 

##################
# C:N over space #
##################

# Salal
ggplot(iso.salal.dat, aes(x=dist_group, y=CN_ratio)) +
  geom_point()

min(iso.salal.dat$CN_ratio) # 24.5
max(iso.salal.dat$CN_ratio) # 82.4


# Weevils
ggplot(iso.weevil.dat, aes(x=dist_group, y=CN_ratio)) +
  geom_point()

min(iso.weevil.dat$CN_ratio) # 5.5
max(iso.weevil.dat$CN_ratio) # 6.7


# Ground beetles
ggplot(iso.gbeetle.dat, aes(x=dist_group, y=CN_ratio)) +
  geom_point()

min(iso.gbeetle.dat$CN_ratio) # 4.1
max(iso.gbeetle.dat$CN_ratio) # 8.4

# All of these are C:N > 4 and show some level of variability over space, therefore they should be lipid-normalized as per Post et al. 2007
# However although salal has really  high C:N, Post et al 2007 had no relationship between C:N and Dd13C/% lipid, so we can't use the C:N to correct. 
# There is a slight relationship though between C:N and Dd13C/%lipid so we can correct for the invert lipid variation. 


#######################
# LIPID NORMALIZATION #
#######################

# FOR AQUATIC ORGANISMS:
# d13Cnorm = d13Cuntreated + Dd13C    therefore
# d13Cnorm = d13Cuntreated - 3.32 + 0.99 * C:N

# FOR TERRESTRIAL ANIMALS then would be: 
# d13cnorm = d13Cuntreated + Dd13C    therefore
# d13Cnorm = d13Cuntreated - 3.44 + 1.00 * C:N  (Equation 6, Post et al 2007. Note: only explained 25% of variance)


# Salal - making this column anyway just for cbind column matching later on 
iso.salal.dat <- iso.salal.dat %>% 
  mutate(d13C_norm = d13C) %>% 
  print()

ggplot(iso.weevil.dat, aes(x=dist_group, y=d13C_norm)) +
  geom_point()


# Weevils
  iso.weevil.dat <- iso.weevil.dat %>% 
  mutate(d13C_norm = d13C-3.44+1*CN_ratio) %>% 
  print()

ggplot(iso.weevil.dat, aes(x=dist_group, y=d13C_norm)) +
  geom_point()


# Ground beetles
iso.gbeetle.dat <- iso.gbeetle.dat %>% 
  mutate(d13C_norm = d13C-3.44+1*CN_ratio) %>% 
  print()

ggplot(iso.gbeetle.dat, aes(x=dist_group, y=d13C_norm)) +
  geom_point()




########################################################################################################################################################

#                                                           SUMMARY STATS

# Sample sizes for each focal food item at each distance interval 

# Salal
iso.salal.dat %>% 
  group_by(dist_group) %>% 
  summarize(n=n())

# Weevil
iso.weevil.dat %>% 
  group_by(dist_group) %>% 
  summarize(n=n())

# Ground beetle
iso.gbeetle.dat %>% 
  group_by(dist_group) %>% 
  summarize(n=n())


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
kruskal.test(iso.weevil.dat$d13C_norm ~ iso.weevil.dat$dist_group)

#####
# Ground beetle
#####
# will continue as above with non-parametric test for equivalent/consistent results
kruskal.test(iso.gbeetle.dat$d13C_norm ~ iso.gbeetle.dat$dist_group)
dunnTest(iso.gbeetle.dat$d13C_norm ~ iso.gbeetle.dat$dist_group)



#######################################################################################################################################################

#                                                           PLOTS

# combine into one df for plotting

combo.df <- rbind(iso.salal.dat, iso.weevil.dat, iso.gbeetle.dat)

# Summarize mean +/- SE 
combo.summary <- combo.df %>% 
  group_by(ID, dist_group) %>% 
  summarize(meanC = mean(d13C_norm), seC = sd(d13C_norm)/sqrt(length(d13C_norm)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
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
  annotate(geom="text", label="a", x=1, y=-0.8, size=10) +
  annotate(geom="text", label="b", x=2, y=-6.2, size=10) +
  annotate(geom="text", label="b", x=3, y=-5.7, size=10) +
  #annotate(geom="text", label="a", x=2, y=-2.5, size=10) +
  #annotate(geom="text", label="a", x=4, y=-4.5, size=10) +
  annotate(geom="text", label="a", x=1, y=4.5, size=10) +
  annotate(geom="text", label="b", x=2, y=3.3, size=10) +
  annotate(geom="text", label="ab", x=3, y=3.5, size=10) +
  annotate(geom="text", label="b", x=4, y=2.7, size=10) +
  annotate(geom="text", x=0.55, y=4.7, label="A", size=11) + 
  geom_text(data=combo.summary%>%filter(ID=="Salal"), aes(label=n), hjust=1.2, vjust=1.4, size=8, colour="gray35") +
  geom_text(data=combo.summary%>%filter(ID=="Weevil"), aes(label=n), hjust=-0.4, vjust=-0.6, size=8, colour="gray35") +
  geom_text(data=combo.summary%>%filter(ID=="Ground beetle"), aes(label=n), hjust=1.2, vjust=1.6, size=8, colour="gray35") +
  scale_shape_manual(values=c("Salal"=23, "Weevil"=22, "Ground beetle"=24)) +
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  scale_y_continuous(labels=scaleFUN, limits=c(-7,4.7), breaks=seq(-6,4.5,by=2)) +
  theme_bw() +
  theme(axis.title = element_text(size=30, face = "bold"),
        axis.title.y = element_text(margin=margin(t=2,l=0,r=4,b=0)),
        axis.text = element_text(size=27, colour="black"),
        axis.ticks = element_line(size=1),
        axis.ticks.length = unit(1.5, "mm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size=1.1),
        legend.position = "none")


# ----------------------------- CARBON 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=combo.summary, aes(x=dist_group, y=meanC, group=ID)) + 
  geom_errorbar(aes(ymin = meanC-seC, ymax = meanC+seC), colour="black", width=0, size=1.5) + 
  geom_line(size=1.1, colour="gray80") +
  geom_point(aes(shape=ID), fill="gray80", colour="black", size=7, stroke=1.3) +
  annotate(geom="text", label="a", x=1, y=-28.2, size=10) +
  annotate(geom="text", label="b", x=2, y=-29.0, size=10) +
  annotate(geom="text", label="b", x=3, y=-29.6, size=10) +
  #annotate(geom="text", label="a", x=2, y=-26.3, size=10) +
  #annotate(geom="text", label="a", x=4, y=-25.9, size=10) +
  #annotate(geom="text", label="a", x=1, y=-26.1, size=10) +
  #annotate(geom="text", label="a", x=2, y=-27.2, size=10) +
  #annotate(geom="text", label="a", x=3, y=-26.9, size=10) +
  #annotate(geom="text", label="a", x=4, y=-26.6, size=10) +
  annotate(geom="text", x=0.55, y=-22, label="B", size=11) + 
  scale_shape_manual(values=c("Salal"=23, "Weevil"=22, "Ground beetle"=24)) +
  labs(x = "Distance from beach (m)") +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  scale_y_continuous(labels=scaleFUN, breaks=seq(-30,-22, by=2), limits=c(-30.5,-22)) +
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
        legend.position = c(0.8,0.4),
        legend.background = element_rect(colour="black", size=0.8),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(t=-2,b=4,l=5,r=8))























