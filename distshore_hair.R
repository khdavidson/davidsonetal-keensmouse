# distance from shore hair 
# jun 2020 revision

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

hair.data = read.csv("fieldmouse_distshore_hair.csv")

library(tidyverse)
library(FSA)    # for Dunn test

#######################################################################################################################################################

#                                                           DATA CLEANING

hair.data <- hair.data %>% 
  rename(individual_id=Sample.ID, 
         region=Region) %>% 
  mutate(dist_group = ifelse(loc_frst_cap =="0"|loc_frst_cap=="25", "0-25", 
                             ifelse(loc_frst_cap=="50"|loc_frst_cap=="75", "50-75",
                                    ifelse(loc_frst_cap=="100"|loc_frst_cap=="125", "100-125", "150-200")))) %>%  
  print()


#######################################################################################################################################################

#                                                           SUMMARY STATS

# Sample size per distance interval 

hair.data %>% 
  group_by(dist_group) %>% 
  summarize(n=n()) 

#######################################################################################################################################################

#                                                           STATISTICAL COMPARISONS

# ----------------------------- NITROGEN 

#####
# TEST ASSUMPTIONS
#####
lmN <- lm(hair.data$d15N ~ hair.data$dist_group)
rN <- resid(lmN)
plot(rN)
hist(rN)
qqnorm(rN)
qqline(rN)

# log-transform does it improve? -- NO
lmN.l <- lm(log(hair.data$d15N) ~ hair.data$dist_group)
rN.l <- resid(lmN.l)
plot(rN.l)
hist(rN.l)
qqnorm(rN.l)
qqline(rN.l)

# compare results of anova and kruskal-wallis -- Both cases very non-significant
summary(aovN <- aov(hair.data$d15N ~ hair.data$dist_group))
TukeyHSD(aovN)

kwN <- kruskal.test(hair.data$d15N ~ hair.data$dist_group)
dunnTest(hair.data$d15N ~ hair.data$dist_group)


# ----------------------------- CARBON 

#####
# TEST ASSUMPTIONS
#####
lmC <- lm(hair.data$d13C ~ hair.data$dist_group)
rC <- resid(lmC)
plot(rC)
hist(rC)
qqnorm(rC)
qqline(rC)

# log-transform does it improve? -- YES
lmC.l <- lm(log(hair.data$d13C*-1) ~ hair.data$dist_group)
rC.l <- resid(lmC.l)
plot(rC.l)
hist(rC.l)
qqnorm(rC.l)
qqline(rC.l)

# compare results of anova and kruskal-wallis -- Both cases very non-significant
summary(aovC <- aov(hair.data$d13C ~ hair.data$dist_group))
TukeyHSD(aovC)

kwC <- kruskal.test(hair.data$d13C ~ hair.data$dist_group)
kwC
dunnTest(hair.data$d13C ~ hair.data$dist_group)




#######################################################################################################################################################

#                                                           PLOTS

hair.summary <- hair.data %>% 
  group_by(dist_group) %>%
  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), 
            meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>%
  print()
hair.summary$dist_group <- factor(hair.summary$dist_group, levels=c("0-25", "50-75", "100-125", "150-200"), ordered=T)


# ----------------------------- NITROGEN 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=hair.summary, aes(x=dist_group, y=meanN)) + 
  #ylim(-30,-17) +
  geom_errorbar(aes(ymin=meanN-seN, ymax=meanN+seN), width=0, size=1.5, alpha=0.7) + 
  geom_point(shape=21, size=7, stroke=1.5, fill="black") +
  #scale_x_continuous(breaks = c(0,25,50,75,100,125,150,175,200)) +
  scale_y_continuous(labels=scaleFUN) + 
  labs(x = "Distance from the beach (m)", y = "") +
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
        legend.text = element_text(size=27), 
        legend.title = element_blank(),
        legend.position = c(0.88,0.88),
        legend.background = element_rect(colour="black", size=0.8),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(t=-2,b=4,l=5,r=8))


# ----------------------------- CARBON 

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data=hair.summary, aes(x=dist_group, y=meanC)) + 
  #ylim(-30,-17) +
  geom_errorbar(aes(ymin=meanC-seC, ymax=meanC+seC), width=0, size=1.5, alpha=0.7) + 
  geom_point(shape=21, size=7, stroke=1.5, fill="black") +
  #scale_x_continuous(breaks = c(0,25,50,75,100,125,150,175,200)) +
  scale_y_continuous(labels=scaleFUN) + 
  labs(x = "Distance from the beach (m)", y = "") +
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
        legend.position = c(0.88,0.88),
        legend.background = element_rect(colour="black", size=0.8),
        legend.key.size = unit(10, "mm"),
        legend.margin = margin(t=-2,b=4,l=5,r=8))

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

ggplot(data=combo.summary, aes(x=dist_group, y=meanN, group=source)) + 
  geom_line(aes(colour=source), size=1.3) +
  geom_errorbar(aes(ymin=meanN-seN, ymax=meanN+seN, colour=source), width=0, size=1.5) + 
  geom_point(aes(fill=source), colour="black", size=7, stroke=1.3, shape=21) +
  annotate(geom="text", x=1, y=7, label="a", size=10) + 
  annotate(geom="text", x=2, y=5.2, label="b", size=10) + 
  annotate(geom="text", x=3, y=2.7, label="c", size=10) + 
  annotate(geom="text", x=4, y=1.8, label="c", size=10) + 
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

ggplot(data=combo.summary, aes(x=dist_group, y=meanC, group=source)) + 
  geom_line(aes(colour=source), size=1.3) +
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















































#######################################################################################################################################################
#######################################################################################################################################################

# lm equation code save for future reference 

#lm_eqnGS <- function(hair.data){
#  m <- lm(d13C[Region=="GS"] ~ loc_frst_cap[Region=="GS"], hair.data);
#  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                   list(a = format(coef(m)[1], digits = 2), 
#                        b = format(coef(m)[2], digits = 2), 
#                        r2 = format(summary(m)$r.squared, digits = 3)))
#  as.character(as.expression(eq));                 
#}

#c_hair + ylab(expression(paste(delta^{13},'C hair (\211)'))) 


#######################################################################################################################################################
#######################################################################################################################################################

# code from prelim analysis years ago - ignore, not included in MS 
# mouse gen-rep and isotopes 

data <- read.csv("mouse_genrep_iso.csv")

## plot 
subset <- data %>% 
  group_by(mouse_ID) %>% 
  filter(source=="trap") %>% 
  print(subset)

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(subset, aes(mouse_d13C, mouse_d15N, colour=gn)) + 
  labs(x="d13C", y="d15N") +
  scale_y_continuous(labels=scaleFUN) +
  ylim(c(0,15))+
  geom_point(size=6,shape=21,width=2) +
  scale_colour_manual(breaks = c("M", "F"), 
                      values=c("red","blue")) + 
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position=c(0.15,0.85))+ 
  theme(legend.text=element_text(size=50)) +
  theme(legend.title=element_blank()) +
  theme(legend.text.align = 0) +
  theme(legend.title.align = 0.5) +
  theme(legend.key = element_rect(colour="black")) +
  theme(legend.key.size = unit(2, 'lines')) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))