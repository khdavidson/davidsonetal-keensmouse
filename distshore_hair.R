# distance from shore hair 
# jun 2020 revision
# 

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













lm_eqnGS <- function(hair.data){
  m <- lm(d13C[Region=="GS"] ~ loc_frst_cap[Region=="GS"], hair.data);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqnCV <- function(hair.data){
  m <- lm(d13C[Region=="CV"] ~ loc_frst_cap[Region=="CV"], hair.data);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


chair + ylab(expression(paste(delta^{13},'C hair (\211)'))) 







## d15N

ggplot(data = hair.data,
              aes(x = loc_frst_cap,
                  y = d15N,colour=Region)) + 
  #ylim(0,20) +
  labs(x = "",
       y = "") +
  ggtitle("") + 
  geom_point(data=hair.data, aes(colour=Region),size=12) +
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150,175,200)) +
  scale_y_continuous(limits=c(0,20), breaks=c(0,5,10,15,20), labels=scaleFUN) + 
  scale_color_manual(breaks = c("CV", "GS"),
                     values=c("#ffaa00", "#4596ff")) +
  theme(text = element_text(size=80)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
  theme(panel.grid.major = element_line(colour = "white", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 1, colour = "white")) +
  theme(legend.position="none") +
  geom_smooth(method='lm',formula=y~x,size=3,se=F) +
  theme(plot.margin=unit(c(0,4,1,0),"cm"))+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))


lm_eqnGS <- function(hair.data){
  m <- lm(d15N[Region=="GS"] ~ loc_frst_cap[Region=="GS"], hair.data);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqnCV <- function(hair.data){
  m <- lm(d15N[Region=="CV"] ~ loc_frst_cap[Region=="CV"], hair.data);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

nhair + ylab(expression(paste(delta^{15},'N (\211)')))



## linear models
lmc = lm(hair.data$d13C ~ hair.data$loc_frst_cap)
summary(lmc)

lmn = lm(hair.data$d15N ~ hair.data$loc_frst_cap)
summary(lmn)

############################################################################################################################ white figures

hair.data = read.csv("fieldmouse_distshore_hair.csv")
library(dplyr)

# summarize by mean +/- SE
summary <- hair.data %>%
  group_by(Region, loc_frst_cap) %>% 
  summarize(meanC = mean(d13C), meanN = mean(d15N), seC = sd(d13C)/sqrt(length(d13C)), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(summary)

summary2 <- hair.data %>%
  group_by(loc_frst_cap) %>% 
  summarize(meanC = mean(d13C), meanN = mean(d15N), seC = sd(d13C)/sqrt(length(d13C)), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(summary2)


# d13C trendline 
lm_eqnC <- function(summary2){
  m <- lm(d13C ~ loc_frst_cap, summary2);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


## d13C white --- ALL REGIONS GROUPED 
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data = summary2,
       aes(x = loc_frst_cap,
           y = meanC)) + 
  ylim(-30,-17) +
  labs(x = "Distance from beach (m)",
       y = "") +
  ggtitle("") + 
  geom_smooth(method='lm',formula=y~x,se=F,size=1, colour="black") +
  geom_point(data=summary2, size=7, shape = 16) +
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150,175,200)) +
  scale_y_continuous(labels=scaleFUN) + 
 # scale_shape_manual(breaks = c("CV", "GS"),
  #                   values=c(21,21)) +
#  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  #scale_colour_manual(breaks = c("CV","GS"), values=c("gray", "black")) + 
  geom_errorbar(data = summary2, aes(ymin = meanC-seC,
                                    ymax = meanC+seC),width=3, size=0.7) + 
  annotate("text", x = -5, y = -23.1, label = "14", size=10) +
  annotate("text", x = 21, y = -23.8, label = "9", size=10) +
  annotate("text", x = 45, y = -22.8, label = "11", size=10) +
  annotate("text", x = 71, y = -24.2, label = "7", size=10) +
  annotate("text", x = 96, y = -22.5, label = "4", size=10) +
  annotate("text", x = 121, y = -21.9, label = "5", size=10) +
  annotate("text", x = 146, y = -22.9, label = "2", size=10) +
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white",size=3)) + 
  #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black",size=1.2,linetype="solid")) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.2))+
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") +
  theme(plot.margin=unit(c(0,4,1,0),"cm"))+
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))


################################3

# d15N trendline 
lm_eqnN <- function(summary2){
  m <- lm(d15N ~ loc_frst_cap, summary2);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


## d15N white --- ALL REGIONS GROUPED 
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data = summary2,
       aes(x = loc_frst_cap,
           y = meanN)) + 
 # ylim(-30,-17) +
  labs(x = "Distance from beach (m)",
       y = "") +
  ggtitle("") + 
  geom_smooth(method='lm',formula=y~x,se=F,size=1, colour="black") +
  geom_point(data=summary2, size=7, shape = 16) +
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150,175,200)) +
  scale_y_continuous(labels=scaleFUN) + 
  # scale_shape_manual(breaks = c("CV", "GS"),
  #                   values=c(21,21)) +
  #  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  #scale_colour_manual(breaks = c("CV","GS"), values=c("gray", "black")) + 
  geom_errorbar(data = summary2, aes(ymin = meanN-seN,
                                     ymax = meanN+seN),width=3, size=0.7) + 
  annotate("text", x = -5, y = 8.75, label = "14", size=10) +
  annotate("text", x = 21, y = 6.8, label = "9", size=10) +
  annotate("text", x = 45, y = 8, label = "11", size=10) +
  annotate("text", x = 71, y = 6.4, label = "7", size=10) +
  annotate("text", x = 96, y = 6.8, label = "4", size=10) +
  annotate("text", x = 121, y = 7.3, label = "5", size=10) +
  annotate("text", x = 146, y = 6.3, label = "2", size=10) +
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white",size=3)) + 
  #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black",size=1.2,linetype="solid")) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.2))+
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") +
  theme(plot.margin=unit(c(0,4,1,0),"cm"))+
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))





############################################ STATSSSSSSSS
setwd("~/Documents/`Field Work 2016/`RESULTS/Data files")
hair.data = read.csv("fieldmouse_distshore_hair.csv")

target_dist <- c("0", "150")

sub_0_150 <- hair.data %>% 
  group_by(loc_frst_cap) %>% 
  filter(loc_frst_cap %in% target_dist) %>% 
  print(sub_0_150)


# wilcox for C 
wilcox.test(sub_0_150$d13C ~ sub_0_150$loc_frst_cap)

#wilcox for N
wilcox.test(sub_0_150$d15N ~ sub_0_150$loc_frst_cap)








############################################################  MANUSCRIPT white figures #####################################################################
setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")
hair.data = read.csv("fieldmouse_distshore_hair.csv")
library(dplyr)

# summarize by mean +/- SE
summary <- hair.data %>%
  group_by(Region, dist_group) %>% 
  summarize(n = n(), meanC = mean(d13C), meanN = mean(d15N), seC = sd(d13C)/sqrt(length(d13C)), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(summary)

summary$dist_group <- factor(summary$dist_group, levels = c('0-25','50-75', '100-150'),ordered = TRUE)

# STATS
#N
#comparison accross distance groups 
kruskal.test(hair.data$d15N ~ hair.data$dist_group)
# dunn post hoc
library(FSA)
dunnTest(hair.data$d15N ~ hair.data$dist_group, method="bh")

# C
#comparison accross distance groups 
kruskal.test(hair.data$d13C ~ hair.data$dist_group)
# dunn post hoc
library(FSA)
dunnTest(hair.data$d13C ~ hair.data$dist_group, method="bh")




## N 

# change site labels to full names for the lot 
scaleFUN <- function(x) sprintf("%.1f", x)
summary$Region <- ifelse(summary$Region == "CV", "Calvert Island", "Goose Archipelago")

ggplot(summary, aes(x=dist_group, y=meanN)) + 
  labs(x="Distance from beach (m)", y="") +
  scale_y_continuous(labels=scaleFUN, limits = c(4.5,11.5), breaks=seq(4.5,11.5,1.5)) +
  geom_errorbar(data = summary, aes(ymin = meanN - seN,
                                    ymax = meanN + seN, group=Region), width=0.08, size=1.4, position=position_dodge(width = 0.1)) +
  geom_point(aes(fill=Region), shape=21, size = 10, stroke=1.9, position=position_dodge(width = 0.1)) + 
  scale_fill_manual(values = c("gray80", "gray40")) +
  scale_colour_manual(values = c("gray80", "gray40")) +
      #  geom_text(aes(label=ssize, y = (meanN+seN)),hjust=0.4, vjust=-1, size = 14) +
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))+
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme(legend.position = c(0.735,0.90),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
        legend.title = element_blank(),
        legend.margin=margin(t = 0.3,r=0.3,b=0.3,l=0.3, unit='cm'),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(1.5,"cm")) +
  guides(size = "legend", colour = "none")
 # annotate("text", x = 0.97, y = 7.2, label = "13", size=14)+
#  annotate("text", x = 1.98, y = 6.9, label = "7", size=14)+
#  annotate("text", x = 2.97, y = 8.4, label = "9", size=14)+
#  annotate("text", x = 1.02, y = 11.8, label = "10", size=14)+
#  annotate("text", x = 2.02, y = 9.7, label = "11", size=14)+
#  annotate("text", x = 3.03, y = 10.8, label = "2", size=14)
#



## C 
ggplot(summary, aes(x=dist_group, y=meanC)) + 
  labs(x="Distance from beach (m)", y="") +
  scale_y_continuous(labels=scaleFUN) +
  geom_errorbar(data = summary, aes(ymin = meanC - seC,
                                    ymax = meanC + seC, group=Region), width=0.08, size=1.4, position=position_dodge(width = 0.2)) +
  geom_point(aes(fill=Region), shape=21, size = 10, stroke=1.9, position=position_dodge(width = 0.2)) + 
  scale_fill_manual(values = c("gray80", "gray40")) +
  scale_colour_manual(values = c("gray80", "gray40")) +
  #  geom_text(aes(label=ssize, y = (meanN+seN)),hjust=0.4, vjust=-1, size = 14) +
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))+
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme(legend.position = c(2.0,2.0),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
        legend.title = element_blank(),
        legend.margin=margin(t = 0.3,r=0.3,b=0.3,l=0.3, unit='cm'),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(1.5,"cm")) +
  guides(size = "legend", colour = "none") +
  annotate("text", x = 0.95, y = -23.45, label = "13", size=14)+
  annotate("text", x = 1.96, y = -22.65, label = "7", size=14)+
  annotate("text", x = 2.95, y = -20.9, label = "9", size=14)+
  annotate("text", x = 1.04, y = -21, label = "10", size=14)+
  annotate("text", x = 2.04, y = -21.6, label = "11", size=14)+
  annotate("text", x = 3.05, y = -21.15, label = "2", size=14)
#




#### new stats - on all data points 
hair.data = read.csv("fieldmouse_distshore_hair.csv")

# N
kruskal.test(hair.data$d15N ~ hair.data$loc_frst_cap)

#C
kruskal.test(hair.data$d13C ~ hair.data$loc_frst_cap)








































#######################################################################################################################################
#######################################################################################################################################


# mouse gen-rep and isotopes! 


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























