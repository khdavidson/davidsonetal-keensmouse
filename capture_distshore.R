## distance from shore relative abundance anova/tukey 

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")
library(tidyverse)

#### start with excel file that just has CPUE worked out for each phase, for each distance class, at each site 
## i need to add the CPUE calcs for some of the distance classes together (e.g., right now GF and IP each have two CPUE calcs for
## the 0-25m interval, so these need to be added together)
file1 <- read.csv("CPUE_phases.csv")

CPUE_added <- file1 %>%
  group_by(group, site) %>%
  filter(group %in% c("0-25", "50-75", "100-125", "150-200")) %>%
  summarize(sum_CPUEfrst = sum(cpue_frstcap), sum_CPUErecap = sum(cpue_recap)) %>%
  print(CPUE_added)

write.csv(CPUE_added, "CPUE_added.csv")


## now need to calculate proportions based on sums - this is easier to do in excel so did it in excel and made a new file 
cpuepropn <- read.csv("CPUE_propns.csv")

# now average across the distance classes at each site FOR PLOTTING LATER, NOT FOR STATS

averages <- cpuepropn %>%
  group_by(group) %>%
  summarize(mean_CPUEfrst = mean(propn_CPUEfrst), se_frst = (sd(propn_CPUEfrst)/sqrt(length(propn_CPUEfrst))), 
            mean_CPUErecap = mean(propn_CPUErecap), se_recap = (sd(propn_CPUErecap)/sqrt(length(propn_CPUErecap)))) %>% 
  print(averages)

write.csv(averages, "CPUE_final_avgpropns.csv")

############### NOTE: these files and data are done by phases, taking into account the fact that we only opened 2/4 transects the first
##### nights at GF and IP. These are only ADULT mice, and include both location of first capture as well as all captures. 



############################################################## ANOVA STATS

cpuepropn <- read.csv("CPUE_propns.csv")

# 1. check assumptions- normality and equal variance for both first captures and recaptures 
# first captures
shapiro.test(cpuepropn$propn_CPUEfrst)      # p = 0.714 therefore normal

qqnorm(cpuepropn$propn_CPUEfrst)           # pretty damn normal
qqline(cpuepropn$propn_CPUEfrst, col = "red")

hist(cpuepropn$propn_CPUEfrst)             # normal...?

bartlett.test(cpuepropn$propn_CPUEfrst ~ cpuepropn$group)        # p = 0.243 therefore equal variances 

res1<-resid(lm(cpuepropn$propn_CPUEfrst ~ cpuepropn$group))
plot(res1)
hist(res1)
qqnorm(res1)
qqline(res1)
shapiro.test(res1)
plot(lm(cpuepropn$propn_CPUEfrst ~ cpuepropn$group))

# re-do check to see if arcsine is better 
cpuepropn$asin_CPUE_propn <- asin(cpuepropn$propn_CPUEfrst)
res2<-resid(lm(cpuepropn$asin_CPUE_propn ~ cpuepropn$group))
plot(res2)
hist(res2)
qqnorm(res2)
qqline(res2)
shapiro.test(res2)
plot(lm(cpuepropn$asin_CPUE_propn ~ cpuepropn$group))

# arcsine doesn't really change anything and original raw proportion data were OK, so maintain that

# recaptures
#shapiro.test(cpuepropn$propn_CPUErecap)     # p = 0.678 therefore normal 
#qqnorm(cpuepropn$propn_CPUErecap)           # pretty normal
#qqline(cpuepropn$propn_CPUErecap, col = "red")
#hist(cpuepropn$propn_CPUErecap)             # normal...?
#bartlett.test(cpuepropn$propn_CPUErecap ~ cpuepropn$group)        # p = 0.117 therefore equal variances 


### because both proportion datasets are normal and equal variance, won't arcsine transform, will go ahead with anova

# 2. anova for first caps
frstcap_aov <- aov(cpuepropn$propn_CPUEfrst ~ cpuepropn$group)
summary(frstcap_aov)     # p = 0.017 therefore check out Tukey
TukeyHSD(frstcap_aov)
#### 0-25 - 150-200   p = 0.028
#### 0-25 - 100-125   p = 0.055 

# 3. anova for recaps 
recap_aov <- aov(cpuepropn$propn_CPUErecap ~ cpuepropn$group)
summary(recap_aov)     # p = 0.023 so check out Tukey 
TukeyHSD(recap_aov)
#### 0-25 - 150-200   p = 0.033
#### 0-25 - 100-125   p = 0.075


#################################### in both cases, sig differences between 0-25 and 150-200 





##########################################################   plotting 
library(ggplot2)
library(Rmisc)
setwd("~/Documents/`Field Work 2016/`RESULTS/Data files")

CPUE <- read.csv("CPUE_final_avgpropns.csv")

CPUE$group <- factor(CPUE$group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)


# FIRST CAPTURE plot white for PAPER figure

# create new column for stats labels
CPUE$stat <- c("a","ab","b","ab")

ggplot(CPUE, aes(x=group, y=mean_CPUEfrst)) + 
  labs(x="Distance from beach (m)", y="Proportion of CPUE") +
  scale_y_continuous(limits=c(0, 1.0), breaks=seq(0, 1.0, by = 0.2))  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label=stat), vjust=-4,size=12) +
  geom_errorbar(data = CPUE, aes(ymin = mean_CPUEfrst - se_frst,
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






# plot BLACK POWERPOINT - defense 
ggplot(CPUE, aes(x=group, y=mean_CPUEfrst)) + 
  labs(x="", y="") + #y=bquote( atop("Proportion of captures", ~bar(x) %+-% "SE"))) +
  scale_y_continuous(limits=c(0, 1.0),breaks=seq(0, 1.0, by = 0.2))  +
  geom_bar(stat="identity", fill = "gray", colour="white", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label=stat), vjust=-3,size=16, colour=c("white", "gray30","white","gray30")) +
  geom_errorbar(data = CPUE, aes(ymin = mean_CPUEfrst - se_frst,
                                 ymax = mean_CPUEfrst + se_frst), width=.1, size=1.2, colour="white")  +
  theme(text = element_text(size=70)) +
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
 # theme(panel.grid.major = element_line(colour = "gray30", linetype = "dotted", size = 1.2)) + 
  theme(panel.grid.major = element_blank()) +
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 2, colour = "white")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3)) 
  #theme(axis.title.x = element_text(margin=margin(t=30,r=30,b=30,l=30))) +
#theme(axis.title.y = element_text(margin=margin(t=0,r=30,b=0,l=0),angle=90,vjust=0.5)) +
 # theme(plot.margin=unit(c(8,1,1,1),"cm"))







ggplot(sum, aes(x=group, y=propn)) + 
  labs(x="Distance from the beach (m)", y="") +
  scale_y_continuous(limits=c(0, 1.0),breaks=seq(0, 1.0, by = 0.2))  +
  geom_bar(stat="identity", fill = "gray", colour="gray", size=1) + 
  scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray", color=c("white", "#686868", "#686868", "white"), size = ifelse(sum$group=="0-25", 2.5,
                                                                                                                 ifelse(sum$group=="50-75", 2.5,
                                                                                                                        ifelse(sum$group=="100-125", 1,
                                                                                                                               ifelse(sum$group=="150-200", 1,1))))) +
  guides(fill = FALSE) +
  guides(size = FALSE) +
  guides(color = FALSE)+
  geom_errorbar(data = sum, aes(ymin = propn - se,
                                ymax = propn + se), width=.05, size=1.2, colour="white")  +
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
  theme(panel.grid.major = element_line(colour = "gray40", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "gray40")) + 
  theme(axis.ticks = element_line(size = 1, colour = "gray40")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4,1,1),"cm"))





###############################################################################################################

# remake figure 7 from thesis for MS submission feb-2020

trapdat <- read.csv("Livetraps_subset_no100isls_locnfirstcapture.csv")

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






