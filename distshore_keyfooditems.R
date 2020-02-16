##################################### key food item plots - isotopes by distance ############################################################


############################################################################### ground beetles ####################################################
setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")
iso.dat <- read.csv("invert_veg_iso.csv")
library(ggplot2)
library(dplyr)
# all previous work and manipulation was done in iso-data-exploration.R file

#### plot d13C white - coloured figures are in above R script file 
scaleFUN <- function(x) sprintf("%.1f", x)

gbdist.data.2 <- read.csv("gbdist2.csv")

# summarize into mean +/- SE 
gbmeans <- gbdist.data.2 %>% 
  group_by(dist_group) %>% 
  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(gbmeans)

# ordered 
gbmeans$dist_group <- factor(gbmeans$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)

#d13C
ggplot(gbmeans, aes(x=as.factor(dist_group), y=meanC, fill=region)) +
  #xlim(-35,-13) +
  #ylim(-15,15) +
  labs(x = "Distance from beach (m)",
       y = "") +
  geom_smooth(aes(as.integer(dist_group),y=meanC,colour=region), se=F, method="lm", formula=y~x)+
  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  scale_colour_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  geom_errorbar(data = gbmeans, aes(ymin = meanC - seC,
                                    ymax = meanC + seC), width=.09, size=1, colour="black")  +
  geom_point(data=gbmeans, aes(fill=region), size=7, shape=21) +
  scale_x_discrete(breaks=c("0-25", "50-75", "100-125", "150-200"), labels=c("0-25", "50-75", "100-125", "150-200"))+
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
 #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size =1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))

  




#d15N

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(gbmeans, aes(x=dist_group, y=meanN, fill=region)) +
  #xlim(-35,-13) +
  #ylim(-15,15) +
  labs(x = "Distance from beach (m)",
       y = "") +
  scale_y_continuous(labels=scaleFUN) +
  geom_smooth(aes(as.integer(dist_group),colour=region,y=meanN), se=F,method="lm", formula=y~x)+
  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  scale_colour_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  geom_errorbar(data = gbmeans, aes(ymin = meanN - seN,
                                    ymax = meanN + seN), width=.09, size=1, colour="black")  +
  geom_point(data=gbmeans, aes(fill=region), size=7, shape=21) +
  scale_x_discrete(breaks=c("0-25", "50-75", "100-125", "150-200"), labels=c("0-25", "50-75", "100-125", "150-200"))+
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size =1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))
  


# linear models

# wilcox subset database 
wilcox_gb <- gbdist.data.2 %>% 
  group_by(dist_group) %>% 
  filter(dist_group != "50-75", dist_group != "100-125") %>% 
  print(wilcox_gb)

lmc <- lm(gbdist.data.2$d13C ~ factor(gbdist.data.2$dist_group))
residc <- resid(lmc)
plot(residc)
qqnorm(residc)
qqline(residc)
hist(residc)

aovc <- aov(gbdist.data.2$d13C ~ factor(gbdist.data.2$dist_group))
wilcox.test(wilcox_gb$d13C ~ factor(wilcox_gb$dist_group))
summary(lmc)
summary(aovc)
TukeyHSD(aovc)





lmn <- lm(gbdist.data.2$d15N ~ factor(gbdist.data.2$dist_group))
residn <- resid(lmn)
plot(residn)
qqnorm(residn)
qqline(residn)
hist(residn)

aovn <- aov(gbdist.data.2$d15N ~ factor(gbdist.data.2$dist_group))
wilcox.test(wilcox_gb$d15N ~ factor(wilcox_gb$dist_group))
summary(lmn)
summary(aovn)
TukeyHSD(aovn)









lm_eqnGS <- function(gbdist.data.2){
  m <- lm(d15N[region=="GS"] ~ distance[region=="GS"], gbdist.data.2);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

lm_eqnCV <- function(gbdist.data.2){
  m <- lm(d13C[region=="CV"] ~ distance[region=="CV"], gbdist.data.2);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}





###################################################################### salal by distance ########################################################

# all previous work also done in iso-data-exploration

#plot d13C 
saldist.data.2 <- read.csv("saldist2.csv")

# summarize into mean +/- SE 
sameans <- saldist.data.2 %>% 
  group_by(region, dist_group) %>% 
  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(sameans)

# ordered 
sameans$dist_group <- factor(sameans$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)





#d13C
ggplot(sameans, aes(x=as.factor(dist_group), y=meanC, fill=region)) +
  #xlim(-35,-13) +
  #ylim(-30,-25) +
  labs(x = "Distance from beach (m)",
       y = "") +
  geom_smooth(aes(as.integer(dist_group),y=meanC,colour=region), se=F, method="lm", formula=y~x)+
  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  scale_colour_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  geom_errorbar(data = sameans, aes(ymin = meanC - seC,
                                    ymax = meanC + seC), width=.09, size=1, colour="black")  +
  geom_point(data=sameans, aes(fill=region), size=7, shape=21) +
  scale_x_discrete(breaks=c("0-25", "50-75", "100-125", "150-200"), labels=c("0-25", "50-75", "100-125", "150-200"))+
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size =1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))







#d15N

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(sameans, aes(x=dist_group, y=meanN, fill=region)) +
  #xlim(-35,-13) +
  #ylim(-15,15) +
  labs(x = "Distance from beach (m)",
       y = "") +
  scale_y_continuous(labels=scaleFUN) +
  geom_smooth(aes(as.integer(dist_group),colour=region,y=meanN), se=F,method="lm", formula=y~x)+
  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  scale_colour_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  geom_errorbar(data = sameans, aes(ymin = meanN - seN,
                                    ymax = meanN + seN), width=.09, size=1, colour="black")  +
  geom_point(data=sameans, aes(fill=region), size=7, shape=21) +
  scale_x_discrete(breaks=c("0-25", "50-75", "100-125", "150-200"), labels=c("0-25", "50-75", "100-125", "150-200"))+
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size =1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))





## linear models

# wilcox subset database 
wilcox_salal <- saldist.data.2 %>% 
  group_by(dist_group) %>% 
  filter(dist_group != "50-75") %>% 
  print(wilcox_salal)


lmc <- lm(saldist.data.2$d13C ~ factor(saldist.data.2$dist_group))
residc <- resid(lmc)
plot(residc)
qqnorm(residc)
qqline(residc)
hist(residc)

aovc <- aov(saldist.data.2$d13C ~ factor(saldist.data.2$dist_group))
wilcox.test(wilcox_salal$d13C ~ factor(wilcox_salal$dist_group))
summary(lmc)
summary(aovc)
TukeyHSD(aovc)





lmn <- lm(saldist.data.2$d15N ~ factor(saldist.data.2$dist_group))
residn <- resid(lmn)
plot(residn)
qqnorm(residn)
qqline(residn)
hist(residn)

aovn <- aov(saldist.data.2$d15N ~ factor(saldist.data.2$dist_group))
wilcox.test(wilcox_salal$d15N ~ factor(wilcox_salal$dist_group))
summary(lmn)
summary(aovn)
TukeyHSD(aovn)


###################################################################### WEEVIL by distance ########################################################

# all previous work also done in iso-data-exploration

# pull out weevil
weev <- read.csv("weevdist.csv")

weevmeans <- weev %>% 
  group_by(region, dist_group) %>%
  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(weevmeans)


# ordered 
weevmeans$dist_group <- factor(weevmeans$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)



#d13C

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(weevmeans, aes(x=as.factor(dist_group), y=meanC, fill=region)) +
  #xlim(-35,-13) +
  #ylim(-26.0,-25.4) +
  labs(x = "Distance from beach (m)",
       y = "") +
  scale_y_continuous(breaks=seq(-26.6,-25.4, by = 0.2),labels=scaleFUN) +
  #scale_y_continuous(breaks=c("-26.2","-26.0"," -25.8","-25.6","-25.4"), labels=c("-26.2","-26.0"," -25.8","-25.6","-25.4")) +
  geom_smooth(aes(as.integer(dist_group),y=meanC,colour=region), se=F, method="lm", formula=y~x)+
  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  scale_colour_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  geom_errorbar(data = weevmeans, aes(ymin = meanC - seC,
                                    ymax = meanC + seC), width=.09, size=1, colour="black")  +
  geom_point(data=weevmeans, aes(fill=region), size=7, shape=21) +
  scale_x_discrete(breaks=c("0-75", "100-200"), labels=c("0-75", "100-200"))+
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size =1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))



#d15N

scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(weevmeans, aes(x=dist_group, y=meanN, fill=region)) +
  #xlim(-35,-13) +
  ylim(-6,-2) +
  labs(x = "Distance from beach (m)",
       y = "") +
  scale_y_continuous(labels=scaleFUN) +
  geom_smooth(aes(as.integer(dist_group),colour=region,y=meanN), se=F,method="lm", formula=y~x)+
  scale_fill_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  scale_colour_manual(breaks=c("CV", "GS"), values=c("gray", "black")) +
  geom_errorbar(data = weevmeans, aes(ymin = meanN - seN,
                                    ymax = meanN + seN), width=.09, size=1, colour="black")  +
  geom_point(data=weevmeans, aes(fill=region), size=7, shape=21) +
  scale_x_discrete(breaks=c("0-75", "100-200"), labels=c("0-75", "100-200"))+
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  #theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size =1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="") + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))




## linear models 
lmc <- lm(weev$d13C ~ factor(weev$dist_group))
residc <- resid(lmc)
plot(residc)
qqnorm(residc)
qqline(residc)
hist(residc)

aovc <- aov(weev$d13C ~ factor(weev$dist_group))
wilcox.test(weev$d13C ~ factor(weev$dist_group))
summary(lmc)
summary(aovc)
TukeyHSD(aovc)





lmn <- lm(weev$d15N ~ factor(weev$dist_group))
residn <- resid(lmn)
plot(residn)
qqnorm(residn)
qqline(residn)
hist(residn)

aovn <- aov(weev$d15N ~ factor(weev$dist_group))
wilcox.test(weev$d15N ~ factor(weev$dist_group))
summary(lmn)
summary(aovn)
TukeyHSD(aovn)




############################################################################################################################################

# MASTER FIGURE WITH ALL PREY ITEMS IN ONE FIGURE

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")
iso.dat <- read.csv("invert_veg_iso.csv")
library(ggplot2)
library(dplyr)


# select only weevils, ground beetles and salal
target <- c("weevil", "ground beetle", "salal")
#prey_dist <- iso.dat %>% 
#  group_by(ID, dist_group,region) %>% 
#  filter(ID %in% target, dist_group != "INT", dist_group != "UNK") %>% 
#  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
#  print(prey_dist)

#write.csv(prey_dist, "prey_dist.csv")

# MANUALLY CHANGED WEEVIL DIST GROUPS

prey_dist <- read.csv("prey_dist.csv")


# ordered 
prey_dist$simple_dist_group <- factor(prey_dist$simple_dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)




  

# gets messy with region so summarize by all regions
prey_dist2 <- iso.dat %>% 
  group_by(ID, dist_group) %>% 
  filter(ID %in% target, dist_group != "INT", dist_group != "UNK") %>% 
 # summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(prey_dist2)

write.csv(prey_dist2, "prey_dist2.csv")

# new column 
prey_dist2$simpledist <- c("0-25", "100-125", "150-200", "50-75", "0-25", "100-125", "50-75", "50-75", "150-200")
prey_dist2$n <- c("29","29","24","5", "18","17","18", "6","11")
# ordered new column


# NEW GRAPH BETTER

prey_dist2 <- read.csv("prey_dist2.csv")

prey_dist_2_means <- prey_dist2 %>%
  group_by(ID) %>% 
  summarize(meanC = mean(d13C), meanN = mean(d15N), seC = sd(d13C)/sqrt(length(d13C)), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print()

prey_dist2$dist_fin <- factor(prey_dist2$dist_fin, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)
prey_dist2$ID <- factor(prey_dist2$ID, levels = c('ground beetle','salal', "weevil"),ordered = TRUE)


ggplot(prey_dist2 %>%
         arrange(ID), aes(x=as.factor(dist_fin), y=d13C,  colour=ID, group=ID)) + 
  labs(x="Distance from beach (m)", y="") +
  scale_y_continuous(labels=scaleFUN) +
  scale_x_discrete(breaks=c("0-25","50-75", "100-125", "150-200"), labels=c("0-25","50-75", "100-125", "150-200")) +
  geom_smooth(aes(as.integer(dist_fin),y=d13C, colour=ID), method="lm", formula=y~x, size=1.5) +
  geom_point(aes(colour=ID), shape=21, size = 7, stroke=2.15, fill="white", alpha = 0.9) + 
  scale_colour_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), 
                      breaks=c("Ground beetles", "Salal berries", "Weevils"), 
                      values=c("red", "#119a04", "blue"),
                      guide="legend") +
  theme(text = element_text(size=40)) + 
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
  ylab(expression(paste(delta^{13},'C (\211)'))) 
#





#manuscript figure 
prey_dist_2_means <- prey_dist2 %>%
  group_by(ID, dist_fin) %>% 
  summarize(n = n(), meanC = mean(d13C), meanN = mean(d15N), seC = sd(d13C)/sqrt(length(d13C)), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print()

prey_dist_2_means$dist_fin <- factor(prey_dist_2_means$dist_fin, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)
prey_dist_2_means$ID <- factor(prey_dist_2_means$ID, levels = c('ground beetle','salal', "weevil"),ordered = TRUE)

# figure  CARBON
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(prey_dist_2_means %>%
         arrange(ID), aes(x=as.factor(dist_fin), y=meanC,  colour="black", group=ID, shape=ID)) + 
  labs(x="Distance from beach (m)", y="") +
  geom_line(colour="black", size=1.3) +
  annotate(geom="text", x=0.57, y=-25, label="A", color="black", size=6, fontface=2) +
  geom_errorbar(data = prey_dist_2_means, aes(ymin = meanC - seC,
                                              ymax = meanC + seC), width=0.08, size=1.4, colour="black") +
  geom_point(aes(shape=ID), size = 4, stroke=1.6, fill="gray80", colour="black") + 
  scale_y_continuous(labels=scaleFUN, limits = c(-31, -25), breaks = seq(-31,-25,1.5)) +
  scale_x_discrete(breaks=c("0-25","50-75", "100-125", "150-200"), labels=c("0-25","50-75", "100-125", "150-200")) +
  scale_shape_manual(values=c(23,22,24),
                     labels=c("Ground beetles", "Salal", "Weevils")) +
  theme_bw() +
  theme(text = element_text(size=12)) +
  theme(panel.border = element_rect(colour = "black", size=1.2))+
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(axis.text = element_text(colour = "black", size=14)) + 
  theme(axis.title.x = element_text(colour = "black", size=18)) +
  theme(axis.title.y = element_text(colour = "black", size=18)) +
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(legend.position="none") +
  guides(size = "legend", colour = "none")+
  ylab(expression(paste(delta^{13},'C (\211)')))+
  annotate("text", x = 2, y = -25.4, label = "8", size=6)+
  annotate("text", x = 4, y = -25.7, label = "11", size=6)+
  annotate("text", x = 1, y = -27, label = "29", size=6)+
  annotate("text", x = 2, y = -27.4, label = "29", size=6)+
  annotate("text", x = 3, y = -27.1, label = "24", size=6)+
  annotate("text", x = 4, y = -26.8, label = "5", size=6)+
  annotate("text", x = 1, y = -28.2, label = "18", size=6)+
  annotate("text", x = 2, y = -29, label = "18", size=6)+
  annotate("text", x = 3, y = -29.6, label = "17", size=6)
  #


# figure  NITROGEN
ggplot(prey_dist_2_means %>%
         arrange(ID), aes(x=as.factor(dist_fin), y=meanN,  colour="black", group=ID, shape=ID)) + 
  labs(x="Distance from beach (m)", y="") +
  scale_y_continuous(labels=scaleFUN) +
  scale_x_discrete(breaks=c("0-25","50-75", "100-125", "150-200"), labels=c("0-25","50-75", "100-125", "150-200")) +
  # geom_smooth(aes(as.integer(dist_fin),y=d13C, colour=ID), method="lm", formula=y~x, size=1.5) +
  geom_line(colour="black", size=1.3) +
  geom_errorbar(data = prey_dist_2_means, aes(ymin = meanN - seN,
                                              ymax = meanN + seN), width=0.08, size=1.4, colour="black") +
  geom_point(size = 4, stroke=1.6, fill="gray80", colour="black") + 
  scale_shape_manual(values=c(23,22,24),
                     labels=c("Ground beetles", "Salal", "Weevils")) +
  annotate(geom="text", x=0.55, y=4.05, label="B", color="black", size=6, fontface=2) +
  theme_bw() +
  theme(text = element_text(size=12)) +
  theme(panel.border = element_rect(colour = "black", size=1.2))+
  theme(panel.grid.minor = element_blank()) + 
  theme(panel.grid.major = element_blank()) + 
  theme(axis.text = element_text(colour = "black", size=14)) + 
  theme(axis.title.x = element_text(colour = "black", size=18)) +
  theme(axis.title.y = element_text(colour = "black", size=18)) +
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(axis.ticks.length = unit(0.2, "cm")) +
  theme(legend.position = c(0.79,0.45),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.margin = margin(t=0,r=0.3,b=0,l=0.3, unit='cm'),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(0.7,"cm")) +
  ylab(expression(paste(delta^{15},'N (\211)'))) 
#








# plot d13C 
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(prey_dist2, aes(x=as.factor(simpledist), y=meanC, shape=ID, fill = ID, group=ID)) +
  #xlim(-35,-13) +
  #ylim(-26.0,-25.4) +
  labs(x = "Distance from beach (m)",
       y = "") +
  scale_y_continuous(labels=scaleFUN) +
  scale_x_discrete(breaks=c("0-25","50-75", "100-125", "150-200"), labels=c("0-25","50-75", "100-125", "150-200")) +
    geom_smooth(aes(as.integer(simpledist),y=meanC, colour=ID), se=F, method="lm", formula=y~x) +
  scale_colour_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), 
                      breaks=c("Ground beetles", "Salal berries", "Weevils"), 
                      values=c("#767676", "#e5e2e2", "black"),
                      guide="legend") +
    geom_errorbar(data = prey_dist2, aes(ymin = meanC - seC,
                                      ymax = meanC + seC), width=.09, size=1, colour="black")  +
    geom_point(data=prey_dist2, aes(shape = ID), size=9) +
  scale_fill_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), 
                    breaks=c("ground beetle", "salal", "weevil"), 
                    values=c("#767676", "#e5e2e2", "black"),
                    guide="legend") +
  scale_shape_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), values = c(24,21,22), guide="legend") +
#  annotate("text", x = "50-75", y = -25.6, label = "6", size=10)+
#  annotate("text", x = "150-200", y = -25.85, label = "11", size=10)+
#  annotate("text", x = "0-25", y = -26, label = "29", size=10) +
#  annotate("text", x = "50-75", y = -27.2, label = "5", size=10) +
#  annotate("text", x = "100-125", y = -27, label = "29", size=10) +
#  annotate("text", x = "150-200", y = -26.8, label = "24", size=10) +
#  annotate("text", x = "0-25", y = -28.2, label = "18", size=10) +
#  annotate("text", x = "50-75", y = -29, label = "18", size=10) +
#  annotate("text", x = "100-125", y = -29.7, label = "17", size=10) +
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position=c(0.75,0.5)) +
  guides(shape = guide_legend(title="Food groups", keywidth=0.5, keyheight=0.5, default.unit="inch", fill=NA,
                              override.aes = list(shape=c(24,21,22),
                                                  colour=c("#767676", "#e5e2e2", "black"),
                                                  fill=c("#767676", "#e5e2e2", "black"))),
         colour = guide_legend(title="Food groups", keywidth=0.5, keyheight=0.5, default.unit="inch", fill=NA,
                               override.aes = list(shape=c(24,21,22),
                                                   colour=c("black"),
                                                   fill=c("#767676", "#e5e2e2", "black"))),
         fill = guide_legend(title="Food groups", keywidth=0.5, keyheight=0.5, default.unit="inch", fill=NA,
                             override.aes = list(shape=c(24,21,22),
                                                 colour=c("black"),
                                                 fill=c("#767676", "#e5e2e2", "black"),
                                                 linetype=0))) +
  theme(legend.key=element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))






### new d15N plot better!


ggplot(prey_dist2 %>%
         arrange(ID), aes(x=as.factor(dist_fin), y=d15N,  colour=ID, group=ID)) + 
  labs(x="Distance from beach (m)", y="") +
  scale_y_continuous(labels=scaleFUN) +
  scale_x_discrete(breaks=c("0-25","50-75", "100-125", "150-200"), labels=c("0-25","50-75", "100-125", "150-200")) +
  geom_smooth(aes(as.integer(dist_fin),y=d15N, colour=ID), method="lm", formula=y~x, size=1.5) +
  geom_point(aes(colour=ID), shape=21, size = 7, stroke=2.15, fill="white", alpha = 0.9) + 
  scale_colour_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), 
                      breaks=c("Ground beetles", "Salal berries", "Weevils"), 
                      values=c("red", "#119a04", "blue"),
                      guide="legend") +
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
  ylab(expression(paste(delta^{15},'N (\211)'))) 














# plot d15N w
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(prey_dist2, aes(x=as.factor(simpledist), y=meanN, shape=ID, fill = ID, group=ID)) +
  #xlim(-35,-13) +
  #ylim(-26.0,-25.4) +
  labs(x = "Distance from beach (m)",
       y = "") +
  scale_y_continuous(labels=scaleFUN) +
  scale_x_discrete(breaks=c("0-25","50-75", "100-125", "150-200"), labels=c("0-25","50-75", "100-125", "150-200")) +
    geom_smooth(aes(as.integer(simpledist),y=meanN, colour=ID), se=F, method="lm", formula=y~x) +
  scale_colour_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), 
                      breaks=c("Ground beetles", "Salal berries", "Weevils"), 
                      values=c("#767676", "#e5e2e2", "black"),
                      guide="legend") +
    geom_errorbar(data = prey_dist2, aes(ymin = meanN - seN,
                                       ymax = meanN + seN), width=.09, size=1, colour="black")  +
    geom_point(data=prey_dist2, aes(shape = ID), size=9) +
  scale_fill_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), 
                    breaks=c("ground beetle", "salal", "weevil"), 
                    values=c("#767676", "#e5e2e2", "black"),
                    guide="legend") +
  scale_shape_manual(labels=c("Ground beetles", "Salal berries", "Weevils"), values = c(24,21,22), guide="legend") +
#  annotate("text", x = "50-75", y = -2.6, label = "6", size=10)+
#  annotate("text", x = "150-200", y = -4.4, label = "11", size=10)+
#  annotate("text", x = "0-25", y = 3, label = "29", size=10) +
#  annotate("text", x = "50-75", y = 2, label = "5", size=10) +
#  annotate("text", x = "100-125", y = 2, label = "29", size=10) +
#  annotate("text", x = "150-200", y = 2.7, label = "24", size=10) +
#  annotate("text", x = "0-25", y = -0.9, label = "18", size=10) +
#  annotate("text", x = "50-75", y = -6.3, label = "18", size=10) +
#  annotate("text", x = "100-125", y = -5.8, label = "17", size=10) +
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position=c(0.75,0.5)) +
    guides(shape = guide_legend(title="Food groups", keywidth=0.5, keyheight=0.5, default.unit="inch", fill=NA,
                              override.aes = list(shape=c(24,21,22),
                                                  colour=c("#767676", "#e5e2e2", "black"),
                                                  fill=c("#767676", "#e5e2e2", "black"))),
         colour = guide_legend(title="Food groups", keywidth=0.5, keyheight=0.5, default.unit="inch", fill=NA,
                               override.aes = list(shape=c(24,21,22),
                                                   colour=c("black"),
                                                   fill=c("#767676", "#e5e2e2", "black"))),
         fill = guide_legend(title="Food groups", keywidth=0.5, keyheight=0.5, default.unit="inch", fill=NA,
                             override.aes = list(shape=c(24,21,22),
                                                 colour=c("black"),
                                                 fill=c("#767676", "#e5e2e2", "black"),
                                                 linetype=0))) +
    theme(legend.key=element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3))+
  theme(plot.margin=unit(c(1,4.5,1,1),"cm")) +
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))













