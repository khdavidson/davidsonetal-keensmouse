
###################################
##                               ##
##                               ##
##      BEACH INVERTEBRATES      ##
##                               ##  
##                               ##
###################################

beach_biom <- read.csv("beach_invert_BIOMASS.csv")

# biomass is in mg so it's huge right now, convert to g
beach_biom$biomass_g <- (beach_biom$Biomass/1000)

## TOTAL BIOMASS OF ALL ORGANISMS BY SITE 
total_allspp <- beach_biom %>%
  group_by(Site) %>%
  summarize(sum = sum(biomass_g)) %>%
  print(toatl_allspp)

barplot(total_allspp$sum, names.arg=c("GF", "GOS", "GS-S", "IP", "NB"), xlab="Site", ylab = "Biomass (mg)", ylim=c(0,30))

# just amphipods 
total_amphi <- beach_biom %>%
  group_by(Site) %>%
  filter(taxa == "amphipod") %>%
  summarize(sum = sum(biomass_g)) %>%
  print(toatl_allspp)

barplot(total_amphi$sum, names.arg=c("GF", "GOS", "GS-S", "IP", "NB"), xlab="Site", ylab = "Biomass (mg)", ylim=c(0,30))



############################################################################################################################################



## NEED TO KNOW: proportion of trap biomass occupied by amphipods, and then the average across each site 
beach_biom <- read.csv("beach_invert_BIOMASS.csv")

# calculating average % of amphipods in each trap, across sites
propn_amph <- beach_biom %>%
  group_by(Region,Site, Trap_no) %>%
  summarize(propn = sum(Biomass[taxa=="amphipod"])/sum(Biomass)) %>%
  print(propn_amph)

# average across the sites  
avg_propn_amph <- propn_amph %>%
  group_by(Site) %>%
  summarize(meanprop =mean(propn), se = sd(propn)/sqrt(length(propn))) %>%
  print(avg_propn_amph)

# average across regions
# first need to change some "region" labels
beach_biom$Region=="SC" <- beach_biom$Region=="CV"

avg_propn_amph_r <- propn_amph %>%
  group_by(Region) %>%
  summarize(meanprop =mean(propn), se = sd(propn)/sqrt(length(propn))) %>%
  print(avg_propn_amph_r)



############################################################################################################################################

## AVERAGE BIOMASS/TRAP +/- SE BY SITE 
# first sum biomass per trap
trapsum <- beach_biom %>% 
  group_by(Region, Trap_no, Site) %>% 
  summarize(sum = sum(Biomass)) %>% 
  print(trapsum)

write.csv(trapsum, "trapsum.csv")

#then average across the site 
avg_trap <- trapsum %>% 
  group_by(Site) %>% 
  summarize(mean = mean(sum), se = sd(sum)/(sqrt(length(sum)))) %>%
  print(avg_trap)

write.csv(avg_trap, "beachinverts_sitemeans.csv")

## this is WRONG BELOW
#avg_allspp <- beach_biom %>%
#  group_by(Site) %>%
#  summarize(mean = mean(Biomass), se = sd(Biomass)/(sqrt(length(Biomass)))) %>%
#  print(avg_allspp)

#write.csv(avg_allspp, "beachinverts_sitemeans.csv")

par(mfrow=c(1,2))
b <- barplot(avg_allspp$mean, names.arg=c("GF", "GOS", "GS-S", "IP", "NB"), xlab="Site", ylab = "Total biomass (g)", ylim=c(0,400))
segments(b, avg_allspp$mean - avg_allspp$se, b, avg_allspp$mean + avg_allspp$se, lwd = 1.5)
arrows(b, avg_allspp$mean - avg_allspp$se, b, avg_allspp$mean + avg_allspp$se, lwd = 1.5, angle = 90, code = 3, length = 0.05)

# just amphipods 
avg_amphi <- beach_biom %>%
  group_by(Site) %>%
  filter(taxa == "amphipod") %>%
  summarize(mean = mean(Biomass), se = sd(Biomass)/(sqrt(length(Biomass)))) %>%
  print(avg_allspp)

a <- barplot(avg_amphi$mean, names.arg=c("GF", "GOS", "GS-S", "IP", "NB"), xlab="Site", ylab = "Amphipod only biomass (mg)", ylim=c(0,1000))
segments(a, avg_amphi$mean - avg_amphi$se, a, avg_amphi$mean + avg_amphi$se, lwd = 1.5)
arrows(a, avg_amphi$mean - avg_amphi$se, a, avg_amphi$mean + avg_amphi$se, lwd = 1.5, angle = 90, code = 3, length = 0.05)


# real plot
beach_inverts <- read.csv("beachinverts_sitemeans.csv")
beach_inverts$stat <- c("ac", "bc", "bc", "a", "b")

ggplot(beach_inverts, aes(x=Site, y=mean)) + 
  labs(x="") + #y=bquote( atop("Proportion of captures", ~bar(x) %+-% "SE"))) +
  scale_y_continuous(limits=c(0, 2200),breaks=seq(0, 1800, by = 400))  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label=stat), vjust=-3,size=20) +
  geom_errorbar(data = beach_inverts, aes(ymin = mean - se,
                                                 ymax = mean + se), width=.09, size=1, colour="black")  +
  theme(text = element_text(size=60)) + 
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
  ylab(bquote(''* ~AB[B]~ '(mg)'))





## AVERAGE BIOMASS/TRAP +/- SE BY REGION 

avg_region <- trapsum %>% 
  group_by(Region) %>% 
  summarize(mean = mean(sum), se = sd(sum)/(sqrt(length(sum)))) %>%
  print(avg_region)

par(mfrow=c(1,2))
c <- barplot(avg_allspp$mean, names.arg=c("GF", "GOS", "GS-S", "IP", "NB"), xlab="Site", ylab = "Total biomass (g)", ylim=c(0,400))
segments(c, avg_allspp$mean - avg_allspp$se, c, avg_allspp$mean + avg_allspp$se, lwd = 1.5)
arrows(c, avg_allspp$mean - avg_allspp$se, c, avg_allspp$mean + avg_allspp$se, lwd = 1.5, angle = 90, code = 3, length = 0.05)


############################################################################################################################################

#Stats on total biomass per trap at SITE level

# calculate total biomass per trap (n = ~123)
trap_sums <- beach_biom %>%
  group_by(Site,Trap_no) %>%
  summarize(total = sum(Biomass)) %>%
  print(trap_sums)

###  1. test of normality
shapiro.test(trap_sums$total)    # p << 0.001 therefore not normal 

m <- lm(trap_sums$total ~ trap_sums$Site)
qqnorm(m$residuals)
qqline(m$residuals)
hist(m$residuals)

# so try log-transform and retest
trap_sums$log_total <- log(trap_sums$total)
shapiro.test(trap_sums$log_total)     # p still << 0.001 

m2 <- lm(trap_sums$total ~ trap_sums$Site)
qqnorm(m2$residuals)
qqline(m2$residuals)
hist(m2$residuals)


# so try box-cox transformation - working through youtube tutorial https://www.youtube.com/watch?v=TgVx9Rqsewo / https://www.youtube.com/watch?v=6O_9QUeyBVU
library(MASS)
library(moments)

# create model and check out histo, residuals, skewness, etc. 
aov1 <- aov(trap_sums$total ~ trap_sums$Site)
plot(trap_sums$Site, resid(aov1))
hist(aov1$resid)
qqnorm(aov1$resid)
qqline(aov1$resid)

# skewness
skewness(aov1$resid)      # 1.52 > 0 therefore strongly positively skewed 

# boxcox!
bc <- boxcox(trap_sums$total ~ trap_sums$Site)        # is a power transformation. default search is -2 to 2 in increments of 0.1. LL maximized in this case around 0.2
# lambda is a parameter in the model as well as icpt and slope. the whole thing is estimated by mLL 

# search for lambda that coincides with the mLL value:
lamda = bc$x
lik=bc$y
bc1 <- cbind(lamda,lik)    #compile into two columns in a table
bc1[order(lik),]          # order in descending order in terms of the likelihood; my lambda is 0.2222

# create model
aov2 <- aov(total^(1/5) ~ Site, trap_sums)
plot(trap_sums$Site, resid(aov2))
hist(aov2$resid)
qqnorm(aov2$resid)
qqline(aov2$resid)
# skewness
skewness(aov2$resid)      # -0.407 < 0 therefore strongly negatively skewed, but closer to zero. will accept this 

#### so final model is:
aov2 <- aov(total^(1/5) ~ Site, trap_sums)
summary(aov2)      # site p = 0.002
TukeyHSD(aov2)
## IP-NB p = 0.003 ; IP-GSS p = 0.02


############################################################################################################################################


# Stats on total biomass per trap at REGION level

# calculate total biomass per trap REGIONALLY (n = ~123)
rtrap_sums <- beach_biom %>%
  group_by(Region,Site, Trap_no) %>%
  summarize(total = sum(Biomass)) %>%
  print(rtrap_sums)

###  1. test of normality
shapiro.test(rtrap_sums$total)    # p << 0.001 therefore not normal 

rm <- lm(rtrap_sums$total ~ rtrap_sums$Region)
qqnorm(rm$residuals)
qqline(rm$residuals)
hist(rm$residuals)

# so try log-transform and retest
rtrap_sums$log_total <- log(rtrap_sums$total)
shapiro.test(rtrap_sums$log_total)     # p still << 0.001 


# so try box-cox transformation - working through youtube tutorial https://www.youtube.com/watch?v=TgVx9Rqsewo / https://www.youtube.com/watch?v=6O_9QUeyBVU
library(MASS)
library(moments)

# create model and check out histo, residuals, skewness, etc. 
raov1 <- aov(rtrap_sums$total ~ rtrap_sums$Site)
plot(rtrap_sums$Region, resid(raov1))
hist(raov1$resid)
qqnorm(raov1$resid)
qqline(raov1$resid)

# skewness
skewness(raov1$resid)      # 1.52 > 0 therefore strongly positively skewed 

# boxcox!
rbc <- boxcox(rtrap_sums$total ~ rtrap_sums$Region)        # is a power transformation. default search is -2 to 2 in increments of 0.1. LL maximized in this case around 0.2
# lambda is a parameter in the model as well as icpt and slope. the whole thing is estimated by mLL 

# search for lambda that coincides with the mLL value:
rlamda = rbc$x
rlik=rbc$y
rbc1 <- cbind(rlamda,rlik)    #compile into two columns in a table
rbc1[order(rlik),]          # order in descending order in terms of the likelihood; my lambda is 0.1818

# create model
raov2 <- aov(total^(1/5) ~ Region, rtrap_sums)
plot(rtrap_sums$Region, resid(raov2))
hist(raov2$resid)
qqnorm(raov2$resid)
qqline(raov2$resid)
# skewness
skewness(raov2$resid)      # -0.010 < 0 therefore slightly negatively skewed, but closer to zero. will accept this 

#### so final model is:
raov2 <- aov(total^(1/5) ~ Region, rtrap_sums)
summary(raov2)      # site p = 0.07






###################################
##                               ##
##                               ##
##      GUILD                    ##
##                               ##  
##                               ##
###################################



beach_biom <- read.csv("amphipod_BIOMASS.csv")

# sum biomass per guild in each trap
sumguild <- beach_biom %>% 
  group_by(Site,Guild,Trap_no) %>%
  filter(Guild != "unk", taxa != "amphipod", Guild != "detritivore") %>%
  summarize(sum = sum(Biomass)) %>%
  print(sumguild)

avgguilds <- sumguild %>%
  group_by(Site,Guild) %>%
  summarize(mean = mean(sum), se = sd(sum)/(sqrt(length(sum)))) %>%
  print(avgguilds)


# plooooot 

ggplot(avgguilds, aes(x=Site, y=mean, group=Guild)) + 
  labs(x="", y="Biomass (mg)") +
  ylim(0,100)  +
  geom_bar(aes(fill = Guild), stat = "identity", position = position_dodge(), colour="black", size=1) +
  scale_fill_manual(values = c("gray70", "gray90")) +
  geom_errorbar(data = avgguilds, position=position_dodge(width=0.9), aes(ymin = mean - se,
                                                                                 ymax = mean + se), width=.09, size=1, colour="black") +
  theme(text = element_text(size=60)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position=c(0.80,0.83))+ 
  theme(legend.text=element_text(size=55)) +
  theme(legend.title=element_text(size=55)) +
  theme(legend.text.align = 0) +
  theme(legend.title.align = 0.5) +
  theme(legend.key.size = unit(2, 'lines')) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(4,2,2,2),"cm"))+
  ylab(bquote(''* ~AB[B]~ '(mg)'))





###################################
##                               ##
##                               ##
##      ONLY AMPHI               ##
##                               ##  
##                               ##
###################################

beach_biom <- read.csv("amphipod_BIOMASS.csv")

#select only amphipods by guild 

amphi <- beach_biom %>%
  group_by(Site,Trap_no) %>%
  filter(taxa =="amphipod") %>% 
  summarize(sum = sum(Biomass)) %>%
  print(amphi)

#average biomass per site
avgamphi <- amphi %>%
  group_by(Site) %>%
  summarize(mean = mean(sum), se = sd(sum)/sqrt(length(sum))) %>%
  print(avgamphi)



################################## stats on just amphi at SITE level

# calculate total biomass per trap (n = ~123)
amphi <- beach_biom %>%
  group_by(Site,Trap_no) %>%
  filter(taxa =="amphipod") %>% 
  summarize(sum = sum(Biomass)) %>%
  print(amphi)

###  1. test of normality
shapiro.test(amphi$sum)    # p << 0.001 therefore not normal 

am <- lm(amphi$sum ~ amphi$Site)
qqnorm(am$residuals)
qqline(am$residuals)
hist(am$residuals)

# so try log-transform and retest
amphi$logsum <- log(amphi$sum)
shapiro.test(amphi$logsum)     # p = 0.04 therefore NORMAL! whaaaaat

am2 <- lm(amphi$logsum ~ amphi$Site)
qqnorm(am2$residuals)
qqline(am2$residuals)
hist(am2$residuals)


#### so final model is:
aovamp <- aov(amphi$logsum ~ amphi$Site)
summary(aovamp)      # site p << 0.001
TukeyHSD(aovamp)
## IP-NB p = 0.003 ; IP-GSS p = 0.02



############################ amphipod stats by REGION 


# calculate total biomass per trap REGIONALLY (n = ~123)
amphireg <- beach_biom %>%
  group_by(Region, Site,Trap_no) %>%
  filter(taxa =="amphipod") %>% 
  summarize(sum = sum(Biomass)) %>%
  print(amphireg)

###  1. test of normality
shapiro.test(amphireg$sum)    # p << 0.001 therefore not normal 

ram <- lm(amphireg$sum ~ amphireg$Region)
qqnorm(ram$residuals)
qqline(ram$residuals)
hist(ram$residuals)

# so try log-transform and retest
amphireg$logsum <- log(amphireg$sum)
shapiro.test(amphireg$logsum)     # p = 0.04 SO NORMAL! 


#### so final model is:
rampaov2 <- aov(amphireg$logsum ~ amphireg$Region)
ramplm <- lm(amphireg$logsum ~ amphireg$Region)
summary(ramplm)
summary(rampaov2)      # site p = 0.07
TukeyHSD(rampaov2)





# plooooot 
avgamphi <- amphi %>%
  group_by(Site) %>%
  summarize(mean = mean(sum), se = sd(sum)/sqrt(length(sum))) %>%
  print(avgamphi)

avgamphi$stat = c("a", "b", "b", "a", "b")

ggplot(avgamphi, aes(x=Site, y=mean)) + 
  labs(x="", y="Biomass (mg)") +
  ylim(0,2000)  +
  geom_bar(stat = "identity", position = position_dodge(), fill= "gray", colour="black", size=1) +
 # scale_fill_manual(values = c("#F98009", "#71B16E")) +
  geom_errorbar(data = avgamphi, position=position_dodge(width=0.9), aes(ymin = mean - se,
                                                                          ymax = mean + se), width=.09, size=1, colour="black") +
  geom_text(aes(label=stat), vjust=-2,size=20) +
  theme(text = element_text(size=60)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position=c(0.83,0.83))+ 
  theme(legend.text=element_text(size=33)) +
  theme(legend.title=element_text(size=33)) +
  theme(legend.text.align = 0) +
  theme(legend.title.align = 0.5) +
  theme(legend.key.size = unit(2, 'lines')) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(4,2,2,2),"cm"))+
  ylab(bquote(''* ~IB[B]~ '(mg)'))








