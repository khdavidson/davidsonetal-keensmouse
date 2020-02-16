############# BIOMASS OF EVERYTHING
## wrack, forest inverts and beach inverts 

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

####################################
##                                ##
##                                ##
##      SITE/REGION BIOMASSES     ##
##                                ##  
##                                ##
####################################

library(dplyr)
library(ggplot2)
rawdata = read.csv("invertTERR_BIOMASS.csv")

# METADATA: 

# this data file has all flies removed. have applied some average masses from the weighed taxa to unweighed taxa (e.g., applied small spider
# weight to some unknown beetles, or like isopod weight to jumping bristletails and worms, and so on). So, I want to try biomass comparisons
# using both these derived biomasses, as well as the direct ones, to see how variable it gets. 

############################################################################################################################################
############## first with just the directly measured taxa (SI Table 3). true = biomass of spp i measured; der = applied to non-measured spp 
# (as above). I also want to try with totals, mean +/- SD, mean +/- SE, and by distance as well. 
############################################################################################################################################

# first make df that is just true biomasses - now made, don't need to remake
#truebiomasses <- rawdata %>%
#  group_by(site, ID2, dist_group, biom_stat, biomass) %>%
#  filter(biom_stat != "der", ID != "slug") %>%
#  print(truebiomasses) 


# I: TOTAL BIOMASS ACROSS ALL 4 SITES 

# sum biomass for each trap - will be used for all further analyses
#trapsum_site <- truebiomasses %>%
#  group_by(site, dist_group, Sample) %>%
#  summarize(sum = sum(biomass)) %>%
#  print(trapsum_site)
#write.csv(trapsum_site, "inverttrapsum_site_noslug.csv")

###########################################################################################################################################

#### >> MANUALLY ADDED IN TRAPPING EFFORT AFTER MAKING THIS FILE, SO DON'T RE-RUN THE CODE. 
# NOW THE NEW FILE TO BE USED IS: 
trapsums_effort <- read.csv("inverttrapsum_site_noslug.csv")


################################################################################# TOTAL BIOMASS ###########################################

# create totals for 4 sites
totals <- trapsums_effort %>%
  group_by(site) %>%
  summarize(sum = sum(adj_biom)) %>%
  print(totals)

barplot(totals$sum, names.arg=c("GF", "GOS", "GS-S", "NB"), xlab="Site", ylab = "Biomass (mg)")



######################################################################## AVERAGE BIOMASS +/- SE ACROSS ALL 4 SITES + REGIONALLY ########
library(plotrix)

trapsums_effort <- read.csv("inverttrapsum_site_noslug.csv")


# by SITE
avg_SD <- trapsums_effort %>% 
  group_by(site) %>%
  summarize(mean = mean(adj_biom), sd = sd(adj_biom), se = sd(adj_biom)/(sqrt(length(site)))) %>%
  print(avg_SD)

c <- barplot(avg_SD$mean, names.arg=c("GF", "GOS", "GS-S", "NB"), xlab="Site", ylab = "Biomass (mg)", ylim=c(0,200))
segments(c, avg_SD$mean - avg_SD$se, c, avg_SD$mean + avg_SD$se, lwd = 1.5)
arrows(c, avg_SD$mean - avg_SD$se, c, avg_SD$mean + avg_SD$se, lwd = 1.5, angle = 90, code = 3, length = 0.05)

write.csv(avg_SD,"invert_site_averages_noslug.csv")


# by REGION
ravg_SD <- trapsums_effort %>% 
  group_by(region) %>%
  summarize(mean = mean(adj_biom), sd = sd(adj_biom), se = sd(adj_biom)/(sqrt(length(region)))) %>%
  print(ravg_SD)

# plot!
invert_site_averages <- read.csv("invert_site_averages_noslug.csv")
invert_site_averages$stat <- c("a","b","a","b")

ggplot(invert_site_averages, aes(x=site, y=mean)) + 
  labs(x="Site", y="") + #y=bquote( atop("Proportion of captures", ~bar(x) %+-% "SE"))) +
  scale_y_continuous(limits=c(0, 75),breaks=seq(0, 75, by = 25))  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label=stat), vjust=-3,size=20) +
  geom_errorbar(data = invert_site_averages, aes(ymin = mean - se,
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
  ylab(bquote(''* ~AB[T]~ '(mg)'))




##################################### Stats on by SITE 

trapsums_effort <- read.csv("inverttrapsum_site_noslug.csv")


###  1. test of normality
shapiro.test(trapsums_effort$adj_biom)    # p << 0.001 therefore not normal 

aov_site1 <- aov(trapsums_effort$adj_biom ~ trapsums_effort$site)
lm_site1 <- lm(trapsums_effort$adj_biom ~ trapsums_effort$site)
summary(lm_site1)

qqnorm(aov_site1$residuals)
qqline(aov_site1$residuals)
hist(aov_site1$residuals)
plot(aov_site1$residuals)

qqnorm(lm_site1$residuals)
qqline(lm_site1$residuals)
hist(lm_site1$residuals)
plot(lm_site1$residuals)

# so try log-transform and retest
trapsums_effort$log_biomass <- log(trapsums_effort$adj_biom)
shapiro.test(trapsums_effort$log_biomass)     # p still << 0.001 


# so try box-cox transformation - working through youtube tutorial https://www.youtube.com/watch?v=TgVx9Rqsewo / https://www.youtube.com/watch?v=6O_9QUeyBVU
library(MASS)
library(moments)

# create model and check out histo, residuals, skewness, etc. 
aov_site1 <- aov(trapsums_effort$adj_biom ~ trapsums_effort$site)
plot(trapsums_effort$site, resid(aov_site1))
plot(aov_site1$residuals)
hist(aov_site1$resid)
qqnorm(aov_site1$resid)
qqline(aov_site1$resid)

# skewness
skewness(aov_site1$resid)      # 3.63 > 0 therefore strongly positively skewed 

# boxcox!
sitebc <- boxcox(trapsums_effort$adj_biom ~ trapsums_effort$site)       

# search for lambda that coincides with the mLL value:
slamda = sitebc$x
slik=sitebc$y
sbc1 <- cbind(slamda,slik)    #compile into two columns in a table
sbc1[order(slik),]          # order in descending order in terms of the likelihood; my lambda is 0.2222

# create model
aov_site2 <- aov(adj_biom^(1/5) ~ site, trapsums_effort)
plot(trapsums_effort$site, resid(aov_site2))
plot(aov_site2$residuals)
hist(aov_site2$resid)
qqnorm(aov_site2$resid)
qqline(aov_site2$resid)
# skewness
skewness(aov_site2$resid)      # -0.315 < 0 therefore slightly negatively skewed, but closer to zero. will accept this 

#### so final model is:
aov_site2 <- aov(adj_biom^(1/5) ~ site, trapsums_effort)
summary(aov_site2)      # site p << 0.001
TukeyHSD(aov_site2)


##################################### Stats on by REGION 

trapsums_effort <- read.csv("inverttrapsum_site_noslug.csv")


###  1. test of normality
shapiro.test(trapsums_effort$adj_biom)    # p << 0.001 therefore not normal 

aov_site1 <- aov(trapsums_effort$adj_biom ~ trapsums_effort$region)
qqnorm(aov_site1$residuals)
qqline(aov_site1$residuals)
hist(aov_site1$residuals)
plot(aov_site1$residuals)

# so try log-transform and retest
trapsums_effort$log_biomass <- log(trapsums_effort$adj_biom)
shapiro.test(trapsums_effort$log_biomass)     # p still << 0.001 


# so try box-cox transformation - working through youtube tutorial https://www.youtube.com/watch?v=TgVx9Rqsewo / https://www.youtube.com/watch?v=6O_9QUeyBVU
library(MASS)
library(moments)

# create model and check out histo, residuals, skewness, etc. 
aov_reg1 <- aov(trapsums_effort$adj_biom ~ trapsums_effort$region)
plot(trapsums_effort$region, resid(aov_reg1))
plot(aov_reg1$residuals)
hist(aov_reg1$resid)
qqnorm(aov_reg1$resid)
qqline(aov_reg1$resid)

# skewness
skewness(aov_reg1$resid)      # 3.72 > 0 therefore strongly positively skewed 

# boxcox!
regbc <- boxcox(trapsums_effort$adj_biom ~ trapsums_effort$region)       

# search for lambda that coincides with the mLL value:
rlamda = regbc$x
rlik=regbc$y
rbc1 <- cbind(rlamda,rlik)    #compile into two columns in a table
rbc1[order(rlik),]          # order in descending order in terms of the likelihood; my lambda is 0.2222

# create model
aov_reg2 <- aov(adj_biom^(1/5) ~ region, trapsums_effort)
plot(trapsums_effort$region, resid(aov_reg2))
plot(aov_reg2$residuals)
hist(aov_reg2$resid)
qqnorm(aov_reg2$resid)
qqline(aov_reg2$resid)
# skewness
skewness(aov_reg2$resid)      # -0.138 < 0 therefore slightly negatively skewed, but closer to zero. will accept this 

#### so final model is:
aov_reg2 <- aov(adj_biom^(1/5) ~ region, trapsums_effort)
summary(aov_reg2)      # site p  = 0.571 thr non-sig






####################################
##                                ##
##                                ##
##      DISTANCE    BIOMASSES     ##
##                                ##  
##                                ##
####################################



######################################################################## III: TOTAL BIOMASS BY DISTANCE ###################################

trapsums_effort <- read.csv("inverttrapsum_dist_newbyphase.csv")

# make sure dist_group is ordered correctly 
trapsums_effort$dist_group <- factor(trapsums_effort$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)

# GOS
GOStotaldist <- trapsums_effort %>%
  group_by(site, dist_group) %>%
  filter(site =="GOS") %>%
  summarize(sum = sum(adj_biom)) %>%
  print(GOStotaldist)

# GS-S
GSStotaldist <- trapsums_effort %>%
  group_by(site, dist_group) %>%
  filter(site =="GS-S") %>%
  summarize(sum = sum(adj_biom)) %>%
  print(GSStotaldist)

# NB
NBtotaldist <- trapsums_effort %>%
  group_by(site, dist_group) %>%
  filter(site =="NB") %>%
  summarize(sum = sum(adj_biom)) %>%
  print(NBtotaldist)

# GF 
GFtotaldist <- trapsums_effort %>%
  group_by(site, dist_group) %>%
  filter(site =="GF") %>%
  summarize(sum = sum(adj_biom)) %>%
  print(GFtotaldist)

par(mfrow=c(2,2))
barplot(GOStotaldist$sum, names.arg=GOStotaldist$dist_group, xlab="GOS", ylab = "Biomass (mg)")
barplot(GSStotaldist$sum, names.arg=GSStotaldist$dist_group, xlab="GS-S", ylab = "Biomass (mg)")
barplot(NBtotaldist$sum, names.arg=NBtotaldist$dist_group, xlab="NB", ylab = "Biomass (mg)")
barplot(GFtotaldist$sum, names.arg=GFtotaldist$dist_group, xlab="GF", ylab = "Biomass (mg)")       











######################################################################## IV: AVERAGE +/- SE BIOMASS BY DISTANCE ############################

trapsums_effort <- read.csv("inverttrapsum_dist_newbyphase.csv")
library(dplyr)
library(ggplot2)


# sum raw biomass by each phase (p) - not divided by [p*i*n] yet 
compact <- trapsums_effort %>%
  group_by(dist_group, p, site) %>%
  summarize(sum = sum(sum)) %>%
  print(compact)

# now need to divide by [p] for each sum - this is similar to the CPUE live trapping method 
# new column for adjusted standardized biomass by p 
compact$p_std_biom <- compact$sum/compact$p

# now add together the adjusted biomasses from different sampling phases from sites that had it 
phase_sum <- compact %>% 
  group_by(site, dist_group) %>%
  summarize(sum = sum(p_std_biom)) %>%
  print(phase_sum)

# now try plotting 
ggplot(averages, aes(x=dist_group, y=mean)) + 
  labs(x="Distance from beach (m)", y="Average biomass (mg)") + #y=bquote( atop("Proportion of captures", ~bar(x) %+-% "SE"))) +
  #scale_y_continuous(limits=c(0, 1.0),breaks=seq(0, 1.0, by = 0.2))  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
 # geom_text(aes(label=stat), vjust=-3,size=14) +
  geom_errorbar(data = averages, aes(ymin = mean - se,
                                     ymax = mean + se), width=.09, size=1, colour="black")  +
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
  theme(plot.margin=unit(c(3,2,2,2),"cm"))

## in summary, this method was done by summing biomass at each distance class at each site, sometimes in two phases in a few cases like NB
## and GS-S where certain transects were left out longer than others. Then, divided by TU (p*i*n). Then added distance classes together for
## the same site (i.e., added phase 1 0-25m and phase 2 0-25m for NB together) so that each site had their normal 4-5 distance classes. 
## Then, took an average across the 4 classes, plus 150-200m is given but isn't an average as it only ended up being one value. I think this
## way is problematic because of that extra distance class, and also the way TU is calculated. 

## Next I will try a different way to see if I get the same results. I will start with the full spreadsheet of biomass by trap, as above,
## filename called "inverttrapsum_site.csv". I will divide individual trap biomasses by the # of nights they were out (p). Then I will
## take an average across all the individual traps for each dist_group - by doing this, it is essentially the same TU calculation (taking
## into account both p and n in [p*i*n], except I am actually doing averages for each dist_group, particularly for 150-200.)


######################### Average +/- SE by DISTANCE with method described here ^^

trapsums_effort2 <- read.csv("inverttrapsum_site.csv")

# In the excel file, the biomass has already been adjusted by # nights out. Now, all I need to do is average across the dist_groups
avg_bytrap <- trapsums_effort2 %>%
  group_by(dist_group) %>%
  summarize(avg = mean(adj_biom), se = sd(adj_biom)/(sqrt(length(adj_biom)))) %>%
  print(avg_bytrap)

# maintain order
avg_bytrap$dist_group <- factor(avg_bytrap$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)

# now try plotting 
ggplot(avg_bytrap, aes(x=dist_group, y=avg)) + 
  labs(x="Distance from beach (m)", y="Biomass (mg)") + #y=bquote( atop("Proportion of captures", ~bar(x) %+-% "SE"))) +
  #scale_y_continuous(limits=c(0, 1.0),breaks=seq(0, 1.0, by = 0.2))  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
  # geom_text(aes(label=stat), vjust=-3,size=14) +
  geom_errorbar(data = avg_bytrap, aes(ymin = avg - se,
                                     ymax = avg + se), width=.09, size=1, colour="black")  +
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
  theme(plot.margin=unit(c(3,2,2,2),"cm"))





############################## method 3: sum biomass across all traps  in that distance category (e.g., sum of all traps in 0-25m), then
#### divide by [p*i*n], where p = number of nights, n = number of traps, to gain an adjusted biomass. In this way, these values are 
#### calculated much the same as the capture probabilities are. Then can take an average across all those distance classes (except 150-200m
#### will only have 1 value) for an OVERALL biomass-distance trend. This uses the file "inverttrapsum_dist_newbyphase" 
library(dplyr)
phase_dist <- read.csv("inverttrapsum_dist_newbyphase.csv")

phase_dist$dist_group <- factor(phase_dist$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)

avg_ax_distclass <- phase_dist %>% 
  group_by(dist_group) %>%
  summarize(mean = mean(adj_biom), se = sd(adj_biom)/(sqrt(length(adj_biom)))) %>% 
  print(avg_ax_distclass)

ax<-barplot(avg_ax_distclass$mean, ylim=c(0,75))
segments(ax, avg_ax_distclass$mean - avg_ax_distclass$se, ax, avg_ax_distclass$mean + avg_ax_distclass$se, lwd = 1.5)
arrows(ax, avg_ax_distclass$mean - avg_ax_distclass$se, ax, avg_ax_distclass$mean + avg_ax_distclass$se, lwd = 1.5, angle = 90, code = 3, length = 0.05)

#
#
#
#
#
#
#
#
#
##########################################################################################################################################

############### THIS SEEMS LIKE THE BEST WAY DOWN BELOW - SIMPLY control biomass at the trap level by the # nights that trap was left out,
############### then take an average of all trap biomassess in each dist class. 
library(dplyr)
library(ggplot2)
## need to compare how this looks to the basic average trap biomass (across all sites) at each dist_class
rawdat <- read.csv("inverttrapsum_site_noslug.csv")

as.factor(rawdat$dist_group)
rawdat$dist_group <- factor(rawdat$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)

overall_trap_avg_distclass <- rawdat %>%
  group_by(dist_group) %>% 
  filter(dist_group != "150-200") %>%
  summarize(mean = mean(adj_biom), se = (sd(adj_biom)/(sqrt(length(adj_biom))))) %>%
  print(overall_trap_avg_distclass)


all<-barplot(overall_trap_avg_distclass$mean, ylim=c(0,75), names.arg=overall_trap_avg_distclass$dist_group)
segments(all, overall_trap_avg_distclass$mean - overall_trap_avg_distclass$se, all, overall_trap_avg_distclass$mean + overall_trap_avg_distclass$se, lwd = 1.5)
arrows(all, overall_trap_avg_distclass$mean - overall_trap_avg_distclass$se, all, overall_trap_avg_distclass$mean + overall_trap_avg_distclass$se, lwd = 1.5, angle = 90, code = 3, length = 0.05)

# plot with colour by site 
ggplot(overall_trap_avg_distclass, aes(x=dist_group, y=mean, fill=site, group=site))+
  geom_bar(stat="identity",position=position_dodge(width=0.9))



# paper figure plot
ggplot(overall_trap_avg_distclass, aes(x=dist_group, y=mean)) + 
  labs(x="Distance from beach (m)", y="Terrestrial arthropod biomass (mg/trap)") +
  ylim(0,60)  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
 # geom_text(aes(label=stat), vjust=-3,size=14) +
  geom_errorbar(data = overall_trap_avg_distclass, aes(ymin = mean - se,
                                 ymax = mean + se), width=.09, size=1, colour="black")  +
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
  theme(plot.margin=unit(c(4,2,2,2),"cm"))
 # ylab(bquote(''* ~AB[T]~ '(mg/trap)'))




# plot BLACK POWERPOINT - defense
ggplot(overall_trap_avg_distclass, aes(x=dist_group, y=mean)) + 
  labs(x="", y="") + #y=bquote( atop("Proportion of captures", ~bar(x) %+-% "SE"))) +
  ylim(0,60)  +
  geom_bar(stat="identity", fill = "gray", colour="white", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
  # geom_text(aes(label=stat), vjust=-3,size=14) +
  geom_errorbar(data = overall_trap_avg_distclass, aes(ymin = mean - se,
                                                       ymax = mean + se), width=0.1, size=1.2, colour="white")  +
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





setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

TAB_raw <- read.csv("TAB_species-level.csv")

# STEP 1: FILTER
# To start, want to isolate only the "true" biomass estimates as some were "derived" by applying, e.g., weight of small spider to weight of unk beetle
# And remove slugs as they are unlikely prey
TAB_true <- TAB_raw %>% 
  filter(biom_stat != "der", ID != "slug") 

# STEP 2: SUM 
# Now, sum all species-level biomasses to create a total trap biomass (this dataframe will be used to calculate adjacent buffer biomasses)
TAB_trap <- TAB_true %>% 
  group_by(site, Sample, dist_group) %>% 
  summarize(total_adjbiomass_mg = sum(adj.biomass.mg)) %>% 
  print(TAB_trap)

# STEP 3: AVERAGE ACROSS DISTANCE GROUPS 
TAB_avg <- TAB_trap %>% 
  group_by(dist_group) %>% 
  summarize(mean = mean(total_adjbiomass_mg), se = (sd(total_adjbiomass_mg)/sqrt(length(total_adjbiomass_mg)))) %>% 
  print(TAB_avg)


# paper figure plot
TAB_avg$dist_group <- factor(TAB_avg$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)

ggplot(TAB_avg, aes(x=dist_group, y=mean)) + 
  labs(x="Distance from beach (m)", y="Terrestrial arthropod \n biomass (mg/trap)") +
  ylim(0,60)  +
  geom_bar(stat="identity", fill = "gray", colour="black", size=1.5) + 
  # scale_color_manual(values = c("#CCCCCC", "#CCCCCC")) +
  geom_bar(stat = "identity", fill = "gray") +
  # geom_text(aes(label=stat), vjust=-3,size=14) +
  geom_errorbar(data = TAB_avg, aes(ymin = mean - se,
                                    ymax = mean + se), width=.09, size=1, colour="black")  +
  theme_bw() +
  theme(text = element_text(size=12),
        panel.border = element_rect(size=1.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black", size=14),
        axis.title = element_text(colour = "black", size=18),
        axis.ticks = element_line(size = 1, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        #plot.margin=unit(c(0.5,0.2,0.2,0.2),"cm"),
        legend.position = c(0.87,0.87),
        legend.background = element_rect(colour="black"),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        legend.margin = margin(t=0.1,b=0.1,l=0.1,r=0.1, unit="cm")) 
# ylab(bquote(''* ~AB[T]~ '(mg/trap)'))


# STEP 4: STATS
  # Test of normality , variance 
  lm_dist1 <- lm(total_adjbiomass_mg ~ dist_group, TAB_trap)
  r1 <- resid(lm_dist1)

  plot(TAB_trap$dist_group, resid(lm_dist1))
  qqnorm(r1)                  # lol so not normal
  qqline(r1)
  hist(r1)                    # so very skewed 
  plot(r1)
  plot(lm_dist1)

library(FSA)
  # Kruskal wallis
  k1<-kruskal.test(TAB_trap$total_adjbiomass_mg, TAB_trap$dist_group)
  dunnTest(TAB_trap$total_adjbiomass_mg, TAB_trap$dist_group, method="bh")











###########################################################################################################################################
#
#
#
#
#
#
#
#
#




############################################################################ STATS on DISTANCE for BIOMASS #################################
library(MASS)
library(moments)

rawdat <- read.csv("inverttrapsum_site_noslug.csv")

#remove 150-200 because it's just GF and seasonality could be affecting that trend 
rawdat2 <- rawdat %>% 
  filter(dist_group != "150-200") %>% 
  print(rawdat2)
  

# test of normality 
aov_dist1 <- aov(adj_biom ~ dist_group, rawdat2)
lm_dist1 <- lm(adj_biom ~ dist_group, rawdat2)

shapiro.test(rawdat2$adj_biom)      # p < 2.2e-16 therefore not normal 
plot(rawdat2$dist_group, resid(aov_dist1))
qqnorm(lm_dist1$residuals)                  # lol so not normal
qqline(lm_dist1$residuals, col = "red")
hist(lm_dist1$residuals)              # so very skewed 

#skewness
skewness(lm_dist1$resid)      # 3.47 > 0 therefore strongly positively skewed


# need to transform or use non-parametric - can't use wilcox.test because requires only 2 groups 
# new column for log(biomass)
rawdat2$log_adj_biom <- log10(rawdat2$adj_biom)

summary(aov_dist2 <- aov(log_adj_biom ~ dist_group, rawdat2))
lm_dist2 <- lm(log_adj_biom ~ dist_group, rawdat2)

plot(rawdat2$dist_group, resid(aov_dist2))
plot(lm_dist2)
qqnorm(lm_dist2$residuals)                  
qqline(lm_dist2$residuals, col = "red")
hist(lm_dist2$residuals)              
shapiro.test(lm_dist2$residuals)  # still << 0.0001

aov_dist2 <- aov(log_adj_biom ~ dist_group, rawdat2)
summary(aov_dist2)
TukeyHSD(aov_dist2)













####################################### try BOXCOX! 
library(MASS)
dist_bc <- boxcox(adj_biom ~ dist_group, data = rawdat)

# search for lambda that coincides with the mLL value:
dlamda = dist_bc$x
dlik=dist_bc$y
dbc1 <- cbind(dlamda,dlik)    #compile into two columns in a table
dbc1[order(dlik),]          # order in descending order in terms of the likelihood; my lambda is 0.222

# create model
aov_dist2 <- aov(adj_biom^(1/5) ~ dist_group, rawdat)
plot(rawdat$dist_group, resid(aov_dist2))
hist(aov_dist2$resid)
qqnorm(aov_dist2$resid)
qqline(aov_dist2$resid)
# skewness
skewness(aov_dist2$resid)      # -0.130 < 0 therefore slightly negatively skewed, but closer to zero. will accept this
shapiro.test(aov_dist2$residuals)

#### so final model is:
aov_dist2 <- aov(adj_biom^(1/5) ~ dist_group, rawdat)
summary(aov_dist2)      
TukeyHSD(aov_dist2)


#non para to try
kruskal.test(rawdat$adj_biom, rawdat$dist_group)
pairwise.wilcox.test(rawdat$adj_biom, rawdat$dist_group, p.adjust.method = "BH")
dunn.test(rawdat$adj_biom, rawdat$dist_group)



































