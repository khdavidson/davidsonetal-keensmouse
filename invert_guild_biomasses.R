############# INVERT BIOMASS, GUILD-STYLE BY SITE, REGION, DISTANCE 

# use same approach as before, divide each guild sum by # nights left out 
setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

###################
#                 #
#                 #
###    SITE     ###
#                 #
#                 #
###################

raw <- read.csv("invertTERR_BIOMASS.csv")

## first sum biomass per trap by guild
library(dplyr)
library(ggplot2)

guild_sums <- raw %>% 
  group_by(site, region, dist_group, Sample, Guild) %>%
  filter(biom_stat != "der", Guild != "unk", Guild != "omnivore", ID != "slug") %>%
  summarize(sum = sum(adj.biomass)) %>% 
  print(guild_sums)
    
# mean +- SE biomass of guilds by site 
guild_site_means <- guild_sums %>% 
  group_by(site, Guild) %>% 
  summarize(mean = mean(sum), se = sd(sum)/sqrt(length(sum))) %>% 
  print(guild_site_means)
    
# biomass guild SITE plot 
ggplot(guild_site_means, aes(x=site, y=mean, group=Guild)) + 
  labs(x="Site", y="") +
  ylim(0,60)  +
  geom_bar(aes(fill = Guild), stat = "identity", position = position_dodge(), colour="black", size=1) +
  scale_fill_manual(values = c("gray70", "black", "gray90")) +
  geom_errorbar(data = guild_site_means, position=position_dodge(width=0.9), aes(ymin = mean - se,
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
  ylab(bquote(''* ~AB[T]~ '(mg)'))


############

###################
#                 #
#                 #
###    DIST     ###
#                 #
#                 #
###################


# mean +- SE biomass of guilds by DISTANCE 
guild_site_means <- guild_sums %>% 
  group_by(dist_group, Guild) %>% 
  summarize(mean = mean(sum), se = sd(sum)/sqrt(length(sum))) %>% 
  print(guild_site_means)


# biomass guild DIST plot 

guild_site_means$dist_group <- factor(guild_site_means$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)

ggplot(guild_site_means, aes(x=dist_group, y=mean, group=Guild)) + 
  labs(x="Distance from beach (m)", y="Terrestrial arthropod biomass (mg/trap)") +
  ylim(0,40)  +
  geom_bar(aes(fill = Guild), stat = "identity", position = position_dodge(), colour="black", size=1) +
  scale_fill_manual(values = c("gray70", "black", "gray90")) +
  geom_errorbar(data = guild_site_means, position=position_dodge(width=0.9), aes(ymin = mean - se,
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
  #  theme(legend.key = element_rect(colour="black")) +
  theme(legend.key.size = unit(2, 'lines')) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(4,2,2,2),"cm"))
#  ylab(bquote(''* ~AB[T]~ '(mg)'))


### stats on dist 
guild_sums <- raw %>% 
  group_by(site, region, dist_group, Sample, Guild) %>%
  filter(biom_stat != "der", Guild != "unk", Guild != "omnivore", ID != "slug") %>%
  summarize(sum = sum(adj.biomass)) %>% 
  print(guild_sums)



#### two-way anova!

summary(aov_2way <- aov(sum ~ dist_group + Guild, data = guild_sums))
TukeyHSD(aov_2way)

# test
plot(guild_sums$dist_group, resid(aov_2way))
hist(aov_2way$resid)
qqnorm(aov_2way$resid)
qqline(aov_2way$resid)
# skewness
skewness(aov_2way$resid)      # 4.44 < 0 therefore slightly negatively skewed, but closer to zero. will accept this 

#
guild_sums$logsum <- log10(guild_sums$sum)
summary(aov_2way <- aov(logsum ~ dist_group + Guild, data = guild_sums))
plot(guild_sums$dist_group, resid(aov_2way))
hist(aov_2way$resid)
qqnorm(aov_2way$resid)
qqline(aov_2way$resid)

summary(aov_2way <- aov(logsum ~ dist_group + Guild, data = guild_sums))
TukeyHSD(aov_2way)






setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

TAB_raw <- read.csv("TAB_species-level.csv")

# STEP 1: FILTER
# To start, want to isolate only the "true" biomass estimates as some were "derived" by applying, e.g., weight of small spider to weight of unk beetle
# And remove slugs as they are unlikely prey
TAB_true <- TAB_raw %>% 
  filter(biom_stat != "der", ID != "slug", Guild != "unk", Guild != "omnivore") 

# STEP 2: SUM 
# Now, sum all species-level biomasses to create a total trap biomass (this dataframe will be used to calculate adjacent buffer biomasses)
TAB_trap <- TAB_true %>% 
  group_by(site, Sample, dist_group, Guild) %>% 
  summarize(total_adjbiomass_mg = sum(adj.biomass.mg)) %>% 
  print(TAB_trap)

# STEP 3: AVERAGE by GUILD 
TAB_guild_avg <- TAB_trap %>% 
  group_by(dist_group, Guild) %>% 
  summarize(mean = mean(total_adjbiomass_mg), se = sd(total_adjbiomass_mg)/sqrt(length(total_adjbiomass_mg))) %>% 
  print(TAB_guild_avg)



# STEP 4: STATS                            ## shouldn't do a 2-way anova because this compares guild biomasses within distance intervals. pitfall traps likely have bias the guilds they capture, so this type of comparison isn't appropriate. instead should be doing separate 1-way tests for carnivores and herbivores.
  # Test for assumptions
  aov1<-aov(log(total_adjbiomass_mg) ~ dist_group, data=TAB_trap)
  #aov2<-aov(log(total_adjbiomass_mg) ~ dist_group + Guild, data = TAB_trap)
  r2 <- resid(aov1)
  
  plot(r2)
  qqnorm(r2)
  qqline(r2)
  hist(r2)
  plot(aov2)

  ### STEP 4.1 - test for just CARNIVORES 
  TAB_carn <- TAB_trap %>% 
    filter(Guild=="Carnivore") %>% 
    print()
  aovc<-aov(log(total_adjbiomass_mg) ~ dist_group, data=TAB_carn)
  rc <- resid(aovc)
  plot(rc)
  qqnorm(rc)
  qqline(rc)
  hist(rc)
  summary(aovc)
  TukeyHSD(aovc)
  kc<-kruskal.test(TAB_carn$total_adjbiomass_mg, TAB_carn$dist_group)
  
  ### STEP 4.2 - test for just HERBIVORES 
  TAB_herb <- TAB_trap %>% 
    filter(Guild=="Herbivore") %>% 
    print()
  aovh<-aov(log(total_adjbiomass_mg) ~ dist_group, data=TAB_herb)
  rh <- resid(aovh)
  plot(rh)
  qqnorm(rh)
  qqline(rh)
  hist(rh)
  summary(aovh)
  TukeyHSD(aovh)
  kh<-kruskal.test(TAB_herb$total_adjbiomass_mg, TAB_herb$dist_group)
  

# STEP 5: PLOT 
  
  TAB_guild_avg$dist_group <- factor(TAB_guild_avg$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)
  TAB_guild_avg$Guild <- factor(TAB_guild_avg$Guild, levels = c("Carnivore", "Herbivore", "Detritivore", ordered = TRUE))
  
  ggplot(TAB_guild_avg, aes(x=dist_group, y=mean, group=Guild)) + 
    labs(x="Distance from beach (m)", y="Terrestrial arthropod \n biomass (mg/trap)") +
    ylim(0,40)  +
    geom_bar(aes(fill = Guild), stat = "identity", position = position_dodge(), colour="black", size=1) +
    scale_fill_manual(values = c("gray50","gray90", "black")) +
    geom_errorbar(data = TAB_guild_avg, position=position_dodge(width=0.9), aes(ymin = mean - se,
                                                                                   ymax = mean + se), width=0.15, size=1, colour="black") +
    theme(text = element_text(size=50)) +
    theme(panel.background = element_rect(fill = "white")) +
    theme(panel.grid.minor = element_line(colour = "transparent")) + 
    theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
    theme(plot.background = element_rect(fill = "white")) + 
    theme(axis.text = element_text(colour = "black")) + 
    theme(axis.title = element_text(colour = "black")) + 
    theme(panel.background = element_rect(colour = "red")) + 
    theme(axis.ticks = element_line(size = 1, colour = "black")) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
    theme(axis.title.x = element_text(size=50, margin=margin(t=30,r=0,b=0,l=0))) +
    theme(axis.title.y = element_text(size=50, margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
    theme(plot.margin=unit(c(3,2,2,2),"cm"))+
    # theme(axis.title.y = element_text(angle=0))+
    theme(axis.ticks.length = unit(0.2,"cm"))+
    theme(axis.ticks = element_line(size=1))+
    theme(legend.position = c(0.82,0.855),
          legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
          legend.title = element_blank(),
          legend.margin=margin(t = 0.3,r=0.3,b=0.3,l=0.3, unit='cm'),
          legend.key = element_rect(fill = "white"),
          legend.key.size = unit(1.6,"cm")) #+
  #  scale_color_hue(labels = c("Female", "Male"))
  














 ### OLD SHIT BELOW

# boxcox? 
dist_biom <- boxcox(guild_sums$sum ~ guild_sums$dist_group)

# search for lambda that coincides with the mLL value:
dlamda = dist_biom$x
dlik=dist_biom$y
dbc1 <- cbind(dlamda,dlik)    #compile into two columns in a table
dbc1[order(dlik),]          # order in descending order in terms of the likelihood; my lambda is -0.02

# test
aov_dist2 <- aov(sum^(1/7) ~ dist_group, data =carns)
plot(carns$dist_group, resid(aov_dist2))
hist(aov_dist2$resid)
qqnorm(aov_dist2$resid)
qqline(aov_dist2$resid)
# skewness
skewness(aov_dist2$resid)      # -0.320 < 0 therefore slightly negatively skewed, but closer to zero. will accept this 

#### so final model is:
aov_dist2 <- aov(sum^(1/7) ~ dist_group, data =carns)
summary(aov_dist2)      
TukeyHSD(aov_dist2)















## CARNIVORES
carns <- guild_sums %>% 
  group_by(site,region,dist_group, Sample) %>% 
  filter(Guild=="Carnivore") %>% 
  print(carns)

lm1 <- lm(carns$sum ~ carns$dist_group)
res1 <- resid(lm1)
qqnorm(res1)
qqline(res1)
hist(res1)


dist_carn <- boxcox(carns$sum ~ carns$dist_group)

# search for lambda that coincides with the mLL value:
dlamda = dist_carn$x
dlik=dist_carn$y
dbc1 <- cbind(dlamda,dlik)    #compile into two columns in a table
dbc1[order(dlik),]          # order in descending order in terms of the likelihood; my lambda is 0.1414

# create model
aov_dist2 <- aov(sum^(1/7) ~ dist_group, data =carns)
plot(carns$dist_group, resid(aov_dist2))
hist(aov_dist2$resid)
qqnorm(aov_dist2$resid)
qqline(aov_dist2$resid)
# skewness
skewness(aov_dist2$resid)      # -0.320 < 0 therefore slightly negatively skewed, but closer to zero. will accept this 

#### so final model is:
aov_dist2 <- aov(sum^(1/7) ~ dist_group, data =carns)
summary(aov_dist2)      
TukeyHSD(aov_dist2)


kruskal.test(carns$sum, carns$dist_group)
pairwise.wilcox.test(carns$sum, carns$dist_group, p.adjust.method = "BH")
dunn.test(carns$sum, carns$dist_group, method="BH")


# # # # # # # # # # # # # # # #
library(MASS)
library(moments)


## HERBIVORES
herbs <- guild_sums %>% 
  group_by(site,region,dist_group, Sample) %>% 
  filter(Guild=="Herbivore") %>% 
  print(herbs)

lm2 <- lm(herbs$sum ~ herbs$dist_group)
res2 <- resid(lm2)
qqnorm(res2)
qqline(res2)


# boxcox
dist_herb <- boxcox(herbs$sum ~ herbs$dist_group)

# search for lambda that coincides with the mLL value:
dlamda = dist_herb$x
dlik=dist_herb$y
dbc1 <- cbind(dlamda,dlik)    #compile into two columns in a table
dbc1[order(dlik),]          # order in descending order in terms of the likelihood; my lambda is 0.1414

# create model
aov_dist3 <- aov(sum^(1/7) ~ dist_group, data =herbs)
plot(herbs$dist_group, resid(aov_dist3))
hist(aov_dist3$resid)
qqnorm(aov_dist3$resid)
qqline(aov_dist3$resid)
# skewness
skewness(aov_dist3$resid)      # 0.269 > 0 therefore slightly positively skewed, but closer to zero. will accept this 

#### so final model is:
aov_dist3 <- aov(sum^(1/7) ~ dist_group, data =herbs)
summary(aov_dist3)      
TukeyHSD(aov_dist3)


# non para option 
kruskal.test(herbs$sum, herbs$dist_group)
pairwise.wilcox.test(herbs$sum, herbs$dist_group, p.adjust.method = "BH")
dunn.test(herbs$sum, herbs$dist_group, method="BH")






# # # # # # # # # # # # # # # #



## DETRITIVORES 
dets <- guild_sums %>% 
  group_by(site,region,dist_group, Sample) %>% 
  filter(Guild=="Detritivore") %>% 
  print(dets)

lm4 <- lm(dets$sum ~ dets$dist_group)
res4 <- resid(lm4)
qqnorm(res4)
qqline(res4)


# boxcox
dist_det <- boxcox(dets$sum ~ dets$dist_group)

# search for lambda that coincides with the mLL value:
dlamda = dist_det$x
dlik=dist_det$y
dbc1 <- cbind(dlamda,dlik)    #compile into two columns in a table
dbc1[order(dlik),]          # order in descending order in terms of the likelihood; my lambda is -0.1010

# create model
aov_dist4 <- aov(sum^(-1/10) ~ dist_group, data =dets)
plot(dets$dist_group, resid(aov_dist4))
hist(aov_dist4$resid)
qqnorm(aov_dist4$resid)
qqline(aov_dist4$resid)
# skewness
skewness(aov_dist4$resid)      # -0.18 > 0 therefore slightly positively skewed, but closer to zero. will accept this 

#### so final model is:
aov_dist4 <- aov(sum^(-1/10) ~ dist_group, data =dets)
summary(aov_dist4)      
TukeyHSD(aov_dist4)

  
  