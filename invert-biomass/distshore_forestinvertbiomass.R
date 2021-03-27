# forest invertebrate biomass over distance 
# new cleaned script for MS june 2020

library(tidyverse)

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

raw.invert.data = read.csv("invertTERR_BIOMASS.csv")

# metadata notes on this file ('raw.invert.data'):
## flies have already been excluded because none were sub-sampled for weights, so they weren't carried through to this final spreadsheet
## Some average masses from the weighed taxa were applied to unweighed taxa (e.g., applied small spider weight to some unknown beetles of similar size, 
#### isopod weight to jumping bristletails and worms, etc.) - these are biomass derived i.e., 'der' designation. 
#### These groups weren't directly sampled for weight because they were rare, so omitting them and just focussing on biomass of species with known (i.e.,
#### sub-sampled weights) will be fine. 

#########################################################################################################################################################
#########################################################################################################################################################

#                                                              ###################
                                                               # OVERALL BIOMASS #
#                                                              ###################

#########################################################################################################################################################
#########################################################################################################################################################


#                                                               DATA CLEANING 

# first make df that is just true biomasses and remove slugs - not a prey item 
biomass.data <- raw.invert.data %>%
  filter(biom_stat != "der", ID != "slug") %>%
  rename(trap = Sample,
         guild = Guild,
         order = Order,
         date_set = date.set,
         date_collected = date.collected,
         n_trap_nights = X..trap.nights,
         n_traps = X..traps,
         n_effective_traps = X..effective.traps,
         adj_biomass = adj.biomass,
         adj_biomass_mg = adj.biomass.mg) %>%
  select(region:adj_biomass_mg) %>%
  print() 

# Calculate TOTAL biomass per trap, adjusted for effort
# columns were re-named to follow variable naming convention used in calculating mouse CPUE and Nelson & Clark 1973 
# Ends up being biomass/# nights trap was out - simple adjusted biomass effort calculation
trap.total.adj <- biomass.data %>%
  rename(p=n_trap_nights,
         i=n_traps) %>%
  group_by(site, dist_group, trap, p, i) %>%
  summarize(total_biomass = sum(biomass)) %>%
  mutate(adj_biomass = total_biomass/(p*i)) %>%
  print()



#########################################################################################################################################################

#                                                               STATISTICAL COMPARISONS

# Over distance 
lmB <- lm(trap.total.adj$adj_biomass ~ trap.total.adj$dist_group)
resB <- resid(lmB)
plot(resB)
hist(resB)
qqnorm(resB)
qqline(resB)

# Log-transformation
lmB.l <- lm(log10(trap.total.adj$adj_biomass) ~ trap.total.adj$dist_group)
resB.l <- resid(lmB.l)
plot(resB.l)
hist(resB.l)
qqnorm(resB.l)
qqline(resB.l)

summary(aov.l <- aov(log10(trap.total.adj$adj_biomass) ~ trap.total.adj$dist_group))



#########################################################################################################################################################

#                                                               PLOTS


# Summarize mean +/- SE 
biomass.adj.summary <- trap.total.adj %>% 
  group_by(dist_group) %>% 
  summarize(mean_adj_biom = mean(adj_biomass), se_adj_biom = sd(adj_biomass)/sqrt(length(adj_biomass))) %>% 
  print()

biomass.adj.summary$dist_group <- factor(biomass.adj.summary$dist_group, levels=c("0-25", "50-75", "100-125", "150-200"), ordered=T)

ggplot(data=biomass.adj.summary, aes(x=dist_group, y=mean_adj_biom)) + 
  geom_bar(stat="identity", colour="black", size=1.5) +
  geom_errorbar(aes(ymin = mean_adj_biom-se_adj_biom, ymax = mean_adj_biom+se_adj_biom), width=.09, size=1, colour="black") + 
  labs(x = "Distance from beach (m)", y="Biomass (g)") +
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
        legend.margin = margin(t=-2,b=4,l=5,r=8)) 













#########################################################################################################################################################
#########################################################################################################################################################

#                                                              ############
#                                                              # BY GUILD #
#                                                              ############
# this is exploratory - not included because pitfall traps have been shown to target mobile predatory inverts over more stationary species

#########################################################################################################################################################
#########################################################################################################################################################


#                                                               DATA CLEANING 

# first make df that is just true biomasses and remove slugs - not a prey item 
biomass.data <- raw.invert.data %>%
  filter(biom_stat != "der", ID != "slug") %>%
  rename(trap = Sample,
         guild = Guild,
         order = Order,
         date_set = date.set,
         date_collected = date.collected,
         n_trap_nights = X..trap.nights,
         n_traps = X..traps,
         n_effective_traps = X..effective.traps,
         adj_biomass = adj.biomass,
         adj_biomass_mg = adj.biomass.mg) %>%
  select(region:adj_biomass_mg) %>%
  print() 

# Calculate TOTAL biomass per trap, adjusted for effort
# columns were re-named to follow variable naming convention used in calculating mouse CPUE and Nelson & Clark 1973 
# Ends up being biomass/# nights trap was out - simple adjusted biomass effort calculation
guilds_of_interest <- c("carnivore", "herbivore")

trap.total.guild.adj <- biomass.data %>%
  rename(p=n_trap_nights,
         i=n_traps) %>%
  filter(guild %in% guilds_of_interest) %>%
  group_by(site, dist_group, trap, guild, p, i) %>%
  summarize(total_biomass = sum(biomass)) %>%
  mutate(adj_biomass = total_biomass/(p*i)) %>%
  print()


#########################################################################################################################################################

#                                                               STATISTICAL COMPARISONS

# Over distance by guild
lmBG <- lm(trap.total.guild.adj$adj_biomass ~ trap.total.guild.adj$dist_group + trap.total.guild.adj$guild)
resBG <- resid(lmBG)
plot(resBG)
hist(resBG)
qqnorm(resBG)
qqline(resBG)

# Log-transformation
lmBG.l <- lm(log10(trap.total.guild.adj$adj_biomass) ~ trap.total.guild.adj$dist_group + trap.total.guild.adj$guild)
resBG.l <- resid(lmBG.l)
plot(resBG.l)
hist(resBG.l)
qqnorm(resBG.l)
qqline(resBG.l)

summary(aovBG.l <- aov(log10(trap.total.guild.adj$adj_biomass) ~ trap.total.guild.adj$dist_group * trap.total.guild.adj$guild))
TukeyHSD(aovBG.l)



#########################################################################################################################################################

#                                                               PLOTS


# Summarize mean +/- SE 
biomass.adj.guild.summary <- trap.total.guild.adj %>% 
  group_by(guild, dist_group) %>% 
  summarize(mean_adj_biom = mean(adj_biomass), se_adj_biom = sd(adj_biomass)/sqrt(length(adj_biomass))) %>% 
  print()

biomass.adj.guild.summary$dist_group <- factor(biomass.adj.guild.summary$dist_group, levels=c("0-25", "50-75", "100-125", "150-200"), ordered=T)

ggplot(data=biomass.adj.guild.summary, aes(x=dist_group, y=mean_adj_biom, fill=guild)) + 
  geom_bar(position="dodge", stat="identity", colour="black", size=1.5) +
  geom_errorbar(aes(ymin = mean_adj_biom-se_adj_biom, ymax = mean_adj_biom+se_adj_biom), position=position_dodge(width=0.9), width=.09, size=1, colour="black") + 
  labs(x = "Distance from beach (m)", y="Biomass (g)") +
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
        legend.margin = margin(t=-2,b=4,l=5,r=8)) 












#########################################################################################################################################################
#########################################################################################################################################################

#                                                              ################
#                                                              # JUST SPIDERS #
#                                                              ################
# to compare to other tropical/xeric island spider studies
#########################################################################################################################################################
#########################################################################################################################################################


#                                                               DATA CLEANING 

# first make df that is just true biomasses and only spiders (includes both small and large bodied spiders)
biomass.spider.data <- raw.invert.data %>%
  filter(biom_stat != "der", ID2 == "spider") %>%
  rename(trap = Sample,
         guild = Guild,
         order = Order,
         date_set = date.set,
         date_collected = date.collected,
         n_trap_nights = X..trap.nights,
         n_traps = X..traps,
         n_effective_traps = X..effective.traps,
         adj_biomass = adj.biomass,
         adj_biomass_mg = adj.biomass.mg) %>%
  select(region:adj_biomass_mg) %>%
  print() 

# Calculate TOTAL biomass per trap, adjusted for effort
# columns were re-named to follow variable naming convention used in calculating mouse CPUE and Nelson & Clark 1973 
# Ends up being biomass/# nights trap was out - simple adjusted biomass effort calculation

trap.total.spider.adj <- biomass.spider.data %>%
  rename(p=n_trap_nights,
         i=n_traps) %>%
  group_by(site, dist_group, trap, p, i) %>%
  summarize(total_biomass = sum(biomass)) %>%
  mutate(adj_biomass = total_biomass/(p*i)) %>%
  print()


#########################################################################################################################################################

#                                                               STATISTICAL COMPARISONS

# Over distance 
lmBs <- lm(trap.total.spider.adj$adj_biomass ~ trap.total.spider.adj$dist_group)
resBs <- resid(lmBs)
plot(resBs)
hist(resBs)
qqnorm(resBs)
qqline(resBs)

# Log-transformation
lmBs.l <- lm(log10(trap.total.spider.adj$adj_biomass) ~ trap.total.spider.adj$dist_group)
resBs.l <- resid(lmBs.l)
plot(resBs.l)
hist(resBs.l)
qqnorm(resBs.l)
qqline(resBs.l)

summary(aovBs.l <- aov(log10(trap.total.spider.adj$adj_biomass) ~ trap.total.spider.adj$dist_group))
TukeyHSD(aovBs.l)


#########################################################################################################################################################

#                                                               PLOTS


# Summarize mean +/- SE 
biomass.adj.spider.summary <- trap.total.spider.adj %>% 
  group_by(dist_group) %>% 
  summarize(mean_adj_biom = mean(adj_biomass), se_adj_biom = sd(adj_biomass)/sqrt(length(adj_biomass))) %>% 
  print()

biomass.adj.spider.summary$dist_group <- factor(biomass.adj.spider.summary$dist_group, levels=c("0-25", "50-75", "100-125", "150-200"), ordered=T)

ggplot(data=biomass.adj.spider.summary, aes(x=dist_group, y=mean_adj_biom)) + 
  geom_bar(stat="identity", colour="black", size=1.5) +
  geom_errorbar(aes(ymin = mean_adj_biom-se_adj_biom, ymax = mean_adj_biom+se_adj_biom), width=.09, size=1, colour="black") + 
  labs(x = "Distance from beach (m)", y="Biomass (g)") +
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
        legend.margin = margin(t=-2,b=4,l=5,r=8)) 





























