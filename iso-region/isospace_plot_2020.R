# SIMPLE ISOSPACE PLOT - post CJZ reviews. 
# 3 food groups only considered here, as used in MixSIAR 
# isospace_6nov17 is the archival script
# 20 June 2020

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")

#########################################################################################################################################################


#########################################################################################################################################################

#                                                           DATA PROCESSING TO CREATE BASE FILES 

# CV and GS summaries were made in 'mixsair_new_6nov17' script. Copied here for simplicity.
# NO NEED TO RE-RUN, FILES ALREADY CREATED


# #------------ Load/processing
#iso_db <- read.csv("invert_veg_iso_mouse_6nov17.csv")
#m <- read.csv("mouse_genrep_iso.csv")

# create new column for easy simplification 
#iso_db <- iso_db %>% 
#  mutate(simple_source = paste(paste(gsub("-", "-", taxa)), location, sep="-")) 


#------------ CV source
#iso_CV_s <- iso_db %>% 
#  group_by(region, simple_source) %>% 
#  filter(region =="CV", ID != "mouse", ID != "mushroom", ID != "lichen/fungus", ID != "slug", ID != "myanthemum") %>%
#  summarize(Meand13C = mean(d13C), Meand15N = mean(d15N), SDd13C = sd(d13C), SDd15N = sd(d15N), n = length(d13C_FINAL)) %>%
#  print(iso_CV) %T>%
#  write.csv("CV_preygroups_simple.csv", row.names = F)

#------------ GS source 
#iso_GS_s <- iso_db %>% 
#  group_by(region, simple_source) %>% 
#  filter(region =="GS", ID != "mouse", ID != "mushroom", ID != "grass", ID !="sedge", ID != "slug", ID != "bunchberry") %>% 
#  summarize(Meand13C = mean(d13C), Meand15N = mean(d15N), SDd13C = sd(d13C), SDd15N = sd(d15N), n = length(d13C_FINAL)) %>%
#  print(iso_GS) %T>%
#  write.csv("GS_preygroups_simple.csv", row.names = F)


#########################################################################################################################################################
#########################################################################################################################################################

#                                                  PLOTS AND DETAILS

######
# CV #
######

# food sources 
CV.food <- read.csv("CV_preygroups_simple.csv")
CV.food <- CV.food %>% 
  mutate(meanCdisc = ifelse(simple_source == "plant-forest", Meand13C+2, Meand13C+1)) %>% 
  mutate(sdCdisc = ifelse(simple_source == "plant-forest", SDd13C+2, SDd13C+1)) %>% 
  mutate(meanNdisc = Meand15N+3.3) %>% 
  mutate(sdNdisc = SDd15N+3.3) %>% 
  print()

# plot
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot() +
  geom_segment(mapping=aes(x=-24.98431, y=(4.253474+6.567474), xend=-29.22368, yend=(-2.459992+5.549859)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-29.22368, y=(-2.459992+5.549859), xend=(-29.22368-3.241711), yend=-2.459992), colour="black",size=1) +
  geom_segment(mapping=aes(x=(-29.22368-3.241711), y=-2.459992, xend=-29.22368, yend=(-2.459992-5.549859)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-29.22368, y=(-2.459992-5.549859), xend=-24.98431, yend=(4.253474-6.567474)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-24.98431, y=(4.253474-6.567474), xend=-16.18191, yend=(13.754791-4.984103)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-16.18191, y=(13.754791-4.984103), xend=(-16.18191+2.414513), yend=13.754791), colour="black",size=1) +
  geom_segment(mapping=aes(x=(-16.18191+2.414513), y=13.754791, xend=-16.18191, yend=(13.754791+4.984103)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-16.18191, y=(13.754791+4.984103), xend=-24.98431, yend=(4.253474+6.567474)), colour="black",size=1) +
  geom_errorbar(data=CV.food, aes(x=meanCdisc, y=meanNdisc, ymin = meanNdisc-sdNdisc, ymax = meanNdisc+sdNdisc), width=0.4, size=1.1) +
  geom_errorbarh(data=CV.food, aes(x=meanCdisc, y=meanNdisc, xmin=meanCdisc-sdCdisc, xmax=meanCdisc+sdCdisc), height=0.6, size=1.1) +
  geom_point(data=CV.food, aes(x=meanCdisc, y=meanNdisc), size=7, fill="white", colour="black", shape=21, stroke=1.3) + 
  geom_point(data=subset(m, region == "CV"), aes(x=mouse_d13C, y=mouse_d15N, shape=Gender, fill=Gender), size=6, colour="black", stroke=1.3) +
  scale_shape_manual(values=c(22, 24)) +
  scale_fill_manual(values=c("gray70", "black")) +
  scale_y_continuous(limits=c(-10,20), labels=scaleFUN) +
  scale_x_continuous(labels=scaleFUN) +
  labs(x="", y="", shape="Sex", fill="Sex") +
  xlab(expression(paste(delta^{13},'C (\211)')))+
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black", size=30),
        axis.title.x = element_text(colour = "black", size=34),
        axis.title.y = element_text(colour = "black", size=34),
        axis.ticks = element_line(size = 1, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position="none") 
  #annotate(geom="text", x=-34, y=20, label="A", color="black", size=6, fontface=2) 






######
# GS #
######

# food sources 
GS.food <- read.csv("GS_preygroups_simple.csv")
GS.food <- GS.food %>% 
  mutate(meanCdisc = ifelse(simple_source == "plant-forest", Meand13C+2, Meand13C+1)) %>% 
  mutate(sdCdisc = ifelse(simple_source == "plant-forest", SDd13C+2, SDd13C+1)) %>% 
  mutate(meanNdisc = Meand15N+3.3) %>% 
  mutate(sdNdisc = SDd15N+3.3) %>% 
  print()

# plot
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot() +
  geom_segment(mapping=aes(x=-28.00610, y=(0.932606+7.927660), xend=(-28.00610-3.913290), yend=0.932606), colour="black",size=1) +
  geom_segment(mapping=aes(x=(-28.00610-3.913290), y=0.932606, xend=-28.00610, yend=(0.932606-7.927660)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-28.00610, y=(0.932606-7.927660), xend=-14.10833, yend=(14.162294-4.923006)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-14.10833, y=(14.162294-4.923006), xend=(-14.10833+3.195423), yend=14.162294), colour="black",size=1) +
  geom_segment(mapping=aes(x=(-14.10833+3.195423), y=14.162294, xend=-14.10833, yend=(14.162294+4.923006)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-14.10833, y=(14.162294+4.923006), xend=-25.67047, yend=(6.055598+6.688425)), colour="black",size=1) +
  geom_segment(mapping=aes(x=-25.67047, y=(6.055598+6.688425), xend=-28.00610, yend=(0.932606+7.927660)), colour="black",size=1) +
  geom_errorbar(data=GS.food, aes(x=meanCdisc, y=meanNdisc, ymin=meanNdisc-sdNdisc, ymax=meanNdisc+sdNdisc), width=0.4, size=1.1) +
  geom_errorbarh(data=GS.food, aes(x=meanCdisc, y=meanNdisc, xmin=meanCdisc-sdCdisc, xmax=meanCdisc+sdCdisc), height=0.6, size=1.1) +
  geom_point(data=GS.food, aes(x=meanCdisc, y=meanNdisc), size=7, fill="white", colour="black", shape=21, stroke=1.3) + 
  geom_point(data=subset(m, region == "GS"), aes(x=mouse_d13C, y=mouse_d15N, shape=Gender, fill=Gender), size=6, colour="black", stroke=1.3) +
  scale_shape_manual(values=c(22, 24)) +
  scale_fill_manual(values=c("gray70", "black")) +
  scale_y_continuous(limits=c(-10,20), labels=scaleFUN) +
  scale_x_continuous(limits=c(-32,-10), breaks = seq(-31,-10, by=6), labels=scaleFUN) +
  labs(x="", y="", shape="Sex", fill="Sex") + 
  xlab(expression(paste(delta^{13},'C (\211)')))+
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", size=1.2),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text = element_text(colour = "black", size=30),
        axis.title.x = element_text(colour = "black", size=34),
        axis.title.y = element_text(colour = "black", size=34),
        axis.ticks = element_line(size = 1, colour = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        legend.position="none")
  #annotate(geom="text", x=-32, y=20, label="B", color="black", size=6, fontface=2) 



