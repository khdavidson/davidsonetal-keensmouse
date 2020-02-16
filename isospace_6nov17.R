## isospace plot

setwd("~/UVic/`Field Work 2016/`RESULTS/Data files")
isodb <- read.csv("invert_veg_iso_mouse_6nov17.csv")
library(dplyr)
library(ggplot2)

#summarize PREY to mean +/- SE for CV
#CViso_groups <- isodb %>% 
#  group_by(region,guild2, location) %>% 
#  filter(region =="CV",guild != "mouse", ID != "lichen/fungus", guild != "mushroom", guild != "grass", guild !="sedge", region != "all", ID != "slug", ID != "myanthemum") %>% 
#  summarize(meanC = mean(d13C_frac), seC = sd(d13C_frac)/sqrt(length(d13C_frac)), meanN = mean(d15Nfrac), 
#            seN = sd(d15Nfrac)/sqrt(length(d15Nfrac))) %>% 
#  print(iso_groups)

#write.csv(CViso_groups, "CVfinal_iso_groups.csv")

#summarize PREY to mean +/- SE for GS
#GSiso_groups <- isodb %>% 
#  group_by(region,guild2, location) %>% 
#  filter(region =="GS",guild != "mouse", ID != "lichen/fungus", guild != "mushroom", guild != "grass", guild !="sedge", region != "all", ID != "slug", ID != "myanthemum") %>% 
#  summarize(meanC = mean(d13C_frac), seC = sd(d13C_frac)/sqrt(length(d13C_frac)), meanN = mean(d15Nfrac), 
#            seN = sd(d15Nfrac)/sqrt(length(d15Nfrac))) %>% 
#  print(iso_groups)

#write.csv(GSiso_groups, "GSfinal_iso_groups.csv")


########

#summarize MICE separately because need to divide by GS/CV 
## first CV  REGION 
#CViso_mice_R <- isodb %>% 
#  group_by(region,guild2,location) %>%
#  filter(region =="CV",ID == "mouse") %>% 
#  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
#  print(CViso_mice_R)

## first CV  IND 
#CViso_mice_I <- isodb %>% 
#  group_by(sampleID) %>%
#  filter(region =="CV",ID == "mouse") %>% 
#  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
#  print(CViso_mice_I)

#write.csv(CViso_mice_I, "CViso_mice_Ind.csv")




## now GS  REGION
#GSiso_mice_R <- isodb %>% 
#  group_by(region,guild2,location) %>%
#  filter(region =="GS",ID == "mouse") %>% 
#  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
#  print(GSiso_mice_R)

## now GS  IND
#GSiso_mice_I <- isodb %>% 
#  group_by(sampleID) %>%
#  filter(region =="GS",ID == "mouse") %>% 
#  summarize(meanC = mean(d13C), seC = sd(d13C)/sqrt(length(d13C)), meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
#  print(GSiso_mice_I)

#write.csv(GSiso_mice_I, "GSiso_mice_Ind.csv")





#####   EVERYTHING DONE ABOVE WAS MANUALLY JOINED IN EXCEL - DON'T RE-RUN THE CODE!  USE FILES BELOW INSTEAD: 


CV_df <- read.csv("ISOSPACE_CV.csv")
GS_df <- read.csv("ISOSPACE_GS.csv")


CVcoords <- read.csv("CVcoords.csv")

# make plot 
scaleFUN <- function(x) sprintf("%.1f", x)

CV_df2 <- CV_df %>%
  group_by(guild2) %>% 
  filter(location != "unk") %>% 
  print(CV_df2)


##############################################################################################################

#
#
##  CV
#
#


## CV mouse isospace - thesis version 
ggplot(CV_df2, aes(x=meanC, y=meanN,  group = location, shape = shape)) +
  xlim(-33,-5) +
  ylim(-8,17) +
  labs(x = "",
       y = "") +
  geom_segment(mapping=aes(x = -29.22368421, y = -1.943839165, xend=-24.93081633, yend=6.070566521), colour="black",size=1) +
  geom_segment(mapping=aes(x = -24.93081633, y = 6.070566521, xend=-17.52622024, yend=15.61976636), colour="black",size=1) +
  geom_segment(mapping=aes(x = -16.78201506, y = 15.61976636, xend=-15.44778331, yend=12.69797113), colour="black",size=1) +
  geom_segment(mapping=aes(x = -15.44778331, y = 12.69797113, xend=-25.148125, yend=-1.286202519), colour="black",size=1) +
  geom_segment(mapping=aes(x = -25.148125, y = -1.286202519, xend=-29.22368421, yend=-2.976145453), colour="black",size=1) +
  geom_errorbar(data = CV_df2, aes(ymin = meanN-seN,
                                       ymax = meanN+seN),width=.2,size=1) + 
  geom_errorbarh(data = CV_df2, aes(xmin = meanC-seC,
                                        xmax = meanC+seC),height=.2,size=1) +
  scale_shape_manual(values=c(22,24,21)) +
  scale_fill_manual(breaks = c("forest", "intertidal", "RM", "NrM", "RF", "NrF"), 
                    values=c("#19c740","#333ccc","#ffe0b2","#e0b2ff","#FF9A00", "#8a00e5")) +
  geom_point(data = CV_df2, aes(fill = location),size=10,stroke=2) +
 # geom_text(aes(label = location),hjust=0, vjust=0) +
  scale_y_continuous(labels=scaleFUN) + 
  scale_x_continuous(labels=scaleFUN) + 
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  # theme(panel.grid.minor = element_line(colour = "transparent")) + 
  # theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = " black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  xlab(expression(paste(delta^{13},'C (\211)')))+
  ylab(expression(paste(delta^{15},'N (\211)')))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))

  



## CV mouse isospace - paper version  
ggplot(CV_df2, aes(x=meanC, y=meanN,  group = location, shape = shape)) +
  xlim(-33,-5) +
  ylim(-8,17) +
  labs(x = "",
       y = "") +
  geom_segment(mapping=aes(x = -29.22368421, y = -1.943839165, xend=-24.93081633, yend=6.070566521), colour="black",size=1) +
  geom_segment(mapping=aes(x = -24.93081633, y = 6.070566521, xend=-17.52622024, yend=15.61976636), colour="black",size=1) +
  geom_segment(mapping=aes(x = -16.78201506, y = 15.61976636, xend=-15.44778331, yend=12.69797113), colour="black",size=1) +
  geom_segment(mapping=aes(x = -15.44778331, y = 12.69797113, xend=-25.148125, yend=-1.286202519), colour="black",size=1) +
  geom_segment(mapping=aes(x = -25.148125, y = -1.286202519, xend=-29.22368421, yend=-2.976145453), colour="black",size=1) +
  geom_errorbar(data = CV_df2, aes(ymin = meanN-seN,
                                   ymax = meanN+seN),width=.2,size=1) + 
  geom_errorbarh(data = CV_df2, aes(xmin = meanC-seC,
                                    xmax = meanC+seC),height=.2,size=1) +
  scale_shape_manual(values=c(22,24,21)) +
  scale_fill_manual(breaks = c("forest", "intertidal", "RM", "NrM", "RF", "NrF"), 
                    values=c("white","white","gray30","gray30","gray70", "gray70")) +
  geom_point(data = CV_df2, aes(fill = location),size=7,stroke=2.2) +
  # geom_text(aes(label = location),hjust=0, vjust=0) +
  scale_y_continuous(labels=scaleFUN) + 
  expand_limits(y = c(-5,17)) +
  scale_x_continuous(labels=scaleFUN) + 
  expand_limits(x = c(-32,-13))+
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  # theme(panel.grid.minor = element_line(colour = "transparent")) + 
  # theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = " black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  xlab(expression(paste(delta^{13},'C (\211)')))+
  ylab(expression(paste(delta^{15},'N (\211)')))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))





# CV - black presentation version - defense 
CV_df2$location <- factor(CV_df2$location, levels = c('forest','intertidal', "NrF", "NrM", "RM", "RF"),ordered = TRUE)

CV_sum <- CV_df2 %>% 
  group_by(type) %>% 
  summarize(meanC1 = mean(meanC), seC = sd(meanC)/sqrt(length(meanC)), meanN1 = mean(meanN), seN = sd(meanN)/sqrt(length(meanN))) %>% 
  print(CV_sum)

names(CV_sum)[2] <- "meanCC"
names(CV_sum)[4] <- "meanNN"


ggplot() +
  labs(x = "",
       y = "") +
  geom_segment(mapping=aes(x = -29.22368421, y = -1.943839165, xend=-24.93081633, yend=6.070566521), colour="white",size=1) +
  geom_segment(mapping=aes(x = -24.93081633, y = 6.070566521, xend=-17.52622024, yend=15.61976636), colour="white",size=1) +
  geom_segment(mapping=aes(x = -16.78201506, y = 15.61976636, xend=-15.44778331, yend=12.69797113), colour="white",size=1) +
  geom_segment(mapping=aes(x = -15.44778331, y = 12.69797113, xend=-25.148125, yend=-1.286202519), colour="white",size=1) +
  geom_segment(mapping=aes(x = -25.148125, y = -1.286202519, xend=-29.22368421, yend=-2.976145453), colour="white",size=1) +
  geom_errorbar(data=subset(CV_df2, shape=="prey"), aes(x=meanC, y=meanN,ymin = meanN-seN,
                                   ymax = meanN+seN),width=.2,size=1, colour = "white") + 
  geom_errorbarh(data=subset(CV_df2, shape=="prey"), aes(x=meanC, y=meanN,xmin = meanC-seC,
                                    xmax = meanC+seC),height=.2,size=1, colour = "white") +
  geom_errorbar(data=subset(CV_sum, type=="consumer"), aes(x=meanCC,y=meanNN, ymin = meanNN-seN,
                                                        ymax = meanNN+seN),width=.2,size=1, colour = "white") + 
  geom_errorbarh(data=subset(CV_sum, type=="consumer"), aes(x=meanCC,y=meanNN, xmin = meanCC-seC,
                                                         xmax = meanCC+seC),height=.2,size=1, colour = "white") +
 # scale_shape_manual(values=c(24,21)) +
  geom_point(data=subset(CV_df2, shape == "prey"), aes (x=meanC, y=meanN),
             size =10, stroke=2, fill="black", colour="white", shape=21) +
  geom_point(data=subset(CV_sum, type == "consumer"), aes(x=meanCC, y=meanNN), 
             size =10, stroke=2, colour="white", shape=24, fill="gray50") + 
 #  geom_text(aes(label = location),hjust=0, vjust=0,colour="white") +
  scale_y_continuous(labels=scaleFUN) + 
  expand_limits(y = c(-5,17)) +
  scale_x_continuous(labels=scaleFUN) + 
  expand_limits(x = c(-32,-13))+
  theme(text = element_text(size=58)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 1, colour = "white")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))+
  #xlab(expression(paste(delta^{13},'C (\211)')))+
  #ylab(expression(paste(delta^{15},'N (\211)')))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))
#














 

#########################################################################################################################

#
#
##  GS
#
#


## GS mouse isospace - thesis version
ggplot(GS_df, aes(x=meanC, y=meanN, group = location, shape=shape,  fill = location)) +
  xlim(-30,-15) +
  ylim(-8,17) +
  labs(x = "",
       y = "") +
  geom_segment(mapping=aes(x = -27.79166667, y = 1.216805048, xend=-25.96143948, yend=7.184430355), colour="black",size=1) +
  geom_segment(mapping=aes(x = -25.77942308, y = 7.498934178, xend=-14.53915774, yend=15.96501975), colour="black",size=1) +
  geom_segment(mapping=aes(x = -13.8672059, y = 15.96501975, xend=-13.5907909, yend=12.92292023), colour="black",size=1) +
  geom_segment(mapping=aes(x = -13.5907909, y = 12.92292023, xend=-25.19833333, yend=0.261058367), colour="black",size=1) +
  geom_segment(mapping=aes(x = -25.19833333, y = 0.261058367, xend=-27.79166667, yend=0.457938396), colour="black",size=1) +
  geom_errorbar(data = GS_df, aes(ymin = meanN-seN,
                                       ymax = meanN+seN),width=.2,size=1) + 
  geom_errorbarh(data = GS_df, aes(xmin = meanC-seC,
                                        xmax = meanC+seC),height=.2,size=1) +
  geom_point(data = GS_df, aes(fill = location),size=10,stroke=2) +
 # geom_text(aes(label = location),hjust=0, vjust=0) +
  scale_shape_manual(values=c(22,24,21)) +
  scale_fill_manual(breaks = c("forest", "intertidal", "RM", "NrM", "RF", "NrF"), 
                    values=c("#19c740","#333ccc","#ffe0b2","#e0b2ff","#FF9A00", "#8a00e5")) +
  scale_y_continuous(labels=scaleFUN) + 
  scale_x_continuous(limits=c(-30, -13),breaks=seq(-30, -13, by = 5), labels=scaleFUN) + 
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  # theme(panel.grid.minor = element_line(colour = "transparent")) + 
  # theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = " black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  xlab(expression(paste(delta^{13},'C (\211)')))+
  ylab(expression(paste(delta^{15},'N (\211)')))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))



# GS isospace - MS version
ggplot(GS_df, aes(x=meanC, y=meanN, group = location, shape=shape,  fill = location)) +
  xlim(-30,-15) +
  ylim(-8,17) +
  labs(x = "",
       y = "") +
  geom_segment(mapping=aes(x = -27.79166667, y = 1.216805048, xend=-25.96143948, yend=7.184430355), colour="black",size=1) +
  geom_segment(mapping=aes(x = -25.77942308, y = 7.498934178, xend=-14.53915774, yend=15.96501975), colour="black",size=1) +
  geom_segment(mapping=aes(x = -13.8672059, y = 15.96501975, xend=-13.5907909, yend=12.92292023), colour="black",size=1) +
  geom_segment(mapping=aes(x = -13.5907909, y = 12.92292023, xend=-25.19833333, yend=0.261058367), colour="black",size=1) +
  geom_segment(mapping=aes(x = -25.19833333, y = 0.261058367, xend=-27.79166667, yend=0.457938396), colour="black",size=1) +
  geom_errorbar(data = GS_df, aes(ymin = meanN-seN,
                                  ymax = meanN+seN),width=.2,size=1) + 
  geom_errorbarh(data = GS_df, aes(xmin = meanC-seC,
                                   xmax = meanC+seC),height=.2,size=1) +
  geom_point(data = GS_df, aes(fill = location),size=7,stroke=2.2) +
  # geom_text(aes(label = location),hjust=0, vjust=0) +
  scale_shape_manual(values=c(22,24,21)) +
  scale_fill_manual(breaks = c("forest", "intertidal", "RM", "NrM", "RF", "NrF"), 
                    values=c("white","white","gray30","gray30","gray70", "gray70")) +
  scale_y_continuous(labels=scaleFUN) + 
  expand_limits(y = c(-2,18)) +
  scale_x_continuous(labels=scaleFUN) + 
  expand_limits(x = c(-30,-12))+
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  # theme(panel.grid.minor = element_line(colour = "transparent")) + 
  # theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = " black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.5))+
  xlab(expression(paste(delta^{13},'C (\211)')))+
  ylab(expression(paste(delta^{15},'N (\211)')))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))






# GS isospace -  powerpoint 
GS_df$location <- factor(GS_df$location, levels = c('forest','intertidal', "NrF", "NrM", "RM", "RF"),ordered = TRUE)

ggplot(GS_df, aes(x=meanC, y=meanN)) +
  labs(x = "",
       y = "") +
  geom_segment(mapping=aes(x = -27.79166667, y = 1.216805048, xend=-25.96143948, yend=7.184430355), colour="white",size=1) +
  geom_segment(mapping=aes(x = -25.77942308, y = 7.498934178, xend=-14.53915774, yend=15.96501975), colour="white",size=1) +
  geom_segment(mapping=aes(x = -13.8672059, y = 15.96501975, xend=-13.5907909, yend=12.92292023), colour="white",size=1) +
  geom_segment(mapping=aes(x = -13.5907909, y = 12.92292023, xend=-25.19833333, yend=0.261058367), colour="white",size=1) +
  geom_segment(mapping=aes(x = -25.19833333, y = 0.261058367, xend=-27.79166667, yend=0.457938396), colour="white",size=1) +
  geom_errorbar(data=subset(GS_df, shape=="prey"), aes(ymin = meanN-seN,
                                   ymax = meanN+seN),width=.2,size=1, colour = "white") + 
  geom_errorbarh(data=subset(GS_df, shape=="prey"), aes(xmin = meanC-seC,
                                    xmax = meanC+seC),height=.2,size=1, colour = "white") +
#  scale_shape_manual(values=c(22,24,21)) +
  #scale_fill_manual(breaks = c('forest','intertidal', "NrF", "NrM", "RM", "RF"), 
  #                  values=c("black","black","#fdbf99","#abe3f5","#0700db", "#fb5f02")) +
  geom_point(data=subset(GS_df, shape=="prey"), shape=21,size=15,stroke=2.2, colour="white",fill="black") +
  #  geom_text(aes(label = location),hjust=0, vjust=0,colour="white") +
  scale_y_continuous(labels=scaleFUN) + 
  expand_limits(y = c(-2,18)) +
  scale_x_continuous(labels=scaleFUN) + 
  expand_limits(x = c(-30,-12))+
  theme(text = element_text(size=80)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 1, colour = "white")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))+
  #xlab(expression(paste(delta^{13},'C (\211)')))+
  #ylab(expression(paste(delta^{15},'N (\211)')))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))




GS_sum <- GS_df %>% 
  group_by(type) %>% 
  summarize(meanCC = mean(meanC), seC = sd(meanC)/sqrt(length(meanC)), meanNN = mean(meanN), seN = sd(meanN)/sqrt(length(meanN))) %>% 
  print(GS_sum)





ggplot() +
  labs(x = "",
       y = "") +
  geom_segment(mapping=aes(x = -27.79166667, y = 1.216805048, xend=-25.96143948, yend=7.184430355), colour="white",size=1) +
  geom_segment(mapping=aes(x = -25.77942308, y = 7.498934178, xend=-14.53915774, yend=15.96501975), colour="white",size=1) +
  geom_segment(mapping=aes(x = -13.8672059, y = 15.96501975, xend=-13.5907909, yend=12.92292023), colour="white",size=1) +
  geom_segment(mapping=aes(x = -13.5907909, y = 12.92292023, xend=-25.19833333, yend=0.261058367), colour="white",size=1) +
  geom_segment(mapping=aes(x = -25.19833333, y = 0.261058367, xend=-27.79166667, yend=0.457938396), colour="white",size=1) +
  geom_errorbar(data=subset(GS_df, shape=="prey"), aes(x=meanC, y=meanN,ymin = meanN-seN,
                                                        ymax = meanN+seN),width=.2,size=1, colour = "white") + 
  geom_errorbarh(data=subset(GS_df, shape=="prey"), aes(x=meanC, y=meanN,xmin = meanC-seC,
                                                         xmax = meanC+seC),height=.2,size=1, colour = "white") +
  geom_errorbar(data=subset(GS_sum, type=="consumer"), aes(x=meanCC,y=meanNN, ymin = meanNN-seN,
                                                           ymax = meanNN+seN),width=.2,size=1, colour = "white") + 
  geom_errorbarh(data=subset(GS_sum, type=="consumer"), aes(x=meanCC,y=meanNN, xmin = meanCC-seC,
                                                            xmax = meanCC+seC),height=.2,size=1, colour = "white") +
  # scale_shape_manual(values=c(24,21)) +
  geom_point(data=subset(GS_df, shape == "prey"), aes (x=meanC, y=meanN),
             size =10, stroke=2, fill="black", colour="white", shape=21) +
  geom_point(data=subset(GS_sum, type == "consumer"), aes(x=meanCC, y=meanNN), 
             size =10, stroke=2, colour="white", shape=24, fill="gray50") + 
  #  geom_text(aes(label = location),hjust=0, vjust=0,colour="white") +
  scale_y_continuous(labels=scaleFUN) + 
  expand_limits(y = c(-2,18)) +
  scale_x_continuous(labels=scaleFUN) + 
  expand_limits(x = c(-30,-12))+
  theme(text = element_text(size=58)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
  theme(panel.grid.major = element_blank()) + 
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 1, colour = "white")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))+
  #xlab(expression(paste(delta^{13},'C (\211)')))+
  #ylab(expression(paste(delta^{15},'N (\211)')))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))









##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

# SIMPLE ISOSPACE PLOT - post CJZ reviews. 3 food groups only considered here. 

iso_db <- read.csv("invert_veg_iso_mouse_6nov17.csv")
m <- read.csv("mouse_genrep_iso.csv")

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
  geom_segment(mapping=aes(x=-24.98431, y=(4.253474+6.567474), xend=-29.22368, yend=(-2.459992+5.549859)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-29.22368, y=(-2.459992+5.549859), xend=(-29.22368-3.241711), yend=-2.459992), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=(-29.22368-3.241711), y=-2.459992, xend=-29.22368, yend=(-2.459992-5.549859)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-29.22368, y=(-2.459992-5.549859), xend=-24.98431, yend=(4.253474-6.567474)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-24.98431, y=(4.253474-6.567474), xend=-16.18191, yend=(13.754791-4.984103)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-16.18191, y=(13.754791-4.984103), xend=(-16.18191+2.414513), yend=13.754791), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=(-16.18191+2.414513), y=13.754791, xend=-16.18191, yend=(13.754791+4.984103)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-16.18191, y=(13.754791+4.984103), xend=-24.98431, yend=(4.253474+6.567474)), colour="black",size=0.5) +
  geom_errorbar(data=CV.food, aes(x=meanCdisc, y=meanNdisc, ymin = meanNdisc-sdNdisc, ymax = meanNdisc+sdNdisc), width=0.4, size=1) +
  geom_errorbarh(data=CV.food, aes(x=meanCdisc, y=meanNdisc, xmin=meanCdisc-sdCdisc, xmax=meanCdisc+sdCdisc), height=0.4, size=1) +
  geom_point(data=CV.food, aes(x=meanCdisc, y=meanNdisc), size=4, fill="white", colour="black", shape=21, stroke=2) + 
  geom_point(data=subset(m, region == "CV"), aes(x=mouse_d13C, y=mouse_d15N, shape=Gender, fill=Gender), size=3, colour="black", stroke=2) +
  scale_shape_manual(values=c(22, 24)) +
  scale_fill_manual(values=c("gray70", "black")) +
  scale_y_continuous(limits=c(-10,20), labels=scaleFUN) +
  scale_x_continuous(labels=scaleFUN) +
  labs(x="", y="", shape="Sex", fill="Sex") +
  xlab(expression(paste(delta^{13},'C (\211)')))+
  ylab(expression(paste(delta^{15},'N (\211)'))) +
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
  annotate(geom="text", x=-34, y=20, label="A", color="black", size=6, fontface=2) 
  
    




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
  geom_segment(mapping=aes(x=-28.00610, y=(0.932606+7.927660), xend=(-28.00610-3.913290), yend=0.932606), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=(-28.00610-3.913290), y=0.932606, xend=-28.00610, yend=(0.932606-7.927660)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-28.00610, y=(0.932606-7.927660), xend=-14.10833, yend=(14.162294-4.923006)), colour="black",size=0.5) +
  #geom_segment(mapping=aes(x=-28.00610, y=(0.932606-7.927660), xend=(-28.00610+3.913290), yend=0.932606), colour="black",size=0.5) +
  #geom_segment(mapping=aes(x=(-28.00610+3.913290), y=0.932606, xend=-14.10833, yend=(14.162294-4.923006)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-14.10833, y=(14.162294-4.923006), xend=(-14.10833+3.195423), yend=14.162294), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=(-14.10833+3.195423), y=14.162294, xend=-14.10833, yend=(14.162294+4.923006)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-14.10833, y=(14.162294+4.923006), xend=-25.67047, yend=(6.055598+6.688425)), colour="black",size=0.5) +
  geom_segment(mapping=aes(x=-25.67047, y=(6.055598+6.688425), xend=-28.00610, yend=(0.932606+7.927660)), colour="black",size=0.5) +
  geom_errorbar(data=GS.food, aes(x=meanCdisc, y=meanNdisc, ymin=meanNdisc-sdNdisc, ymax=meanNdisc+sdNdisc), width=0.4, size=1) +
  geom_errorbarh(data=GS.food, aes(x=meanCdisc, y=meanNdisc, xmin=meanCdisc-sdCdisc, xmax=meanCdisc+sdCdisc), height=0.4, size=1) +
  geom_point(data=GS.food, aes(x=meanCdisc, y=meanNdisc), size=4, fill="white", colour="black", shape=21, stroke=2) + 
  geom_point(data=subset(m, region == "GS"), aes(x=mouse_d13C, y=mouse_d15N, shape=Gender, fill=Gender), size=3, colour="black", stroke=2) +
  scale_shape_manual(values=c(22, 24)) +
  scale_fill_manual(values=c("gray70", "black")) +
  scale_y_continuous(limits=c(-10,20), labels=scaleFUN) +
  scale_x_continuous(limits=c(-32,-10), breaks = seq(-31,-10, by=6), labels=scaleFUN) +
  labs(x="", y="", shape="Sex", fill="Sex") + 
  xlab(expression(paste(delta^{13},'C (\211)')))+
#  ylab(expression(paste(delta^{15},'N (\211)'))) +
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
  annotate(geom="text", x=-32, y=20, label="B", color="black", size=6, fontface=2) 



















