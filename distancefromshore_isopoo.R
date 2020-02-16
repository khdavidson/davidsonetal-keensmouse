##### Dist from shore isotope 2015

setwd("~/UVic/`Hakai 100 Islands 2015/`DATA/Isotopes")
poo = read.csv("Tray70bRversion_13Ccorr.csv")
library(ggplot2)

sand = subset(poo, substrate=="Sand", select=dist_beach:d15N)
cobble = subset(poo, substrate=="Cobble", select=dist_beach:d15N)


### check out if sand and cobble trends are the same trends/signatures (pool them if so)

#d15N
plot(sand$dist_beach, sand$d15N,
     xlim=c(0,130),
     xaxt="n",
     xlab = "Distance from Beach (m)",
     ylab = "d15N",
     pch = 20,
     cex = 1,
     font.lab = 2)
axis(side = 1, at=c(0,25,50,75,100,125), las=0)

plot(cobble$dist_beach, cobble$d15N,
     xlim=c(0,130),
     xaxt="n",
     xlab = "Distance from Beach (m)",
     ylab = "d15N",
     pch = 20,
     cex = 1,
     font.lab = 2)
axis(side = 1, at=c(0,25,50,75,100,125), las=0)


#d13C
plot(sand$dist_beach, sand$d13C,
     xlim=c(0,130),
     xaxt="n",
     xlab = "Distance from Beach (m)",
     ylab = "d15N",
     pch = 20,
     cex = 1,
     font.lab = 2)
axis(side = 1, at=c(0,25,50,75,100,125), las=0)

plot(cobble$dist_beach, cobble$d13C,
     xlim=c(0,130),
     xaxt="n",
     xlab = "Distance from Beach (m)",
     ylab = "d15N",
     pch = 20,
     cex = 1,
     font.lab = 2)
axis(side = 1, at=c(0,25,50,75,100,125), las=0)



#######################


## d15N black presentation  version
ggplot(data = newdata,
              aes(x = dist_beach,
                  y = d15N)) + 
  ylim(0,10.0) +
  labs(x = "",
       y = "") +
  ggtitle("") + 
  geom_point(colour="#4596ff", size = 12) +
  scale_x_continuous(breaks = c(0,25,50,75,100,125)) +
  theme(text = element_text(size=80)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
  theme(panel.grid.major = element_line(colour = "white", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 1, colour = "white")) +
  theme(legend.position="none")  +
  geom_smooth(aes(x=as.integer(dist_beach),y=d15N),size = 3, colour="#4596ff", method="lm",se=F)+
  theme(plot.margin=unit(c(0,4,1,0),"cm"))+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))
#geom_text(x = 100, y = 7.5, label = lm_eqnGS(newdata), size=2,parse = TRUE)


lm_eqnGS <- function(newdata){
  m <- lm(d15N ~ dist_beach, newdata);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
summary(m)


#npoo + ylab(expression(paste(delta^{15},'N (\211)'))) 





## d13C norm bw presentation version
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data = newdata,
              aes(x = dist_beach,
                  y = d13C)) + 
  ylim(-30,-16) +
  labs(x = "Distance from the beach (m)",
       y = "") +
  ggtitle("") + 
  geom_point(colour = "#4596ff", size=12) +
  scale_x_continuous(breaks = c(0,25,50,75,100,125)) +
  scale_y_continuous(labels=scaleFUN) +
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
  geom_smooth(aes(x=as.integer(dist_beach),y=d13C),size=3, colour= "#4596ff",method="lm",se=F)+
  theme(plot.margin=unit(c(0,4,1,0),"cm"))+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))



lm_eqnGS <- function(newdata){
  m <- lm(d13C ~ dist_beach, newdata);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


cpoo + ylab(expression(paste(delta^{13},'C (\211)'))) 






#################################################################################################
library(dplyr)
# subset data for sand less than 150m 
sand = subset(poo, substrate=="Sand" & dist_beach <= 150, select=dist_beach:Tray.Name)

# summarize and pull out mean +/- SE 
mean <- sand %>% 
  group_by(dist_beach) %>% 
  summarize(meanC = mean(d13C_corr_FINAL), seC = sd(d13C_corr_FINAL)/sqrt(length(d13C_corr_FINAL)), 
            meanN = mean(d15N), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(mean)

lmN <- lm(sand$d15N ~ sand$dist_beach)
summary(lmN)

lmC <- lm(sand$d13C ~ sand$dist_beach)
summary(lmC)

# white plots for figures

## d15N white for figure
ggplot(data = mean,
       aes(x = dist_beach,
           y = meanN)) + 
  ylim(0,10.0) +
  labs(x = "Distance from beach (m)")+
  ggtitle("") + 
  geom_point(colour="black", size = 5) +
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150)) +
  geom_errorbar(data = mean, aes(ymin = meanN-seN,
                                 ymax = meanN+seN),width=3,size=0.7) + 
  annotate("text", x = -4, y = 8.4, label = "3", size=10) +
  annotate("text", x = 21, y = 7.4, label = "3", size=10) +
  annotate("text", x = 46, y = 5.5, label = "3", size=10) +
  annotate("text", x = 71, y = 4.5, label = "2", size=10) +
  annotate("text", x = 96, y = 2.6, label = "3", size=10) +
  annotate("text", x = 121, y = 2.1, label = "2", size=10) +
  annotate("text", x = 146, y = 2.4, label = "2", size=10) +
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
 # theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")  +
  geom_smooth(aes(x=as.integer(dist_beach),y=meanN),size = 1, colour="black", method="lm",se=F)+
  theme(plot.margin=unit(c(0,4,1,0),"cm"))+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.2))+
  ylab(expression(paste(delta^{15},'N (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))
#geom_text(x = 100, y = 7.5, label = lm_eqnGS(newdata), size=2,parse = TRUE)




## d13C not normalized white 
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(data = mean,
       aes(x = dist_beach,
           y = meanC)) + 
  ylim(-20,-10) +
  labs(x = "Distance from beach (m)") +
  ggtitle("") + 
  geom_point(colour = "black", size=5) +
  scale_x_continuous(breaks = c(0,25,50,75,100,125,150)) +
  scale_y_continuous(labels=scaleFUN) +
  geom_errorbar(data = mean, aes(ymin = meanC-seC,
                                 ymax = meanC+seC),width=3,size=0.7) + 
  annotate("text", x = -4.5, y = -18.25, label = "3", size=10) +
  annotate("text", x = 20, y = -17.3, label = "3", size=10) +
  annotate("text", x = 45, y = -17.5, label = "3", size=10) +
  annotate("text", x = 70, y = -19, label = "2", size=10) +
  annotate("text", x = 95, y = -19.3, label = "3", size=10) +
  annotate("text", x = 120, y = -19.85, label = "2", size=10) +
  annotate("text", x = 145, y = -17, label = "2", size=10) +
  theme(text = element_text(size=40)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "white")) + 
#  theme(panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 1.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "black")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none") +
  geom_smooth(aes(x=as.integer(dist_beach),y=meanC),size=1, colour= "black",method="lm",se=F)+
  theme(plot.margin=unit(c(0,4,1,0),"cm"))+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.2)) + 
  ylab(expression(paste(delta^{13},'C (\211)'))) +
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5))








########################################################## 

# stats 
# just going to compare 0 to 150m for poo and hair to test if a significant decline happened over the length of the grid
# also this way i can just have 2 groups and still use Wilcox test 

# subset data for sand less than 150m 
setwd("~/UVic/`Hakai 100 Islands 2015/`DATA/Isotopes")
poo = read.csv("Tray70bRversion_13Ccorr.csv")
sand = subset(poo, substrate=="Sand", select=dist_beach:Tray.Name)

sand$dist_beach <- factor(sand$dist_beach)

## d15N
summary(aovN <- aov(sand$d15N ~ factor(sand$dist_beach)))

fligner.test(sand$d15N ~ as.factor(sand$grp))   #p = 0.15
shapiro.test(aovN$residuals)   #p = 0.99
plot(aovN$residuals)
hist(aovN$residuals)
qqnorm(aovN$residuals)
qqline(aovN$residuals)

#comparison accross all distance intervals 
kruskal.test(sand$d15N ~ sand$dist_beach)
#comparison across distance groupings
kruskal.test(sand$d15N ~ sand$dist_group)
# dunn post-hoc
library(FSA)
dunnTest(sand$d15N ~ sand$dist_group, method="bh")


scaleFUN <- function(x) sprintf("%.1f", x)
sand$dist_group <- factor(sand$dist_group, levels = c('0-25','50-75', "100-125", "150-200"),ordered = TRUE)
ssize <- c("6", "5", "5", "4")

sand_means <- sand %>%
  group_by(dist_group) %>% 
  summarize(meanC = mean(d13C_corr_FINAL), meanN = mean(d15N), seC = sd(d13C_corr_FINAL)/sqrt(length(d13C)), seN = sd(d15N)/sqrt(length(d15N))) %>% 
  print(sand_means)

ggplot(sand_means, aes(x=dist_group, y=meanN, group=1)) + 
  labs(x="Distance from beach (m)", y="") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,10)) +
  geom_text(x=0.56, y=10, label="B", colour="black", size=6, fontface=2) +
  geom_line(colour="black", size=1.3) +
  geom_errorbar(data = sand_means, aes(ymin = meanN - seN,
                                              ymax = meanN + seN), width=0.08, size=1.4, colour="black") +
  geom_point(shape=21, size = 4, stroke=1.9, fill="gray80") + 
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
  ylab(expression(paste(delta^{15},'N (\211)'))) 
  





# d13C
summary(aovC <- aov(sand$d13C_corr_FINAL ~ factor(sand$dist_beach)))

fligner.test(sand$d13C_corr_FINAL ~ as.factor(sand$grp))   #p = 0.55
shapiro.test(aovC$residuals)   #p = 0.85
plot(aovC$residuals)
hist(aovC$residuals)
qqnorm(aovC$residuals)
qqline(aovC$residuals)

# comparison across all distance intervals 
kruskal.test(sand$d13C_corr_FINAL ~ sand$dist_beach)
#comparison accross distance groups 
kruskal.test(sand$d13C_corr_FINAL ~ sand$dist_group)
# dunn post hoc
library(FSA)
dunnTest(sand$d13C_corr_FINAL ~ sand$dist_group, method="bh")


scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(sand_means, aes(x=dist_group, y=meanC, group=1)) + 
  labs(x="Distance from beach (m)", y="") +
  scale_y_continuous(breaks = c(-20, -19,-18,-17,-16), limits=c(-20,-16), labels=scaleFUN) +
  geom_line(colour="black", size=1.3) +
  geom_errorbar(data = sand_means, aes(ymin = meanC - seC,
                                       ymax = meanC + seC), width=0.08, size=1.4, colour="black") +
  geom_point(shape=21, size = 4, stroke=1.9, fill="gray80") + 
  geom_text(x=0.57, y=-16, label="A", colour="black", size=6, fontface=2) +
  geom_text(aes(label=ssize, y = (meanC+seC)),hjust=0.4, vjust=-1, size = 6) +
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
  ylab(expression(paste(delta^{13},'C (\211)'))) 



## N poo plot BLACK POWERPOINT - defense 
scaleFUN <- function(x) sprintf("%.1f", x)

ggplot(sand, aes(x=dist_beach, y=d15N)) + 
  labs(x="", y="") +
  scale_y_continuous(labels=scaleFUN) +
  geom_smooth(method = "lm", formula = y~x, colour="white",size=1.5) +
  geom_point(shape=21, size = 9, stroke=2.15, fill="black", colour="white") + 
  theme(text = element_text(size=55)) +
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
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))




## C poo plot BLACK PPT - defense 
ggplot(sand, aes(x=dist_beach, y=d13C_corr_FINAL)) + 
  labs(x="", y="") +
  expand_limits(y = c(-21,-14)) +
  scale_y_continuous(breaks=seq(-21,-14,2), labels = scaleFUN) +
  geom_smooth(method = "lm", formula = y~x, colour="white", size=1.5) +
  geom_point(shape=21, size = 9, stroke=2.15, fill="black", colour="white") + 
  theme(text = element_text(size=55)) +
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
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3)) +
  theme(plot.margin=unit(c(1,2,1,1),"cm"))










###### old stats below!
# subsample the dataframe to just have 0 and 150 
library(dplyr)
target_dist <- c("0","150")
sub_0_150 <- sand %>% 
  group_by(dist_beach) %>%
  filter(dist_beach %in% target_dist) %>% 
  print(sub_0_150)

sub_0_150$dist_class <- c("0-25", "0-25", "0-25", "0-25", "0-25", "0-25", "125-150", "125-150", "125-150", "125-150")

# wilcox test for C
wilcox.test(sub_0_150$d13C_corr_FINAL ~ sub_0_150$dist_beach)

# wilcox test for N 
wilcox.test(sub_0_150$d15N ~ sub_0_150$dist_beach)


### just re-do this with 0 and 125, to illustrate that the relationship is sig until 150m mark 
target_dist <- c("0","125")
sub_0_125 <- sand %>% 
  group_by(dist_beach) %>%
  filter(dist_beach %in% target_dist) %>% 
  print(sub_0_125)

sub_0_125$dist_class <- c("0-25", "0-25", "0-25", "0-25", "0-25", "0-25", "125-150", "125-150", "125-150", "125-150")

# wilcox test for C
wilcox.test(sub_0_125$d13C_corr_FINAL ~ sub_0_125$dist_beach)

# wilcox test for N 
wilcox.test(sub_0_125$d15N ~ sub_0_125$dist_beach)







###################################################################################################################################

### d15N for powerpoint  black
ggplot(sand, aes(x=as.integer(dist_beach), y=d15N)) + 
  labs(x="", y="") +
  scale_y_continuous(labels=scaleFUN) +
  geom_smooth(aes(x=as.integer(dist_beach),y=d15N),size=1, colour= "white",method="lm")+
  geom_point(shape=21, size = 10, stroke=2.15, fill="black", colour = "white") + 
  theme(text = element_text(size=80)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
 # theme(panel.grid.major = element_line(colour = "gray30", linetype = "dotted", size = 1.2)) + 
  theme(panel.grid.major = element_blank()) +
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 2.5,colour = "white")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,2,1),"cm"))




### d13C for powerpoint  black
ggplot(sand, aes(x=as.integer(dist_beach), y=d13C_corr_FINAL)) + 
  labs(x="", y="") +
  scale_y_continuous(labels=scaleFUN) +
  expand_limits(y = c(-14,-21)) +
  geom_smooth(aes(x=as.integer(dist_beach),y=d13C_corr_FINAL),size=1, colour= "white",method="lm")+
  geom_point(shape=21, size = 10, stroke=2.15, fill="black", colour = "white") + 
  theme(text = element_text(size=80)) + 
  theme(panel.background = element_rect(fill = "black")) +
  theme(panel.grid.minor = element_line(colour = "black")) + 
  # theme(panel.grid.major = element_line(colour = "gray30", linetype = "dotted", size = 1.2)) + 
  theme(panel.grid.major = element_blank()) +
  theme(plot.background = element_rect(fill = "black")) + 
  theme(axis.text = element_text(colour = "white")) + 
  theme(axis.title = element_text(colour = "white")) + 
  theme(panel.background = element_rect(colour = "white")) + 
  theme(axis.ticks = element_line(size = 2.5,colour = "white")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "white", fill=NA, size=3))+
  theme(axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(1,2,2,1),"cm"))






