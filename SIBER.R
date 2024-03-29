
# SIBER niche exploration

################################################################################################################################################################ 

#library(devtools)
#devtools::install_github("andrewljackson/SIBER@master", build_vingettes = TRUE)
library(SIBER)
library(tidyverse)

setwd("~/zzzpersonal")

siber.dat.raw <- read.csv("SIBER_2021.csv",header=T)
set.seed(1)

# CLEAN 
siber.dat <- siber.dat.raw %>%
  filter(!is.na(iso1)) %>%
  #mutate(group = ifelse(group%in%c("mouse-F", "mouse-M"), group, "food")) %>%
  print()

################################################################################################################################################################ 

# SIBER object
siber_nf <- createSiberObject(siber.dat)

# Create lists of plotting arguments to be passed onwards to each of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# plot raw data
par(mfrow=c(1,1))
plotSiberObject(siber_nf, ax.pad = 2, 
                hulls=F, community.hulls.args, 
                ellipses=T, group.ellipses.args,
                group.hulls=F, group.hull.args,
                bty = "L", iso.order = c(1,2), xlab = expression({delta}^13*C~'\u2030'), ylab = expression({delta}^15*N~'\u2030'))

# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber_nf)
print(group.ML)

# You can add more ellipses by directly calling plot.group.ellipses(). Add an additional p.interval % prediction ellipse
plotGroupEllipses(siber_nf, n=100, p.interval=0.90, ci.mean=T, lty=1, lwd=2)

# Calculate the various Layman metrics on each of the communities.
community.ML <- communityMetricsML(siber_nf) 
print(community.ML)


#######
# FIT #
#######

# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10         # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior on the covariance matrix Sigma, and a vague normal prior on the means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber_nf, parms, priors)




###########
# COMPARE #
###########

#----- AMONG GROUPS using SEA

# The posterior estimates of the ellipses for each group can be used to calculate the SEA.B for each group.
SEA.B <- siberEllipses(ellipses.posterior)
# Plotted
siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), bty = "L", las = 1, main = "SIBER ellipses on each group",
                 xlab = c("Community | Group"), ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ))

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)

# Calculate some credible intervals 
cr.p <- c(0.95, 0.99) # vector of quantiles

# call to hdrcde:hdr using lapply()
SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p)

# do similar to get the modes, taking care to pick up multimodal posterior distributions if present
SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T)











#----- AMONG GROUPS using LAYMAN

# extract the posterior means
mu.post <- extractPosteriorMeans(siber_nf, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)


#-----
# Visualise the first community
#-----
siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), bty="L", ylim = c(0,20))

# add the ML estimates (if you want). Extract the correct means 
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(siber_nf$ML.mu[[1]][1,1,], siber_nf$ML.mu[[1]][1,2,])
points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)


#-----
# Visualise the second community
#-----
siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), bty="L", ylim = c(0,30))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(siber_nf$ML.mu[[2]][1,1,], siber_nf$ML.mu[[2]][1,2,])
points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)


#-----
# Alternatively, pull out TA from both and aggregate them into a 
# single matrix using cbind() and plot them together on one graph.
#-----
# go back to a 1x1 panel plot
par(mfrow=c(1,1))
siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"]), xticklabels = c("Community 1", "Community 2"), 
                 bty="L", ylim = c(0,50), las = 1, ylab = "TA - Convex Hull Area", xlab = "")














################################################################################################################################################################ 
 # OLD STUFF


# SIBER GS vs CV communities  

siber_nf <- createSiberObject(siber_dat)

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# plot raw data
par(mfrow=c(1,1))
plotSiberObject(siber_nf,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls =F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'),
)

# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber_nf)
print(siber_nf)


# You can add more ellipses by directly calling plot.group.ellipses()
# Add an additional p.interval % prediction ellilpse
plotGroupEllipses(siber_nf,n=100,p.interval=0.95,ci.mean=F,lty=1,lwd=2)


# A second plot provides information more suitable to comparing the two communities based on the community-level Layman metrics
plotSiberObject(siber_nf,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5
)

# or you can add the XX% confidence interval around the bivariate means by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber_nf, n = 100, p.interval = 0.95, ci.mean = T, lty = 1, lwd = 2) 

# Calculate the various Layman metrics on each of the communities.
community.ML <- communityMetricsML(siber_nf) 
print(community.ML)



######Bayes for days


# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior on the covariance matrix Sigma, and a vague normal prior on the means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber_nf, parms, priors)


### compare communities using Layman 


# extract the posterior means
mu.post <- extractPosteriorMeans(siber_nf, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)


# --------------------------------------
# Visualise the first community
# --------------------------------------
siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates (if you want). Extract the correct means 
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(siber_nf$ML.mu[[1]][1,1,],
                                 siber_nf$ML.mu[[1]][1,2,]
)
points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)



# --------------------------------------
# Visualise the second community
# --------------------------------------
siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), 
                 bty="L", ylim = c(0,30))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(siber_nf$ML.mu[[2]][1,1,],
                                 siber_nf$ML.mu[[2]][1,2,]
)
points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)




# --------------------------------------
# Alternatively, pull out TA from both and aggregate them into a 
# single matrix using cbind() and plot them together on one graph.
# --------------------------------------

# go back to a 1x1 panel plot
par(mfrow=c(1,1))

siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"]),
                 xticklabels = c("Community 1", "Community 2"), 
                 bty="L", ylim = c(0,50),
                 las = 1,
                 ylab = "TA - Convex Hull Area",
                 xlab = "")






##################################################################################### SIBER nearshore vs forest communities for terr. 
iso.dat <- read.csv("invert_veg_iso.csv")



siber_nf <- read.csv("SIBER_nearfar.csv",header=T)

siber_nf <- createSiberObject(siber_nf)

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# plot raw data
par(mfrow=c(1,1))
plotSiberObject(siber_nf,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls =F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'),
)

# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber_nf)
print(siber_nf)


# You can add more ellipses by directly calling plot.group.ellipses()
# Add an additional p.interval % prediction ellilpse
plotGroupEllipses(siber_nf,n=100,p.interval=0.95,ci.mean=F,lty=1,lwd=2)


# A second plot provides information more suitable to comparing the two communities based on the community-level Layman metrics
plotSiberObject(siber_nf,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5
)

# or you can add the XX% confidence interval around the bivariate means by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber_nf, n = 100, p.interval = 0.95, ci.mean = T, lty = 1, lwd = 2) 

# Calculate the various Layman metrics on each of the communities.
community.ML <- communityMetricsML(siber_nf) 
print(community.ML)



######Bayes for days


# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior on the covariance matrix Sigma, and a vague normal prior on the means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber_nf, parms, priors)


### compare communities using Layman 


# extract the posterior means
mu.post <- extractPosteriorMeans(siber_nf, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)


# --------------------------------------
# Visualise the first community
# --------------------------------------
siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates (if you want). Extract the correct means 
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(siber_nf$ML.mu[[1]][1,1,],
                                 siber_nf$ML.mu[[1]][1,2,]
)
points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)



# --------------------------------------
# Visualise the second community
# --------------------------------------
siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), 
                 bty="L", ylim = c(0,30))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(siber_nf$ML.mu[[2]][1,1,],
                                 siber_nf$ML.mu[[2]][1,2,]
)
points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)




# --------------------------------------
# Alternatively, pull out TA from both and aggregate them into a 
# single matrix using cbind() and plot them together on one graph.
# --------------------------------------

# go back to a 1x1 panel plot
par(mfrow=c(1,1))

siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"]),
                 xticklabels = c("Community 1", "Community 2"), 
                 bty="L", ylim = c(0,50),
                 las = 1,
                 ylab = "TA - Convex Hull Area",
                 xlab = "")

