#step1 调入数据
rm(list=ls())
graphics.off()
#install.packages("export")
library(SIBER)
library(ggplot2)
library(scales)
library(RColorBrewer)
#library(viridis)
#palette(viridis(4))

a1 = rgb(213,238,238,maxColorValue = 255)
a2 = rgb(155,197,221,maxColorValue = 255)
a3 = rgb(98,164,205,maxColorValue = 255)
a4 = rgb(32,122,188,maxColorValue = 255)
a =c (a1,a2,a3,a4)
palette(a)

#library(export)
setwd("E:/000-trophic diversity and structure along a coastal wetland/DATA/SIBER")

mydata1 <- read.csv("E:/000-trophic diversity and structure along a coastal wetland/DATA/SIBER/PASSSASG_SIBER.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata2 <- read.csv("E:/000-trophic diversity and structure along a coastal wetland/DATA/SIBER/mydata2.csv", header=T,fileEncoding = "UTF-8-BOM")

siber.example1 <- createSiberObject(mydata1)
siber.example2 <- createSiberObject(mydata2)
# --------------------------------------
#step2 绘制散点分布???-标准椭圆
#a<-brewer.pal(4, "Blues")
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.5, lty = 1, lwd = 2)#,col = a[c(2,4,6,8)])#lty改成2是虚??? lwd改成1是变???
group.hull.args      <- list(lty = 2, col = "grey20") 
par(mfrow=c(1,2),pin = c(3,4))#,
plotSiberObject(siber.example1,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'(\u2030)'),
                ylab = expression({delta}^15*N~'(\u2030)'),
                x.limits = c(-35,0),
                y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "n"
 ) 

plotSiberObject(siber.example2,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "n",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'),
                x.limits = c(-35,0),
                y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL
               ) 




p + geom_point(colour = "red")

#graph2ppt(x=p, paper=A4)
# --------------------------------------
#step3 统计与绘???
par(mfrow=c(1,1))
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")
# this time we will make the points a bit smaller by
# cex = 0.5
plotSiberObject(siber.example,
                ax.pad = 2,
                hulls = F, community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5
)
#每个group计算TA SEA SEAc
group.ML <- groupMetricsML(siber.example)
print(group.ML)
write.csv(group.ML, "group_TA_SEA_SEAc.csv")
#添加xx%的置信区???
#or you can add the XX% confidence interval around the bivariate means
#by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  ci.mean = T, lty = 1, lwd = 2) 

# --------------------------------------
#step3 另一种表示形式，适合两个不同系统的比???
# A second plot provides information more suitable to comparing
# the two communities based on the community-level Layman metrics
plotSiberObject(siber.example,
                ax.pad =2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5
)
#！！！每个group计算TA SEA SEAc
group.ML <- groupMetricsML(siber.example)
print(group.ML)
#添加xx%的置信区???
#or you can add the XX% confidence interval around the bivariate means
#by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  ci.mean = T, lty = 1, lwd = 2) 

#！！！计???5个指???
# Calculate the various Layman metrics on each of the communities.
community.ML <- communityMetricsML(siber.example)
print(community.ML)
write.csv(community.ML, "community_comparisions.csv")
# --------------------------------------
#step4 贝叶斯模型与数据的拟???
# options for running jags
.libPaths()
install.packages("rjags")
library(rjags)
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
# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the
# means. Fitting is via the JAGS method.
#拟合协方差矩阵Sigma上使用逆Wishart先验，均值上使用模糊正态先验的椭圆???
#通过JAGS方法进行拟合???
ellipses.posterior <- siberMVN(siber.example, parms, priors)

# --------------------------------------
#step5 比较group间的SEA,使用标准椭圆面积进行组间比较
# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.每组椭圆的后验估计可用于计算每组的SEA-B
SEA.B <- siberEllipses(ellipses.posterior)
write.csv(SEA.B, "SEA.B.csv")
# --------------------------------------
siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), 
                 xlab = c("Community | Group"),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each group"
)

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)

# Calculate some credible intervals 
cr.p <- c(0.95, 0.99) # vector of quantiles

# call to hdrcde:hdr using lapply()
SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p)

# do similar to get the modes, taking care to pick up multimodal posterior
# distributions if present
SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T)

# --------------------------------------
#step6 基于Layman指标的生态系统间的比???
# extract the posterior means
mu.post <- extractPosteriorMeans(siber.example, ellipses.posterior)
# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)
write.csv(layman.B , "layman_bayesian.csv")

# Visualise the first community(灰色平均???+置信区间)
siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), 
                 bty="L", ylim = c(0,20))
# add the ML estimates (if you want). Extract the correct means(添加ML)
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(siber.example$ML.mu[[1]][1,1,],
                                 siber.example$ML.mu[[1]][1,2,])
points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)


# Visualise the second community
siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(siber.example$ML.mu[[2]][1,1,],
                                 siber.example$ML.mu[[2]][1,2,])
points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)

# --------------------------------------
# Alternatively, pull out TA from both and aggregate them into a 
# single matrix using cbind() and plot them together on one graph.
# --------------------------------------
#整合为一个图，比较两个TA
# go back to a 1x1 panel plot
par(mfrow=c(1,1))

siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"],layman.B[[3]][,"TA"],layman.B[[4]][,"TA"]),
                 xticklabels = c("PA", "SS","SA","SG"), 
                 bty="L", ylim = c(0,20),
                 las = 1,
                 ylab = "TA - Convex Hull Area",
                 xlab = "")


bayes95.overlap <- bayesianOverlap(ellipse1, ellipse2, ellipses.posterior,
                                   draws = 100, p.interval = 0.95, n = 100)

