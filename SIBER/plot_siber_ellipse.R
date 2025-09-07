#step1 ????????
rm(list=ls())
graphics.off()
#install.packages("export")
library(SIBER)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(viridis)
#palette(viridis(4))
#a1 = rgb(213,238,238,maxColorValue = 255)
#a2 = rgb(155,197,221,maxColorValue = 255)
#a3 = rgb(98,164,205,maxColorValue = 255)
#a4 = rgb(32,122,188,maxColorValue = 255)
#a =c (a1,a2,a3,a4)
#palette(a)

#library(export)
setwd("E:/1-已发表论文专利书稿详细工作/9-Frontiers in Marine Science 2022/DATA/SIBER")

mydata1 <- read.csv("resource.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata2 <- read.csv("consumer.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata3 <- read.csv("Zoo.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata4 <- read.csv("Biv.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata5 <- read.csv("Gas.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata6 <- read.csv("Cra.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata7 <- read.csv("C.a.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata8 <- read.csv("P.h.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata9 <- read.csv("S.h.csv", header=T,fileEncoding = "UTF-8-BOM")
mydata10 <- read.csv("L.j.csv", header=T,fileEncoding = "UTF-8-BOM")

#------------------------------------------------
siber.example1 <- createSiberObject(mydata1)
siber.example2 <- createSiberObject(mydata2)
siber.example3 <- createSiberObject(mydata3)
siber.example4 <- createSiberObject(mydata4)
siber.example5 <- createSiberObject(mydata5)
siber.example6 <- createSiberObject(mydata6)
siber.example7 <- createSiberObject(mydata7)
siber.example8 <- createSiberObject(mydata8)
siber.example9 <- createSiberObject(mydata9)
siber.example10 <- createSiberObject(mydata10)
#group <- mydata8$group
#-------------------------------
#group.ML1 <- groupMetricsML(siber.example8)
#print(group.ML1)
#----------------------------------------

# --------------------------------------
#step2 ????ɢ???ֲ????-??׼??Բ
#a<-brewer.pal(4, "Blues")
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.5, lty = 1, lwd = 2)#,col = a[c(2,4,6,8)])#lty?ĳ?2??????? lwd?ĳ?1?Ǳ????
group.hull.args      <- list(lty = 2, col = "grey20") 
#par(mfrow=c(1,1),pin = c(3,4))
layout(matrix(c(1,1,2,2,3,4,5,6,7,8,9,10),3,4,byrow=TRUE),widths=c(1,1,1,1),height=c(1,0.8,0.8))
par(mar=c(4,4,1,1),oma=c(1,1,1,2))
#par(mar = c(1, 1, 1, 1))

plotSiberObject(siber.example1,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression(),
                ylab = expression({delta}^15*N~'(\u2030)'),
                x.limits = c(-35,0),
                y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o")
                #legend("topleft", as.character(paste("Group ",unique(group))), pch=1, col=1:length(unique(group))
                      # bty = "n",cex=1.5,lwd=2)


                     
plotSiberObject(siber.example2,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "o",
                iso.order = c(1,2),
                xlab = expression(),
                ylab = expression(),
                x.limits = c(-35,0),
                y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL
) 

plotSiberObject(siber.example3,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression(),
                ylab = expression({delta}^15*N~'(\u2030)'),
               # x.limits = c(-35,0),
                #y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 

plotSiberObject(siber.example4,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression(),
               ylab = expression(),
               # x.limits = c(-35,0),
              #  y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 

plotSiberObject(siber.example5,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression(),
                ylab = expression(),
                #x.limits = c(-35,0),
                #y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 

plotSiberObject(siber.example6,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression(),
                ylab = expression(),
                #x.limits = c(-35,0),
                #y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 

plotSiberObject(siber.example7,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'(\u2030)'),
                ylab = expression({delta}^15*N~'(\u2030)'),
               # x.limits = c(-35,0),
              #  y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 

plotSiberObject(siber.example8,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'(\u2030)'),
                ylab = expression(),
               # x.limits = c(-35,0),
              #  y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 

plotSiberObject(siber.example9,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'(\u2030)'),
                ylab = expression(),
                #x.limits = c(-35,0),
                #y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 

plotSiberObject(siber.example10,
                ax.pad = 2,
                hulls = F, community.hulls.args, #community.hulls.args,
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'(\u2030)'),
                ylab = expression(),
               # x.limits = c(-35,0),
              #  y.limits = c(0,15),
                points.order = 1:25,
                font.axis=1,
                font.lab=NULL,
                bty = "o"
) 


###############3
ellipse1 <- "3.3" 
ellipse2 <- "3.4"

sea.overlap <- maxLikOverlap(ellipse1, ellipse2, siber.example10, 
                             p.interval = NULL, n = 100)
sea.overlap
ellipse95.overlap <- maxLikOverlap(ellipse1, ellipse2, siber.example10, 
                                   p.interval = 0.95, n = 100)
ellipse95.overlap

prop.95.over <- ellipse95.overlap[3] / (ellipse95.overlap[2] + 
                                          ellipse95.overlap[1] -
                                          ellipse95.overlap[3])
prop.95.over

##--



#????????????????????????????????????????????????????????????????????????????????????????--
bayes95.overlap <- bayesianOverlap(ellipse1, ellipse2, ellipses.posterior,
                                   draws = 100, p.interval = 0.95, n = 100)


