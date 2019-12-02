
rm(list = ls())
packages <- c("ggplot2", "HSAUR2", "lattice", "KernSmooth", "MVA", "mclust",
              "andrews", "aplpack", "fmsb")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)  
}

############################################################################
library(HSAUR2)
head(USairpollution)
x <- USairpollution
cm <-colMeans(x)
S <- cov(x)
d <- apply(x,1,function(x) t(x-cm) %*% solve(S) %*% (x-cm))
plot(qc <- qchisq((1:nrow(x)-1/2) / nrow(x),df=7),
     sd <- sort(d),
     xlab = expression(paste(chi[7]^2, "Quantile")),
     ylab = "Ordered distances", xlim = range(qc) * c(1,1.1))
oups <- which(rank(abs(qc-sd), ties ="random") > nrow(x)-3)
text(qc[oups], sd[oups]-1.5,names(oups))
abline(a=0,b=1)

###########################################################################
head(CYGOB1)
library(KernSmooth)
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1,dpik))
plot(CYGOB1, xlab = "log surface temperature", ylab="log light intensity")
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat, add = TRUE)

## The chi-plot

library(MVA)
with(USairpollution, chiplot(manu,popul))


## Glyph plots

ylim <- with(USairpollution, range(wind))*c(0.95,1)
plot(wind~temp,data=USairpollution,
     xlab="Average annual temperature (Fahrenheit)",
     ylab="Average annual wind speed (m.p.h.)",pch=10,
     ylim=ylim)
with(USairpollution,symbols(temp,wind,circles=SO2, inches=0.5, add=TRUE))

### stars
stars(USairpollution, cex=0.55)

# spider plots
library(fmsb)
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")
data <- rbind(rep(20,5) , rep(0,5) , data)

# plot with default options:
radarchart(data)

## Chernoff faces

library(mclust)
data(banknote)
banknote[c(1:5,101:105),]
library(aplpack)
faces(banknote[c(1:5,101:105),2:7],fill=FALSE,face.type=0)

## Andrews curves

library(andrews)
andrews(iris,type=1,clr=5,ymax=3)

## Trellis graphics

library(lattice)
xyplot(SO2~temp|cut(wind,2), data=USairpollution)

## Stalactite plot

stalac(USairpollution)
