library(raster)
library(sp)
library(rgdal)

n <- 6
mycolors <- colorRampPalette(c("darkgreen", "yellow", "orangered"))(n)
barplot(rep(1,times=n),col=mycolors,border=mycolors, space=0, names.arg=breakpoints)

## points map
prov_boun <- readOGR("D:/DTB100/Map/Prinvce_boundary.shp")
plot(prov_boun)


# DTB map
windows()
obj <- raster("D:/DTB100/Map/DTB_Lambert.tif")
par(oma=c(0,0,0,0), mar=c(0,0,0,0))
boun <- readOGR("D:/DTB100/Map/China_boundary.shp")
plot(boun, col="grey")
mycolors <- colorRampPalette(c("darkgreen", "yellow", "orangered"))(10)
breakpoints <- c(0, 5, 10, 15, 20, 30, 50, 100, 200, 500)
plot(obj, add = TRUE, axes=FALSE, breaks=breakpoints,col=mycolors)

# quantreg 
windows()
obj <- raster("D:/DTB100/Map/qrf01_Lambert.tif")
par(oma=c(0,0,0,0), mar=c(0,0,0,0))
boun <- readOGR("D:/DTB100/Map/China_boundary.shp")
plot(boun, col="grey")
mycolors <- colorRampPalette(c("darkgreen", "yellow", "orangered"))(10)
breakpoints <- c(0, 5, 10, 15, 20, 30, 50, 100, 200, 500)
plot(obj, add = TRUE, axes=FALSE, breaks=breakpoints,col=mycolors, colkey = list(side = 1, length = 0.5))

## dot plot
library(ggplot2)
d <- DATA[DATA$DTB < 500,]
x <- d$PX3WCL
x <- sqrt(x)

y <- d$DTB

fit <- lm(y~x, data = data.frame(x = x, y = y)) 
plot(x, y, pch = 20)
abline(fit)







