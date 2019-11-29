######
#variogram of DTB, residuals of generalized linear model and randomForest model
######


load(file = "D:/SCI data/data.RData")
d <- DATA[,2:4]
spdf <- SpatialPointsDataFrame(coords = d[,1:2], data = data.frame(DTB=d[,3]))

library(rgdal)
proj <- "+proj=lcc +lat_1=30 +lat_2=62 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
y <- readGDAL(fname = "E:/dtb/DTB_CHINA_1k.tif")
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(spdf) <- wgs84
spdf2 <- spTransform(spdf, CRS(proj))
plot(spdf2)

DTB_v <- variogram(spdf2$DTB ~ 1, spdf2, width = 10000)
plot(DTB_v, main = "Variogram of DTB observations")

glm_sp_resid <- spdf2
glm_sp_resid$DTB <- resid
glm_v <- variogram(glm_sp_resid$DTB ~ 1, sp_resid, width = 10000)
plot(glm_v, main = "Variagram of residuals(glm)")


load("D:/SCI data/rf.Rdata")
rf_resid <- rf$y - rf$predicted
rf_sp_resid <- spdf2
rf_sp_resid$DTB <- rf_resid
rf_v <- variogram(rf_sp_resid$DTB ~ 1, sp_resid, width = 10000)
plot(rf_v, main = "Variagram of residuals(randomForest)")
