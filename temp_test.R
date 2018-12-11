
#update.packages(checkBuilt=TRUE, ask=FALSE)

library(raster)
library(sp)
library(rgdal)
file_name <- "D:/DTB100/DEM_PAR/TWI/116_33.sdat"
file.exists(file_name)
x <- raster(file_name, layer = 1, values = TRUE)

edit(raster)
