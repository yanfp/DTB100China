
library(raster)
library(maptools)
library(sp)
library(rgdal)

points <- readOGR("D:/DTB100/Map/samples_Lambert.shp")

load("D:/DTB100/samples.RData")
obj <- samples
coordinates(obj) <- ~ LON + LAT
prj_str <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(obj) <- prj_str

cell.size = c(0.2, 0.2)
n <- 1
obj$gid <- 1:length(obj)
bbox <- obj@bbox
r <- GridTopology(cellcentre.offset = c(bbox[1,1] + cell.size[1] / 2, bbox[2,1] + cell.size[2] / 2), 
                  cellsize = cell.size,
                  cells.dim = c(ceiling(abs(diff(bbox[1,]) / cell.size[1])),
                  ncols = ceiling(abs(diff(bbox[2,]) / cell.size[2]))))

r.sp <- SpatialGrid(r, proj4string = obj@proj4string)
grd <- as(r.sp, "SpatialGridDataFrame")
grd@data <- data.frame(id = 1:(r@cells.dim[1] * r@cells.dim[2]))


res <- over(obj, grd)
value <- ifelse(grd@data[,1] %in% res[,1], 1, 0)
grd@data <- data.frame(band1 = value)

rr <- raster(grd)
writeRaster(rr, filename = "D:/DTB100/Map/grid.tif")
