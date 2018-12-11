

####################################
#generate TPI based on DEM
####################################
library(pkgmaker)
dem_file_path <- "D:/DTB100/DEM/"
tpi_file_path <- "D:/DTB100/TPI/"
if(!dir.exists(tpi_file_path))
  dir.create(tpi_file_path)
dem_file_list <- list.files(dem_file_path)
for(i in dem_file_list)
{
  if(pkgmaker::file_extension(i) == "tif")
  {
    dem_file_name <- paste0(dem_file_path, i)
    p <- raster(dem_file_name)
    tpi_obj <- spatialEco::tpi(p)
    writeRaster(tpi_obj, filename = paste0(tpi_file_path, i))
  }
}

###################################
#overlay
###################################
load("D:/DTB100/samples.RData")
coordinates(samples) <- ~LON + LAT
prj_str <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(samples) <- prj_str

tpi_file_path <- "D:/DTB100/Covariates/"
tpi_file_list <- list.files(tpi_file_path)
#st <- stack()
df <- matrix()
tpi_matrix <- data.frame(ID = NA, TPI = NA)
tpi_matrix <- tpi_matrix[-1, ]
count <- 0
for(i in tpi_file_list)
{
  file_name <- paste0(tpi_file_path, i, "/TPI.tif")
  if(file.exists(file_name))
  {
    count <- count + 1
    print(count)
    
    p <- raster(file_name)
    proj4string(p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    st <- stack(list(p))
  
    df <- extract(st, samples)
  
    if(is.null(nrow(df)))
    {
      next
    }
    else if(nrow(df) == 0)
    {
      next
    }
  
    df <- cbind(ID = samples$ID, TPI = df[, 1])
    df <- na.omit(df)
  
    tpi_matrix <- rbind(tpi_matrix, df)
  }
}
nrow(tpi_matrix)
str(tpi_matrix)
tpi_matrix <- tpi_matrix[!duplicated(tpi_matrix$ID),]
str(tpi_matrix)
colnames(tpi_matrix) <- c("ID", "TPI")
data2 <- merge(data, tpi_matrix, by = "ID", all.x = TRUE)

