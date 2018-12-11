library(raster)

load("D:/DTB100/samples.RData")
coordinates(samples) <- ~LON + LAT
prj_str <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(samples) <- prj_str

file_twi_path <- "D:/DTB100/Covariates/"
file_list <- list.files(file_twi_path)
#st <- stack()
df <- matrix()
twi_matrix <- data.frame(ID = NA, TWI = NA)
twi_matrix <- twi_matrix[-1, ]
for(i in file_list)
{
  file_name <- paste0(file_twi_path, i, "/TWI.tif")
  if(file.exists(file_name))
  {
    print(i)
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

    df <- cbind(ID = samples$ID, TWI = df[, 1])
    df <- na.omit(df)

    twi_matrix <- rbind(twi_matrix, df)
  }
}
nrow(twi_matrix)
str(twi_matrix)
twi_matrix <- twi_matrix[!duplicated(twi_matrix$ID),]
str(twi_matrix)

data2 <- merge(data, twi_matrix, by = "ID", all.x = TRUE)
data <- data2