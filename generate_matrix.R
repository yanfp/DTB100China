# overlay samples on covariates to generate matrix
# 2018/11/06

library(rgdal)
library(sp)
library(raster)
library(pkgmaker)

generateMatrix <- function(cov_cat, cov_num, std_cov)
{
  covariates_path <- paste0("D:/DTB100/", cov_cat, "/")
  block_list <- list.files(covariates_path)
  matrix_for_modeling <- matrix(nrow = 0, ncol = cov_num + 1)
  count <- 1
  #browser()
  for(i in block_list)
  {
    block_path <- paste0(covariates_path, i, "/")
    block_covariate_list <- list.files(block_path)
    
    #st <- stack()
    cov_list <- list()
    
    s_block <- raster(paste0(block_path, std_cov, ".tif"))
    ext <- s_block@extent
    nrows <- s_block@nrows
    ncols <- s_block@ncols
    
    for(j in block_covariate_list)
    {
      strs <- strsplit(j, "[.]")[[1]]
      file_type <- strs[length(strs)]
      if(file_type == "tif")
      {
        block_covariate_fn <- paste0(block_path, j)
        p <- raster(block_covariate_fn)
        proj4string(p) <- proj4string(s_block)
        
        if(compareRaster(p, s_block, stopiffalse = FALSE) == FALSE | cov_num == 1)
        {
          p <- projectRaster(p, s_block, method = 'ngb')
        }
        cov_list <- c(cov_list, p)
      }
    }
    
    st <- stack(cov_list)
    
    df <- data.frame()
    try(df <- extract(st, samples))
    
    print(i)
    
    if(is.null(nrow(df)))
    {
      next
    }
    else if(nrow(df) == 0)
    {
      next
    }
    
    df <- cbind(ID = samples@data$ID, df)
    df <- na.omit(df)
    
    matrix_for_modeling <- rbind(matrix_for_modeling, df)
#}    
    
  }
  #save(matrix_for_modeling, file = paste0("D:/DTB100/matrix_for_modelling", "_", cov_cat, ".RData"))
  assign(paste0(cov_cat, "_matrix"), matrix_for_modeling)
  save(matrix_for_modeling, file = paste0("D:/DTB100/matrix_for_modelling_13", "_", cov_cat, ".RData"))
}


load(file = "D:/DTB100/samples.RData")
coordinates(samples) <- ~LON + LAT
prj_str <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(samples) <- prj_str

#
# generateMatrix("SOI_PAR", 1, "STGHWS")
# 
# generateMatrix("ORG_PAR", 4, "COVER")
# #
# generateMatrix("EAR_PAR", 4, "ENTROPY")
# #
# generateMatrix("TIM_PAR", 9, "EVMMOD")
# #
# generateMatrix("CLI_PAR", 22, "MODCF")

# generateMatrix("DEM_PAR", 14, "DEM")

samples <- samples[samples$DTB == 300.0, ]
x <- generateMatrix("Covariates", 55, "DEM")





