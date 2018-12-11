

##### predict using xgboost model ######
### block_name: 1буб┴ 1бу block name
########################################
xgPredict <- function(block_name)
{
  library(xgboost)
  library(caret)
  library(pkgmaker)
  library(raster)
  
  ### if the block exists, return
  out_path <- "G:/xg2/"
  result_file <- paste0(out_path, block_name, ".tif")
  if(file.exists(result_file))
    return()
  
  cov_path <- "D:/DTB100/Covariates/"
  prj_str <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  China_mask <- raster("D:/DTB100/China_1w.tif")
  block_path <- paste0(cov_path, block_name, "/")
  
  cov_file_list <- list.files(block_path)
  std_block <- raster(paste0(block_path, "DEM.tif"))
  ### if the block is out of China, return
  block_mask <- crop(China_mask, std_block)
  mask_values <- getValues(block_mask)
  na_tab <- table(is.na(mask_values))
  if((names(na_tab) == "TRUE") &  (na_tab == length(mask_values)))
  {
    return()
  }
  
  cov_list <- list()
  for(i in cov_file_list)
  {
    if(pkgmaker::file_extension(i) == "tif")
    {
      cov_name <- strsplit(i, "[.]")[[1]][1]
      file_name <- paste0(block_path, i)
      p <- raster(file_name)
      proj4string(p) <- prj_str
      if(compareRaster(p, std_block, stopiffalse = FALSE) == FALSE)
      {
        p <- projectRaster(p, std_block, method = 'ngb')
      }
      cov <- getValues(p)
      ### deal with NA in some cells
      if(anyNA(cov))
      {
        med <- median(cov, na.rm = TRUE)
        cov <- unlist(lapply(cov, FUN = function(x) {ifelse(is.na(x), med, x)}))
        p <- setValues(p, cov)
      }
      cov_list <- c(cov_list, p)
    }
  }
  cov_stack <- stack(cov_list)

  ret <- predict(cov_stack, rf)
  
  ### write to tif
  writeRaster(ret, filename = result_file)
  
  print(block_name)
  gc()
  gc()
  gc()
}

block_list <- list.files("D:/DTB100/Covariates/")
load("D:/DTB100/xg.RData")

xgPredict("117_40")

parallelPredict <- function(block_list)
{
  ### using parallel computing
  library(parallel)
  if(.Platform$OS.type == "windows")
  {
    cores_num<-detectCores(logical = F)
    cl <- makeCluster(getOption("cl.cores", cores_num))
    clusterExport(cl,varlist="xg")
    
    parLapply(cl, block_list, xgPredict)
    
    stopCluster(cl)
  }
  else if(.Platform$OS.type == "unix")
  {
    core_num <- detectCores(logical = F)
    mc <- getOption("mc.cores", core_num)
    mclapply(block_list, xgPredict, mc.cores = mc)
  }
}
#parallelPredict(block_list)

