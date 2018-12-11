
###### predict using quantile regression forest ######
######### block_name: 1буб┴ 1бу block name
######################################################
qrfPredict <- function(block_name)
{
  library(quantregForest)
  library(caret)
  library(pkgmaker)
  library(raster)
  
  ### if the block exists, return
  out_path <- "G:/qrf/"
  result_file <- paste0(out_path, "1/", block_name, ".tif")
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
  if(names(na_tab) == "TRUE" &  na_tab == length(mask_values))
  {
    return()
  }

  cov_data <- data.frame()
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
      }
      cov <- data.frame(X = cov)
      colnames(cov) <- cov_name
      
      if(ncol(cov_data) == 0)
        cov_data <- cov
      else
        cov_data <- cbind(cov_data, cov)
    }
  }
  cov_data <- cov_data[, rownames(qrf$importance)]
  
  ### divide the raster to several part in predicting to avoid memory exceed
  pred <- matrix(nrow = 0, ncol = 3)
  n_div <- 100
  n_cell <- nrow(cov_data)
  n_part <- floor(n_cell / n_div)
  for(i in 1:n_div)
  {
    part_pred <- vector()
    if(i < n_div)
    {
      part_pred <- predict(qrf, newdata = cov_data[((i - 1) * n_part + 1):(i * n_part),], 
                           what = c(0.1, 0.5, 0.9))
    }
    else
    {
      part_pred <- predict(qrf, newdata = cov_data[((i - 1) * n_part + 1):n_cell,], 
                           what = c(0.1, 0.5, 0.9))
    }
    pred <- rbind(pred, part_pred)
  }
  
  n_band <- ncol(pred)
  for(i in 1:n_band)
  {
    band_path <- paste0(out_path, as.character(i), "/")
    if(!dir.exists(band_path))
    {
      dir.create(band_path)
    }
  }
  for(i in 1:n_band)
  {
    result_file_name <- paste0(out_path, as.character(i), "/", block_name, ".tif")
    ret <- raster::setValues(std_block, pred[,i])
    names(ret) <- cov_name
    writeRaster(ret, filename = result_file_name)
  }

  print(block_name)
  gc()
  gc()
  gc()
}

block_list <- list.files("D:/DTB100/Covariates/")
load("D:/DTB100/qrf.RData")

#qrfPredict("117_40")

parallelPredict <- function(block_list, n_cores = 4)
{
  ### using parallel computing
  library(parallel)
  if(.Platform$OS.type == "windows")
  {
    #cores_num<-detectCores(logical = F)
    cores_num <- n_cores
    cl <- makeCluster(getOption("cl.cores", cores_num))
    clusterExport(cl,varlist="qrf")
    
    parLapply(cl, block_list, qrfPredict)
    
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