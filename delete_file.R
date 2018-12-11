
deleteFile <- function(path, type = "tif")
{
  file_list <- list.files(path)
  for(i in file_list)
  {
    strs <- strsplit(i, "[.]")[[1]]
    file_type <- strs[length(strs)]
    if(file_type != type)
    {
      file.remove(paste0(path, i))
    }
  }
}
while(1)
{
  deleteFile("D:/DTB100/TWI/", "tif")
  Sys.sleep(10)
}


### 检验文件
file_path <- "D:/DTB100/DEM_PAR/"
file_list <- list.files(file_path)
for(i in  file_list)
{
  file_path2 <- paste0(file_path, i, "/")
  file_list2 <- list.files(file_path2)
  len <- length(file_list2)
  if(len != 14)
  {
    print(i)
  }
  
  deleteFile(file_path2, "tif")
}

### 移动文件
file_path <- "D:/DTB100/DEM_PAR/"
file_list <- list.files(file_path)
for(i in  file_list)
{
  file_from <- paste0(file_path, i, "/", "DEM.tif")
  file_to <- paste0("D:/DTB100/DEM/", i, ".tif")
  file.copy(file_from, file_to)
}

