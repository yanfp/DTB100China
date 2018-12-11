
file_path <- "D:/DTB100/DEM_PAR/"
to_path <- "D:/DTB100/Covariates/"
file_list <- list.files(file_path)
for(i in file_list)
{
  blocK_file_path <- paste0(file_path, i, "/")
  blocK_file_list <- list.files(blocK_file_path)
  for(j in blocK_file_list)
  {
    if(j != "TWI.tif")
    {
      from_file_name <- paste0(blocK_file_path, j)
      to_file_name <- paste0(to_path, i, "/", j)
      file.rename(from_file_name, to_file_name)
    }
  }
}

file_path <- "D:/DTB100/TWI/"
file_list <- list.files(file_path)
for(i in file_list)
{
  if(pkgmaker::file_extension(i) == "tif")
  {
    block_name <- strsplit(i, "[.]")[[1]][1]
  
    from_file_name <- paste0(file_path, i)
    to_file_name <- paste0(to_path, block_name, "/TWI.tif")
   
    file.rename(from_file_name, to_file_name)
  }
}

