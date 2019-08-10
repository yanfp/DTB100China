

tifToSdat <- function(tif_file, sdat_file)
{
  if(!file.exists(sdat_file))
  {
    tif <- raster(tif_file)
    writeRaster(tif, sdat_file)
  }
}

sdatToTif <- function(sdat_file, tif_file)
{
  sdat_file <- gsub(".sgrd", ".sdat", sdat_file)
  if(file.exists(sdat_file))
  {
    sdat <- raster(sdat_file)
    if(!file.exists(tif_file))
    {
      writeRaster(sdat, tif_file)
      
      file.remove(sdat_file)
      file.remove(gsub(".sdat", ".sgrd", sdat_file))
      file.remove(gsub(".sdat", ".prj", sdat_file))
      file.remove(gsub(".sdat", ".mgrd", sdat_file))
    }
  }
  
}

#TWI
twi <- function(in_tif_dem, out_sg_twi, out_sg_catchm, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_twi)))
  {
    try( suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                         ' ta_hydrology 15 -DEM=\"', input, 
                                         '\" -SLOPE_MIN=0.04 -SLOPE_OFF=0.3 -AREA_MOD=\"', out_sg_catchm, 
                                         '\" -SLOPE_TYPE=0 -TWI=\"', out_sg_twi) ) ) ) ## gsub("100", "250", INPUT)
  
    tif_twi <- gsub(".sgrd", ".tif",out_sg_twi)
    sdatToTif(out_sg_twi, tif_twi)
    
    tif_catchm <- gsub(".sgrd", ".tif",out_sg_catchm)
    sdatToTif(out_sg_catchm, tif_catchm)
  }
}

##SLP
slp <- function(in_tif_dem, out_sg_slp, out_sg_prof, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)
  
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_slp)))
  {
    try( suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                         ' ta_morphometry 0 -ELEVATION=\"', input, 
                                         '\" -SLOPE=\"', out_sg_slp, 
                                         '\" -C_PROF=\"', out_sg_prof, '\"') ) ) )
  
    tif_slp <- gsub(".sgrd", ".tif",out_sg_slp)
    sdatToTif(out_sg_slp, tif_slp)
  
    tif_prof <- gsub(".sgrd", ".tif",out_sg_prof)
    sdatToTif(out_sg_prof, tif_prof)
  }
}

##CRV
crv <- function(in_tif_dem, out_sg_crv, out_sg_cru, out_sg_crd, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)
  
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_crv)))
  {
    try( suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                         ' ta_morphometry 26 -DEM=\"', input,
                                         '\" -C_DOWN_LOCAL=\"', out_sg_crv, 
                                         '\" -C_UP_LOCAL=\"', out_sg_cru, 
                                         '\" -C_UP=\"tmp.sgrd\" -C_LOCAL=\"tmp.sgrd\" -C_DOWN=\"', out_sg_crd, '\"') ) ) )
  
    tif_crv <- gsub(".sgrd", ".tif",out_sg_crv)
    sdatToTif(out_sg_crv, tif_crv)
    
    tif_cru <- gsub(".sgrd", ".tif",out_sg_cru)
    sdatToTif(out_sg_cru, tif_cru)
    
    tif_crd <- gsub(".sgrd", ".tif",out_sg_crd)
    sdatToTif(out_sg_crd, tif_crd)
  }
  
}

##VBF
vbf <- function(in_tif_dem, out_sg_vbf, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)

  if(!file.exists(gsub(".sgrd", ".tif", out_sg_vbf)))
  {
    try( suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                         ' ta_morphometry 8 -DEM=\"', input, 
                                         '\" -MRVBF=\"', out_sg_vbf, 
                                         '\" -T_SLOPE=10 -P_SLOPE=3') ) ) )
    tif_vbf <- gsub(".sgrd", ".tif",out_sg_vbf)
    sdatToTif(out_sg_vbf, tif_vbf)
  }
  
}

##VDP
vdp <- function(in_tif_dem, out_sg_vdp, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)
  
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_vdp)))
  {
    try( suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                         ' ta_channels 7 -ELEVATION=\"', input, 
                                         '\" -VALLEY_DEPTH=\"', out_sg_vdp, '\"') ) ) )
  
    tif_vdp <- gsub(".sgrd", ".tif",out_sg_vdp)
    sdatToTif(out_sg_vdp, tif_vdp)
  }
  
}

##OPN
opn <- function(in_tif_dem, out_sg_popn, out_sg_nopn, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)
  
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_popn)))
    {
      try( suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                           ' ta_lighting 5 -DEM=\"', input, 
                                           '\" -POS=\"', out_sg_popn, 
                                           '\" -NEG=\"', out_sg_nopn, 
                                           '\" -METHOD=0' ) ) ) )
  
    tif_popn <- gsub(".sgrd", ".tif",out_sg_popn)
    sdatToTif(out_sg_popn, tif_popn)
    
    tif_nopn <- gsub(".sgrd", ".tif",out_sg_nopn)
    sdatToTif(out_sg_nopn, tif_nopn)
  }
  
}

#DVM
dvm <- function(in_tif_dem, out_sg_dvm, out_sg_dvm2, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)
  
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_dvm)))
  {
    suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                    ' statistics_grid 1 -GRID=\"', input, 
                                    '\" -DEVMEAN=\"', out_sg_dvm, 
                                    '\" -RADIUS=', 9 ) ) )
    tif_dvm <- gsub(".sgrd", ".tif",out_sg_dvm)
    sdatToTif(out_sg_dvm, tif_dvm)
  }
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_dvm2)))
  {
    suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                    ' statistics_grid 1 -GRID=\"', input, 
                                    '\" -DEVMEAN=\"', out_sg_dvm2, 
                                    '\" -RADIUS=', 13 ) ) )
  
    tif_dvm2 <- gsub(".sgrd", ".tif",out_sg_dvm2)
    sdatToTif(out_sg_dvm2, tif_dvm2)
  }
}

mrn <- function(in_tif_dem, out_sg_mrn, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)
  
  if(!file.exists(gsub(".sgrd", ".tif", out_sg_mrn)))
  {
    suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                    ' ta_hydrology 23 -DEM=\"', input, 
                                    '\" -AREA=\"tmp.sgrd\" -MRN=\"', out_sg_mrn, 
                                    '\" -ZMAX=\"tmp.sgrd\"' ) ) )
  
    tif_mrn <- gsub(".sgrd", ".tif",out_sg_mrn)
    sdatToTif(out_sg_mrn, tif_mrn)
  }
}

tpi <- function(in_tif_dem, out_sg_tpi, cpus = 8)
{
  sg_dem <- gsub("tif", "sdat", in_tif_dem)
  tifToSdat(in_tif_dem, sg_dem)
  
  input <- gsub("tif", "sgrd", in_tif_dem)

  if(!file.exists(gsub(".sgrd", ".tif", out_sg_tpi)))
  {
    suppressWarnings( system(paste0(saga_cmd, ' -c=', cpus,
                                    ' ta_morphometry 18 -DEM=\"', input, 
                                    '\" -STANDARD=0 -TPI=\"', out_sg_tpi, 
                                    '\" -RADIUS_MIN=0 -RADIUS_MAX=20 -DW_WEIGHTING=3 -DW_BANDWIDTH=75' ) ) )
  
    tif_tpi <- gsub(".sgrd", ".tif",out_sg_tpi)
    sdatToTif(out_sg_tpi, tif_tpi)
  }
}

library(pkgmaker)
library(raster)
library(sp)
DEM_PAR_path <- "D:/DTB100/DEM_PAR/"
DEM_path <- "D:/DTB100/DEM_PAR/DEM/"
saga_cmd <- "D:/saga-7.0.0_x64/saga_cmd.exe"

# dem_list <- list.files(DEM_path)
# for(i in dem_list)
# {
#   if(pkgmaker::file_extension(i) == "tif")
#   {
#     dem_file <- paste0(DEM_path, i)
#     out_sg_twi <- paste0(DEM_PAR_path, "TWI/", gsub("tif", "sgrd", i))
#     twi(dem_file, out_sg_twi)
#     
#     out_sg_slp <- paste0(DEM_PAR_path, "SLP/", gsub("tif", "sgrd", i))
#     out_sg_prof <- paste0(DEM_PAR_path, "PROF/", gsub("tif", "sgrd", i))
#     slp(dem_file, out_sg_slp, out_sg_prof)
#     
#     out_sg_crv <- paste0(DEM_PAR_path, "CRV/", gsub("tif", "sgrd", i))
#     out_sg_cru <- paste0(DEM_PAR_path, "CRU/", gsub("tif", "sgrd", i))
#     out_sg_crd <- paste0(DEM_PAR_path, "CRD/", gsub("tif", "sgrd", i))
#     crv(dem_file, out_sg_crv, out_sg_cru, out_sg_crd)
#     
#     out_sg_vbf <- paste0(DEM_PAR_path, "VBF/", gsub("tif", "sgrd", i))
#     vbf(dem_file, out_sg_vbf)
#     
#     # out_sg_vdp <- paste0(DEM_PAR_path, "VDP/", gsub("tif", "sgrd", i))
#     # vdp(dem_file, out_sg_vdp)
#     
#     out_sg_popn <- paste0(DEM_PAR_path, "POPN/", gsub("tif", "sgrd", i))
#     out_sg_nopn <- paste0(DEM_PAR_path, "NOPN/", gsub("tif", "sgrd", i))
#     opn(dem_file, out_sg_popn, out_sg_nopn)
#     
#     out_sg_dvm <- paste0(DEM_PAR_path, "DVM/", gsub("tif", "sgrd", i))
#     out_sg_dvm2 <- paste0(DEM_PAR_path, "DVM2/", gsub("tif", "sgrd", i))
#     dvm(dem_file, out_sg_dvm, out_sg_dvm2)
#     
#     out_sg_mrn <- paste0(DEM_PAR_path, "MRN/", gsub("tif", "sgrd", i))
#     mrn(dem_file, out_sg_mrn)
#     
#     # out_sg_tpi <- paste0(DEM_PAR_path, "TPI/", gsub("tif", "sgrd", i))
#     # tpi(dem_file, out_sg_tpi)
#   }
#   
# }
# 
# 

# ### produce twi
# file_path <- "D:/DTB100/DEM/"
# file_list <- list.files(file_path)
# for(i in file_list)
# {
#   if(pkgmaker::file_extension(i) == "tif")
#   {
#     in_dem <- paste0(file_path, i)
#     out_twi_tif <- gsub("DEM", "TWI", in_dem)
#     out_twi_sg <- gsub(".tif", ".sgrd", out_twi_tif)
#     out_catchm_sg <- gsub("TWI", "CATCHM", out_twi_sg)
#     
#     twi(in_dem, out_twi_sg, out_catchm_sg)
#   }
# }

# ### produce vdp
# file_path <- "D:/DTB100/DEM/"
# file_list <- list.files(file_path)
# for(i in file_list)
# {
#   if(pkgmaker::file_extension(i) == "tif")
#   {
#     in_dem <- paste0(file_path, i)
#     out_vdp_tif <- gsub("DEM", "VDP", in_dem)
#     out_vdp_sg <- gsub(".tif", ".sgrd", out_vdp_tif)
#     vdp(in_dem, out_vdp_sg)
#   }
# }

### produce tpi
file_path <- "D:/DTB100/DEM/"
file_list <- list.files(file_path)
for(i in file_list)
{
  if(pkgmaker::file_extension(i) == "tif")
  {
    in_dem <- paste0(file_path, i)
    out_tpi_tif <- gsub("DEM", "TPI", in_dem)
    out_tpi_sg <- gsub(".tif", ".sgrd", out_tpi_tif)
    tpi(in_dem, out_tpi_sg)
  }
}



