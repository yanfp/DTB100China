
### overlay observations on covariates to generate matrix for modelling

rm(list = ls())
gc()
memory.limit(size = 200*1024)

source("F:\\China\\Code\\points_show.R")
overForModelling<-function()
{
  library(rgdal)
  ## depth point data 
  wellsSum<-read.table("F:/China/ChinaSub.txt",header = TRUE,sep="\t",quote = "",stringsAsFactors = FALSE)
  
  addTable1<-read.table("F:/China/addTable1.txt",header = TRUE,sep="\t",quote = "",stringsAsFactors = FALSE)  
  addTable1$depth<-as.numeric(addTable1$depth)
  wellsSum<-rbind(wellsSum,addTable1)
  (nsum=nrow(wellsSum))
  wells<-wellsSum[!is.na(wellsSum$depth),] #extract rows with DTB
  (nvalid=nrow(wells))
  nrow(wells[wells$depth==0,])# row of observations where depth is 0.
  
  # the actual DTB is the product of depth and angle 
  wells$angle<-ifelse(wells$angle>45,wells$angle,90-wells$angle) #倾角取余处理
  wells$angle<-ifelse(is.na(wells$angle),90,wells$angle) #倾角缺失处理
  wells$depth<-wells$depth*sinpi(wells$angle/180) ##岩石深度需要乘以钻孔倾角
  
  ## deeper than rows
  wellsDeeper<-wellsSum[!is.na(wellsSum$deeper.than),]
  wellsDeeper$angle<-ifelse(is.na(wellsDeeper$angle),90,wellsDeeper$angle) #倾角缺失处理
  wellsDeeper$angle<-ifelse(wellsDeeper$angle>45,wellsDeeper$angle,90-wellsDeeper$angle)
  wellsDeeper$depth<-wellsDeeper$deeper.than*sinpi(wellsDeeper$angle/180) ##岩石深度需要乘以钻孔倾角
  wellsDeeper<-wellsDeeper[wellsDeeper$depth>100,]
  
  wells<-rbind(wells,wellsDeeper)
  
  nrow(wells)
  wells<-wells[,1:5]
  
  #####
  ### deserts
  tklmg<-read.table("F:\\China\\PseuPoint\\Desert.txt",sep = ",",header = TRUE)
  tklmg$depth<-300
  tklmg<-data.frame(no=10001:(10000+nrow(tklmg)),lon=tklmg$lon,lat=tklmg$lat,lit="",depth=tklmg$depth)
  wells<-rbind(wells,tklmg)
  ### steep areas
  slopePoint<-read.table("F:\\China\\PseuPoint\\slope45_Point.txt",sep = ",",header = TRUE)
  slopePoint$depth<-0.1
  slopePoint<-data.frame(no=11001:(11000+nrow(slopePoint)),lon=slopePoint$lon,lat=slopePoint$lat,lit="",depth=slopePoint$depth)
  wells<-rbind(wells,slopePoint)
  ######
  
  ##transfer into spatial object
  coordinates(wells)<-~lon+lat
  prj.str<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  proj4string(wells)<-prj.str
  
  ## overlay
  fileList<-list.files("F:\\China\\Covariates\\ChinaCovariates")
  #fileList<-list.files("F:\\China\\EnvironmentData\\NA")
  china_mask<-readGDAL("F:\\China\\ChinaBou\\china1w_mask.tif")
  china_grid<-data.frame(no=1:nrow(china_mask@data),mask=china_mask@data)
  
  ## result of overlay
  d<-data.frame(wells@coords,depth=wells$depth)
  
  
  for(i in fileList)
  {
    if(i=="China_dem.tif" | i=="Entropy.tif" | i=="evenness.tif" | i=="GLOBCOVER.tif" | i=="GLOBCOVER_CLA_QL.tif"|
       i=="Maximum_01_05.tif" | i=="MODCF_meanannual.tif" | i=="range_01_05.tif")
    {
      next()
    }
    file_type=strsplit(i,'[.]')[[1]]
    cov.Name=file_type[1]
    filetype=file_type[length(file_type)]
    if(filetype!="tif")
    {
      next()
    }
    covariate<-readGDAL(paste0("F:/China/Covariates/ChinaCovariates/",i)) #read covariates
    proj4string(covariate)<-prj.str #define coordinate reference
    point.ov<-over(wells,covariate) #oerlay
  
    colnames(point.ov)<-cov.Name #colname 
    d<-cbind(d,point.ov) #add result to data.frame
    
    flag<-0
    if(flag==1)
    {
      grid.ov<-over(china_mask,covariate)
      colnames(grid.ov)<-cov.Name
      china_grid<-cbind(china_grid,grid.ov)
      rm(grid.ov)
    }
    rm(point.ov)
    gc()
    gc()
    gc()
    gc()
  }
  return(d)
}


### over for modelling
d<-overForModelling()

save(d,file = "F:/China/d/d.RData")

