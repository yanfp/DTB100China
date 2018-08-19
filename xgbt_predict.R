
### apply spatial prediction using gradient boosting tree.
library(xgboost)
library(rgdal)
library(sp)
library(caret)

rm(list = ls())
gc()
memory.limit(size = 50*1024)
load("../Final/blockList_1h.RData")

load("F:/New/RData/xgbt.RData")

### predict DTB for every 1¡ã ¡Á 1¡ã block
predictFunForParallel<-function(bList)
{
  library(xgboost)
  library(rgdal)
  prj.str<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  lon=round(bList@bbox[1,1])
  lat=round(bList@bbox[2,1])
  blockName<-paste0(as.character(lon),"_",as.character(lat))
  
  filePath<-"F:/New/Results/xg_1k"
  
  if((lon>=112 && lon<=122 && lat>=30 && lat<=41) | 1==1)
  {
    blockPath<-paste0("F:/China/Covariates/BlockCovariates/",blockName)
    blockCovariatesNameList<-list.files(blockPath)
    covaDf<-data.frame()
    for(i in blockCovariatesNameList)
    {
      strList<-strsplit(i,"[.]")
      covaName<-strList[[1]][1]
      strList<-strsplit(i,"[.]")
      covaName<-strList[[1]][1]
      
      if(covaName=="China_dem")
        covaName<-"CHINADEM"
      else if(covaName=="Ecological2015_250")
        covaName<-"ECOLOGICAL"
      else if(covaName=="ELU2015_250")
        covaName<-"ELU2015"
      else if(covaName=="ESACCI-WI")
        covaName<-"ESACCI_WI"
      else if(covaName=="MODCF_meanannual")
        covaName<-"MODCF_MEAN"
      else if(covaName=="GLOBCOVER_CLA_QL")
        covaName<-"GLOBC_CLA"
      else if(covaName=="range_01_05")
        covaName<-"RANGE0105"
      else if(covaName=="SedDepth")
        covaName<-"SED_DEP"
      
      blockCovariates<-readGDAL(paste0(blockPath,"\\",i))
      proj4string(blockCovariates)<-prj.str
      ov.grid<-over(bList,blockCovariates)
      colnames(ov.grid)<-covaName
      if(ncol(covaDf)==0)
      {
        covaDf<-data.frame(no=1:nrow(ov.grid))
      }
      covaDf<-cbind(covaDf,ov.grid)
    }
    covariateNum<-ncol(covaDf)-1
    for(l in 2:(covariateNum+1))
    {
      ave<-mean(covaDf[,l],na.rm = TRUE)
      if(is.nan(ave))
      {
        ave=0
      }
      covaDf[,l]<-ifelse(is.na(covaDf[,l]),ave,covaDf[,l])
    }
    #### Óöµ½¿ÕÖµ£¬Ìø³ö
    if(anyNA(covaDf))
    {
      return()
    }
    
    pre<-vector()
    
    dmCovaDf<-as.matrix(covaDf[,-1])
    
    pre<-predict(xgbt,newdata=dmCovaDf)
  
    pre<-ifelse(pre>=0,pre,0)
    
    #pre<-exp(pre)-1
    
    bList@data<-data.frame(pred=pre)
  
   
  
    if(!dir.exists(filePath))
      dir.create(filePath)
    name<-paste0(as.character(round(bList@bbox[1,1])),"_",as.character(round(bList@bbox[2,1])))
    writeGDAL(bList,paste0(filePath,"/",as.character(name),".tif"))
    #rm(dfTemp)
    #flag=flag+1
    gc()
    gc()
    gc()
    #Sys.time()
  }
}

### parallel computing for predicting
library(parallel)
coresNum<-detectCores(logical = F)
cl <- makeCluster(getOption("cl.cores", coresNum))
clusterExport(cl,varlist="xgbt")
system.time({
  res <- parLapply(cl, blockList,predictFunForParallel)
})
stopCluster(cl)



