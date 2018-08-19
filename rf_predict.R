
### apply spatial prediction using ramdom forest
library(randomForest)
library(rgdal)
library(sp)

rm(list = ls())
gc()
memory.limit(size = 100*1024)

load("F:/New/RData/rf.RData")
load("F:/China/Final/blockList_1h.RData")

### predict DTB for every 1¡ã ¡Á 1¡ã block
predictFunForParallel<-function(bList)
{
  library(randomForest)
  library(rgdal)
  prj.str<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  lon=round(bList@bbox[1,1])
  lat=round(bList@bbox[2,1])
  blockName<-paste0(as.character(lon),"_",as.character(lat))
  
  filePath<-"F:/New/Results/rf_1h" # result output path
  
  fileList<-list.files(filePath)
  if(paste0(blockName,".tif") %in% fileList)
    return()
  
  if((lon>=133 && lon<=136 && lat>=18 && lat<=54) | 1==1)
  {
    print(blockName)
    
    blockPath<-paste0("F:/China/Covariates/BlockCovariates/",blockName)
    blockCovariatesNameList<-list.files(blockPath)
    covaDf<-data.frame()
    for(i in blockCovariatesNameList)
    {
      strList<-strsplit(i,"[.]")
      covaName<-strList[[1]][1]
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
    
    pre<-predict(rf,newdata = covaDf[,-1],na.action=na.pass)
    
    #pre<-ifelse(pre>=0,pre,0)
    
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
  }
  #Sys.time()
}

### parallel computing for predicting
library(parallel)
coresNum<-detectCores(logical = F)
cl <- makeCluster(getOption("cl.cores", coresNum))
clusterExport(cl,varlist="rf")
system.time({
  res <- parLapply(cl, blockList,predictFunForParallel)
})
stopCluster(cl)


