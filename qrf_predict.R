
### apply spatial prediction using quantile regression forest

library(rgdal)
library(sp)
library(quantregForest)

rm(list = ls())
gc()
memory.limit(size = 50*1024)

load("F:/New/RData/qrf.RData")
load("F:/China/Final/blockList_1h.RData")

### predict DTB for every 1бу б┴ 1бу block
predictFunForParallel<-function(bList)
{
  library(quantregForest)
  library(rgdal)
  prj.str<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  lon=round(bList@bbox[1,1])
  lat=round(bList@bbox[2,1])
  blockName<-paste0(as.character(lon),"_",as.character(lat))
  
  filePath<-"G:/qrf_1h"
  fileList<-list.files(filePath)
  if(!(paste0(blockName,".tif") %in% fileList))
  {
    if((lon>=73 && lon<=100 && lat>=18 && lat<=54) | 1==1)
    {
    
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
      
      pre<-vector()
      pre_part=vector()
      rowIndex<-vector()
      nr<-nrow(covaDf)
      
      n_part<-nr/100
      
      for(i in 1:100)
      {
        rowIndex=((i-1)*n_part+1):(i*n_part)
        pre_part<-predict(qrf,covaDf[rowIndex,-1],what = c(0.1,0.5,0.9))
        pre<-rbind(pre,pre_part)
        gc()
      }
      if(n_part*100<nr)
      {
        rowIndex=(100*n_part+1):nr
        pre_part<-predict(qrf,covaDf[rowIndex,-1],what = c(0.1,0.5,0.9))
        pre<-rbind(pre,pre_part)
      }
      
      pre<-ifelse(pre>=0,pre,0)
      
      bList@data<-data.frame(pred=(pre[,3]-pre[,1])/pre[,2])
      
      gc()
      gc()
      gc()
    
      if(!dir.exists(filePath))
        dir.create(filePath)
      name<-paste0(as.character(round(bList@bbox[1,1])),"_",as.character(round(bList@bbox[2,1])))
      writeGDAL(bList,paste0(filePath,"/",as.character(name),".tif"))

    }
  }
}

### parallel computing for predicting
library(parallel)
coresNum<-detectCores(logical = F)
cl <- makeCluster(getOption("cl.cores", coresNum))
clusterExport(cl,varlist="qrf")
system.time({
  res <- parLapply(cl, blockList,predictFunForParallel)
})
stopCluster(cl)

