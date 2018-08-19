
### generate block for parallel computing

gc()
memory.limit(size = 200*1024)
prj.str<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#########
blockList<-list()
bFileList<-list.files("F:\\China\\Mask\\1h")
for(i in bFileList)
{
  strList<-strsplit(i,split = "[.]")[[1]]
  if(tail(strList,1)=="tif")
  {
    block<-readGDAL(paste0("F:\\China\\Mask\\1h\\",as.character(i)))
    proj4string(block)<-prj.str #定义坐标系统
    blockList<-append(blockList,block)
  }
}
#save(blockList,file = "F:/China/Code/blockList_1h.RData")
load(file="blockList_1213.RData")