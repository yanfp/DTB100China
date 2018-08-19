
### data grid plotting

getWells<-function()
{
  library(rgdal)
  ## depth point data 
  wellsSum<-read.table("F:/China/ChinaSub.txt",header = TRUE,sep="\t",quote = "",stringsAsFactors = FALSE)
  
  #wellsSum<-read.table("F:\\China\\WellsData\\wells_cn.txt",header = TRUE,sep="\t",quote = "",stringsAsFactors = FALSE)
  (nsum=nrow(wellsSum))
  wells<-wellsSum[!is.na(wellsSum$depth),] #提取具有深度的记录
  (nvalid=nrow(wells)) #计算基岩深度数据条数
  nrow(wells[wells$depth==0,])#深度为0
  
  #深度和钻孔倾角处理
  wells$angle<-ifelse(wells$angle>45,wells$angle,90-wells$angle) #倾角取余处理
  wells$angle<-ifelse(is.na(wells$angle),90,wells$angle) #倾角缺失处理
  wells$depth<-wells$depth*sinpi(wells$angle/180) ##岩石深度需要乘以钻孔倾角
  
  ## 将deeper than的数据加入训练集中
  wellsDeeper<-wellsSum[!is.na(wellsSum$deeper.than),]
  wellsDeeper$angle<-ifelse(is.na(wellsDeeper$angle),90,wellsDeeper$angle) #倾角缺失处理
  wellsDeeper$angle<-ifelse(wellsDeeper$angle>45,wellsDeeper$angle,90-wellsDeeper$angle)
  wellsDeeper$depth<-wellsDeeper$deeper.than*sinpi(wellsDeeper$angle/180) ##岩石深度需要乘以钻孔倾角
  wellsDeeper<-wellsDeeper[wellsDeeper$depth>100,]
  
  
  wells<-rbind(wells,wellsDeeper)
  return(wells)
}

wells<-getWells()
d<-read.table(file = "F:/China/d/d_all.txt",header = TRUE,sep = ",")
wells<-d[,1:5]

  
setwd("F:/China/")
library(sp)
plist <- read.table("F:\\China\\WellsData\\list.txt", header = T)
tmp <- NULL
#for(i in 1:dim(plist)[1])
for(i in 1:31)
{
  tmp <- rbind(tmp, read.table(paste0("./download/", plist[i,1], ".txt"),
                               header = T, as.is = T,sep = "\t",quote = "",encoding = "UTF-8"))
}
tmp <- tmp[!duplicated(tmp[,1]), ]
nrow(tmp)
coordinates(tmp) <- ~ lon +lat

#obj=tmp[!(tmp$no %in% wellsSum$no),]
obj=tmp
cell.size=c(0.3,0.3);n=1
obj$gid <- 1:length(obj)
bbox <- obj@bbox
x <- GridTopology(cellcentre.offset=c(bbox[1,1]+cell.size[1]/2,bbox[2,1]+cell.size[2]/2), 
                  cellsize=cell.size,
                  cells.dim=c(ceiling(abs(diff(bbox[1,])/cell.size[1])),
                  ncols=ceiling(abs(diff(bbox[2,])/cell.size[2]))))
r.sp <- SpatialGrid(x, proj4string = obj@proj4string)
grd <- as(r.sp, "SpatialGridDataFrame")
grd@data <- data.frame(id=1:(x@cells.dim[1]*x@cells.dim[2]))

nrow(wells)
points<-wells[,c(1,2)]
coordinates(points) <- ~ lon +lat

#res<-over(r.sp,points)
res<-over(points,grd)
value<-ifelse(grd@data[,1] %in% res[,1],1,-1)
grd@data<-data.frame(band1=value)
prj.str<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
proj4string(grd)<-prj.str

plot(grd,col=c("white","blue"))

extractPointsFromShp<-function()
{
  #将中国边界线数据中的点提取出来放入pt.df中
  china.Boun1<-readOGR("F:\\China\\ChinaFile","chinaLine")
  nline=length(china.Boun1@lines)
  pt.df<-data.frame(x=NA,y=NA)
  pt.df<-pt.df[-1,]
  n<-0
  for(i in 1:nline)
  {
    lines(as.data.frame(china.Boun1@lines[[i]]@Lines[[1]]@coords),lw=2)
    #pt.df<-rbind(pt.df,as.data.frame(china.Boun1@lines[[i]]@Lines[[1]]@coords))
  }
  return(pt.df)
}
pts<-extractPointsFromShp()
lines(pts)

writeGDAL(grd,fname = "F:\\China\\ChinaBou\\dataGrid.tif")

china_mask<-readGDAL("F:\\China\\ChinaBou\\china1w_mask.tif")
str(china_mask@data)
writeGDAL(china_mask,"F:\\China\\ChinaBou\\test.tif")
