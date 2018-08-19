

## summary of depth to bedrock of all observations
summary_depth<-function(d,filePath)
{
  depth<-d$depth
  count<-length(depth)
  n0<-length(depth[depth==0])
  
  dmax<-max(depth)
  dmin<-min(depth)
  dmean<-mean(depth)
  dmedian<-median(depth)
  
  n300<-length(depth[depth>300])
  n100<-length(depth[depth>100])
  n50<-length(depth[depth>50])
  n10<-length(depth[depth>10])
  n2<-length(depth[depth>2])
  
  result<-paste0("count:\t",as.character(count),"\n",
    "0 count:\t",as.character(n0),"\n",
    "max:\t",as.character(dmax),"\n",
    "min:\t",as.character(dmin),"\n",
    "mean:\t",as.character(dmean),"\n",
    "median:\t",as.character(dmedian),"\n",
    ">300 count:\t",as.character(n300),"\n",
    ">100 count:\t",as.character(n100),"\n",
    ">50 count:\t",as.character(n50),"\n",
    ">10 count:\t",as.character(n10),"\n",
    ">2 count:\t",as.character(n2),"\n"
  )
  write(result,filePath)
}
summary_depth(d,"F:\\China\\Pic\\summary.txt")

d<-d.all
depth<-d$depth[d$depth<100]
ZeroDepthIndex=which(depth==0)
DepthIndex=which(depth!=0)
depth<-depth[c(DepthIndex,ZeroDepthIndex[1:500])]
jpeg(file = "F:\\China\\Pic\\hist_of_depth.jpg", res = 150, width=850, height=850, type="cairo")
hist(depth,breaks = 100,col="gray",main = "Histograms of depth to bedrock",
     xlab="DTB(m)",ylab="Frequency")
dev.off()

depth<-d$depth
depth.log<-log(ifelse(depth==0,0.01,depth))
jpeg(file = "F:\\China\\Pic\\hist_of_depth_log.jpg", res = 150, width=850, height=850, type="cairo")
hist(depth.log,breaks = 50,col="gray",main = "Histograms of depth to bedrock",xlab="DTB(m)",ylab="Frequency")
dev.off()


strftime(as.character(Sys.time()),"%Y%m%d_%H%M")

