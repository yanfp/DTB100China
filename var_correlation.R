
### dot plot of DTB and covariates

library(randomForest)
load("F:/China/Final/rf.RData")
impDf<-rf$importance
impDf<-impDf[order(impDf[,1],decreasing = TRUE),]
impDf<-impDf[1:20]
impCovaName<-names(impDf)

d<-read.table(file = "F:/China/d/d_all.txt",header = TRUE,sep = ",")

drawDotPlot<-function(i)
{
  dTemp<-d[d$depth<600,]
  x<-sqrt(dTemp[,impCovaName[i]])
  y<-log1p(dTemp$depth)
  plot(x,y,xlab=impCovaName[i],ylab="Depth to bedrock",col="black",bg="grey",type = "p")
}
drawDotPlot(4)

