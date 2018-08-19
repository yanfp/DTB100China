
### plotting the importance of covariates
library(randomForest)
load("F:/China/Final/rf.RData")
rfTemp<-rf
importanceDf<-rfTemp$importance
colnames(importanceDf)<-"Scaled importance"
importanceDf<-importanceDf/10000
rfTemp$importance<-importanceDf
impDf<-rfTemp$importance

impDf<-data.frame(cova=rownames(impDf),importance=impDf)
str(impDf)

impDf<-impDf[order(impDf[,2],decreasing = TRUE),]
impDf<-impDf[1:20,]

covariatesStr<-"Topographic Wetness Index,
Physiographic landform units,
Topopgraphic Openess Index,
Slope,
Digital elevation model,
Land cover,
Digital Relief Model,
Long-term averaged mean cloud cover,
Topography-based range from DEM,
Monthly precipitation for October,
Mean monthly precipitation (annual),
Monthly precipitation for July,
Long-term precipitation,
Minimum of the 8-day MODIS day-time,
Standard deviation of the PISR,
Mean of the 8-day MODIS day-time,
Mean of the 8-d M d-t Feb/Mar,
Monthly precipitation August,
Mean of the monthly MODIS EVI time,
Evenness index"
importantCovariates<-strsplit(covariatesStr,",\n")
importantCovariates<-importantCovariates[[1]]
importantCovariates<-importantCovariates[length(importantCovariates):1]
impDf$covaDescribe<-importantCovariates

varImpPlot(rfTemp,labels=impDf$covaDescribe,n.var=20,main = "Depth to bedrock of China",bg="red",pt.cex=1.5)



