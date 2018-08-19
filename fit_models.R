###fit models for prediction

d<-na.omit(d)
###random forest
library(randomForest)
rf<-randomForest(x=d[,4:ncol(d)],y=d$depth,ntree = 1000,mtry = (ncol(d)-3)/3)

1-var(rf$y-rf$predicted)/var(rf$y-mean(rf$y))

sqrt(mean((rf$y - rf$predicted)^2, na.rm=TRUE))

save(rf,file = "F:/New/RData/rf.RData")

###gradient boosting tree
library(xgboost) #install.packages("xgboost")
dm<-as.matrix(d[,3:ncol(d)])
xgbt <- xgboost(data = dm[,2:ncol(dm)], label = dm[,1],
                silent=1,
                eta=0.08,
                nrounds = 36,
                gamma=0,
                max_depth=7,
                min_child_weight=1,
                subsample=1,
                colsample_bytree=1,
                num_parallel_bytree=1)

pred<-predict(xgbt,newdata=dm[,2:ncol(dm)])

1-var(dm[,1]-pred)/var(dm[,1]-mean(dm[,1]))

sqrt(mean((dm[,1] - rf$predicted)^2, na.rm=TRUE))
save(xgbt,file = "F:/New/RData/xgbt.RData")

###quantile regerssion forest
library(quantregForest)
qrf<-quantregForest(x=d[,4:ncol(d)],y=d$depth,ntree = 1000,mtry = (ncol(d)-3)/3)
save(qrf,file = "F:/New/RData/qrf.RData")

