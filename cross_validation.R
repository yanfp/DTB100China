
### cross validation to get the best parameters for modelling

library(gbm) #install.packages("gbm")
library(xgboost)
library(randomForest)

nfold<-10
### d containes coordinates,depth and variables
crossValidation<-function(nfold,model,xgt.depth=6)
{
  m_pData<-data.frame(lon=NA,lat=NA,meas=NA,pred=NA)
  m_pData<-m_pData[-1,]
  
  depth<-d$depth
  
  nSumCount<-nrow(d)
  nSamCount<-floor(nSumCount/nfold)
  
  allIndex<-1:nSumCount
  sampledIndex<-vector()
  indexList<-list()
  for(i in 1:nfold)
  {
    if(i==nfold)
    {
      indexList<-append(indexList,list(allIndex))
      break
    }
    index<-sample(allIndex,nSamCount)
    indexList<-append(indexList,list(index))
    sampledIndex<-c(sampledIndex,index)
    allIndex<-1:nSumCount
    allIndex<-allIndex[-sampledIndex]
  }
  r2Vector<-vector()
  for(j in 1:nfold)
  {
    trainIndex<-vector()
    testIndex<-indexList[[j]]
    for(k in (1:nfold)[-j])
    {
      trainIndex<-c(trainIndex,indexList[[k]])  
    }
    
    trainData<-d[trainIndex,]
    testData<-d[testIndex,]
    r2<-0;
    pred<-0
    if(model=="randomForest")
    {
      rf<-randomForest(x=trainData[,4:ncol(d)],y=trainData[,3],ntree=800,ntry=(ncol(d)-3)/3)
      pred<-predict(rf,newdata=testData[4:ncol(d)])
      r2<-cor(pred,testData[,3])**2
    }
    else if(model=="xgboost")
    {
      dmTrain<-as.matrix(trainData[,3:ncol(d)])
      bst <- xgboost(data = dmTrain[,2:ncol(dmTrain)], 
                     label = dmTrain[,1], 
                     silent=1,
                    eta=0.08,
                    nrounds = 36,
                    gamma=0,
                    max_depth=7,
                    min_child_weight=1,
                    subsample=1,
                    colsample_bytree=1,
                    num_parallel_bytree=1)
      
      dmTest<-as.matrix(testData[,3:ncol(d)])
      pred<-predict(bst,newdata=dmTest[,2:ncol(dmTest)])
      r2<-cor(pred,dmTest[,1])**2
    }
    else if(model=="gbm")
    {
      model <- gbm(depth~.,data=trainData[,3:ncol(trainData)],distribution = "gaussian" ,shrinkage=0.005,n.trees=5000,
                   cv.folds=10,verbose=F)
      best.iter<-gbm.perf(model,method = "cv")
      pred<-predict(model,newdata = testData[,4:ncol(testData)],best.iter)
      r2<-cor(pred,testData[,3])**2
    }
    r2Vector=c(r2Vector,r2)
    m_pData<-rbind(m_pData,cbind(testData[,1:3],pred))
    print(paste("...",as.character(j*10),"/",as.character("100")))
  }

  colnames(m_pData)<-c("lon","lat","meas","pred")
  Rsquare<-cor(m_pData[,3],m_pData[,4])**2
  RMSE<-sqrt(mean((m_pData$meas - m_pData$pred)^2, na.rm=TRUE))
  print("R-square:")
  print(Rsquare)
  print("RMSE:")
  print(RMSE)
  return(m_pData)
}

d<-na.omit(d)
options(digits = 4)
rf.mp<-crossValidation(10,"randomForest")
xg.mp<-crossValidation(10,"xgboost",7)
gbm.mp<-crossValidation(10,"gbm")
mp<-crossValidation(10,"gbm")                                         


### RMSE
RMSE <- sqrt(mean((rf.mp$meas - rf.mp$pred)^2, na.rm=TRUE));RMSE
RMSE <- sqrt(mean((xg.mp$meas - xg.mp$pred)^2, na.rm=TRUE));RMSE
mean(rf.mp$meas-rf.mp$pred)
mean(xg.mp$meas-xg.mp$pred)
mean(abs(rf.mp$meas-rf.mp$pred))
mean(abs(xg.mp$meas-xg.mp$pred))
sum((rf.mp$meas-rf.mp$pred)**2)/nrow(rf.mp)

rf.mp.sort<-rf.mp[order(rf.mp[,1],rf.mp[,2]),]
xg.mp.sort<-xg.mp[order(xg.mp[,1],xg.mp[,2]),]
mp.inte<-cbind(meas=rf.mp.sort[,3],data.frame(pred=(rf.mp.sort[,4]+xg.mp.sort[,4])/2))
cor(mp.inte$meas,mp.inte$pred)**2
RMSE <- sqrt(mean((mp.inte$meas - mp.inte$pred)^2, na.rm=TRUE));RMSE
mean(mp.inte$meas-mp.inte$pred)

save(rf.mp,file = "F:\\China\\rf_mp.Rdata")
save(xg.mp,file = "F:\\China\\xg_mp.Rdata")
load("F:\\China\\rf_mp.Rdata")
