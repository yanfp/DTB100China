
### Plot for cross-validation results for depth to bedrock
library(lattice)
library(plotKML) #install.packages("plotKML")
library(hexbin) #install.packages("hexbin")

plot_hexbin <- function(mp, main, colorcut=c(0,0.01,0.03,0.07,0.15,0.25,0.5,0.75,1),
                        pal=R_pal[["bpy_colors"]][1:18], log.plot=TRUE)
{
  out.file = paste0("F:\\China\\pic\\plot_CV_",main,".jpg")
  if(!file.exists(out.file)){
  #load(in.file)
    #assign("m", get(paste0("CV_", t.vars[j])))
    m<-list(data.frame(Observed=mp$meas,Predicted=mp$pred))
    d.meas <- min(m[[1]]$Observed, na.rm=TRUE)
    pred <- m[[1]]$Predicted
    meas <- m[[1]]$Observed
    R.squared = round(1-var(meas - pred, na.rm=TRUE)/var(meas-mean(meas), na.rm=TRUE), 2)
    
    main.txt = paste0("Absolute depth to bedrock(in m)", "  (CV R-squared: ", R.squared, ")")
    jpeg(file = out.file, res = 150, width=850, height=850, type="cairo")
    if(log.plot==TRUE){
      ## confidence limits based on RMSE:
      pfun <- function(x,y, ...){
        panel.hexbinplot(x,y, ...)  
        panel.abline(0,1,lty=1,lw=2,col="black")
        #panel.abline(0+m$Summary$logRMSE,1,lty=3,lw=2,col="black")
        #panel.abline(0-m$Summary$logRMSE,1,lty=3,lw=2,col="black")
      }
      pred <- pred+ifelse(d.meas==0, 1, d.meas)
      meas <- meas+ifelse(d.meas==0, 1, d.meas)
      
      breaks<-list(mp$meas,mp$pred)
      lim <- range(breaks)+ifelse(d.meas==0, 1, d.meas)
      meas <- ifelse(meas<lim[1], lim[1], ifelse(meas>lim[2], lim[2], meas))

      plt <- hexbinplot(pred~meas, colramp=colorRampPalette(pal), main=main.txt, 
                        xlab="measured", ylab="predicted", type="g", lwd=1, lcex=8, 
                        inner=.2, cex.labels=.8, scales=list(x = list(log = 2, equispaced.log = FALSE),
                                                             y = list(log = 2, equispaced.log = FALSE)),
                        asp=1, xbins=30, ybins=30, xlim=lim, ylim=lim, panel=pfun, colorcut=colorcut)
    } else {
      ## confidence limits based on RMSE:
      pfun <- function(x,y, ...){
        panel.hexbinplot(x,y, ...)  
        panel.abline(0,1,lty=1,lw=2,col="black")
        #panel.abline(0+m$Summary$RMSE,1,lty=3,lw=2,col="black")
        #panel.abline(0-m$Summary$RMSE,1,lty=3,lw=2,col="black")
      }
      lim <- range(breaks)
      meas <- ifelse(meas<lim[1], lim[1], ifelse(meas>lim[2], lim[2], meas))
      plt <- hexbinplot(pred~meas, colramp=colorRampPalette(pal), main=main.txt, xlab="measured", ylab="predicted", type="g", lwd=1, lcex=8, inner=.2, cex.labels=.8, xlim=lim, ylim=lim, asp=1, xbins=30, ybins=30, panel=pfun, colorcut=colorcut)
    }
    print(plt)
    dev.off()
  }
}
#####
rf.mp$meas<-rf.mp$meas*100
rf.mp$pred<-rf.mp$pred*100
rf.mp$meas<-exp(rf.mp$meas)-1
rf.mp$pred<-exp(rf.mp$pred)-1
xg.mp$meas<-exp(xg.mp$meas)-1
xg.mp$pred<-exp(xg.mp$pred)-1
#####
rf.mp$meas<-m_pData$depth
rf.mp$pred<-m_pData$pred

plot_hexbin(rf.mp,strftime(as.character(Sys.time()),"%Y%m%d_%H%M"))
plot_hexbin(xg.mp,strftime(as.character(Sys.time()),"%Y%m%d_%H%M"))
plot_hexbin(mgb.mp,strftime(as.character(Sys.time()),"%Y%m%d_%H%M"))

mp.inte$pred<-ifelse(mp.inte$pred>=0,mp.inte$pred,0)
mp.inte$meas<-mp.inte$meas*100
mp.inte$pred<-mp.inte$pred*100
mp.inte<-rbind(mp.inte,data.frame(meas=c(2,3),pred=c(2,3)))
#mp.inte<-mp.inte[mp.inte$pred>10,]
plot_hexbin(mp.inte,strftime(as.character(Sys.time()),"%Y%m%d_%H%M"))


breaks<-list(mp.inte$meas,mp.inte$pred)
