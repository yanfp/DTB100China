library(lattice)
library(plotKML) #install.packages("plotKML")
library(hexbin) #install.packages("hexbin")

plot_hexbin <- function(mp, main, colorcut=c(0,0.01,0.03,0.07,0.15,0.25,0.5,0.75,1),
                        pal=R_pal[["bpy_colors"]][1:18], log.plot=TRUE)
{
  out.file = paste0("D:/DTB100/Chart/cv_plot_",main,".jpg")
  if(!file.exists(out.file)){
  #load(in.file)
    #assign("m", get(paste0("CV_", t.vars[j])))
    m<-list(data.frame(Observed=mp$measured,Predicted=mp$predicted))
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
      
      breaks<-list(mp$measured,mp$predicted)
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

R2 <- function(ret)
{
  return(1 - var(ret$measured - ret$predicted, na.rm = TRUE) / var(ret$measured - mean(ret$measured, na.rm = TRUE)))
}

RMSE <- function(ret)
{
  return(sqrt(mean((ret$measured - ret$predicted)^2, na.rm=TRUE)))
}

load(file = "D:/DTB100/rf_ret.RData")
load(file = "D:/DTB100/xg_ret.RData")
cv_result <- merge(rf_ret, xg_ret, by = "ID", all.x = TRUE)
cv_result$measured <- (cv_result$measured.x + cv_result$measured.y) / 2
cv_result$predicted <- (cv_result$predicted.x + cv_result$predicted.y) / 2

R2(cv_result)
RMSE(cv_result)
ME(cv_result)

windows()
plot_hexbin(cv_result, "DTB")
