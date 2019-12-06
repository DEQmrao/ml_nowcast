
library(dplyr)
library(lubridate)
library(ggplot2)

#get predictions
Noise_nowcast <- read.csv("inputADDnoise/Noise_nowcast.csv", stringsAsFactors=FALSE)
nowXnoise_pred <- Noise_nowcast %>% filter(year(dtime) == 2015) 

lmXnoise_pred <- read.csv("predictions/sel_noise_lm.csv", stringsAsFactors=FALSE)
rfXnoise_pred <- read.csv("predictions/sel_noise_ranger.csv", stringsAsFactors=FALSE)
gbmXnoise_pred <- read.csv("predictions/sel_noise_gbm.csv", stringsAsFactors=FALSE)
mlpXnoise_pred <- read.csv("predictions/sel_noise_mlp.csv", stringsAsFactors=FALSE)


Drift_nowcast <- read.csv("inputADDdrift/Drift_nowcast.csv", stringsAsFactors=FALSE)
nowXdrift_pred <- Drift_nowcast %>% filter(year(dtime) == 2015) 

lmXdrift_pred <- read.csv("predictions/sel_drift_lm.csv", stringsAsFactors=FALSE)
rfXdrift_pred <- read.csv("predictions/sel_drift_ranger.csv", stringsAsFactors=FALSE)
gbmXdrift_pred <- read.csv("predictions/sel_drift_gbm.csv", stringsAsFactors=FALSE)
mlpXdrift_pred <- read.csv("predictions/sel_drift_mlp.csv", stringsAsFactors=FALSE)

MakeThisFig <- function(x,y,xlab2write,ylab2write,zlab2write,xAXlab,yAXlab){
  
  #find wildfire
  focus <- x >= 90
  
  plot(x, y,
       pch=NA,
       xlab=xlab2write,
       ylab=ylab2write,
       ylim=c(0,210),
       xlim=c(0,210),
       axes=FALSE)
  lines(c(-100,250),c(-100,250),lty=2,col="gray60")
  points(x,y,cex=0.5,pch=16,col=alpha("gray25",0.4))
  points(x[focus],y[focus],cex=0.5,col="red4")
  
  axis(side = 2, tck = -.03, col = 'gray20', at=c(0,200), labels= yAXlab)
  axis(side = 1, tck = -.03, col = 'gray20', at=c(0,200), labels= xAXlab)
  
  box()
  mtext(zlab2write,side=3,line=0.5,cex=0.8)
  mtext(ylab2write,side=2,line=2.5)
  mtext(xlab2write,side=1,line=3)
}

pm_ylab = expression("Particulate Matter " ~ (mu ~ g ~ m^{-3}))

png(filename = paste0("C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast/figures/RegsModelPerformance.png"),
    width = 10, height = 5, units = "in", pointsize = 16, res=300)

#now make the figure
par(mfrow=c(2,5),
    mar=c(0.5,0.5,0.5,0.5),
    oma=c(4,4,3,1))

MakeThisFig(nowXnoise_pred$pm25_0,nowXnoise_pred$sel5,"","Predicted w/ Noise","nowcast", FALSE,c(0,200))
MakeThisFig(lmXnoise_pred$pm25_0,lmXnoise_pred$sel5.00,"","","linear reg.",FALSE,FALSE)
MakeThisFig(rfXnoise_pred$pm25_0,rfXnoise_pred$sel5.00,"","","random forest",FALSE,FALSE)
MakeThisFig(gbmXnoise_pred$pm25_0,gbmXnoise_pred$sel5.00,"","","boosted trees",FALSE,FALSE)
MakeThisFig(mlpXnoise_pred$pm25_0,mlpXnoise_pred$sel5.00,"","","multilayer perceptron",FALSE,FALSE)

MakeThisFig(nowXdrift_pred$pm25_0,nowXdrift_pred$sel5,"","Predicted w/ Drift","",c(0,200),c(0,200))
MakeThisFig(lmXdrift_pred$pm25_0,lmXdrift_pred$sel5.00,"","","",c(0,200),FALSE)
MakeThisFig(rfXdrift_pred$pm25_0,rfXdrift_pred$sel5.00,pm_ylab,"","",c(0,200),FALSE)
MakeThisFig(gbmXdrift_pred$pm25_0,gbmXdrift_pred$sel5.00,"","","",c(0,200),FALSE)
MakeThisFig(mlpXdrift_pred$pm25_0,mlpXdrift_pred$sel5.00,"","","",c(0,200),FALSE)

dev.off()
