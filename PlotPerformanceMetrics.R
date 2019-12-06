

#what do you want to plot? noise or drift
whatplot <- "drift"

#choose metric w/ RMSE=-1;r2=0,MAE=1
n <- 0

#x-scale: How much SD range
x2 <- 11 

  
#compile summaries:
run_summaries <- read.csv("performanceMetrics/run_summaries.csv", stringsAsFactors=FALSE)
now_summ <- read.csv("performanceMetrics/now_summ.csv", stringsAsFactors=FALSE)
mlp_summ <- read.csv("performanceMetrics/mlp_summ.csv", stringsAsFactors=FALSE)

X <- c("Linear RMSE noise",
       "Linear r2 noise",
       "Linear MAE noise",
       "Linear RMSE drift",
       "Linear r2 drift",
       "Linear MAE drift",
       "Boosted Tree RMSE noise",
       "Boosted Tree r2 noise",
       "Boosted Tree MAE noise",
       "Boosted Tree RMSE drift",
       "Boosted Tree r2 drift",
       "Boosted Tree MAE drift",
       "Random Forest RMSE noise",
       "Random Forest r2 noise",
       "Random Forest MAE noise",
       "Random Forest RMSE drift",
       "Random Forest r2 drift",
       "Random Forest MAE drift")

run_summaries <- cbind(X, run_summaries)

#gather data
AddedSD <- c(0,0.01,0.05,0.1,0.2,0.5,1,5,10,20)

######################################################
#make the figure
######################################################
##(1) noise##
if(whatplot=="noise"){
  linMETRn  <- as.numeric(run_summaries[(2+n),2:x2])
  btMETRn   <- as.numeric(run_summaries[(8+n),2:x2])
  rfMETRn   <- as.numeric(run_summaries[(14+n),2:x2])
  nowMETRn  <- as.numeric(now_summ[(2+n),2:x2])
  mlpMETRn  <- as.numeric(mlp_summ[(2+n),2:x2])
}
##(2) drift##
if(whatplot=="drift"){
  linMETRn  <- as.numeric(run_summaries[(5+n),2:x2])
  btMETRn   <- as.numeric(run_summaries[(11+n),2:x2])
  rfMETRn   <- as.numeric(run_summaries[(17+n),2:x2])
  nowMETRn  <- as.numeric(now_summ[(5+n),2:x2])
  mlpMETRn  <- as.numeric(mlp_summ[(5+n),2:x2])
}

#now make the figure
png(filename = paste0("C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast/figures/revision/Performance_", whatplot, ".png"),
    width = 5, height = 5, units = "in", pointsize = 16, res=300)

par(mar=c(3.5,3.5,1,1),
    mgp=c(1,0.75,0))

#nowcast
plot(AddedSD,nowMETRn,
    col="black",
    xlim=c(0,20),
    ylim=c(0,1),
    cex=1,
    pch=16,
    axes=FALSE,
    ylab="",
    xlab="")

lines(AddedSD,nowMETRn,col="black",lwd=2)

#multiple linear
points(AddedSD,linMETRn,cex=1,pch=1,col="gray30")
lines(AddedSD,linMETRn,col="gray30",lwd=2,lty=2)

#random forest
points(AddedSD,rfMETRn,cex=1,col="green4",pch=1)
lines(AddedSD,rfMETRn,col="green4",lwd=2,lty=2)

#boosted tree
points(AddedSD,btMETRn,cex=1,col="blue4",pch=1)
lines(AddedSD,btMETRn,col="blue4",lwd=2,lty=2)

#mlp
points(AddedSD,mlpMETRn,cex=1,pch=1,col="red4")
lines(AddedSD,mlpMETRn,col="red4",lwd=2,lty=2)


box(col = 'gray20')
axis(side = 1, tck = -.03, col = 'gray20', at=c(0,10,20), labels=c("0", "10","20"))
axis(side = 2, tck = -.03, col = 'gray20', at=c(0,0.25,0.5,0.75,1), labels= c(0,0.25,0.5,0.75,1))

#mtext(text="Standard Deviation Increase",side=1,line=2)
if(whatplot=="noise"){ mtext(text="Noise (Standard Deviation)",side=1,line=2)}
if(whatplot=="drift"){ mtext(text="Drift (Standard Deviation)",side=1,line=2)}
mtext(text="Coef. of Determination",side=2,line=2)

if(whatplot=="noise"){
  legend(7.5,1,c("nowcast","linear regression","random forest","boosted trees", "multilayer perceptron"),
         col=c("black","gray30","green4","blue4","red4"),
         pch=c(16,1,1,1,1),
         pt.cex=1.2,
         lty=c(1,2,2,2,2),
         lwd=c(2,2,2,2,2),
         cex=0.75)
}

dev.off()