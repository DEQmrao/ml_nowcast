

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
n <- 0
linMETRn  <- as.numeric(run_summaries[(2+n),2])
btMETRn   <- as.numeric(run_summaries[(8+n),2])
rfMETRn   <- as.numeric(run_summaries[(14+n),2])
nowMETRn  <- as.numeric(now_summ[(2+n),2])
mlpMETRn  <- as.numeric(mlp_summ[(2+n),2])

R2<- c(nowMETRn,linMETRn,rfMETRn,btMETRn, mlpMETRn)

n <- -1
linMETRn  <- as.numeric(run_summaries[(2+n),2])
btMETRn   <- as.numeric(run_summaries[(8+n),2])
rfMETRn   <- as.numeric(run_summaries[(14+n),2])
nowMETRn  <- as.numeric(now_summ[(2+n),2])
mlpMETRn  <- as.numeric(mlp_summ[(2+n),2])

RMSE <- c(nowMETRn,linMETRn,rfMETRn,btMETRn, mlpMETRn)

######################################################
#make the figure
######################################################
labels_vec <- c("nowcast ", 
                "linear reg ", 
                "random forest ", 
                "boosted trees", 
                "multilayer perceptron")

#now make the figure
png(filename = paste0("C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast/figures/BaseModelPerformance.png"),
    width = 5, height = 10, units = "in", pointsize = 16, res=300)

par(mfrow=c(2,1),
    mar=c(0,0,0,0),
    oma=c(10,4,1,1))

mp1 <- barplot(R2, ylim=c(0,1), 
               axisnames = FALSE,
               axes=FALSE)
axis(1, at = mp1, labels = rep("", 5))
axis(2,at= c(0,0.25,0.5,0.75,1),labels=c("0","0.25","0.5","0.75","1"))
box()

mtext("Coef. of Determination",side=2,line=2.5)

mp2 <- barplot(RMSE, ylim=c(0,6), 
               axisnames = FALSE,
               axes=FALSE)
axis(2,at= c(0,2.5,5),labels=TRUE)
axis(1, at = mp2, labels = rep("", 5))
box()

mtext("RMSE",side=2,line=2.5)

text(mp2, par("usr")[3], 
     labels = labels_vec, 
     srt = 45, 
     adj = c(1.1,1.1), 
     xpd = NA,
     col="gray20",
     cex=1.3) 

dev.off()