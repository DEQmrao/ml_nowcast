#EPA nowcast

#nowcast function:
DoNOWCAST <- function(DATA){
  
  NowcastOUT <- rep(NA, length(DATA))
  
  for(ind in 12:length(DATA)){
    
    #subset data
    subDATA <- DATA[(ind-11):ind]
    good <- !is.na(subDATA)
    
    #calculate wst
    cmin <- min(subDATA[good])
    cmax <- max(subDATA[good])
    
    wst  <- cmin/cmax
    if (wst <= 0.5){wst <- 0.5}
    
    wst12XsubDATA <- c((wst^11 * subDATA[1]),
                       (wst^10 * subDATA[2]),
                       (wst^9 * subDATA[3]),
                       (wst^8 * subDATA[4]),
                       (wst^7 * subDATA[5]),
                       (wst^6 * subDATA[6]),
                       (wst^5 * subDATA[7]),
                       (wst^4 * subDATA[8]),
                       (wst^3 * subDATA[9]),
                       (wst^2 * subDATA[10]),
                       (wst^1 * subDATA[11]),
                       (wst^0 * subDATA[12])) 
    
    wst12 <- c((wst^11), 
               (wst^10),  
               (wst^9),  
               (wst^8),  
               (wst^7), 
               (wst^6), 
               (wst^5), 
               (wst^4), 
               (wst^3), 
               (wst^2), 
               (wst^1), 
               (wst^0))
    
    DoNOWer <- sum(!is.na(subDATA[10:12])) #last three obs
    if (DoNOWer >= 2){NowcastOUT[ind] <- (sum(wst12XsubDATA[good]))/(sum(wst12[good]))} 
    
  }
  
  return(NowcastOUT)
  
}



#Analysis
library(dplyr)
library(caret)

####################################################
#
# read in tweaked files
#
####################################################
sel <- read.csv("inputs/sel_multi.csv", stringsAsFactors = FALSE)


sel.01d <- read.csv("inputADDdrift/sel_multi_0.01.csv", stringsAsFactors = FALSE)
sel.05d <- read.csv("inputADDdrift/sel_multi_0.05.csv", stringsAsFactors = FALSE)
sel.1d  <- read.csv("inputADDdrift/sel_multi_0.1.csv", stringsAsFactors = FALSE)
sel.2d  <- read.csv("inputADDdrift/sel_multi_0.2.csv", stringsAsFactors = FALSE)
sel.5d  <- read.csv("inputADDdrift/sel_multi_0.5.csv", stringsAsFactors = FALSE)
sel1d   <- read.csv("inputADDdrift/sel_multi_1.csv", stringsAsFactors = FALSE)
sel5d   <- read.csv("inputADDdrift/sel_multi_5.csv", stringsAsFactors = FALSE)
sel10d  <- read.csv("inputADDdrift/sel_multi_10.csv", stringsAsFactors = FALSE)
sel20d  <- read.csv("inputADDdrift/sel_multi_20.csv", stringsAsFactors = FALSE)

## set up the list of drift data frames
dr <- list(sel, sel.01d, sel.05d, sel.1d, sel.2d, sel.5d, sel1d, sel5d, sel10d, sel20d)

Drift_nowcast <- sapply(dr, function(x) {DoNOWCAST(x$pm25_1)})
Drift_nowcast <- as.data.frame(Drift_nowcast)
Drift_nowcast <- cbind(sel$dtime, sel$pm25_0, Drift_nowcast)
names(Drift_nowcast) <- c("dtime", "pm25_0", "sel", "sel.01", "sel.05", "sel.1", "sel.2", "sel.5", "sel1", "sel5", "sel10", "sel20")

valn0 <- Drift_nowcast %>% filter(year(dtime) == 2015) 
dr_nowcast <- apply(valn0[,3:12],2,postResample, obs=valn0$pm25_0)

#fle2save <- "inputADDdrift/Drift_nowcast.csv"
#write.csv(Drift_nowcast, file = fle2save, row.names=FALSE)

sel.01n <- read.csv("inputADDnoise/sel_multi_0.01.csv", stringsAsFactors = FALSE)
sel.05n <- read.csv("inputADDnoise/sel_multi_0.05.csv", stringsAsFactors = FALSE)
sel.1n  <- read.csv("inputADDnoise/sel_multi_0.1.csv", stringsAsFactors = FALSE)
sel.2n  <- read.csv("inputADDnoise/sel_multi_0.2.csv", stringsAsFactors = FALSE)
sel.5n  <- read.csv("inputADDnoise/sel_multi_0.5.csv", stringsAsFactors = FALSE)
sel1n   <- read.csv("inputADDnoise/sel_multi_1.csv", stringsAsFactors = FALSE)
sel5n   <- read.csv("inputADDnoise/sel_multi_5.csv", stringsAsFactors = FALSE)
sel10n  <- read.csv("inputADDnoise/sel_multi_10.csv", stringsAsFactors = FALSE)
sel20n  <- read.csv("inputADDnoise/sel_multi_20.csv", stringsAsFactors = FALSE)

## set up the list of noise data frames
nr <- list(sel, sel.01n, sel.05n, sel.1n, sel.2n, sel.5n, sel1n, sel5n, sel10n, sel20n)

Noise_nowcast <- sapply(nr, function(x) {DoNOWCAST(x$pm25_1)})
Noise_nowcast <- as.data.frame(Noise_nowcast)
Noise_nowcast <- cbind(sel$dtime, sel$pm25_0, Noise_nowcast)
names(Noise_nowcast) <- c("dtime", "pm25_0", "sel", "sel.01", "sel.05", "sel.1", "sel.2", "sel.5", "sel1", "sel5", "sel10", "sel20")

valn0 <- Noise_nowcast %>% filter(year(dtime) == 2015) 
nr_nowcast <- apply(valn0[,3:12],2,postResample, obs=valn0$pm25_0)

#fle2save <- "inputADDnoise/Noise_nowcast.csv"
#write.csv(Noise_nowcast, file = fle2save, row.names=FALSE)