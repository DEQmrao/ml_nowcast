#add zero drift to input data

##test##
#get data
root <- "C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast/"
site <- "sel_multi"
data_multi <- read.csv(paste0(root, "/inputs/", site, ".csv"), stringsAsFactors = FALSE)

#make drift pattern
scaleAMT <- 20
nMonths2drift <- 3

#functions
MakeDrift <- function(nLong, driftRise, nMonths2drift){
    
    run4slope <- nMonths2drift 
    inc2drift <- driftRise/run4slope
    climb <- seq(from=0,to=driftRise,by=inc2drift)
    
    #make a vector to add drift
    driftpattern <- rep(0,2*run4slope) # default no drift
    driftpattern[run4slope:(2*run4slope)] <- climb
    
    #now make a timeseries for drift
    doREPS <- floor(nLong/length(driftpattern))+1
    DriftSeries <- rep(driftpattern, doREPS)
    DriftSeries <- DriftSeries[1:nLong]
    
    return(DriftSeries)
    
  }

lagDrift <- function(x, k) {
  if (k>0) {
    return (c(rep(0, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(0, -k)));
  }
}

#set seed
set.seed(131)

driftPMrise <- sd(data_multi$pm25_1, na.rm = TRUE) * scaleAMT
data_multi$pm25_1  <- data_multi$pm25_1  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),1)
data_multi$pm25_2  <- data_multi$pm25_2  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),2)
data_multi$pm25_3  <- data_multi$pm25_3  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),3)
data_multi$pm25_4  <- data_multi$pm25_4  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),4)
data_multi$pm25_5  <- data_multi$pm25_5  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),5)
data_multi$pm25_6  <- data_multi$pm25_6  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),6)
data_multi$pm25_7  <- data_multi$pm25_7  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),7)
data_multi$pm25_8  <- data_multi$pm25_8  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),8)
data_multi$pm25_9  <- data_multi$pm25_9  + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),9)
data_multi$pm25_10 <- data_multi$pm25_10 + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),10)
data_multi$pm25_11 <- data_multi$pm25_11 + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),11)
data_multi$pm25_12 <- data_multi$pm25_12 + lagDrift(MakeDrift(length(data_multi$pm25_0),driftPMrise,(nMonths2drift*30*24)),12)

##met
driftPRrise <- sd(data_multi$pr, na.rm = TRUE) * scaleAMT
data_multi$pr <- data_multi$pr + lagDrift(MakeDrift(length(data_multi$pr),driftPRrise,(nMonths2drift*30*24)),0)

driftWSrise <- sd(data_multi$ws, na.rm = TRUE) * scaleAMT
data_multi$ws <- data_multi$ws + lagDrift(MakeDrift(length(data_multi$ws),driftWSrise,(nMonths2drift*30*24)),0)

driftTEMPrise <- sd(data_multi$temp, na.rm = TRUE) * scaleAMT
data_multi$temp <- data_multi$temp + lagDrift(MakeDrift(length(data_multi$temp),driftTEMPrise,(nMonths2drift*30*24)),0)

driftWDrise <- sd(data_multi$wd, na.rm = TRUE) * scaleAMT
data_multi$wd <- data_multi$wd + lagDrift(MakeDrift(length(data_multi$wd),driftWDrise,(nMonths2drift*30*24)),0)

driftWDSrise <- sd(data_multi$wds, na.rm = TRUE) * scaleAMT
data_multi$wds <- data_multi$wds + lagDrift(MakeDrift(length(data_multi$wds),driftWDSrise,(nMonths2drift*30*24)),0)

driftWDCrise <- sd(data_multi$wdc, na.rm = TRUE) * scaleAMT
data_multi$wdc <- data_multi$wdc + lagDrift(MakeDrift(length(data_multi$wdc),driftWDCrise,(nMonths2drift*30*24)),0)

#write out a .csv
fleOUT <- paste0(root, "inputADDdrift/", site, '_', toString(scaleAMT), '.csv')
write.csv(data_multi, file = fleOUT, row.names=FALSE)