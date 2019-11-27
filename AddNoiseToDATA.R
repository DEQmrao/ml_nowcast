#get data
root <- "C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast/"
site <- "sel_multi"
data_multi <- read.csv(paste0(root, "/inputs/", site, ".csv"), stringsAsFactors = FALSE)

#set seed
set.seed(131)

#function
lagDrift <- function(x, k) {
  if (k>0) {
    return (c(rep(x[1], k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(x[length(x)], -k)));
  }
}

#scale noise
scaleAMT <- 20

#add noise to data - use standard deviation of input
##pm
jitterPMamt <- sd(data_multi$pm25_0, na.rm = TRUE) * scaleAMT
JitterPM25_1 <- jitter(data_multi$pm25_1, factor=1, amount=jitterPMamt)
JitterOffset <- JitterPM25_1 - data_multi$pm25_1

data_multi$pm25_1  <- data_multi$pm25_1 + lagDrift(JitterOffset,0)
data_multi$pm25_2  <- data_multi$pm25_2 + lagDrift(JitterOffset,1)
data_multi$pm25_3  <- data_multi$pm25_3 + lagDrift(JitterOffset,2)
data_multi$pm25_4  <- data_multi$pm25_4 + lagDrift(JitterOffset,3)
data_multi$pm25_5  <- data_multi$pm25_5 + lagDrift(JitterOffset,4)
data_multi$pm25_6  <- data_multi$pm25_6 + lagDrift(JitterOffset,5)
data_multi$pm25_7  <- data_multi$pm25_7 + lagDrift(JitterOffset,6)
data_multi$pm25_8  <- data_multi$pm25_8 + lagDrift(JitterOffset,7)
data_multi$pm25_9  <- data_multi$pm25_9 + lagDrift(JitterOffset,8)
data_multi$pm25_10 <- data_multi$pm25_10 + lagDrift(JitterOffset,9)
data_multi$pm25_11 <- data_multi$pm25_11 + lagDrift(JitterOffset,10)
data_multi$pm25_12 <- data_multi$pm25_12 + lagDrift(JitterOffset,11)

##met
jitterPRamt <- sd(data_multi$pr, na.rm = TRUE) * scaleAMT
data_multi$pr <- jitter(data_multi$pr, factor=1, amount=jitterPRamt)

jitterWSamt <- sd(data_multi$ws, na.rm = TRUE) * scaleAMT
data_multi$ws <- jitter(data_multi$ws, factor=1, amount=jitterWSamt)

jitterTEMPamt <- sd(data_multi$temp, na.rm = TRUE) * scaleAMT
data_multi$temp <- jitter(data_multi$temp, factor=1, amount=jitterTEMPamt)

jitterWDamt <- sd(data_multi$wd, na.rm = TRUE) * scaleAMT
data_multi$wd <- jitter(data_multi$wd, factor=1, amount=jitterWDamt)

jitterWDSamt <- sd(data_multi$wds, na.rm = TRUE) * scaleAMT
data_multi$wds <- jitter(data_multi$wds, factor=1, amount=jitterWDSamt)

jitterWDCamt <- sd(data_multi$wdc, na.rm = TRUE) * scaleAMT
data_multi$wdc <- jitter(data_multi$wdc, factor=1, amount=jitterWDCamt)

#write out a .csv
fleOUT <- paste0(root, "inputADDnoise/", site, '_', toString(scaleAMT), '.csv')
write.csv(data_multi, file = fleOUT, row.names=FALSE)