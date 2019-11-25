
#get data
root <- "C:/Users/afellow/Desktop/RworkAQM/nowcast/ml_nowcast/"
site <- "sel_multi"
data_multi <- read.csv(paste0(root, "/inputs/", site, ".csv"), stringsAsFactors = FALSE)

#scale noise
scaleAMT <- 0.05

#set seed
set.seed(131)

#add noise to data - use standard deviation of input
##pm
jitterPMamt <- sd(data_multi$pm25_0, na.rm = TRUE) * scaleAMT
data_multi$pm25_0 <- jitter(data_multi$pm25_0, factor=1, amount=jitterPMamt)
data_multi$pm25_1 <- jitter(data_multi$pm25_1, factor=1, amount=jitterPMamt)
data_multi$pm25_2 <- jitter(data_multi$pm25_2, factor=1, amount=jitterPMamt)
data_multi$pm25_3 <- jitter(data_multi$pm25_3, factor=1, amount=jitterPMamt)
data_multi$pm25_4 <- jitter(data_multi$pm25_4, factor=1, amount=jitterPMamt)
data_multi$pm25_5 <- jitter(data_multi$pm25_5, factor=1, amount=jitterPMamt)
data_multi$pm25_6 <- jitter(data_multi$pm25_6, factor=1, amount=jitterPMamt)
data_multi$pm25_7 <- jitter(data_multi$pm25_7, factor=1, amount=jitterPMamt)
data_multi$pm25_8 <- jitter(data_multi$pm25_8, factor=1, amount=jitterPMamt)
data_multi$pm25_9 <- jitter(data_multi$pm25_9, factor=1, amount=jitterPMamt)
data_multi$pm25_10 <- jitter(data_multi$pm25_10, factor=1, amount=jitterPMamt)
data_multi$pm25_11 <- jitter(data_multi$pm25_11, factor=1, amount=jitterPMamt)
data_multi$pm25_12 <- jitter(data_multi$pm25_12, factor=1, amount=jitterPMamt)

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