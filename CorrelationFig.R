library(dplyr)
library(lubridate)
library(corrplot)

#setup directories
root <- "C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast"
Basedir  <- "/inputs/"
site <- "sel_multi"

#read data in
sel_multi   <- read.csv(paste0(root, Basedir, site, '.csv'), stringsAsFactors = FALSE)


#split into training and validation
df.tr <- sel_multi %>% filter(year(dtime) < 2015)

#data prep.
#subset 
dft <- df.tr[, c("hour", "month", "wday", "temp", "pr", "ws", "wd", "wds", "wdc",
                 "pm25_0", "pm25_1", "pm25_2", "pm25_3", "pm25_4", "pm25_5", "pm25_6")]

dft.run <- dft[complete.cases(dft),]

#get corr
dft_cor <- cor(dft.run)

#make plot
corrplot(dft_cor, type="upper", tl.col="black", tl.srt=45)

#autocorrelation plot
ACFout <- acf(dft.run$pm25_0, plot=FALSE)
plot(ACFout, ci = 0, main = "", xlab = "Hour lag", ylab="Correlation")
