

#setup directories
root <- "C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast"
Basedir  <- "/inputs/"
Driftdir <- "/inputADDdrift/"
Noisedir <- "/inputADDnoise/"
ext <- "_20.csv"
site <- "sel_multi"

#read data in
sel_multi   <- read.csv(paste0(root, Basedir, site, '.csv'), stringsAsFactors = FALSE)
sel_multi01 <- read.csv(paste0(root, Driftdir, site, '_0.01.csv'), stringsAsFactors = FALSE)
sel_multi1  <- read.csv(paste0(root, Driftdir, site, '_1.csv'), stringsAsFactors = FALSE)
sel_multi20 <- read.csv(paste0(root, Driftdir, site, '_20.csv'), stringsAsFactors = FALSE)


plot(sel_multi$pm25_0-sel_multi20$pm25_0)
summary(sel_multi$pm25_0-sel_multi20$pm25_0)

sd(sel_multi$pr-sel_multi01$pr, na.rm=TRUE)

plot(sel_multi$pm25_1,sel_multi20$pm25_1)
plot(sel_multi$pm25_1-sel_multi20$pm25_1)
summary(sel_multi$pm25_1-sel_multi20$pm25_1)


plot(sel_multi$pm25_1,sel_multi1$pm25_1)
plot(sel_multi$pm25_1-sel_multi1$pm25_1)
summary(sel_multi$pm25_1-sel_multi1$pm25_1)
