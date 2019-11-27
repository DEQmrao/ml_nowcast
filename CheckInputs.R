

#setup directories
root <- "C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast"
Basedir  <- "/inputs/"
Driftdir <- "/inputADDdrift/"
Noisedir <- "/inputADDnoise/"
site <- "sel_multi"

#read data in
sel_multi   <- read.csv(paste0(root, Basedir, site, '.csv'), stringsAsFactors = FALSE)

sel_multi001 <- read.csv(paste0(root, Driftdir, site, '_0.01.csv'), stringsAsFactors = FALSE)
sel_multi005 <- read.csv(paste0(root, Driftdir, site, '_0.05.csv'), stringsAsFactors = FALSE)
sel_multi01  <- read.csv(paste0(root, Driftdir, site, '_0.1.csv'), stringsAsFactors = FALSE)
sel_multi02  <- read.csv(paste0(root, Driftdir, site, '_0.2.csv'), stringsAsFactors = FALSE)
sel_multi05  <- read.csv(paste0(root, Driftdir, site, '_0.5.csv'), stringsAsFactors = FALSE)
sel_multi1   <- read.csv(paste0(root, Driftdir, site, '_1.csv'), stringsAsFactors = FALSE)
sel_multi5   <- read.csv(paste0(root, Driftdir, site, '_5.csv'), stringsAsFactors = FALSE)
sel_multi10  <- read.csv(paste0(root, Driftdir, site, '_10.csv'), stringsAsFactors = FALSE)
sel_multi20  <- read.csv(paste0(root, Driftdir, site, '_20.csv'), stringsAsFactors = FALSE)

#low rnage
plot(sel_multi$pm25_1)
points(sel_multi001$pm25_1, col = "red")
points(sel_multi005$pm25_1, col = "red4")
points(sel_multi01$pm25_1, col = "blue4")
points(sel_multi05$pm25_1, col = "blue")

plot(sel_multi05$pm25_1-sel_multi$pm25_1, col = "blue")
points(sel_multi001$pm25_1-sel_multi$pm25_1, col = "red")
points(sel_multi005$pm25_1-sel_multi$pm25_1, col = "red4")
points(sel_multi01$pm25_1-sel_multi$pm25_1, col = "blue4")
points(sel_multi02$pm25_1-sel_multi$pm25_1, col = "orange")

#high range
plot(sel_multi$pm25_1)
points(sel_multi1$pm25_1, col = "red")
points(sel_multi5$pm25_1, col = "red4")
points(sel_multi10$pm25_1, col = "blue4")
points(sel_multi20$pm25_1, col = "blue")

plot(sel_multi20$pm25_1-sel_multi$pm25_1, col = "blue")
points(sel_multi1$pm25_1-sel_multi$pm25_1, col = "red")
points(sel_multi5$pm25_1-sel_multi$pm25_1, col = "red4")
points(sel_multi10$pm25_1-sel_multi$pm25_1, col = "blue4")
points(sel_multi20$pm25_1-sel_multi$pm25_1, col = "orange")

sel_multi001 <- read.csv(paste0(root, Noisedir, site, '_0.01.csv'), stringsAsFactors = FALSE)
sel_multi005 <- read.csv(paste0(root, Noisedir, site, '_0.05.csv'), stringsAsFactors = FALSE)
sel_multi01  <- read.csv(paste0(root, Noisedir, site, '_0.1.csv'), stringsAsFactors = FALSE)
sel_multi02  <- read.csv(paste0(root, Noisedir, site, '_0.2.csv'), stringsAsFactors = FALSE)
sel_multi05  <- read.csv(paste0(root, Noisedir, site, '_0.5.csv'), stringsAsFactors = FALSE)
sel_multi1   <- read.csv(paste0(root, Noisedir, site, '_1.csv'), stringsAsFactors = FALSE)
sel_multi5   <- read.csv(paste0(root, Noisedir, site, '_5.csv'), stringsAsFactors = FALSE)
sel_multi10  <- read.csv(paste0(root, Noisedir, site, '_10.csv'), stringsAsFactors = FALSE)
sel_multi20  <- read.csv(paste0(root, Noisedir, site, '_20.csv'), stringsAsFactors = FALSE)

#low rnage
plot(sel_multi$pm25_1)
points(sel_multi001$pm25_1, col = "red")
points(sel_multi005$pm25_1, col = "red4")
points(sel_multi01$pm25_1, col = "blue4")
points(sel_multi05$pm25_1, col = "blue")

plot(sel_multi05$pm25_1-sel_multi$pm25_1, col = "blue")
points(sel_multi001$pm25_1-sel_multi$pm25_1, col = "red")
points(sel_multi005$pm25_1-sel_multi$pm25_1, col = "red4")
points(sel_multi01$pm25_1-sel_multi$pm25_1, col = "blue4")
points(sel_multi02$pm25_1-sel_multi$pm25_1, col = "orange")

sd(sel_multi05$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
sd(sel_multi001$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
sd(sel_multi005$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
sd(sel_multi01$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
sd(sel_multi02$pm25_1-sel_multi$pm25_1, na.rm=TRUE)

#high range
plot(sel_multi$pm25_1)
points(sel_multi1$pm25_1, col = "red")
points(sel_multi5$pm25_1, col = "red4")
points(sel_multi10$pm25_1, col = "blue4")
points(sel_multi20$pm25_1, col = "blue")

plot(sel_multi20$pm25_1-sel_multi$pm25_1, col = "blue")
points(sel_multi10$pm25_1-sel_multi$pm25_1, col = "blue4")
points(sel_multi5$pm25_1-sel_multi$pm25_1, col = "red4")
points(sel_multi1$pm25_1-sel_multi$pm25_1, col = "red")

sd(sel_multi20$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
sd(sel_multi1$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
sd(sel_multi5$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
sd(sel_multi10$pm25_1-sel_multi$pm25_1, na.rm=TRUE)
