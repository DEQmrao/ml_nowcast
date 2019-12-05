#SEL timeseries figure 
library(lubridate)

whatplot <- "drift"

##grab data
if(whatplot == "none"){sel <- read.csv("inputs/sel_multi.csv", stringsAsFactors = FALSE)}
if(whatplot == "noise"){sel <- read.csv("inputADDnoise/sel_multi_1.csv", stringsAsFactors = FALSE)}
if(whatplot == "drift"){sel <- read.csv("inputADDdrift/sel_multi_1.csv", stringsAsFactors = FALSE)}

##prep time

time <- as.POSIXct(sel$dtime)
#SmokeFocus <- (time >= as.POSIXct("2015-08-22 00:00:00")) & (time < as.POSIXct("2015-08-24 00:00:00"))
SmokeFocus <- (year(sel$dtime) == 2015) & sel$pm25_1 > 90

xTickpos <- c(as.POSIXct("2011-01-01 00:00:00"),
              as.POSIXct("2012-01-01 00:00:00"),
              as.POSIXct("2013-01-01 00:00:00"),
              as.POSIXct("2014-01-01 00:00:00"),
              as.POSIXct("2015-01-01 00:00:00"),
              as.POSIXct("2016-01-01 00:00:00"))

xTicklab <- c("2011","2012","2013","2014","2015","2016")

#polygon
xpos4poly <- c("2015-01-01 00:00:00","2015-01-01 00:00:00","2016-01-01 00:00:00","2016-01-01 00:00:00")
xpos4poly <- as.POSIXct(xpos4poly)

ypos4poly <- c(-10,250,250,-10)

##now make the figure
png(filename = paste0("C:/Users/afellow/Desktop/RworkAQM/GITWORK/ml_nowcast/figures/SELtimeseries", whatplot, ".png"),
    width = 10, height = 5, units = "in", pointsize = 18, res=300)

par(mar=c(4,4,1,1))

pm_ylab = expression("Particulate Matter " ~ (mu ~ g ~ m^{-3}))

plot(time, sel$pm25_1,pch=NA,
     xlim=c(as.POSIXct("2011-01-01 00:00:00"),as.POSIXct("2016-1-01 00:00:00")),
     ylim=c(0,200),
     axes=FALSE,
     ylab="",
     xlab="")
polygon(xpos4poly,ypos4poly,col="gray80")
points(time, sel$pm25_1,cex=0.35,pch=16,col=alpha("gray25",0.4))
points(time[SmokeFocus],sel$pm25_1[SmokeFocus],cex=0.35,col="red4")

box(col = 'gray20')
axis(side = 1, tck = -.03, col = 'gray20', at = xTickpos, labels=xTicklab)
axis(side = 2, tck = -.03, col = 'gray20', at = c(0,50,100,150,200), labels=c("0","50","100","150","200"))

mtext(text="Year",side=1,line=2.5)
mtext(text=pm_ylab,side=2,line=2.5)

dev.off()