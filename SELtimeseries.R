#SEL timeseries figure 

##grab data
sel <- read.csv("inputs/sel_multi.csv", stringsAsFactors = FALSE)

##prep time

time <- as.POSIXct(sel$dtime)
SmokeFocus <- (time >= as.POSIXct("2015-08-22 00:00:00")) & (time < as.POSIXct("2015-08-24 00:00:00"))

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
par(mar=c(3,4,1,1))

pm_ylab = expression("Particulate Matter " ~ (mu ~ g ~ m^{-3}))

plot(time, sel$pm25_0,pch=NA,
     xlim=c(as.POSIXct("2011-01-01 00:00:00"),as.POSIXct("2016-1-01 00:00:00")),
     ylim=c(0,200),
     axes=FALSE,
     ylab="",
     xlab="")
polygon(xpos4poly,ypos4poly,col="gray80")
points(time, sel$pm25_0,cex=0.8)
points(time[SmokeFocus],sel$pm25_0[SmokeFocus],cex=0.8,col="red4")

box(col = 'gray20')
axis(side = 1, tck = -.03, col = 'gray20', at = xTickpos, labels=xTicklab)
axis(side = 2, tck = -.03, col = 'gray20', at = c(0,50,100,150,200), labels=c("0","50","100","150","200"))

mtext(text="Year",side=1,line=2.5)
mtext(text=pm_ylab,side=2,line=2.5)