library(feather)    ## fast read and write to file
library(dplyr)
library(ggplot2)
library(imputeTS)   ## imputing missing time series values
library(dplyr)
library(imputeTS)   ## impute time series data
library(lubridate)  ## Datetime helper functions
library(hashmap)    ## for named lists
library(zeallot)    ## %<-% operator for multiple assignments
library(pryr)       ## for memory usage



## set up the stations & years we are interested in, as well as a hashmap of stn_ids to 3 letter codes for naming
stations <- list(410510080, 410130100, 410250003, 410330114, 410370001)
stn_name <- hashmap(c(410510080, 410130100, 410250003, 410330114, 410370001), c("sel", "pdp", "bws", "gpp", "lcm"))
yrs <- list(2015, 2014, 2013, 2012, 2011)
param <- hashmap(c(88502, 62101, 61103, 61104, 64101, 62201), c("neph", "temp", "ws", "wd", "pr", "rh"))

## function used to read data files and extract station info
## used to extract neph pm2.5, temp, rh, and wind speed &d direction data
## reqires the stations list, stn_name and param hashmaps set up (above)
stn_data <- function(stn, in_data, file_substr) {
  x <- lapply(in_data, function(df) {df %>% filter(stn_id == stn) %>% 
      select(stn_id, Datetime.Local, Date.Local, Parameter.Code, Sample.Measurement)}) %>%
    bind_rows()
  colnames(x)[which(colnames(x) == "Sample.Measurement")] <- paste0(param[[x$Parameter.Code[1]]])
  write.csv(x, paste0("data/stn", as.character(stn), "_", file_substr, ".csv"), row.names = FALSE)
  return(x)
}


## "regularize" data - format datasets prior to running random forest - columns with previous 
## 24 hours of readings (6 for RF predictors, 12 for Reff)
## interpolate missing data 


regular_df <- function(df, ...) {
  vars <- list(...)
  df$Date.Local <- as.Date(df$Date.Local)
  st_day <- min(df$Date.Local) - days(1)
  end_day <- max(df$Date.Local) + days(1)
  
  dfh <- data.frame(stn_id = df$stn_id[1], dtime=as.character(seq(from=(as.POSIXct(st_day) + hours(8) ), 
                                                                  to=(as.POSIXct(end_day) + hours(7)),by="hour")), stringsAsFactors = FALSE)
  dfh$hour <- hour(dfh$dtime)
  dfh$month <- month(dfh$dtime) - 1
  dfh$wday <- as.numeric(format(as.Date(dfh$dtime), "%w"))
  lenvar <- length(vars)
  
  for (i in 1:lenvar) {
    vardf <- vars[[i]]
    cur_par <- param[[vardf$Parameter.Code[1]]]
    vardf <- vardf %>% select(stn_id, Datetime.Local, contains(cur_par))
    vardf <- vardf %>% rename(dtime = Datetime.Local)
    dfh <- merge(dfh, vardf, all.x = TRUE, by = c("stn_id", "dtime"))
    if (cur_par == "wd") {
      dfh$wds <- NA
      dfh$wdc <- NA
      dfh[!is.na(dfh$wd), "wds"] <- sin(dfh[!is.na(dfh$wd), "wd"]*pi/180)
      dfh[!is.na(dfh$wd), "wdc"] <- cos(dfh[!is.na(dfh$wd), "wd"]*pi/180)
      
    }
  }
  
  df <- df %>% select(Datetime.Local, contains(param[[df$Parameter.Code[1]]])) %>% rename(dtime = Datetime.Local)
  
  df <- merge(dfh, df, all.x = TRUE, by = "dtime")
  
  pm25_0 <- df %>% select(contains("neph")) %>% na.interpolation
  colnames(pm25_0)[1] <- "pm25_0"
  df <- cbind(df, pm25_0[1])
  
  
  len <- length(df$pm25_0)
  for (i in 1:24) {
    coln <- paste0("pm25_", i)
    st <- i + 1
    en <- len - i
    
    df[, coln] <- NA
    df[, coln][st:len] <- df$pm25_0[1:en]
    df[, coln]
  }
  df<- df[25:len,]
  pmi <- which(colnames(df) == "pm25_1")
  df$min <- apply(df[,pmi:(pmi+11)], 1, min)
  df$max <- apply(df[,pmi:(pmi+11)], 1, max)
  df$fac <- 1 - (df$max - df$min)/df$max
  df$fac[df$fac <= 0.5] <- 0.5 #awf 11/26/2019
  df$reff <- (df[,"pm25_1"]*(df$fac^0) + df[,"pm25_2"]*(df$fac^1) + df[,"pm25_3"]*(df$fac^2) + df[,"pm25_4"]*(df$fac^3) + 
                df[,"pm25_5"]*(df$fac^4) + df[,"pm25_6"]*(df$fac^5) + df[,"pm25_7"]*(df$fac^6) + df[,"pm25_8"]*(df$fac^7) + 
                df[,"pm25_9"]*(df$fac^8) + df[,"pm25_10"]*(df$fac^9) + df[,"pm25_11"]*(df$fac^10) + df[,"pm25_12"]*(df$fac^11))/
    (df$fac^0 + df$fac^1 + df$fac^2 + df$fac^3 + df$fac^4 + df$fac^5 + 
       df$fac^6 + df$fac^7 + df$fac^8 + df$fac^9 + df$fac^10 + df$fac^11)
  return(df)
  
}

#pdx_multi <- regular_df(df = pdx_neph, temp = pdx_temp, ws = pdx_ws, wd = pdx_wd, rh = pdx_rh)
