library(randomForest)
library(caret)
library(dplyr)
library(lubridate)
library(zeallot)
library(hashmap)    ## for named lists


stations <- list(410510080, 410130100, 410250003, 410330114, 410370001)
stn_name <- hashmap(c(410510080, 410130100, 410250003, 410330114, 410370001), c("sel", "pdp", "bws", "gpp", "lcm"))


## read in the input data files
c(sel_multi, pdp_multi, bws_multi, gpp_multi, lcm_multi) %<-% lapply(stations, function(stn) {
  read.csv(paste0("inputs/", stn_name[[stn]], "_multi.csv"), stringsAsFactors = FALSE)
})


## split into training and validation
sel_tr <- sel_multi %>% filter(year(dtime) < 2015)
sel_val <- sel_multi %>% filter(year(dtime) == 2015)
pdp_tr <- pdp_multi %>% filter(year(dtime) < 2015)
pdp_val <- pdp_multi %>% filter(year(dtime) == 2015)
bws_tr <- bws_multi %>% filter(year(dtime) < 2015)
bws_val <- bws_multi %>% filter(year(dtime) == 2015)
gpp_tr <- gpp_multi %>% filter(year(dtime) < 2015)
gpp_val <- gpp_multi %>% filter(year(dtime) == 2015)
lcm_tr <- lcm_multi %>% filter(year(dtime) < 2015)
lcm_val <- lcm_multi %>% filter(year(dtime) == 2015)

train_dfs <- list(sel_tr = sel_tr, pdp_tr = pdp_tr, bws_tr = bws_tr, gpp_tr = gpp_tr, lcm_tr = lcm_tr)
val_dfs <- list(sel_val = sel_val, pdp_val = pdp_val, bws_val = bws_val, gpp_val = gpp_val, lcm_val = lcm_val)

## the formulas for which we will run random forest
form1 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6")
form2 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6 + hour + wday + month")
form3 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6 + 
                 hour + wday + month + temp + pr + ws + wd")
form4 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6 + 
                 hour + wday + month + temp + pr + ws + wds + wdc")
forms <- list(form1, form2, form3, form4)

n_tr <- length(train_dfs)
for (t in 1:n_tr) {
  start.time <- Sys.time()
  df.tr <-  train_dfs[[t]]
  df.val <- val_dfs[[t]]
  dft <- df.tr[, c("hour", "month", "wday", "temp", "pr", "ws", "wd", "wds", "wdc",
                  "pm25_1", "pm25_2", "pm25_3", "pm25_4", "pm25_5", "pm25_6")]
  dfv <- df.val[, c("hour", "month", "wday", "temp", "pr", "ws", "wd","wds", "wdc",
                   "pm25_1", "pm25_2", "pm25_3", "pm25_4", "pm25_5", "pm25_6")]
  nvals <- ncol(dft)
  sc.fac <- data.frame(avg = sapply(dft, function(x) mean(x, na.rm = TRUE)), 
                       sd = sapply(dft, function(x) sd(x, na.rm = TRUE)))
  sc.dft <- sapply(1:nvals, function(n) { (dft[, n] - sc.fac$avg[n])/sc.fac$sd[n] })
  sc.dfv <- sapply(1:nvals, function(n) { (dfv[, n] - sc.fac$avg[n])/sc.fac$sd[n] })
  colnames(sc.dft) <- colnames(dft)
  colnames(sc.dfv) <- colnames(dfv)
  dft.in <- cbind(df.tr[, c("dtime", "neph", "reff", "pm25_0")], sc.dft)
  dft.out <- dft.in
  dfv.in <- cbind(df.val[, c("dtime", "neph", "reff", "pm25_0")], sc.dfv)
  dfv.out <- dfv.in
  
  for (i in 1:length(forms)) {
    dft.run <- dft.in[complete.cases(dft.in),]
    dfv.run <- dfv.in[complete.cases(dfv.in),]
    rf <- randomForest(forms[[i]], data = dft.run, importance = TRUE, mtry = 4, ntree = 500, na.action = na.omit )
    dft.run <- cbind(dft.run, rf$predicted)
    dfv.run <- cbind(dfv.run, rf_pred = predict(rf, dfv.run))
    dft.out <- merge(dft.out, dft.run[, c("dtime", "rf$predicted")], by = "dtime", all.x = TRUE)
    dfv.out <- merge(dfv.out, dfv.run[, c("dtime", "rf_pred")], by = "dtime", all.x = TRUE)
    colnames(dft.out)[ncol(dft.out)] <- paste0("rf", as.character(i))
    colnames(dfv.out)[ncol(dfv.out)] <- paste0("rf", as.character(i))
    saveRDS(rf, paste0("models/", names(train_dfs)[[t]], "_form", as.character(i), ".rds"))
    end.time <- Sys.time()
    rftime <- as.character(round((end.time - start.time), 4))
    print(paste0("formula ", as.character(i), "  ", rftime))
  }
  write.csv(dft.out, paste0("models/", names(train_dfs)[[t]], "_train_rf.csv" ), row.names = FALSE)
  write.csv(dfv.out, paste0("models/", names(train_dfs)[[t]], "_val_rf.csv" ), row.names = FALSE)
  end.time <- Sys.time()
  rftime <- as.character(round((end.time - start.time), 4))
  
  print(paste0(names(train_dfs)[[t]], "...........", rftime, ".......done"))
}
