library(gbm)
library(dplyr)
library(lubridate)
library(caret)

form1 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6")
form2 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6 + hour + wday + month")
form3 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6 + 
                 hour + wday + month + temp + pr + ws + wd")
form4 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6 + 
                 hour + wday + month + temp + pr + ws + wds + wdc")

## read in the input data files
c(sel_multi, pdp_multi, bws_multi, gpp_multi, lcm_multi) %<-% lapply(stations, function(stn) {
  read.csv(paste0("inputs/", stn_name[[stn]], "_multi.csv"), stringsAsFactors = FALSE)
})



in_dfs <- list(sel = sel_multi, pdp = pdp_multi, bws = bws_multi, gpp= gpp_multi, lcm = lcm_multi)
forms <- list(form1, form2, form3, form4)

ff <- data.frame(site = c("sel", "pdp", "bws", "gpp", "lcm"), 
                 form1 = c(NA, NA, NA, NA, NA),
                 form2 = c(NA, NA, NA, NA, NA),
                 form3 = c(NA, NA, NA, NA, NA),
                 form4 = c(NA, NA, NA, NA, NA))

set.seed(1973)
ns <- length(in_dfs)
set.seed(1973)
ns <- length(in_dfs)
for (s in 1:ns) {
  df <- in_dfs[[s]]
  tr <- df %>% filter(year(dtime) < 2015) %>% filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                                                           hour, wday, month, temp, pr, ws, wd, wds, wdc))
  val <- df %>% filter(year(dtime) == 2015) %>% filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                                                             hour, wday, month, temp, pr, ws, wd, wds, wdc))
  for (i in 1:length(forms)) {
    gbmm <- gbm( forms[[i]], data = tr, distribution = "gaussian", n.trees = 1200, n.minobsinnode = 6,
                 interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = 1.0, cv.folds = 10 )
    tr <- cbind(tr, gbm_pred = predict(gbmm))
    val <- cbind(val, gbm_pred = predict(gbmm, newdata = val))
    
    rest <- summary(lm(data = tr, gbm_pred ~ neph))
    r2t <- round(rest$adj.r.squared, 3)
    
    
    resv <- summary(lm(data = val, gbm_pred ~ neph))
    r2v <- round(resv$adj.r.squared, 3)
    
    colnames(tr)[which(colnames(tr) == "gbm_pred")] <- paste0("gbm", as.character(i))
    colnames(val)[which(colnames(val) == "gbm_pred")] <- paste0("gbm", as.character(i))
    saveRDS(gbmm, paste0("models/", names(in_dfs)[[s]], "_gbm_form", as.character(i), ".rds"))

    ff[s,(i+1)] <- r2v
    print(rest)
    print(resv)
    
  }
  write.csv(tr, paste0("models/", names(in_dfs)[[s]], "_train_gbm.csv" ), row.names = FALSE)
  write.csv(val, paste0("models/", names(in_dfs)[[s]], "_val_gbm.csv" ), row.names = FALSE)
  print(ff)
    
}
  

















sel <- read.csv("inputs/sel_multi.csv", stringsAsFactors = FALSE)
sel_tr <- sel %>% filter(year(dtime) < 2015) %>% filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                                                       hour, wday, month, temp, pr, ws, wd, wds, wdc))
sel_val <- sel %>% filter(year(dtime) == 2015) %>% filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                                                       hour, wday, month, temp, pr, ws, wd, wds, wdc))

sel_gbm1 <- gbm( form1, data = sel_tr, distribution = "gaussian", n.trees = 5000, n.minobsinnode = 12,
                 interaction.depth = 4, shrinkage = 0.001, bag.fraction = 0.5, train.fraction = 1.0, cv.folds = 10 )
gbm.perf(sel_gbm1)
summary(sel_gbm1)
sel_tr$gbm1 <- predict(sel_gbm1, n.trees = 5000)
summary(lm(data = sel_tr, gbm1 ~ neph))
sel_val$gbm1 <- predict(sel_gbm1, newdata = sel_val, n.trees = 5000)
summary(lm(data = sel_val, gbm1 ~ neph))

Sys.time()
sel_gbm2 <- gbm( form2, data = sel_tr, distribution = "gaussian", n.trees = 5000, n.minobsinnode = 6,
                 interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = 1.0, cv.folds = 10 )
gbm.perf(sel_gbm2)
summary(sel_gbm2)
sel_tr$gbm2 <- predict(sel_gbm2, n.trees = 1432)
summary(lm(data = sel_tr, gbm2 ~ neph))
sel_val$gbm2 <- predict(sel_gbm2, newdata = sel_val, n.trees = 1432)
summary(lm(data = sel_val, gbm2 ~ neph))

Sys.time()


Sys.time()
sel_gbm3 <- gbm( form3, data = sel_tr, distribution = "gaussian", n.trees = 2500, n.minobsinnode = 6,
                 interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = 0.8, cv.folds = 10 )
gbm.perf(sel_gbm3)
summary(sel_gbm3)
sel_tr$gbm3 <- predict(sel_gbm3, n.trees = 2500)
summary(lm(data = sel_tr, gbm3 ~ neph))
sel_val$gbm3 <- predict(sel_gbm3, newdata = sel_val, n.trees = 2500)
summary(lm(data = sel_val, gbm3 ~ neph))

Sys.time()

tr <- pdp_multi %>% filter(year(dtime) < 2015) %>% filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                                                       hour, wday, month, temp, pr, ws, wd, wds, wdc))
val <- pdp_multi %>% filter(year(dtime) == 2015) %>% filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                                                      hour, wday, month, temp, pr, ws, wd, wds, wdc))
Sys.time()
gbm2 <- gbm( form3, data = tr, distribution = "gaussian", n.trees = 1200, n.minobsinnode = 6,
                 interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = 1.0, cv.folds = 10 )
gbm.perf(gbm2)
summary(gbm2)
tr$gbm2 <- predict(gbm2, n.trees = 1200)
summary(lm(data = tr, gbm2 ~ neph))
val$gbm2 <- predict(gbm2, newdata = val, n.trees = 1200)
summary(lm(data = val, gbm2 ~ neph))
Sys.time()
Sys.time()
gbm3 <- gbm( form3, data = tr, distribution = "gaussian", n.trees = 3200, n.minobsinnode = 12,
             interaction.depth = 4, shrinkage = 0.01, bag.fraction = 0.5, train.fraction = 1.0, cv.folds = 10 )
gbm.perf(gbm3)
summary(gbm3)
tr$gbm3 <- predict(gbm3, n.trees = 4800)
summary(lm(data = tr, gbm3 ~ neph))
val$gbm3 <- predict(gbm3, newdata = val, n.trees = 4800)
summary(lm(data = val, gbm3 ~ neph))
Sys.time()










set.seed(2377)
control <- trainControl(method = "cv", number = 10 )
gbma <- train(form1, data = sel_tr, method = "gbm", trControl = control)
gbmb
