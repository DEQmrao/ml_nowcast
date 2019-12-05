library(dplyr)
library(caret)
library(lubridate)
library(zeallot)
library(ranger)
library(e1071)
library(gbm)
library(pryr)

sel <- read.csv("inputs/sel_multi.csv", stringsAsFactors = FALSE)


sel.01d <- read.csv("inputADDdrift/sel_multi_0.01.csv", stringsAsFactors = FALSE)
sel.05d <- read.csv("inputADDdrift/sel_multi_0.05.csv", stringsAsFactors = FALSE)
sel.1d <- read.csv("inputADDdrift/sel_multi_0.1.csv", stringsAsFactors = FALSE)
sel.2d <- read.csv("inputADDdrift/sel_multi_0.2.csv", stringsAsFactors = FALSE)
sel.5d <- read.csv("inputADDdrift/sel_multi_0.5.csv", stringsAsFactors = FALSE)
sel1d <- read.csv("inputADDdrift/sel_multi_1.csv", stringsAsFactors = FALSE)
sel5d <- read.csv("inputADDdrift/sel_multi_5.csv", stringsAsFactors = FALSE)
sel10d <- read.csv("inputADDdrift/sel_multi_10.csv", stringsAsFactors = FALSE)
sel20d <- read.csv("inputADDdrift/sel_multi_20.csv", stringsAsFactors = FALSE)
dr <- list(sel, sel.01d, sel.05d, sel.1d, sel.2d, sel.5d, sel1d, sel5d, sel10d, sel20d)
dr_names <- list("sel_drift", "sel0.01_drift","sel0.05_drift", "sel0.10_drift", "sel0.20_drift",
                 "sel0.50_drift", "sel1.00_drift", "sel5.00_drift", "sel10.0_drift", "sel20.0_drift")
summ_colnames <- list("pm25_0", "sel", "sel0.01","sel0.05", "sel0.10", "sel0.20",
                 "sel0.50", "sel1.00", "sel5.00", "sel10.0", "sel20.0")


sel.01n <- read.csv("inputADDnoise/sel_multi_0.01.csv", stringsAsFactors = FALSE)
sel.05n <- read.csv("inputADDnoise/sel_multi_0.05.csv", stringsAsFactors = FALSE)
sel.1n <- read.csv("inputADDnoise/sel_multi_0.1.csv", stringsAsFactors = FALSE)
sel.2n <- read.csv("inputADDnoise/sel_multi_0.2.csv", stringsAsFactors = FALSE)
sel.5n <- read.csv("inputADDnoise/sel_multi_0.5.csv", stringsAsFactors = FALSE)
sel1n <- read.csv("inputADDnoise/sel_multi_1.csv", stringsAsFactors = FALSE)
sel5n <- read.csv("inputADDnoise/sel_multi_5.csv", stringsAsFactors = FALSE)
sel10n <- read.csv("inputADDnoise/sel_multi_10.csv", stringsAsFactors = FALSE)
sel20n <- read.csv("inputADDnoise/sel_multi_20.csv", stringsAsFactors = FALSE)
nr <- list(sel, sel.01n, sel.05n, sel.1n, sel.2n, sel.5n, sel1n, sel5n, sel10n, sel20n)
nr_names <- list("sel_noise", "sel0.01_noise","sel0.05_noise", "sel0.10_noise", "sel0.20_noise",
                 "sel0.50_noise", "sel1.00_noise", "sel5.00_noise", "sel10.0_noise", "sel20.0_noise")


form3 <- formula("pm25_0 ~ pm25_1 + pm25_2 + pm25_3 + pm25_4 + pm25_5 + pm25_6 + hour + wday + month + temp + pr + ws + wd")



####################################################
#
# run a ML algorith on drift and noise data
# save models and predictions for validation sets
#
####################################################

#run_control <- list(trControl, tuneGrid, method)
run_ml <- function(df, run_control) {
  trControl <- run_control[[1]]
  tuneGrid <- run_control[[2]]
  method <- run_control[[3]]
  trn0 <- df %>% filter(year(dtime) < 2015) %>%
               filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                     hour, wday, month, temp, pr, ws, wd))
  valn0 <- df %>% filter(year(dtime) == 2015) %>%
                  filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                        hour, wday, month, temp, pr, ws, wd))
  set.seed(42)
  base_mod <- train(form3, 
                    data = trn0, 
                    method = method, 
                    trControl = trControl,
                    tuneGrid = tuneGrid)
  return(base_mod)
}

pred_ml <- function(mods, dfs, type = "drift_lm") {
  len <- length(dfs)
  valn0 <- dfs[[1]] %>% filter(year(dtime) == 2015) %>%
                        filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                              hour, wday, month, temp, pr, ws, wd))
  pred_df <- data.frame(pm25_0 = valn0$pm25_0)
  summ_df <- data.frame()[1:3, ]
  rownames(summ_df) <- c("RMSE", "Rsquared", "MAE")
  for (n in 1:len) {
    valn0 <- dfs[[n]] %>% filter(year(dtime) == 2015) %>%
                          filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6,
                                                hour, wday, month, temp, pr, ws, wd))
    rpred <- predict(mods[[n]], newdata = valn0)
    pred_df <- cbind(pred_df, rpred)
    summ <- postResample(pred = rpred, obs = pred_df$pm25_0)
    summ_df <- cbind(summ_df, summ)
  }
  fname <- paste0("./results/sel_", type, ".csv" )
  colnames(pred_df) <- summ_colnames
  colnames(summ_df) <- summ_colnames[-1]
  write.csv(pred_df, fname, row.names = FALSE)
  rownames(summ_df) <- c(paste0(type, "_RMSE"), paste0(type, "_Rsquared"), paste0(type, "_MAE"))
  return(summ_df)
}

cv <- trainControl(## 7-fold CV
  method = "cv",
  number = 7)

## lm - noise
lm_control = list(cv, NULL, "lm")
c(lmm, lmm.01n, lmm.05n, lmm.1n, lmm.2n, lmm.5n, lmm1n, lmm5n, lmm10n, lmm20n) %<-% lapply(nr, run_ml, run_control = lm_control)
lmn_mods <- list(lmm, lmm.01n, lmm.05n, lmm.1n, lmm.2n, lmm.5n, lmm1n, lmm5n, lmm10n, lmm20n)
lmn_summ <- pred_ml(lmn_mods, nr, "noise_lm")
file_names <- paste0("./models/", nr_names, "_lm.RDS")
n <- length(lmn_mods)
for (i in 1:n) {
  saveRDS(lmn_mods[[i]], file_names[i])
}
rm(lmm, lmm.01n, lmm.05n, lmm.1n, lmm.2n, lmm.5n, lmm1n, lmm5n, lmm10n, lmm20n)
rm(lmn_mods)


##lm - drift
lm_control = list(cv, NULL, "lm")
c(lmm, lmm.01d, lmm.05d, lmm.1d, lmm.2d, lmm.5d, lmm1d, lmm5d, lmm10d, lmm20d) %<-% lapply(dr, run_ml, run_control = lm_control)
lmd_mods <- list(lmm, lmm.01d, lmm.05d, lmm.1d, lmm.2d, lmm.5d, lmm1d, lmm5d, lmm10d, lmm20d)
lmd_summ <- pred_ml(lmd_mods, dr, "drift_lm")
file_names <- paste0("./models/", dr_names, "_lm.RDS")
n <- length(lmd_mods)
for (i in 1:n) {
  saveRDS(lmd_mods[[i]], file_names[i])
}
rm(lmm, lmm.01d, lmm.05d, lmm.1d, lmm.2d, lmm.5d, lmm1d, lmm5d, lmm10d, lmm20d)
rm(lmd_mods)


summ <- rbind(lmn_summ, lmd_summ)

## gbm - noise
gbm_grid <- expand.grid(
  .n.trees = 1200,
  .interaction.depth = 4,
  .shrinkage = 0.01,
  .n.minobsinnode = 6)
gbm_control = list(cv, gbm_grid, "gbm")
c(gbb, gbb.01n, gbb.05n, gbb.1n, gbb.2n, gbb.5n, gbb1n, gbb5n, gbb10n, gbb20n) %<-% lapply(nr, run_ml, run_control = gbm_control)
gbn_mods <- list(gbb, gbb.01n, gbb.05n, gbb.1n, gbb.2n, gbb.5n, gbb1n, gbb5n, gbb10n, gbb20n)
gbn_summ <- pred_ml(gbn_mods, nr, "noise_gbm")
summ <- rbind(summ, gbn_summ)
file_names <- paste0("./models/",nr_names, "_gbm.RDS")
n <- length(gbn_mods)
for (i in 1:n) {
  saveRDS(gbn_mods[[i]], file_names[i])
}
rm(gbb, gbb.01n, gbb.05n, gbb.1n, gbb.2n, gbb.5n, gbb1n, gbb5n, gbb10n, gbb20n)
rm(gbn_mods)


## gbm - drift
gbm_grid <- expand.grid(
  .n.trees = 1200,
  .interaction.depth = 4,
  .shrinkage = 0.01,
  .n.minobsinnode = 6)
gbm_control = list(cv, gbm_grid, "gbm")
c(gbb, gbb.01d, gbb.05d, gbb.1d, gbb.2d, gbb.5d, gbb1d, gbb5d, gbb10d, gbb20d) %<-% lapply(dr, run_ml, run_control = gbm_control)
gbd_mods <- list(gbb, gbb.01d, gbb.05d, gbb.1d, gbb.2d, gbb.5d, gbb1d, gbb5d, gbb10d, gbb20d)
gbd_summ <- pred_ml(gbd_mods, dr, "drift_gbm")
summ <- rbind(summ, gbd_summ)
file_names <- paste0("./models/",dr_names, "_gbm.RDS")
n <- length(gbd_mods)
for (i in 1:n) {
  saveRDS(gbd_mods[[i]], file_names[i])
}
rm(gbb, gbb.01d, gbb.05d, gbb.1d, gbb.2d, gbb.5d, gbb1d, gbb5d, gbb10d, gbb20d)
rm(gbd_mods)


## ranger - noise
tgrid <- expand.grid(
  .mtry = 6,
  .splitrule = "variance",
  .min.node.size = 5
)
rn_control = list(cv, tgrid, "ranger")
c(rnmn, rnm.01n, rnm.05n, rnm.1n, rnm.2n, rnm.5n, rnm1n, rnm5n, rnm10n, rnm20n) %<-% 
                lapply(nr, run_ml, run_control = rn_control)
rnn_mods <- list(rnmn, rnm.01n, rnm.05n, rnm.1n, rnm.2n, rnm.5n, rnm1n, rnm5n, rnm10n, rnm20n)
rnn_summ <- pred_ml(rnn_mods, nr, "noise_ranger")
summ <- rbind(summ, rnn_summ)
file_names <- paste0("./models/", nr_names, "_ranger.RDS")
n <- length(rnn_mods)
for (i in 1:n) {
  saveRDS(rnn_mods[[i]], file_names[i])
}
rm(rnmn, rnm.01n, rnm.05n, rnm.1n, rnm.2n, rnm.5n, rnm1n, rnm5n, rnm10n, rnm20n)
rm(rnn_mods)


## ranger - drift
tgrid <- expand.grid(
  .mtry = 6,
  .splitrule = "variance",
  .min.node.size = 5
)
rn_control = list(cv, tgrid, "ranger")
c(rnmd, rnm.01d, rnm.05d, rnm.1d, rnm.2d, rnm.5d, rnm1d, rnm5d, rnm10d, rnm20d) %<-% 
  lapply(dr, run_ml, run_control = rn_control)
rnd_mods <- list(rnmd, rnm.01d, rnm.05d, rnm.1d, rnm.2d, rnm.5d, rnm1d, rnm5d, rnm10d, rnm20d)
rnd_summ <- pred_ml(rnd_mods, dr, "drift_ranger")
summ <- rbind(summ, rnd_summ)
file_names <- paste0("./models/", dr_names, "_ranger.RDS")
n <- length(rnd_mods)
for (i in 1:n) {
  saveRDS(rnd_mods[[i]], file_names[i])
}
rm(rnmd, rnm.01d, rnm.05d, rnm.1d, rnm.2d, rnm.5d, rnm1d, rnm5d, rnm10d, rnm20d)
rm(rnd_mods)
