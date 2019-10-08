library(dplyr)
library(ggplot2)
library(keras)
install_keras()

## read in the input data files
c(sel_multi, pdp_multi, bws_multi, gpp_multi, lcm_multi) %<-% lapply(stations, function(stn) {
  read.csv(paste0("inputs/", stn_name[[stn]], "_multi.csv"), stringsAsFactors = FALSE)
})



in_dfs <- list(sel = sel_multi, pdp = pdp_multi, bws = bws_multi, gpp= gpp_multi, lcm = lcm_multi)
in_shape <- list(6, 9, 13, 14)

ff <- data.frame(site = c("sel", "pdp", "bws", "gpp", "lcm"), 
                 shp6 = c(NA, NA, NA, NA, NA),
                 shp9 = c(NA, NA, NA, NA, NA),
                 shp13 = c(NA, NA, NA, NA, NA),
                 shp14 = c(NA, NA, NA, NA, NA))
set.seed(1973)
ns <- length(in_dfs)
for (s in 1:ns) {
  df <- in_dfs[[s]]
  df <- df[sample(nrow(df)),]
  dfi <- df %>% select(dtime, neph, reff, pm25_0, pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                       pm25_6, hour, wday, month, temp, pr, ws, wd, wds, wdc) %>% 
                filter(complete.cases(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                                      pm25_6, hour, wday, month, temp, pr, ws, wd, wds, wdc))
  dft_out <- dfi %>% filter(year(dtime) < 2015) 
  dfv_out <- dfi %>% filter(year(dtime) == 2015)
  
  nvals <- ncol(dfi)
  sc.fac <- data.frame(avg = sapply(dfi[, 5:nvals], function(x) mean(x, na.rm = TRUE)), 
                        sd = sapply(dfi[, 5:nvals], function(x) sd(x, na.rm = TRUE)))
  scdf <- sapply(5:nvals, function(n) { (dfi[, n] - sc.fac$avg[n - 4])/sc.fac$sd[n - 4] })
  scdf <- cbind(dfi[, c("dtime", "neph", "reff", "pm25_0")], scdf)
  colnames(scdf) <- colnames(dfi)
  scdf <- as.data.frame(scdf)


  for (shp in in_shape ) {
    if (shp == 6) { 
       dft <- scdf %>% filter(year(dtime) < 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6)
       dfv <- scdf %>% filter(year(dtime) == 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, pm25_6)
 
    }
    if (shp == 9) {
      dft <- scdf %>% filter(year(dtime) < 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                                                         pm25_6, hour, wday, month)
      dfv <- scdf %>% filter(year(dtime) == 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                                                           pm25_6, hour, wday, month)
    }
    if (shp == 13) {
      dft <- scdf %>% filter(year(dtime) < 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                                                         pm25_6, hour, wday, month, temp, pr, ws, wd)
      dfv <- scdf %>% filter(year(dtime) == 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                                                           pm25_6, hour, wday, month, temp, pr, ws, wd)
    } 
    if (shp == 14) {
      dft <- scdf %>% filter(year(dtime) < 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                                                            pm25_6, hour, wday, month, temp, pr, ws, wds, wdc)
      dfv <- scdf %>% filter(year(dtime) == 2015) %>% select(pm25_1, pm25_2, pm25_3, pm25_4, pm25_5, 
                                                             pm25_6, hour, wday, month, temp, pr, ws, wds, wdc)
    } 
    tr <- as.matrix(dft)
    val <- as.matrix(dfv)
    dimnames(tr) <- NULL
    dimnames(val) <- NULL
    
    tr_lab <- scdf %>% filter(year(dtime) < 2015) %>% select(pm25_0)
    val_lab <- scdf %>% filter(year(dtime) == 2015) %>% select(pm25_0)
    tr_lab <- as.matrix(tr_lab)
    val_lab <- as.matrix(val_lab)
    dimnames(tr_lab) <- NULL
    dimnames(val_lab) <- NULL
    
    if (exists("mlp")) rm(mlp)
    mlp <- keras_model_sequential()
    mlp %>% layer_dense(units = 48, activation = 'linear', input_shape = c(shp)) %>%
                  layer_dropout(rate = 0.4) %>%
                  layer_dense(units = 32, activation = 'elu') %>%
                  layer_dropout(rate = 0.4) %>%
                  layer_dense(units = 32, activation = 'elu') %>%
                  layer_dropout(rate = 0.3) %>%
      #            layer_dense(units = 16, activation = 'linear') %>%
      #            layer_dropout(rate = 0.3) %>%
                  layer_dense(units = 1) 
    
    
    mlp %>% compile( loss = 'mse', optimizer = optimizer_nadam(), metrics = 'mae')
    hist <- mlp %>% fit(tr, tr_lab, epochs = 30, batch_size = 200, validation_split = 0.4)
    pred <- mlp %>% predict(tr)
    dft_out <- cbind(dft_out, mlp_pred = pred)
    print("***")
    print("***")
    print("***")
    print(summary(lm(data = dft_out, mlp_pred ~ neph)))
    suff <- ncol(dft_out)
    colnames(dft_out)[ncol(dft_out)] <- paste0("mlp", as.character(shp))
    
    dfv_out <- cbind(dfv_out, mlp_pred = predict(mlp, val))
    print(paste0(names(in_dfs)[[s]], "...................shape: ", as.character(shp)))
    res <- summary(lm(data = dfv_out, mlp_pred ~ pm25_0))
    r2 <- round(res$adj.r.squared, 3)
    print(paste0("Adj. R2 : ", as.character(r2)))
    colnames(dfv_out)[ncol(dfv_out)] <- paste0("mlp", as.character(shp))
    print("***")
    print("***")
    print("***")
    if (shp == 6) { ff[s,2] <- r2 }
    if (shp == 9) { ff[s,3] <- r2 }
    if (shp == 14) { ff[s,4] <- r2 }
    if (shp == 14) { ff[s,5] <- r2 }
  }
  write.csv(dft_out, paste0("models/", names(in_dfs)[[s]], "_tr_mlp.csv" ), row.names = FALSE)
  write.csv(dfv_out, paste0("models/", names(in_dfs)[[s]], "_val_mlp.csv" ), row.names = FALSE)
  print(ff)
}













set.seed(23)
if (exists("mlp")) rm(mlp)
mlp <- keras_model_sequential()
mlp %>% layer_dense(units = 48, activation = 'elu', input_shape = c(shp)) %>%
              layer_dropout(rate = 0.4) %>%
              layer_dense(units = 32, activation = 'linear') %>%
              layer_dropout(rate = 0.4) %>%
              layer_dense(units = 32, activation = 'elu') %>%
              layer_dropout(rate = 0.4) %>%
  #            layer_dense(units = 16, activation = 'linear') %>%
  #            layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1) 
mlp %>% compile( loss = 'mse', optimizer = optimizer_nadam(), metrics = 'mae')
hist <- mlp %>% fit(tr, tr_lab, epochs = 20, batch_size = 100, validation_split = 0.2)
pred <- mlp %>% predict(tr)
dft_out <- cbind(dft_out, mlp_pred = pred)
summary(mlp)
print(paste0(names(in_dfs)[[s]], "...................shape: ", as.character(shp)))
print(summary(lm(data = dft_out, mlp_pred ~ neph)))
suff <- ncol(dft_out)
colnames(dft_out)[ncol(dft_out)] <- paste0("mlp", as.character(shp), "_", as.character(suff))
dfv_out <- cbind(dfv_out, mlp_pred = predict(mlp, val))
dfv_out$diff13 <- dfv_out$mlp_pred - dfv_out$neph

res <- summary(lm(data = dfv_out, mlp_pred ~ neph))
print(res)
colnames(dfv_out)[which(colnames(dfv_out) == "mlp_pred")] <- paste0("mlp", as.character(shp),"_", as.character(suff))

plot(data = dfv_out, diff13 ~ neph)
range(dfv_out$diff13, na.rm = TRUE)

dfv_out <- dfv_out %>% select(-diff13)
print("************************************************************")




pdx_tr <- pdx_multi[,13:18]
pdx_lab <- pdx_multi[, "pm25_0"]
pdx_tr <- as.matrix(pdx_tr)
dimnames(pdx_tr) <- NULL
dimnames(pdx_lab) <- NULL
#dimnames(pdx_val) <- NULL


set.seed(126)
rm(mlp1_pdx)
mlp1_pdx <- keras_model_sequential()
mlp1_pdx %>% layer_dense(units = 32, activation = 'linear', input_shape = c(6)) %>%
#            layer_dense(units = 32, activation = 'sigmoid') %>%
#            layer_dense(units = 16, activation = 'sigmoid') %>%
            layer_dense(units = 1) 


mlp1_pdx %>% compile( loss = 'mse', optimizer = optimizer_nadam(), metrics = 'mae')

hist1 <- mlp1_pdx %>% fit(pdx_tr, pdx_lab, epochs = 20, batch_size = 100, validation_split = 0.2)

pred <- mlp1_pdx %>% predict(pdx_tr)
res <- lm(pred ~ pdx_lab)

summary(res)
pred_val <- mlp1_pdx %>% predict(pdx_val[, 1:6])
res_val <- lm(pred_val ~ pdx16[, 3])
summary(res_val)

plot(hist1$metrics$loss, main = 'Model Loss', xlab = 'epoch', ylab = 'MSE', col = "blue", , type = 1)
lines(hist1$metrics$val_loss, col = "green")






pdx_tr <- pdx_multi[,c(4:6,13:18)]
pdx_lab <- pdx_multi[, "pm25_0"]
pdx_tr <- as.matrix(pdx_tr)
dimnames(pdx_tr) <- NULL
dimnames(pdx_lab) <- NULL
dimnames(pdx_val) <- NULL

set.seed(126)
rm(mlp2_pdx)
mlp2_pdx <- keras_model_sequential()
mlp2_pdx %>% layer_dense(units = 32, activation = 'linear', input_shape = c(9)) %>%
  #            layer_dense(units = 32, activation = 'sigmoid') %>%
  #            layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1) 


mlp2_pdx %>% compile( loss = 'mse', optimizer = optimizer_nadam(), metrics = 'mae')

hist2 <- mlp2_pdx %>% fit(pdx_tr, pdx_lab, epochs = 20, batch_size = 100, validation_split = 0.2)

pred <- mlp2_pdx %>% predict(pdx_tr)
res <- lm(pred ~ pdx_lab)
summary(res)




#pdx_tr <- pdx_multi[,c(4:10,13:18)]
pdx_tr <- pdx_multi[complete.cases(pdx_multi[,c(4:10, 13:18)]), c(4:10, 13:18)]
pdx_lab <- pdx_multi[complete.cases(pdx_multi[,c(4:10, 13:18)]),  12]
pdx_tr <- as.matrix(pdx_tr)
dimnames(pdx_tr) <- NULL
dimnames(pdx_lab) <- NULL
dimnames(pdx_val) <- NULL

set.seed(39)
rm(mlp3_pdx)
mlp3_pdx <- keras_model_sequential()
mlp3_pdx %>% layer_dense(units = 32, activation = 'linear', input_shape = c(13)) %>%
  #            layer_dense(units = 32, activation = 'sigmoid') %>%
  #            layer_dense(units = 16, activation = 'sigmoid') %>%
  layer_dense(units = 1) 


mlp3_pdx %>% compile( loss = 'mse', optimizer = optimizer_nadam(), metrics = 'mae')

hist3 <- mlp3_pdx %>% fit(pdx_tr, pdx_lab, epochs = 50, batch_size = 100, validation_split = 0.2)

pred <- mlp3_pdx %>% predict(pdx_tr)
res <- lm(pred ~ pdx_lab)
summary(res)
