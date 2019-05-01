##d estimerings parameterens algoritmen

d_est <-seq(0.001,0.499,length.out = 500) 
aic_vec <- c()

for (i in 1:length(d_est)) {
 diff_X_t <-  diffseries(X_t,d_est[i])
 b <- auto.arima(diff_X_t,stepwise = F,approximation = F,max.order = 10,max.p = 10,max.q = 10)
 aic_vec[i] <- AIC(arfima(X_t,order=c(b$arma[1],0,b$arma[2]),fixed = list(frac=d_est[i]))
}


for (i in 1:2) {
  diff_X_t <-  diffseries(X_t,d_est[i])
  b <- auto.arima(diff_X_t,stepwise = F,approximation = F,max.order = 10,max.p = 10,max.q = 10)
  aic_vec[i] <- AIC(arfima(X_t,order=c(b$arma[1],0,b$arma[2]),fixed = list(frac=d_est[i])))
}
aic_vec
