forecast_ARFIMA <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- c(X_t,PRICES_2019SA)
  forecast_step <- c()
  
  for (i in 1:x) {
    fitarfima <- arfima(Spot_pris[1:(start+i-1)],order = c(3,0,4),fixed = list(frac=0.19))
    
    fore <- predict(fitarfima,n.ahead=1)
    
    forecast_step <- as.ts(rbind(forecast_step,c(fore[[1]]$Forecast,fore[[1]]$exactSD)))
  }
  
  colnames(forecast_step) <- c("Predict","Std. Error")
  
  # if (i==x) {
  # forecast_step[,1] <- rbind(frakdiff(forecast_step[,1],-0.19))
  #}
  
  return(forecast_step)
}

time1 <- Sys.time()
test <- forecast_ARFIMA(110)
time2 <- Sys.time()
time <- time2-time1



plot.ts(PRICES_2019SA)
lines(test[,1],col="red")
rmse(test[,1],PRICES_2019SA)



fitarfima <- arfima(Spot_pris[1:(start+2-1)],order = c(3,0,4),fixed = list(frac=0.19))

fore <- predict(fitarfima,n.ahead=1)
fore$predint
fore[[1]]$exactSD

forecast_step <- as.ts(rbind(forecast_step,c(fore$pred,fore$se)))
