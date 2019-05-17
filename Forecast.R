###DATA opsætning
{data_2019_files <- list.files("2019data", full.names = 1)

PRICES_2019 <- read.csv2(data_2019_files[2], header = TRUE)[1:110,10]
PRICES_2019 <- log(PRICES_2019)

dato19 <- seq(ISOdate(2019,1,1),by="day", length.out = 110)
helligedage19 <- c("2019-01-01 12:00:00 GMT","2019-04-18 12:00:00 GMT","2019-04-19 12:00:00 GMT","2019-04-22 12:00:00 GMT")
dummy_week19 <-  rep(c(0,0,0,0,0,1,1),16)[1:110]
helligedage19 <-  as.POSIXct(strptime(helligedage19, format = "%Y-%m-%d %H:%M:%S", "GMT"))
dato2019 <- strptime(dato19, format = "%Y-%m-%d %H:%M:%S", "GMT")
dummyhelligdage19 <- numeric(length = length(dato19))
dummyhelligdage19[match(helligedage19,dato19)] <- 1
dummyhelligweekend19 <- dummyhelligdage19+dummy_week19
}

######Forecast
{PRICES_2019 
plot.ts(PRICES_2019)


model1_deterministisk <- function(x){
  model1$coefficients[1]+
    model1$coefficients[2]*x+
    model1$coefficients[3]*x^2+
    cos(2*pi/365*x)*model1$coefficients[4]+
    sin(2*pi/365*x)*model1$coefficients[5]+
    cos(4*pi/365*x)*model1$coefficients[6]+
    sin(4*pi/365*x)*model1$coefficients[7]
}

trend2019 <- model1_deterministisk(2192:(2192+length(dummyhelligdage19)-1))+dummyhelligweekend19*model1$coefficients[8]
plot.ts(trend2019)


PRICES_2019SA <- PRICES_2019-trend2019#Sæson justeret log pris
}
##Forecasting function for ARMA(1,2)
{
forecast_function <- function(x){

start <- length(X_t)#2191
Spot_pris <- c(X_t,PRICES_2019SA)
Spot_pris <- ts(Spot_pris)
forecast_step <- c()

for (i in 1:x) {
  
    fitarma <- arima(Spot_pris[1:(start+i-1)],order = c(7,0,2),include.mean = F,fixed = c(NA,0,0,0,0,0,NA,NA,NA))

    fore <- predict(fitarma,n.ahead=1)
  
  forecast_step <- as.ts(rbind(forecast_step,c(fore$pred,fore$se)))
}

colnames(forecast_step) <- c("Predict","Std. Error")

if (i==x) {
  forecast_spotpris <- exp(forecast_step[,1]+trend2019)
  plot(exp(PRICES_2019),type = "l",col="red")
  lines(forecast_spotpris)
  lines(exp(forecast_step[,1]-forecast_step[,2]+trend2019),col="blue",lty=2)
  lines(exp(forecast_step[,1]+forecast_step[,2]+trend2019),col="blue",lty=2)
  }
return(forecast_step)
}
fore_cast100 <- forecast_function(110)



rmse(exp(fore_cast100[,1]+trend2019),exp(PRICES_2019))
mape(forecast_spotpris,PRICES_2019)
plot.ts(PRICES_2019SA)
lines(fore_cast100[,1],col="red")
lines(fore_cast100[,1]+fore_cast100[,2],col="blue")
lines(fore_cast100[,1]-fore_cast100[,2],col="blue")

rmse(fore_cast100[,1],PRICES_2019SA)
mape(fore_cast100[,1],PRICES_2019SA)

}
###ARFIMA(3,0.19,4)
{forecast_ARFIMA <- function(x){
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
#log plots
plot.ts(PRICES_2019SA)
lines(test[,1],col="red")
lines(test[,1]+test[,2],col="blue",lty=2)
lines(test[,1]-test[,2],col="blue",lty=2)

#forecast pris
plot.ts(exp(PRICES_2019SA+trend2019))
lines(exp(test[,1]+trend2019),col="red")
lines(exp(test[,1]+test[,2]+trend2019),col="blue",lty=2)
lines(exp(test[,1]-test[,2]+trend2019),col="blue",lty=2)

rmse(exp(test[,1]+trend2019),exp(PRICES_2019SA+trend2019))


rmse(test[,1],PRICES_2019SA)

time1 <- Sys.time()
test <- forecast_ARFIMA(110)
time2 <- Sys.time()
time <- time2-time1}

###ARMAX
{forecast_ARMAX <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- c(X_t,PRICES_2019SA)
  Spot_pris <- ts(Spot_pris)
  forecast_step <- c()
  x_reg <- 
for (i in 1:x) {
  
  armax1_0_2 <- TSA::arima(Spot_pris[1:start+i-1],order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg = testxreg, include.mean = F)
  fore <- predict(fitarfima,n.ahead=1)
  forecast_step <- as.ts(rbind(forecast_step,c(fore[[1]]$Forecast,fore[[1]]$exactSD)))
} 
  if (i==x) {
    colnames(forecast_step) <- c("Predict","Std. Error")
    forecast_spotpris <- exp(forecast_step[,1]+trend2019)
    plot(exp(PRICES_2019),type = "l",col="red")
    lines(forecast_spotpris)
    lines(exp(forecast_step[,1]-forecast_step[,2]+trend2019),col="blue",lty=2)
    lines(exp(forecast_step[,1]+forecast_step[,2]+trend2019),col="blue",lty=2)
  }
  
  return(forecast_step)
}
  
  
  
  
}
