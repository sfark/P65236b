###DATA opsætning
{data_2019_files <- list.files("2019data", full.names = 1)

PRICES_2019 <- read.csv2(data_2019_files[3], header = TRUE)[1:110,10]
PRICES_2019 <- log(PRICES_2019)

dato19 <- seq(ISOdate(2019,1,1),by="day", length.out = 110)
helligedage19 <- c("2019-01-01 12:00:00 GMT","2019-04-18 12:00:00 GMT","2019-04-19 12:00:00 GMT","2019-04-22 12:00:00 GMT")
dummy_week19 <-  rep(c(0,0,0,0,0,1,1),16)[1:110]
helligedage19 <-  as.POSIXct(strptime(helligedage19, format = "%Y-%m-%d %H:%M:%S", "GMT"))
dato2019 <- strptime(dato19, format = "%Y-%m-%d %H:%M:%S", "GMT")
dummyhelligdage19 <- numeric(length = length(dato19))
dummyhelligdage19[match(helligedage19,dato19)] <- 1
dummyhelligweekend19 <- dummyhelligdage19+dummy_week19

rolingxreg <-as.data.frame(cbind(xreghydro[,3:1],xregtemp[,3:1],as.ts(WEATHER$Precipitation),xregcon[,11:1])[1:2191,] )
}
##data opsætning 2019 XREG!!!!!!!!!!!!!!!!!!!!!####
####Xreg hydro2019
xreghydro_2019 <- c()
ALTHYDRO <- as.ts(c(hydrodayli,hydrodayli_2019))
for (i in 1:length(laghydro)) {
  xreghydro_2019 <- cbind(xreghydro_2019,stats::lag(ALTHYDRO,k=(laghydro[i])))
}

xreghydro_2019 <- xreghydro_2019[2192:2301,]
colnames(xreghydro_2019) <- c("lag 20","lag 19","lag 16")


###xreg consumption 2019
xregcon_2019 <- c()
for (i in 1:length(lagcon)) {
  xregcon_2019 <- cbind(xregcon_2019,stats::lag(as.ts(c(consump_daligi[,1], CONSUMPTION_2019)),k=(lagcon[i])))
}
xregcon_2019 <- xregcon_2019[2192:2301,]
colnames(xregcon_2019) <- c("lag 30","lag 23","lag 22","lag 17","lag 16","lag 11","lag 10","lag 9","lag 4","lag 2","lag 0")

###Xreg temp 2019
xregtemp_2019 <- c()
for (i in 1:length(lagtemp)) {
  xregtemp_2019 <- cbind(xregtemp_2019,stats::lag(as.ts(c(WEATHER[,1], WEATHER_2019[,1])),k=(lagtemp[i])))
}
xregtemp_2019 <- xregtemp_2019[2192:2301,]
colnames(xregtemp_2019) <- c("lag 10","lag 1","lag 0")

### Xreg participation 2019

xreg_2019 <- cbind(xregcon_2019,xreghydro_2019,xregtemp_2019,as.ts(WEATHER_2019[,2]))[1:110,]

  
######Forecast####
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
  
    fitarma <- arima(Spot_pris[1:(start+i-1)],order = c(1,0,2),include.mean = F)

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
forecast_arma12 <- forecast_function(110)
}
###ARFIMA(3,0.19,4)
{forecast_ARFIMA <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- c(X_t,PRICES_2019SA)
  forecast_step <- c()
  
  for (i in 1:x) {
    fitarfima <- arfima(Spot_pris[1:(start+i-1)],order = c(3,0,4),fixed = list(frac=0.19,phi=c(0.506415,-0.550276,0.909485),theta=c(-0.0737454,-0.554481,0.442508, 0.21124)))
    
    fore <- predict(fitarfima,n.ahead=1)
    
    forecast_step <- as.ts(rbind(forecast_step,c(fore[[1]]$Forecast,fore[[1]]$exactSD)))
  }
  colnames(forecast_step) <- c("Predict","Std. Error")
  return(forecast_step)
}
  time1 <- Sys.time()
  Forecast_ARFIMA34 <- forecast_ARFIMA(110)
  time2 <- Sys.time()
  time <- time2-time1}
###ARMAX(1,2) 
{forecast_ARMAX <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- c(X_t,PRICES_2019SA)
  Spot_pris <- ts(Spot_pris)
  forecast_step <- c()
  xreghelelortet <- rbind(rolingxreg[,c(modellagarma)],xreg_2019[,c(modellagarma)])
for (i in 1:x) {
  
  armax1_0_2 <- TSA::arima(Spot_pris[1:(start+i-1)], order = c(1, 0, 2),xreg= xreghelelortet[1:(start+i-1),],include.mean = F)
  
  fore <- predict(armax1_0_2,n.ahead=1,newxreg =t(xreghelelortet[start+i,]),interval="prediction")
  
  forecast_step <- as.ts(rbind(forecast_step,c(fore$pred,fore$se)))
} 
  if (i==x) {
    
    plot.ts(PRICES_2019SA)
    lines(forecast_armax12[,1],col="red")
    lines(forecast_armax12[,1]+forecast_armax12[,2],col="blue",lty=2)
    lines(forecast_armax12[,1]-forecast_armax12[,2],col="blue",lty=2)
    
  }
  return(forecast_step)
}
forecast_armax12 <- forecast_ARMAX(110)  
}
###ARfiMAX(3,0.19,4) 
{forecast_ARfiMAX <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- ts(c(X_t,PRICES_2019SA))
  forecast_step <- c()
  xreghelelortet <- rbind(rolingxreg[,c(modellagarmax)],xreg_2019[,c(modellagarmax)])
  xreghelelortet[1,3] <- 2.9
  for (i in 1:x) {
    
    fitarfimax <- arfima(Spot_pris[1:(start+i-1)],order = c(3,0,4),fixed = list(frac=0.19,phi=c(0.506415,-0.550276,0.909485),theta=c(-0.0737454,-0.554481,0.442508, 0.21124)),xreg= xreghelelortet[1:(start+i-1),])
    
    nuxreg <- t(xreghelelortet[start+i,])
    
    fore <- predict(fitarfimax,n.ahead=1,newxreg =nuxreg)
    
    forecast_step <- as.ts(rbind(forecast_step,c(fore[[1]]$Forecast,fore[[1]]$exactSD)))
  } 
  if (i==x) {
    plot.ts(PRICES_2019SA)
    lines(forecast_step [,1],col="red")
    lines(forecast_step [,1]+forecast_step [,2],col="blue",lty=2)
    lines(forecast_step [,1]-forecast_step [,2],col="blue",lty=2)
    
  }
  return(forecast_step)
}
  forecast_arfimax <- forecast_ARfiMAX(110)  
}
###RMSE####
rmse(forecast_arma12[,1],PRICES_2019SA)                               #logARMA(1,2)
rmse(exp(forecast_arma12[,1]+trend2019),exp(PRICES_2019))             #ARMA(1,2)
rmse(forecast_ARFIMA34[,1],PRICES_2019SA)                             #logARFIMA(3,0.19,4)
rmse(exp(Forecast_ARFIMA34[,1]+trend2019),exp(PRICES_2019))           #ARFIMA(3,0.19,4)
rmse(forecast_armax12[,1],PRICES_2019SA)                              #logARMAX(1,2)
rmse(exp(forecast_armax12[,1]+trend2019),exp(PRICES_2019SA+trend2019))#ARMAX(1,2)
rmse(forecast_arfimax[,1],PRICES_2019SA)                              #logARFIMAX(3,0.19,4)
rmse(exp(forecast_arfimax[,1]+trend2019),exp(PRICES_2019SA+trend2019))#ARFIMAX(3,0.19,4)

###MAPE####
mape(forecast_arma12[,1],PRICES_2019SA)                               #logARMA(1,2)
mape(exp(forecast_arma12[,1]+trend2019),exp(PRICES_2019))             #ARMA(1,2)
mape( Forecast_ARFIMA34[,1],PRICES_2019SA)                             #logARFIMA(3,0.19,4)
mape(exp(Forecast_ARFIMA34[,1]+trend2019),exp(PRICES_2019))           #ARFIMA(3,0.19,4)
mape(forecast_armax12[,1],PRICES_2019SA)                              #logARMAX(1,2)
mape(exp(forecast_armax12[,1]+trend2019),exp(PRICES_2019SA+trend2019))#ARMAX(1,2)
mape(forecast_arfimax[,1],PRICES_2019SA)                              #logARFIMAX(3,0.19,4)
mape(exp(forecast_arfimax[,1]+trend2019),exp(PRICES_2019SA+trend2019))#ARFIMAX(3,0.19,4)


###PLOTS####
PRICES_2019SA <- as.data.frame(PRICES_2019SA)

plot.ts(PRICES_2019SA)
lines(forecast_arma12[,1],col="red")
lines(forecast_arma12[,1]+forecast_arma12[,2],col="blue",lty=2)
lines(forecast_arma12[,1]-forecast_arma12[,2],col="blue",lty=2)




##GGplots 
#ARMA12
datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_arma12[,1]+trend2019)),
                                c(exp(forecast_arma12[,1]+forecast_arma12[,2]+trend2019)),
                                c(exp(forecast_arma12[,1]-forecast_arma12[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+
  xlab("Time")+ylab("Price")
  
#ARFIMA34
datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(Forecast_ARFIMA34[,1]+trend2019)),
                                c(exp(Forecast_ARFIMA34[,1]+Forecast_ARFIMA34[,2]+trend2019)),
                                c(exp(Forecast_ARFIMA34[,1]-Forecast_ARFIMA34[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+
xlab("Time")+ylab("Price")
#ARMAX
datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_armax12[,1]+trend2019)),
                                c(exp(forecast_armax12[,1]+forecast_armax12[,2]+trend2019)),
                                c(exp(forecast_armax12[,1]-forecast_armax12[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+xlab("Time")+ylab("Price")

#ARFIMAX
datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_arfimax[,1]+trend2019)),
                                c(exp(forecast_arfimax[,1]+forecast_arfimax[,2]+trend2019)),
                                c(exp(forecast_arfimax[,1]-forecast_arfimax[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+xlab("Time")+ylab("Price")



#log plots
plot.ts(PRICES_2019SA)
lines(test[,1],col="red")
lines(test[,1]+test[,2],col="blue",lty=2)
lines(test[,1]-test[,2],col="blue",lty=2)

#forecast pris
plot.ts(exp(PRICES_2019))
lines(exp(test[,1]+trend2019),col="red")
lines(exp(test[,1]+test[,2]+trend2019),col="blue",lty=2)
lines(exp(test[,1]-test[,2]+trend2019),col="blue",lty=2)



rmse(exp(test[,1]+trend2019),exp(PRICES_2019SA+trend2019))


rmse(test[,1],PRICES_2019SA)



fitarma <- arima(X_t,order = c(1,0,2),include.mean = F)

fore <- predict(fitarma,n.ahead=110)

plot.ts(PRICES_2019SA)
lines(ts(fore$pred),col="red")
