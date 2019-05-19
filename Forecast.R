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
#as.data.frame(cbind(xreghydro[,3:1],xregtemp[,3:1],as.ts(WEATHER$Precipitation),xregcon[,11:1])[1:2191,] )

rolingxreg <-xvaribale
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

###ARMA(1,2) fast
{forecast_ARMAfast <- function(x){

start <- length(X_t)#2191
Spot_pris <- c(X_t,PRICES_2019SA)
Spot_pris <- ts(Spot_pris)
forecast_step <- c()

for (i in 1:x) {
  
    fitarma <- arima(Spot_pris[1:(start+i-1)],order = c(1,0,2),include.mean = F,fixed = c(0.9693,-0.2021,-0.2261))

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
forecast_arma12fast <- forecast_ARMAfast(110)
}
#ARMA(1,2) Rul
{forecast_ARMArul<- function(x){
  
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
forecast_arma12rul <- forecast_ARMArul(110)
}
###ARFIMA(3,0.19,4) fast
{forecast_ARFIMAfast <- function(x){
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
  Forecast_ARFIMA34fast <- forecast_ARFIMAfast(110)
}

###ARFIMA(3,0.19,4) rul
{forecast_ARFIMArul <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- c(X_t,PRICES_2019SA)
  forecast_step <- c()
  
  for (i in 1:x) {
    fitarfima <- arfima(Spot_pris[1:(start+i-1)],order = c(3,0,4),fixed = list(frac=0.19))
    
    fore <- predict(fitarfima,n.ahead=1)
    
    forecast_step <- as.ts(rbind(forecast_step,c(fore[[1]]$Forecast,fore[[1]]$exactSD)))
  }
  colnames(forecast_step) <- c("Predict","Std. Error")
  return(forecast_step)
}
Forecast_ARFIMA34rul <- forecast_ARFIMArul(110)
}

###ARMAX(1,2) Rul
{forecast_ARMAXrul <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- c(X_t,PRICES_2019SA)
  Spot_pris <- ts(Spot_pris)
  forecast_step <- c()
  xreghelelortet <- rbind(rolingxreg[,c(modellagarmax)],xreg_2019[,c(modellagarmax)])
  
for (i in 1:x) {
  
  armax1_0_2 <- TSA::arima(Spot_pris[1:(start+i-1)], order = c(1, 0, 2),xreg= xreghelelortet[1:(start+i-1),],include.mean = F,fixed = c(NA,NA,NA,-0.0099,-0.0047,-0.0012))
  
  fore <- predict(armax1_0_2,n.ahead=1,newxreg =t(xreghelelortet[start+i,]))
  
  forecast_step <- as.ts(rbind(forecast_step,c(fore$pred,fore$se)))
} 
  if (i==x) {
    
    plot.ts(PRICES_2019SA)
    lines(forecast_step[,1],col="red")
    lines(forecast_step[,1]+forecast_step[,2],col="blue",lty=2)
    lines(forecast_step[,1]-forecast_step[,2],col="blue",lty=2)
    
  }
  return(forecast_step)
}
forecast_armax12rul <- forecast_ARMAXrul(110)  
}
###ARMAX(1,2) Fast
{forecast_ARMAXfast <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- c(X_t,PRICES_2019SA)
  Spot_pris <- ts(Spot_pris)
  forecast_step <- c()
  xreghelelortet <- rbind(rolingxreg[,c(modellagarmax)],xreg_2019[,c(modellagarmax)])
  
  for (i in 1:x) {
    
    armax1_0_2 <- TSA::arima(Spot_pris[1:(start+i-1)], order = c(1, 0, 2),xreg= xreghelelortet[1:(start+i-1),],include.mean = F,fixed = c(0.9845,-0.2572,-0.2412,-0.0099,-0.0047,-0.0012))
    
    fore <- predict(armax1_0_2,n.ahead=1,newxreg =t(xreghelelortet[start+i,]))
    
    forecast_step <- as.ts(rbind(forecast_step,c(fore$pred,fore$se)))
  } 
  if (i==x) {
    
    plot.ts(PRICES_2019SA)
    lines(forecast_step[,1],col="red")
    lines(forecast_step[,1]+forecast_step[,2],col="blue",lty=2)
    lines(forecast_step[,1]-forecast_step[,2],col="blue",lty=2)
    
  }
  return(forecast_step)
}
  forecast_armax12fast <- forecast_ARMAXfast(110)  
}

###ARfiMAX(3,0.19,4) Fast
{forecast_ARfiMAXfast <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- ts(c(X_t,PRICES_2019SA))
  forecast_step <- c()
  xreghelelortet <- rbind(rolingxreg[,c(modellagarfimax)],xreg_2019[,c(modellagarfimax)])
  xreghelelortet[1,4] <- 2.9
  for (i in 1:x) {
    
    fitarfimax <- arfima(Spot_pris[1:(start+i-1)],order = c(3,0,4),fixed = list(frac=0.19,phi=c(0.506415,-0.550276,0.909485),theta=c(-0.0737454,-0.554481,0.442508, 0.21124),reg=c(-0.00887165,-0.00110063, 3.05357e-07, -0.00405742)),xreg= xreghelelortet[1:(start+i-1),],dmean = F,itmean = F)
    
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
  forecast_arfimaxfast<- forecast_ARfiMAXfast(110)  
}

###ARfiMAX(3,0.19,4) Rul
{forecast_ARfiMAXrul <- function(x){
  start <- length(X_t)#2191
  Spot_pris <- ts(c(X_t,PRICES_2019SA))
  forecast_step <- c()
  xreghelelortet <- rbind(rolingxreg[,c(modellagarfimax)],xreg_2019[,c(modellagarfimax)])
  xreghelelortet[1,4] <- 2.9
  for (i in 1:x) {
    
    fitarfimax <- arfima(Spot_pris[1:(start+i-1)],order = c(3,0,4),fixed = list(frac=0.19,phi=c(0.506415,-0.550276,0.909485),theta=c(-0.0737454,-0.554481,0.442508, 0.21124)),
                         xreg= xreghelelortet[1:(start+i-1),],dmean = F,itmean = F)
    
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
  forecast_arfimaxrul <- forecast_ARfiMAXrul(110)  
}


###RMSE####
 
rmse(exp(forecast_arma12fast[,1]+trend2019),exp(PRICES_2019)) 
rmse(exp(forecast_arma12rul[,1]+trend2019),exp(PRICES_2019)) 
rmse(exp(Forecast_ARFIMA34fast[,1]+trend2019),exp(PRICES_2019))  
rmse(exp(Forecast_ARFIMA34rul[,1]+trend2019),exp(PRICES_2019))  
rmse(exp(forecast_armax12fast[,1]+trend2019),exp(PRICES_2019SA+trend2019))
rmse(exp(forecast_armax12rul[,1]+trend2019),exp(PRICES_2019SA+trend2019))
rmse(exp(forecast_arfimaxfast[,1]+trend2019),exp(PRICES_2019SA+trend2019))
rmse(exp(forecast_arfimaxrul[,1]+trend2019),exp(PRICES_2019SA+trend2019))

Fastrmse <- c(rmse(exp(forecast_arma12fast[,1]+trend2019),exp(PRICES_2019)),  rmse(exp(Forecast_ARFIMA34fast[,1]+trend2019),exp(PRICES_2019)),  rmse(exp(forecast_armax12fast[,1]+trend2019),exp(PRICES_2019SA+trend2019)), rmse(exp(forecast_arfimaxfast[,1]+trend2019),exp(PRICES_2019SA+trend2019)))
rulrmse <- c(rmse(exp(forecast_arma12rul[,1]+trend2019),exp(PRICES_2019)),rmse(exp(Forecast_ARFIMA34rul[,1]+trend2019),exp(PRICES_2019)),rmse(exp(forecast_armax12rul[,1]+trend2019),exp(PRICES_2019SA+trend2019)),rmse(exp(forecast_arfimaxrul[,1]+trend2019),exp(PRICES_2019SA+trend2019)))


###MAPE####
mape(exp(forecast_arma12fast[,1]+trend2019),exp(PRICES_2019)) 
mape(exp(forecast_arma12rul[,1]+trend2019),exp(PRICES_2019)) 
mape(exp(Forecast_ARFIMA34fast[,1]+trend2019),exp(PRICES_2019))  
mape(exp(Forecast_ARFIMA34rul[,1]+trend2019),exp(PRICES_2019))  
mape(exp(forecast_armax12fast[,1]+trend2019),exp(PRICES_2019SA+trend2019))
mape(exp(forecast_armax12rul[,1]+trend2019),exp(PRICES_2019SA+trend2019))
mape(exp(forecast_arfimaxfast[,1]+trend2019),exp(PRICES_2019SA+trend2019))
mape(exp(forecast_arfimaxrul[,1]+trend2019),exp(PRICES_2019SA+trend2019))

Fastrmape <- c(mape(exp(forecast_arma12fast[,1]+trend2019),exp(PRICES_2019)),  mape(exp(Forecast_ARFIMA34fast[,1]+trend2019),exp(PRICES_2019)),  mape(exp(forecast_armax12fast[,1]+trend2019),exp(PRICES_2019SA+trend2019)), mape(exp(forecast_arfimaxfast[,1]+trend2019),exp(PRICES_2019SA+trend2019)))
rulmape <- c(mape(exp(forecast_arma12rul[,1]+trend2019),exp(PRICES_2019)),mape(exp(Forecast_ARFIMA34rul[,1]+trend2019),exp(PRICES_2019)),mape(exp(forecast_armax12rul[,1]+trend2019),exp(PRICES_2019SA+trend2019)),mape(exp(forecast_arfimaxrul[,1]+trend2019),exp(PRICES_2019SA+trend2019)))

rmse(mean(exp(PRICES_2019SA+trend2019)),exp(PRICES_2019SA+trend2019))
mape(mean(exp(PRICES_2019SA+trend2019)),exp(PRICES_2019SA+trend2019))

###PLOTS####

##GGplots 
####ARMA12 fast
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_arma12fast[,1]+trend2019)),
                                c(exp(forecast_arma12fast[,1]+forecast_arma12fast[,2]+trend2019)),
                                c(exp(forecast_arma12fast[,1]-forecast_arma12fast[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+
  xlab("Time")+ylab("Price")}
####ARMA12 rul
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_arma12rul[,1]+trend2019)),
                                c(exp(forecast_arma12rul[,1]+forecast_arma12rul[,2]+trend2019)),
                                c(exp(forecast_arma12rul[,1]-forecast_arma12rul[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+
  xlab("Time")+ylab("Price")
}
#ARFIMA34 fast
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(Forecast_ARFIMA34fast[,1]+trend2019)),
                                c(exp(Forecast_ARFIMA34fast[,1]+Forecast_ARFIMA34fast[,2]+trend2019)),
                                c(exp(Forecast_ARFIMA34fast[,1]-Forecast_ARFIMA34fast[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+
xlab("Time")+ylab("Price")
}
#ARFIMA34 rul
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(Forecast_ARFIMA34rul[,1]+trend2019)),
                                c(exp(Forecast_ARFIMA34rul[,1]+Forecast_ARFIMA34rul[,2]+trend2019)),
                                c(exp(Forecast_ARFIMA34rul[,1]-Forecast_ARFIMA34rul[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+
  xlab("Time")+ylab("Price")

}
#ARMAX rul
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_armax12rul[,1]+trend2019)),
                                c(exp(forecast_armax12rul[,1]+forecast_armax12rul[,2]+trend2019)),
                                c(exp(forecast_armax12rul[,1]-forecast_armax12rul[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+xlab("Time")+ylab("Price")
}
#ARMAX fast
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_armax12fast[,1]+trend2019)),
                                c(exp(forecast_armax12fast[,1]+forecast_armax12fast[,2]+trend2019)),
                                c(exp(forecast_armax12fast[,1]-forecast_armax12fast[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+xlab("Time")+ylab("Price")
}

#ARFIMAX fast
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_arfimaxfast[,1]+trend2019)),
                                c(exp(forecast_arfimaxfast[,1]+forecast_arfimaxfast[,2]+trend2019)),
                                c(exp(forecast_arfimaxfast[,1]-forecast_arfimaxfast[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+xlab("Time")+ylab("Price")

}
#ARFIMAX rul
{datalort <- as.data.frame(cbind(exp(PRICES_2019SA+trend2019),
                                c(exp(forecast_arfimaxrul[,1]+trend2019)),
                                c(exp(forecast_arfimaxrul[,1]+forecast_arfimaxrul[,2]+trend2019)),
                                c(exp(forecast_arfimaxrul[,1]-forecast_arfimaxrul[,2]+trend2019))      ))
colnames(datalort) <- c("Observed","Predict","SE","se2")

ggplot(datalort,aes(x=1:110,y=Observed))+
  geom_line(aes(x=1:110,y=Observed,color="black"),size=1,show.legend = T)+
  geom_line(aes(x=1:110,y=Predict,color="red"),size=1,show.legend = T)+
  theme(legend.position = "right") +
  scale_color_manual(name="",labels=c("Observed","Predicted"),values = c("black", "red"))+
  geom_ribbon(aes(ymin=se2,ymax=SE),alpha=0.1,show.legend = T)+xlab("Time")+ylab("Price")

}