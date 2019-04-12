#packagesss
library(lubridate)
library(readr)
library(stringr)
library(data.table)
library(tidyverse)
library(rlist)
library(astsa)
library(ggplot2)
library(forecast)
library(tseries)
library(fracdiff)
library(gtools)

#data
PRICES_list <- list.files("PRICES", full.names = 1)
HYDRO_list <- list.files("HYDRO", full.names = 1)
CONSUMPTION_list <- list.files("CONSUMPTION", full.names = 1)

WEATHER_list <- list.files("WEATHER", full.names = 1)

Tal <- list.files("Karsten",full.names = 1)



PRICES <- read.csv2(PRICES_list[1], header = TRUE)[,c(10:15)]
HYDRO <- read.csv2(HYDRO_list[1], header = TRUE)[,c(2:3)]
CONSUMPTION <- read.csv2(CONSUMPTION_list[1], header = TRUE)[,c(2:7)]

WEATHER <- read.csv2(WEATHER_list[1], header = TRUE,skip = 1)

dato <- seq(c(ISOdate(2013,1,1)), by = "day", length.out = 2191)
#fjerne data for 2019
WEATHER <- WEATHER[1:2191,]
WEATHER$Date <- dato

Karsten <- read.csv2(Tal[1], header = T)[,c(3:5)]


for (i in 2:6) {
  PRICES <- rbind(PRICES,read.csv2(PRICES_list[i], header = TRUE)[,c(10:15)])
  HYDRO <- rbind(HYDRO,read.csv2(HYDRO_list[i], header = TRUE)[,c(2:3)])
  CONSUMPTION <- rbind(CONSUMPTION,read.csv2(CONSUMPTION_list[i], header = TRUE)[,c(2:7)])
  #Kasten <- rbind(Karsten,read.csv2(Tal[i], header = T)[,c(3:5)])
}

Karsten[,1] <- Karsten[,1]/2



ggplot(fortify(PRICES),aes(x= dato, y= Oslo)) + geom_line() + xlab("Date")+ylab("El-spot price Oslo")

###GGplot af elpris
dato <- seq(c(ISOdate(2013,1,1)), by = "day", length.out = 2191)
ggplot(fortify(Karsten),aes(x=1:52 , y= Consumption.2017,colour="Consumption")) +
  
  geom_line() + xlab("Date")+ylab("TWh")+
  
  geom_line(aes(x=1:52,y=Production.2017,colour="Production"))+
  
  geom_area(aes(x=1:52,y=Precipitation.2017..right.axis.,colour="In Flow(right axis)"),alpha=0.2,fill="blue")+
  
  scale_colour_manual("", 
                      breaks = c("Consumption", "Production", "In Flow(right axis)"),
                      values = c("red", "blue", "black")) +
  xlab(" ")+

  scale_y_continuous(name="TWh", sec.axis=sec_axis(~./0.5, name="TWh"))+
  labs(x="Weeks",title = "Inflow, Production and Consumption of energy in Norway, 2017")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(fortify(HYDRO),aes(x= dato, y= Oslo)) + geom_line() + geom_line(aes(x=dato, y=Bergen),col="red", alpha = 0.5)


x <- 1:2191

par(mfrow=c(1,1))

ts.plot(HYDRO[,1],xlab="")
###ACF for pris, hydro, og comsumption
acf(log(PRICES[,1]),lag.max = 100)
acf(log(HYDRO[,1]),lag.max=100)
acf(log(CONSUMPTION[,1]),lag.max = 355)

###ARFIMA model
pris <- ts(PRICES[,1])
library(arfima)
#
#y <- log(pris)-mean(log(pris))
#Detrending
y <- resid(lm(log(pris)~c(1:length(pris)))) 

acf2(y,max.lag = 355)

pris.fd <- arfima(y)

summary(pris.fd)

d <- summary(pris.fd)$coef[[1]][1]; d

se.d <- summary(pris.fd)$coef;se.d

residual <- resid(pris.fd)
plot(residual,type="points")
acf(residual,lag.max = 400)
qqnorm(residual)

## de season
seson <- stl(ts(y,frequency = 365),"periodic")
plot(seson)
h <- seasadj(seson)
plot(h)
acf(h,lag.max = 1000)
plot(decompose(h))
arfima(h)

sarima(h,d=4.583013e-05,p=2,q=4)

###Long memory spectra for Priser
{series <- log(PRICES[,1])
d0 <- 0.1
n.per <- nextn(length(series))
m <- (n.per)/2 -1
per <- Mod(fft(series-mean(series))[-1] )^2
per <- per/n.per
g <- 4*(sin(pi*((1:m)/n.per))^2)
g

whit.like <- function(d){
  g.d <- g^d
  sig2 <- (sum(g.d*per[1:m])/m)
  log.like <- m*log(sig2)-d*sum(log(g)) +m
  return(log.like)
}

(est <- optim(d0,whit.like,gr=NULL,method = "L-BFGS-B",hessian=TRUE,
                    lower=-100,upper=100,control = list(trace=1,REPORT=1)))
cat("d.hat =", est$par, "se(dhat) = ",1/sqrt(est$hessian),"\n")  
g.dhat = g^est$par
sig2 = sum(g.dhat*per[1:m])/m
cat("sig2hat =",sig2,"\n")  



#data for NO1(OSLO)

HYDRO_NO <- rep(HYDRO$NO,each=7) #dagli
data_NO1 <- cbind(PRICES$Oslo,HYDRO_NO,CONSUMPTION$NO1)
colnames(data_NO1) <- c("Price","Hydro reserve","Consumption")

#dummy weekend
dummy_week <-  rep(c(0,0,0,0,0,1,1),310)
#dummy sommer vinter

#dummy regn?

auto.arima(log(PRICES$Oslo),xreg=data_NO1[,c(2:3)])

acf(log(PRICES$Oslo))
acf(diff(log(PRICES$Oslo)))

auto.arima(diff(log(PRICES$Oslo)))
decom <- decompose(ts(log(PRICES$Oslo),frequency=365))
autoplot(decom)
ggplot(data = decom ,aes(x= dato, y= seasonal)) 



#weather data

head(WEATHER)
vejr_data <- cbind(WEATHER$Mean.temperature,WEATHER$Precipitation,WEATHER$Snow.depth)
#ændre fra comma til punktum
WEATHER$Precipitation <- as.numeric(sub(",", ".", sub(".", "", WEATHER$Precipitation, fixed=TRUE), fixed=TRUE))
#sætter NA til 0
vejr_data[,2][is.na(vejr_data[,2])] <- 0
#variabler

variable <- cbind(data_NO1[,2:3],vejr_data)
colnames(variable) <- c("Hydro reserve","Consumption","Mean.temperature","Precipitation","Snow.depth")


acf(WEATHER$Mean.temperature)
acf(diff(WEATHER$Mean.temperature,356))

decom_temperatur <- decompose(ts(WEATHER$Mean.temperature,frequency = 365))
decom_temperatur$random




}
###decompose
{pris_2 <- log(ts(PRICES, frequency = 355))
decompo <- decompose(pris_2[,1])
plot(decompo)
}
###De-trending af times serie
trModel <- resid(lm(ts.sa~c(1:length(ts.sa))))
par(mfrow=c(1,2))
plot(resid(trModel), type="l")
plot(pris_2[,1])

###stationær?
adf.test(pris_2[,1])
kpss.test(pris_2[,1])

###de-seasonlize 
library(forecast)
ts.stl <- stl(pris_2[,1],"periodic")
ts.sa <- seasadj(ts.stl)
par(mfrow=c(3,1))
plot(pris_2[,1], type="l")
plot(ts.sa,type="l")
seasonplot(ts.sa,12,col=rainbow(12))

### fracdifff
#detrending og de season
logpris <- log(PRICES[,1])
tid <- 1:length(logpris)
detrendpris <- resid(lm(logpris~tid))

detresea <- decompo$random[-c(1:178)]
detresea <- detresea[!is.na(detresea)]

acf(detresea,lag.max = 100)
pacf(detresea)

x <- c(0,1,2,3,4,5)
per <- permutations(n=6,r=2,v=x,repeats.allowed=T)
armanr <- c()

y <- detresea

for (i in 1:36) {
  ARMAmatrix <- fracdiff(y,nar=per[i,1], nma = per[i,2])
  armanr <- c(armanr,AIC( ARMAmatrix ))
}
min(armanr)


arfima(trModel)

ts.plot(arfima(trModel)$residuals)


res <- arfima(logpris)$residuals

acf(detrendpris)

qqnorm(arfima(trModel)$residuals)


AIC(fit20 <- fracdiff(y, nar = 2, nma = 0))
AIC(fit12 <- fracdiff(y, nar = 1, nma = 2))
AIC(fit11 <- fracdiff(y, nar = 1, nma = 1))
AIC(fit10 <- fracdiff(y, nar = 1, nma = 0))
AIC(fit02 <- fracdiff(y, nar = 0, nma = 2))
AIC(fit01 <- fracdiff(y, nar = 0, nma = 1))
AIC(fit00 <- fracdiff(y, nar = 0, nma = 0))

fracdiff.var(y,fit12,h=0.000000000000000001)
fit12$h


y.fd <- diffseries(y, fit12$d)
fit11arima <- arima(y.fd, order=c(1,0,2))
sarima(y.fd,1,0,2)

acf2(residuals(fit11arima))
fdSperio(y)

memory.long <- fracdiff.sim(1500, d = 0.3)
spm <- fdSperio(memory.long$series)
str(spm, digits=6)

