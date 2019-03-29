#Install
# install.packages("tseries")
# install.packages("lubridate")
# install.packages("fracdiff")
# install.packages("arfima")
# install.packages("tidyr")
# install.packages("forecast")
# install.packages("ggplot2")
# install.packages("astsa")
# install.packages("rlist")
# install.packages("stringr")
# install.packages("data.table")
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("")
# install.packages("")

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
library(tidyr)
library(arfima)
library(fracdiff)
library(tseries)
#data
xtable(PRICES[1:10,])

#SETWD
#setwd("/Users/Rasmus/Desktop/Uni/6 Semester/Data Mining/P65236b/DATA")
setwd("~/P65236b/DATA")

PRICES_list <- list.files("PRICES", full.names = 1)
HYDRO_list <- list.files("HYDRO", full.names = 1)
CONSUMPTION_list <- list.files("CONSUMPTION", full.names = 1)
WEATHER_list <- list.files("WEATHER", full.names = 1)

PRICES <- read.csv2(PRICES_list[1], header = TRUE)[,c(10:15)]
HYDRO <- read.csv2(HYDRO_list[1], header = TRUE)[,c(2:3)]
CONSUMPTION <- read.csv2(CONSUMPTION_list[1], header = TRUE)[,c(2:7)]
WEATHER <- read.csv2(WEATHER_list[1], header = TRUE,skip = 1)

dato <- seq(c(ISOdate(2013,1,1)), by = "day", length.out = 2191)
#fjerne data for 2019
WEATHER <- WEATHER[1:2191,]
WEATHER$Date <- dato
dato2 <- seq(ISOdate(2013,1,1),by="week", length.out = 310)




for (i in 2:6) {
  PRICES <- rbind(PRICES,read.csv2(PRICES_list[i], header = TRUE)[,c(10:15)])
  HYDRO <- rbind(HYDRO,read.csv2(HYDRO_list[i], header = TRUE)[,c(2:3)])
  CONSUMPTION <- rbind(CONSUMPTION,read.csv2(CONSUMPTION_list[i], header = TRUE)[,c(2:7)])
}

pris <- cbind(dato,PRICES)
hydro2 <- cbind(dato2,HYDRO)


ggplot(fortify(PRICES),aes(x= dato, y= Oslo)) + geom_line() + xlab("Date")+ylab("El-spot price")
{
ggplot(fortify(PRICES),aes(x= dato, y= Oslo)) + 
  geom_line() + geom_line(aes(x=dato, y=Bergen),col="red", alpha = 0.5)+
  geom_line(aes(x=dato, y=Troms.),col="blue", alpha = 0.5)+
  geom_line(aes(x=dato, y=Kr.sand),col="green", alpha = 0.5)+
  xlab("Date")+ylab("EL-spot Price")
  
ggplot(fortify(PRICES),aes(x=Oslo,y=Tr.heim))+ 
  geom_point()+
  xlab("EL-Spot Prince, Oslo")+
  ylab("EL-Spot Prince, Tr.Heim") +
  geom_bin2d(bins=250)+
  theme(legend.position = c(0.8, 0.2))+xlim(0, 750) + ylim(0, 750)

ggplot(fortify(PRICES),aes(x=Kr.sand,y=Tr.heim))+ 
  geom_point()+
  xlab("EL-Spot Prince, Kr.sand")+
  ylab("EL-Spot Prince, Tr.Heim") +
  geom_bin2d(bins=250)+
  theme(legend.position = c(0.8, 0.2))+xlim(0, 750) + ylim(0, 750)


ggplot(fortify(PRICES),aes(x=Oslo,y=Bergen))+ 
  geom_point()+
  xlab("EL-Spot Prince, Oslo")+
  ylab("EL-Spot Prince, Bergen") +
  geom_bin2d(bins=250)+
  theme(legend.position = c(0.8, 0.2))+xlim(0, 750) + ylim(0, 750)

ggplot(fortify(PRICES),aes(x=Oslo,y=Troms.))+ 
  geom_point()+
  xlab("EL-Spot Prince, Oslo")+
  ylab("EL-Spot Prince, Troms.") +
  geom_bin2d(bins=250)+
  theme(legend.position = c(0.8, 0.2))+xlim(0, 750) + ylim(0, 750)

ggplot(fortify(HYDRO),aes(x=dato2,y=NO))+geom_line()+
  xlab("Weeks")+ylab(" GWh")

ggplot(fortify(WEATHER),aes(x=Date,y=Mean.temperature))+geom_line()+
  xlab("Daily")+ylab("Mean Temperature")


plot(WEATHER[,1],1:length(WEATHER[,1]))
}
max(PRICES[,1])

CONSUMPTION2 <- CONSUMPTION/1000
Consumption3 <- cbind(dato,CONSUMPTION2)
#consumption in GWh
ggplot(fortify(Consumption3),aes(x=dato,y=NO1))+
  geom_line(linetype=1)+
  xlab("Daily")+
  ylab("Consumption in GWh")




###ACF for pris, hydro, og comsumption
acf(log(PRICES[,1]),lag.max = 500)
acf(log(HYDRO[,1]),lag.max=100)
acf(log(CONSUMPTION[,1]),lag.max = 355)




###ARFIMA model
pris <- ts(PRICES[,1])

y <- log(pris)-mean(log(pris))
acf2(y,max.lag = 355)
pris.fd <- arfima(y)
summary(pris.fd)

d <- summary(pris.fd)$coef[[1]][1]; d
se.d <- summary(pris.fd)$coef[[1]][1,2];se.d
residual <- resid(pris.fd)
plot.ts(residual[[1]])
acf(residual[[1]])

{
  ###Long memory spectra for Priser
  series <- log(PRICES[,1])
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
}

### Den rigtie data behandling!!!!!!!!!
acf(log(PRICES$Oslo))
acf(diff(log(PRICES$Oslo)))

##ACF Log price
{
  bacf <- acf(log(PRICES$Oslo),lag.max = 100 ,plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))
  
  q <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))
  q
}
### ACF diff log price
{
  diffacf <- acf(diff(log(PRICES$Oslo)),lag.max = 100 ,plot = FALSE)
  diffacfdf <- with(diffacf, data.frame(lag, acf))
  
  diff_q <- ggplot(data = diffacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))
  diff_q
}
### acf for mean temperature
{
  acf(WEATHER$Mean.temperature)
  weatheracf <- acf(WEATHER$Mean.temperature,lag.max = 700 ,plot = FALSE)
  weatheracfdf <- with(weatheracf, data.frame(lag, acf))
  
  weather_acf <- ggplot(data = weatheracfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))
  weather_acf
}
###acf diff mean temp
{acf(diff(WEATHER$Mean.temperature))
  weatherdiffacf <- acf(diff(WEATHER$Mean.temperature) ,plot = FALSE)
  weatherdiffacfdf <- with(weatherdiffacf, data.frame(lag, acf))
  
  weather_diff_acf <- ggplot(data = weatherdiffacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))
  weather_diff_acf
}
### decomposed mean weather
{
decom_temperatur <- decompose(ts(WEATHER$Mean.temperature,frequency = 365))

autoplot(decom_temperatur,main="Decomposition of Mean Temperature",range.bars = 0)+theme(plot.title = element_text(hjust = 0.5))
ggdecompose(ts(WEATHER$Mean.temperature,frequency = 365))

p <- ggplot(decom_temperatur$random, aes(sample = decom_temperatur$random))
p + stat_qq() + stat_qq_line()
}

###ARFIMA
y <-log(PRICES$Oslo)
plot.ts(y,ylab="Log of El-Spot Prices")

# Vi prøver så nogle forskellige modeller og ser hvilken der er bedst vha. fx AIC
{
AICMAtrix <-matrix(0,10,10) 
for (i in 1:10) {
  for (j in 1:10) {
   AICMAtrix[i,j] <-  AIC(fracdiff(y, nar = i, nma = j))
  }
}
#hvilket element der har den mindste AIC
which(AICMAtrix==min(AICMAtrix),arr.ind = TRUE)

fit78 <- fracdiff(y, nar = 7, nma = 8)
summary(fit78)
diffx <-diffseries(y,fit78$d) 
plot.ts(diffx)
acf(diffx)
pacf(diffx)
}
par(mfrow=c(1,1))
###Vi fjerner trend og sæson før vi fracdiffer
{plot(decompose(ts(y,frequency=365)))
decom_pris <- na.omit(ts(decompose(ts(y,frequency=365))$random))
qqnorm(decom_pris)
AICMAtrixdeco <-matrix(0,10,10) 
for (i in 1:10) {
  for (j in 1:10) {
    AICMAtrixdeco[i,j] <-  AIC(fracdiff(decom_pris, nar = i, nma = j))
  }
}
#hvilket element der har den mindste AIC
which(AICMAtrixdeco==min(AICMAtrixdeco),arr.ind = TRUE)

fit1010 <- fracdiff(decom_pris, nar = 10, nma = 10)
summary(fit1010)          
diffy <- diffseries(decom_pris,fit1010$d)
acf(diffy)
qqnorm(diffy)
plot.ts(diffy)

qqdiffy <- ggplot(diffy, aes(sample = diffy))
qqdiffy + stat_qq() + stat_qq_line()
}

### ruppert eksempel
{ 
y <-log(PRICES$Oslo)
d_hat <- fracdiff(y)$d#vi finder et estimat på d
diffY <- diffseries(y,d_hat) #vi fraktionel differ tids serien med vores estimerede d_hat
auto.arima(diffY) # Vi bruger auto arima til at finde AR og MA delen på den diffede serie
plot.ts(diffy)
acf(diffY)
acf(HYDRO)}

#opdelling til hverdage, weekender og helligdage.
head(pris)
### Vi f?r en vektor p? med tal fra 1-7 hvor mandag=1 s?ndag=7
dagligpris <- as.data.frame(mutate(select(pris,dato,Oslo),weekday = wday(dato)))

##Vi laver 2 matricer en for hverdags priser og en for weekend priser
hverdagspris <- filter(dagligpris,weekday==2|weekday==3|weekday==4|weekday==5|weekday==6)
weekendpris <- filter(dagligpris,weekday==1|weekday==7)
#gennemsnits prisen for weekend og hverdag
mean(weekendpris[,2])
mean(hverdagspris[,2])

#forskellen p? hverdags pris ?rligt
meanhverdag <- c(
  mean(hverdagspris[1:261,2]),
  mean(hverdagspris[262:522,2]),
  mean(hverdagspris[523:783,2]),
  mean(hverdagspris[784:1044,2]),
  mean(hverdagspris[1045:1304,2]),
  mean(hverdagspris[1305:1565,2])
)
MEANWEEKEND <- c(
  mean(weekendpris[1:104,2]),
  mean(weekendpris[105:208,2]),
  mean(weekendpris[209:312,2]),
  mean(weekendpris[313:417,2]),
  mean(weekendpris[418:522,2]),
  mean(weekendpris[523:626,2])
)
Meandata <- as.data.frame(cbind( c(2013:2018),meanhverdag,MEANWEEKEND))

##?rlig plot gennemsnits pris weekend mod hverdag
{
ggplot(Meandata,aes(x=Meandata[,1],y=meanhverdag))+
  geom_line(size=1)+
  geom_line(aes(x=Meandata[,1],y=MEANWEEKEND),col="red",size=1)+
  ylab("EL-Spot Price")+xlab("Year")+
  geom_point()+geom_point(aes(x=Meandata[,1],y=MEANWEEKEND),col="red")

##m?nedlig gennemsnits pris weekend mod hverdag
meanmonthhverdag <- c(summarize(group_by(hverdagspris,month=floor_date(dato,"month")),Oslo=mean(Oslo)))
meanmonthweekend <-c(summarize(group_by(weekendpris,month=floor_date(dato,"month")),Oslo=mean(Oslo)))

meanmonthdata <- as.data.frame(cbind(c(1:length(meanmonthhverdag$Oslo)),meanmonthhverdag$Oslo,meanmonthweekend$Oslo))

###m?nedligt plot af gennemsnits pris, hverdag mod weekend
ggplot(meanmonthdata,aes(x=meanmonthdata[,1],y=V2))+
  geom_line(size=1)+
  geom_line(aes(x=meanmonthdata[,1],y=V3),col="red",size=1)+
  ylab("EL-Spot Price")+xlab("MOnths")+
  geom_point()+geom_point(aes(x=meanmonthdata[,1],y=V3),col="red")
}

###Decompose frac diff log pris med qq plot og residualer
temp <- cbind(WEATHER[,2],WEATHER[,3])
logpris <- cbind(pris[,1],log(pris[,2]))
adf.test(logpris[,2])
adf.test(temp[,2])

arfima(logpris[,2])
difflogpris <- diffseries(logpris[,2],d=0.499391)
arima(difflogpris)

adf.test(difflogpris)

model <- lm(difflogpris~temp[,2])
res <- resid(model)
qqnorm(res)
acf(difflogpris)
decdifflogpris <- decompose(ts(difflogpris,frequency = 356))
plot.ts(decdifflogpris$random)
qqnorm(decdifflogpris$random)

plot.ts(resid(model))


### Hellllllllllllllllllllllllllllllllllllllllligdag
helligdage <- c("2013-01-01 12:00:00 GMT","2013-03-28 12:00:00 GMT","2013-03-29 12:00:00 GMT","2013-04-01 12:00:00 GMT","2013-05-01 12:00:00 GMT","2013-05-09 12:00:00 GMT","2013-05-17 12:00:00 GMT","2013-05-20 12:00:00 GMT","2013-12-25 12:00:00 GMT","2013-12-26 12:00:00 GMT","2014-01-01 12:00:00 GMT","2014-04-17 12:00:00 GMT","2014-04-18 12:00:00 GMT","2014-04-21 12:00:00 GMT","2014-05-01 12:00:00 GMT","2014-05-29 12:00:00 GMT","2014-06-09 12:00:00 GMT","2014-12-25 12:00:00 GMT","2014-12-26 12:00:00 GMT","2015-01-01 12:00:00 GMT","2015-04-02 12:00:00 GMT","2015-04-03 12:00:00 GMT","2015-04-06 12:00:00 GMT","2015-05-01 12:00:00 GMT","2015-05-14 12:00:00 GMT","2015-05-25 12:00:00 GMT","2015-12-25 12:00:00 GMT","2015-12-26 12:00:00 GMT","2016-01-01 12:00:00 GMT","2016-03-24 12:00:00 GMT","2016-03-25 12:00:00 GMT","2016-03-28 12:00:00 GMT","2016-05-05 12:00:00 GMT","2016-05-16 12:00:00 GMT","2016-05-17 12:00:00 GMT","2016-12-25 12:00:00 GMT","2016-12-26 12:00:00 GMT","2017-04-13 12:00:00 GMT","2017-04-14 12:00:00 GMT","2017-04-17 12:00:00 GMT","2017-05-01 12:00:00 GMT","2017-05-17 12:00:00 GMT","2017-05-25 12:00:00 GMT","2017-12-25 12:00:00 GMT","2017-12-26 12:00:00 GMT","2018-01-01 12:00:00 GMT","2018-03-29 12:00:00 GMT","2018-03-30 12:00:00 GMT","2018-04-02 12:00:00 GMT","2018-05-01 12:00:00 GMT","2018-05-10 12:00:00 GMT","2018-05-17 12:00:00 GMT","2018-05-21 12:00:00 GMT","2018-12-25 12:00:00 GMT","2018-12-26 12:00:00 GMT")

helligedage2019 <- c("2019-01-01 12:00:00 GMT",
                     "2019-04-18 12:00:00 GMT",
                     "2019-04-19 12:00:00 GMT",
                     "2019-02-22 12:00:00 GMT",
                     "2019-05-01 12:00:00 GMT",
                     "2019-05-17 12:00:00 GMT",
                     "2019-05-30 12:00:00 GMT",
                     "2019-06-10 12:00:00 GMT",
                     "2019-12-25 12:00:00 GMT",
                     "2019-12-26 12:00:00 GMT")
  
helligedage2019 <-  strptime(helligedage2019, format = "%Y-%m-%d %H:%M:%S", "GMT")
helligdage <-  strptime(helligdage, format = "%Y-%m-%d %H:%M:%S", "GMT")
dato3 <- strptime(dato, format = "%Y-%m-%d %H:%M:%S", "GMT")

helligdage[1] %in% dato3[1:3]
helligdage[1]==dato[1]
  
dato3 <- as.data.frame(dato)
match(helligdage,dato3)

dummyhelligdage <- numeric(length = length(dato3))
dummyhelligdage[match(helligdage,dato3)] <- 1

pris <- cbind(dato,PRICES,dummyhelligdage)

