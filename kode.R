# #Install
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
# install.packages("LSTS")
# install.packages('gtools')
# install.packages("dplyr")
#install.packages("TSA")
#install.packages("FitAR")
#install.packages("glmnet")
#install.packages("stringr")
#install.packages("Metrics")
#install.packages("fpp")
#load library
library(fpp)
library(Metrics)
library(gtools)
library(stringr)
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
library(LSTS)
library(dplyr)
library(FitAR)
library(glmnet)
library(TSA)

xtable(PRICES[1:10,])

#SETWD
#setwd("/Users/Rasmus/Desktop/Uni/6 Semester/Data Mining/P65236b/DATA")
setwd("~/P65236b/DATA")
##DATA
{
PRICES_list <- list.files("PRICES", full.names = 1)
HYDRO_list <- list.files("HYDRO", full.names = 1)
CONSUMPTION_list <- list.files("CONSUMPTION", full.names = 1)
WEATHER_list <- list.files("WEATHER", full.names = 1)

PRICES <- read.csv2(PRICES_list[1], header = TRUE)[,c(10:15)]
HYDRO <- read.csv2(HYDRO_list[1], header = TRUE)[,c(2:3)]
CONSUMPTION <- read.csv2(CONSUMPTION_list[1], header = TRUE)[,c(2:7)]
WEATHER <- read.csv2(WEATHER_list[1], header = TRUE,skip = 1)[1:2191,c(3,6)]

WEATHER$Precipitation <- as.numeric(gsub(",", ".", WEATHER$Precipitation,ignore.case = "."))
for (i in 1:length(WEATHER$Precipitation)) {
  if(is.na(WEATHER$Precipitation[i])==TRUE){
    WEATHER$Precipitation[i] <- 0
  }else{
    
  }
}

dato <- seq(c(ISOdate(2013,1,1)), by = "day", length.out = 2191)
#fjerne data for 2019
WEATHER <- WEATHER[1:2191,]
WEATHER$Date <- dato
dato2 <- seq(ISOdate(2013,1,1),by="week", length.out = 310)

##### frakdiff
{
  frakdiff <- function(x, d){
    iT <- length(x)
    np2 <- nextn(2*iT - 1, 2)
    k <- 1:(iT-1)
    b <- c(1, cumprod((k - d - 1)/k))
    dx <- fft(fft(c(b, rep(0, np2 - iT))) * fft(c(x, rep(0, np2 - iT))), inverse = T) / np2;
    return(Re(dx[1:iT]))
  }
}


for (i in 2:6) {
  PRICES <- rbind(PRICES,read.csv2(PRICES_list[i], header = TRUE)[,c(10:15)])
  HYDRO <- rbind(HYDRO,read.csv2(HYDRO_list[i], header = TRUE)[,c(2:3)])
  CONSUMPTION <- rbind(CONSUMPTION,read.csv2(CONSUMPTION_list[i], header = TRUE)[,c(2:7)])
}
hydrolang <- rep(HYDRO$NO,each=7)
pris <- cbind(dato,PRICES)
hydro2 <- cbind(dato2,HYDRO)

}
ggplot(fortify(PRICES),aes(x= dato, y= Oslo)) + geom_line() + xlab("Date")+ylab("El-spot price")



CONSUMPTION2 <- CONSUMPTION/1000
Consumption3 <- cbind(dato,CONSUMPTION2)
#consumption in GWh
ggplot(fortify(Consumption3),aes(x=dato,y=NO1))+
  geom_line(linetype=1)+
  xlab("Daily")+
  ylab("Consumption in GWh")


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
  acf(log(PRICES$Oslo))
  acf(diff(log(PRICES$Oslo)))
  
  #auto.arima(diff(log(PRICES$Oslo)))
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
diffx <-frakdiff(y,fit78$d) 
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
diffy <- frakdiff(decom_pris,fit1010$d)
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
diffY <- frakdiff(y,d_hat) #vi fraktionel differ tids serien med vores estimerede d_hat
#auto.arima(diffY) # Vi bruger auto arima til at finde AR og MA delen på den diffede serie
plot.ts(diffy)
acf(diffY)
acf(HYDRO)}

#opdelling til hverdage, weekender og helligdage.
head(pris)

### Vi f?r en vektor p? med tal fra 1-7 hvor mandag=1 s?ndag=7
dagligpris <- as.data.frame(mutate(dplyr::select(pris,dato,Oslo),weekday = wday(dato)))

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
  geom_line(size=0.5)+
  geom_line(aes(x=meanmonthdata[,1],y=V3),col="red",size=0.5)+
  ylab("EL-Spot Price")+xlab("Months")+
  geom_point(aes(x=meanmonthdata[,1],y=V2,color="black"),size=0.9)+
  geom_point(aes(x=meanmonthdata[,1],y=V3,color="red"),size=0.9)+
  theme(legend.position = "right") +
  scale_color_manual(name="Day",labels=c("Weekday","Holyday"),values = c("black", "red"))
 

###Decompose frac diff log pris med qq plot og residualer
{
temp <- cbind(WEATHER[,2],WEATHER[,3])
logpris <- cbind(pris[,1],log(pris[,2]))
adf.test(logpris[,2])
adf.test(temp[,2])

arfima(logpris[,2])
difflogpris <- frakdiff(logpris[,2],d=0.499391)
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
  }

### Hellllllllllllllllllllllllllllllllllllllllligdag

{
helligdage <- c("2013-01-01 12:00:00 GMT","2013-03-28 12:00:00 GMT","2013-03-29 12:00:00 GMT","2013-04-01 12:00:00 GMT","2013-05-01 12:00:00 GMT","2013-05-09 12:00:00 GMT","2013-05-17 12:00:00 GMT","2013-05-20 12:00:00 GMT","2013-12-25 12:00:00 GMT","2013-12-26 12:00:00 GMT","2014-01-01 12:00:00 GMT","2014-04-17 12:00:00 GMT","2014-04-18 12:00:00 GMT","2014-04-21 12:00:00 GMT","2014-05-01 12:00:00 GMT","2014-05-29 12:00:00 GMT","2014-06-09 12:00:00 GMT","2014-12-25 12:00:00 GMT","2014-12-26 12:00:00 GMT","2015-01-01 12:00:00 GMT","2015-04-02 12:00:00 GMT","2015-04-03 12:00:00 GMT","2015-04-06 12:00:00 GMT","2015-05-01 12:00:00 GMT","2015-05-14 12:00:00 GMT","2015-05-25 12:00:00 GMT","2015-12-25 12:00:00 GMT","2015-12-26 12:00:00 GMT","2016-01-01 12:00:00 GMT","2016-03-24 12:00:00 GMT","2016-03-25 12:00:00 GMT","2016-03-28 12:00:00 GMT","2016-05-05 12:00:00 GMT","2016-05-16 12:00:00 GMT","2016-05-17 12:00:00 GMT","2016-12-25 12:00:00 GMT","2016-12-26 12:00:00 GMT","2017-04-13 12:00:00 GMT","2017-04-14 12:00:00 GMT","2017-04-17 12:00:00 GMT","2017-05-01 12:00:00 GMT","2017-05-17 12:00:00 GMT","2017-05-25 12:00:00 GMT","2017-12-25 12:00:00 GMT","2017-12-26 12:00:00 GMT","2018-01-01 12:00:00 GMT","2018-03-29 12:00:00 GMT","2018-03-30 12:00:00 GMT","2018-04-02 12:00:00 GMT","2018-05-01 12:00:00 GMT","2018-05-10 12:00:00 GMT","2018-05-17 12:00:00 GMT","2018-05-21 12:00:00 GMT","2018-12-25 12:00:00 GMT","2018-12-26 12:00:00 GMT")

helligdage = strptime(helligdage, format = "%Y-%m-%d %H:%M:%S", "GMT")


helligedage2019 <- c("2019-01-01 12:00:00 GMT",
                     "2019-04-18 12:00:00 GMT",
                     "2019-04-19 12:00:00 GMT",
                     "2019-04-22 12:00:00 GMT",
                     "2019-05-01 12:00:00 GMT",
                     "2019-05-17 12:00:00 GMT",
                     "2019-05-30 12:00:00 GMT",
                     "2019-06-10 12:00:00 GMT",
                     "2019-12-25 12:00:00 GMT",
                     "2019-12-26 12:00:00 GMT")
  
helligedage2019 <-  strptime(helligedage2019, format = "%Y-%m-%d %H:%M:%S", "GMT")
helligdage <-  strptime(helligdage, format = "%Y-%m-%d %H:%M:%S", "GMT")

dato3 <- strptime(dato, format = "%Y-%m-%d %H:%M:%S", "GMT")

match(helligdage,dato3)

dummyhelligdage <- numeric(length = length(dato3))
dummyhelligdage[match(helligdage,dato3)] <- 1

weekend <- strptime(weekendpris[,1],format = "%Y-%m-%d %H:%M:%S", "GMT")

dummyweekend <- numeric(length = length(dato3))


dummyweekend[match(weekend,dato3)] <- 1

pris <- cbind(dato,PRICES,dummyhelligdage)
}


###ACF temperatur
par(mf)
Acf(temp[,2],lag.max = 100)
pacf(temp[,2],lag.max = 100)
acf(diff(temp[,2]))
pacf(diff(temp[,2]))

###sæson cleaning
dummyhelligweekend <- dummyhelligdage+dummyweekend

for (i in 1:length(dummyhelligweekend)) {
  if (dummyhelligweekend[i]==2){
    dummyhelligweekend[i]=1
  }
}
## dummy spiks
model1 <- glm(dagligpris[,2]~time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend)
summary(model1)

#vores nye tidsserie###################
X_t <- ts(model1$residuals)
##################################
{
  
  dummyspikes <- c()
  for (i in 1:length(X_t)) {
    if (X_t[i]>250) {
      dummyspikes[i] <- 1
    }else if (X_t[i]< -250) {
      dummyspikes[i] <- -1
    }
    else{
      dummyspikes[i] <- 0
    }  
  }
}
########### tilføjer dummy spikes til GLM
model1 <- glm(dagligpris[,2]~time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend+dummyspikes)
summary(model1)

#vores nye tidsserie
X_t <- ts(model1$residuals)

##Diff af x_t
acf(X_t,lag.max = 70)
diffacf <- acf(diff(X_t, plot = FALSE,lag.max = 50))
acfdf <- with(diffacf, data.frame(lag, acf))
ggplot(data = acfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+xlim(0,50)+geom_hline(aes(yintercept=0.04),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.04),col="blue",linetype=2)

autoplot(acf(diff(X_t)),main="")




#De de-trended og de-sæson residualer
ggplot(X_t,aes(y=X_t,x=1:length(X_t)))+
geom_line()+xlab("Time")+ylab("Residuals")

#ACF'en for residualerne
bacf <- acf(X_t, plot = FALSE,lag.max = 100)
bacfdf <- with(bacf, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+xlim(0,100)

autoplot(Acf(X_t,plot = F,lag.max = 100),main="")

#ADF-Test
adf.test(model1$residuals,k = 0)
adf.test(model1$residuals)



## estimering af D-parameteren
d_hat <- fracdiff(X_t)$d#vi finder et estimat på d
d_hat
diffY <- ts(frakdiff(X_t,d_hat)) #vi fraktionel differ tids serien med vores estimerede d_hat
plot.ts(diffY)

##auto.arima(diffY,stepwise = F,approximation = F)
##auto.arima(diffY,stepwise = F,approximation = F,max.order = 10,max.p = 10,max.q = 10)

# Vi bruger auto arima til at finde AR og MA delen på den diffede serie
##auto.arima(X_t)


###Vi finder selv vores p og q værdier ved hjælp af AIC
n <- c(0,1,2,3,4,5)
#pick 2 balls from the urn with replacement
#get all permutations
per <- permutations(n=6,r=2,v=n,repeats.allowed=T)
armanr <- c()

for (i in 1:25) {
  ARMAmatrix <- arima(diffY,order=c(per[i,1],0,per[i,2]))
  armanr <- c(armanr,AIC( ARMAmatrix ))
}

armanr
cbind(per,armanr)
per[20,]
which.min(armanr)

#noget acf værk for residualer
acf(diffY)
res.diffy <- resid(arfima(X_t))
par(mfrow=c(2,1))
acf(res.diffy$Mode1)

#ACF plots og alm plot
diffY_ACF <- acf(diffY ,plot = FALSE)
bacfdf <- with(diffY_ACF, data.frame(lag, acf))
ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.04),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.04),col="blue",linetype=2)

autoplot(diffY,ylab="Sample")
kpss.test(X_t)

###PACF af DiffY
diffY_PACF <- pacf(diffY ,plot = FALSE)
pacfdf <- with(diffY_PACF, data.frame(lag, acf))
ggplot(data = pacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("PACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
pacf(diffY,plot = T)
autoplot(pacf(diffY))

##ljung-box test
res.arima <- resid(arima(diffY,order = c(2,0,3)))
Box.Ljung.Test(res.arima,lag=10)
plot(res.arima)
lines(diffY,col="red")
ts.diag(diffY)


###alternativ d estimation med fdgph()
dd_hat <- fdGPH(X_t)$d
#vi fraktionel differ tids serien med vores estimerede d_hat
diffY2<- frakdiff(X_t,dd_hat) 

#vi fitter en arima funktion til den frak diffede med alternativ d
##auto.arima(diffY2,stepwise = F,approximation = F,max.order = 10,max.p = 10,max.q = 10)
res.arima2 <- resid(arima(diffY2,order = c(3,0,3)))
qqnorm(res.arima2)
acf(diffY2,lag.max = 100)
fdGPH(diffY2)

plot(diffY2)
lines(res.arima2,col="red")

### rekusiv metode for AIC et eller andet
n <- c(0,1,2,3,4,5)
#pick 2 balls from the urn with replacement
#get all permutations
per <- permutations(n=6,r=2,v=n,repeats.allowed=T)
armanr2 <- c()

for (i in 1:25) {
  ARMAmatrix2 <- arima(diffY2,order=c(per[i,1],0,per[i,2]))
  armanr2 <- c(armanr2,AIC( ARMAmatrix2 ))
}

armanr2
cbind(per,armanr2)
which.min(armanr2)

res.arima1 <- residuals(arima(diffY,order = c(7,0,0)))
res.arima2 <- residuals(arima(diffY2,order = c(6,0,2)))
plot(res.arima1)
plot(res.arima2)
acf(res.arima1)
acf(res.arima2)
pacf(res.arima1)
pacf(res.arima2)

par(mfrow=c(2,1))
sarima(diffY2,3,0,5)
sarima(diffY,3,0,5)

###ACF for d=0.199
diffY2_ACF <- acf(diffY2 ,plot = FALSE)
acfdf2 <- with(diffY2_ACF, data.frame(lag, acf))
ggplot(data = acfdf2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

###for alm x_T
#auto.arima(X_t,stepwise = F,approximation = F,max.order = 10,max.p = 10,max.q = 10)
res.arima3 <- residuals(arima(X_t,order = c(4,0,5)))
acf(res.arima3)
plot(res.arima3)

##aic for x_t shit
armanr3 <- c()

for (i in 1:25) {
  ARMAmatrix3 <- arima(X_t,order=c(per[i,1],0,per[i,2]))
  armanr3 <- c(armanr3,AIC( ARMAmatrix3 ))
}

armanr3
cbind(per,armanr)
per[20,]
which.min(armanr)

res.arima1 <- residuals(arima(diffY,order = c(4,0,6)))
res.arima2 <- residuals(arima(diffY2,order = c(3,0,3),optim.control = list(maxit = 1000)))
res.arima3 <- residuals(arima(X_t,order = c(8,0,0)))
res17 <- residuals(arima(DIFFYRIGTIG,order=c(8,0,9)))
acf(res.arima2)

### AIC
AIC(arima(X_t,order = c(4,0,5)))
AIC(arfima(X_t,order = c(4,0,6),fixed = list(frac=0.4992368)))
AIC(arfima(X_t,order = c(3,0,3),fixed = list(frac=0.2943173)))
AIC(arima(X_t,order = c(3,1,1)))

arfima(X_t,order = c(6,0.499081,2))

# det differentierede tidsserie
#auto.arima(X_t,d=1,max.p = 10,max.q = 10,max.order = 10,stepwise = F,approximation = F)



###ACF of the residauls for the arima(4,0,6)
res1_ACF <- acf(res.arima1 ,plot = FALSE)
res1df2 <- with(res1_ACF, data.frame(lag, acf))
ggplot(data = res1df2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

###ACF of the residauls for the arima(3,0,3)
res2_ACF <- acf(res.arima2 ,plot = FALSE)
res2df2 <- with(res2_ACF, data.frame(lag, acf))
ggplot(data = res2df2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

###ACF of the residuals for the original times series X_T with a ARMA(4,5)
res3_ACF <- acf(res.arima3 ,plot = FALSE)
res3df2 <- with(res3_ACF, data.frame(lag, acf))
ggplot(data = res3df2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

acf(res.arima3)


### AIC og d estimations algoritme
# d_est <-seq(0.001,0.499,length.out = 500) 
# aic_vec <- c()
# for (i in 1:length(d_est)) {
#   diff_X_t <-  frakdiff(X_t,d_est[i])
#   b <- #auto.arima(diff_X_t,stepwise = F,approximation = F,max.order = 10,max.p = 10,max.q = 10)
#   aic_vec[i] <- AIC(arfima(X_t,order=c(b$arma[1],0,b$arma[2]),fixed = list(frac=d_est[i])))
# }

##### fjerner spiks
{
  plot(X_t)
dummyspikes <- c()
for (i in 1:length(X_t)) {
  if (X_t[i]>250) {
    dummyspikes[i] <- 1
  }else if (X_t[i]< -250) {
    dummyspikes[i] <- -1
  }
  else{
    dummyspikes[i] <- 0
  }  
}

modelspikes <- glm(dagligpris[,2]~time(dagligpris[,1])+
               I(time(dagligpris[,1])^2)+
               cos((2*pi/365)*I(time(dagligpris[,1])))+
               sin((2*pi/365)*I(time(dagligpris[,1])))+
               cos((4*pi/365)*I(time(dagligpris[,1])))+
               sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend+dummyspikes)
plot.ts(residuals(modelspikes))

summary(modelspikes)

hist(residuals(modelspikes),breaks = 100)

adf.test(residuals(modelspikes))
acf(residuals(modelspikes),lag.max = 100)

}




### corrolation mellem parameterne 
{
  par(mfrow=c(3,1))
  ccf(PRICES$Oslo,WEATHER$Mean.temperature,lag.max = 360,main="Price Vs Mean Temperature")
  ccf(PRICES$Oslo,WEATHER$Precipitation,lag.max = 360)
  ccf(WEATHER$Mean.temperature,WEATHER$Precipitation,lag.max = 360)
  ccf(PRICES$Oslo,data_NO1[,2],lag.max = 360)#HYDRO
  ccf(PRICES$Oslo,data_NO1[,3],lag.max = 360,main="Price Vs Consumption")#CONSUMPTION
  ccf(WEATHER$Mean.temperature,data_NO1[,3],lag.max = 360,main="Mean Temperature Vs Consumption")
  corelationsd <- cbind(WEATHER$Mean.temperature,WEATHER$Precipitation,data_NO1[,2],data_NO1[,3])
  cor(corelationsd)
}

###Model uden spikes
model3 <- lm(dagligpris[,2]~time(dagligpris[,1])+
             I(time(dagligpris[,1])^2)+
             cos((2*pi/365)*I(time(dagligpris[,1])))+
             sin((2*pi/365)*I(time(dagligpris[,1])))+
             cos((4*pi/365)*I(time(dagligpris[,1])))+
             sin((4*pi/365)*I(time(dagligpris[,1])))+
             dummyhelligweekend)

ggplot(ts(model3$residuals),aes(y=ts(model3$residuals),x=1:length(ts(model3$residuals))))+
  geom_line()+xlab("Time")+ylab("Residuals")


### Estimering af d-parameter
AICmatrix_d <- matrix(0,10,10) 
for (i in 1:10) {
  for (j in 1:10) {
    AICmatrix_d[i,j] <-  AIC(fracdiff(X_t, nar = i, nma = j))
  }
}
which(AICmatrix_d == min(AICmatrix_d), arr.ind = TRUE)
min(AICmatrix_d)
ddd_hat <- fracdiff(X_t,nar = 8,nma = 9)$d
DIFFYRIGTIG <- ts(frakdiff(X_t,ddd_hat))


res17 <- residuals(arima(DIFFYRIGTIG,order=c(8,0,9)))
autoplot(DIFFYRIGTIG,ylab="Sample")

#acf with d=0.2305671
di_ACF <- acf(DIFFYRIGTIG ,plot = FALSE,lag.max = 50)
di_acfdf2 <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acfdf2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

res17_ACF <- acf(res17 ,plot = FALSE)
res17df2 <- with(res17_ACF, data.frame(lag, acf))
ggplot(data = res17df2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

#### ARMAX

data_NO1 <- cbind(data_NO1,WEATHER$Mean.temperature,WEATHER$Precipitation)

#auto.arima(X_t,xreg=data_NO1[,2:5],max.p = 10,max.q = 10,max.order = 20,stepwise=FALSE,approximation = FALSE,D=0)

armaxaic <- matrix(nrow = 10,ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    armaxaic[i,j] <- AIC(TSA::arimax(X_t,order=c(i , 0 , j),xreg = data_NO1[,2:5]))
  }
}


which.min(armaxaic)



TSA::arimax(X_t,order=c(10 , 0 , 8),xreg = data_NO1[,2:5],transfer =list(c(0,0),c(1,0)))
#####CCF plot
{
  
  par(mfrow=c(4,1))
  ccf(X_t,data_NO1[,2],lag.max = 365)#hydro
  ccf(X_t,data_NO1[,3],lag.max = 365)#consumption
  ccf(X_t,data_NO1[,4],lag.max = 365)#mean Temp
  ccf(X_t,data_NO1[,5],lag.max = 365)#rain
  ccf(data_NO1[,3],data_NO1[,4],lag.max = 365)
  
  
  Hydro_level <- data_NO1[,2]
  Consumption <- data_NO1[,3]
  Mean.Temp <- data_NO1[,4]
  Paticipation <- data_NO1[,5]
  
  p1 <- ggCcf(X_t,Hydro_level, lag.max = 365, type = "correlation",plot = TRUE)
  p2 <- ggCcf(X_t,Consumption, lag.max = 365, type = "correlation",plot = TRUE)
  p3 <- ggCcf(X_t,Mean.Temp, lag.max = 365, type = "correlation",plot = TRUE)
  p4 <- ggCcf(X_t,Paticipation, lag.max = 365, type = "correlation",plot = TRUE)
  
  multiplot(p1, p4, p2, p3, cols=2) 
}



######### armax lm test
{
  #ARDL model building
  #Setup lagged variables - up to 10 lag, i.e., all exo. var. must have the same length 1339, as we also lag the first
  end = length(X_t)
  price_train_l10 = X_t[10:(end-1)]
  price_train_l9 = X_t[9:(end-2)]
  price_train_l8 = X_t[8:(end-3)]
  price_train_l7 = X_t[7:(end-4)]
  price_train_l6 = X_t[6:(end-5)]
  price_train_l5 = X_t[5:(end-6)]
  price_train_l4 = X_t[4:(end-7)]
  price_train_l3 = X_t[3:(end-8)]
  price_train_l2 = X_t[2:(end-9)]
  #price_train_l1 = X_t[1:(end-(lagmaks))]
  
  
  HYDRO_train_l10 = data_NO1[,2][10:(end-1)]
  HYDRO_train_l9 = data_NO1[,2][9:(end-2)]
  HYDRO_train_l8 = data_NO1[,2][8:(end-3)]
  HYDRO_train_l7 = data_NO1[,2][7:(end-4)]
  HYDRO_train_l6 = data_NO1[,2][6:(end-5)]
  HYDRO_train_l5 = data_NO1[,2][5:(end-6)]
  HYDRO_train_l4 = data_NO1[,2][4:(end-7)]
  HYDRO_train_l3 = data_NO1[,2][3:(end-8)]
  HYDRO_train_l2 = data_NO1[,2][2:(end-9)]
  #HYDRO_train_l1 = data_NO1[,2][1:(end-(lagmaks))]
  
  
  
  hydro_train <- cbind(HYDRO_train_l10,  HYDRO_train_l9,  HYDRO_train_l8,  HYDRO_train_l7,  HYDRO_train_l6,  HYDRO_train_l5,  HYDRO_train_l4,  HYDRO_train_l3,  HYDRO_train_l2)
  
  consumption_train_l10 = data_NO1[,3][10:(end-1)]
  consumption_train_l9 = data_NO1[,3][9:(end-2)]
  consumption_train_l8 = data_NO1[,3][8:(end-3)]
  consumption_train_l7 = data_NO1[,3][7:(end-4)]
  consumption_train_l6 = data_NO1[,3][6:(end-5)]
  consumption_train_l5 = data_NO1[,3][5:(end-6)]
  consumption_train_l4 = data_NO1[,3][4:(end-7)]
  consumption_train_l3 = data_NO1[,3][3:(end-8)]
  consumption_train_l2 = data_NO1[,3][2:(end-9)]
  #consumption_train_l1 = data_NO1[,3][1:(end-(lagmaks))]
  
  temp_train_l10 = data_NO1[,4][10:(end-1)]
  temp_train_l9 = data_NO1[,4][9:(end-2)]
  temp_train_l8 = data_NO1[,4][8:(end-3)]
  temp_train_l7 = data_NO1[,4][7:(end-4)]
  temp_train_l6 = data_NO1[,4][6:(end-5)]
  temp_train_l5 = data_NO1[,4][5:(end-6)]
  temp_train_l4 = data_NO1[,4][4:(end-7)]
  temp_train_l3 = data_NO1[,4][3:(end-8)]
  temp_train_l2 = data_NO1[,4][2:(end-9)]
  #temp_train_l1 = data_NO1[,4][1:(end-(lagmaks))]
  
  rain_train_l10 = data_NO1[,5][10:(end-1)]
  rain_train_l9 = data_NO1[,5][9:(end-2)]
  rain_train_l8 = data_NO1[,5][8:(end-3)]
  rain_train_l7 = data_NO1[,5][7:(end-4)]
  rain_train_l6 = data_NO1[,5][6:(end-5)]
  rain_train_l5 = data_NO1[,5][5:(end-6)]
  rain_train_l4 = data_NO1[,5][4:(end-7)]
  rain_train_l3 = data_NO1[,5][3:(end-8)]
  rain_train_l2 = data_NO1[,5][2:(end-9)]
  rain_train_l1 = data_NO1[,5][1:(end-(lagmaks))]
  
  
  #Normal OLS setup, including all lags of exo. var.
  x_lag = model.matrix(X_t[2:(end-9)] ~ 0 
                       + price_train_l2 + price_train_l3 + price_train_l4 + price_train_l5 + price_train_l6 + price_train_l7 + price_train_l8 + price_train_l9 + price_train_l10 
                       + consumption_train_l2 + consumption_train_l3 + consumption_train_l4 + consumption_train_l5 + consumption_train_l6 + consumption_train_l7 + consumption_train_l8 + consumption_train_l9 + consumption_train_l10 
                       + HYDRO_train_l2 + HYDRO_train_l3 + HYDRO_train_l4 + HYDRO_train_l5 + HYDRO_train_l6 + HYDRO_train_l7 + HYDRO_train_l8 + HYDRO_train_l9 + HYDRO_train_l10
                       +
                       + temp_train_l10+ temp_train_l9+ temp_train_l8 + temp_train_l7+ temp_train_l6+ temp_train_l5+ temp_train_l4+ temp_train_l3+ temp_train_l2
                       + rain_train_l10+rain_train_l9+rain_train_l8+rain_train_l7+rain_train_l6+rain_train_l5+rain_train_l4+rain_train_l3+rain_train_l2+rain_train_l1)
  #antal lag der skal tjekkes for
  lagtjek <- 5*lagmaks
  ### ARDL model - loop to add the most significant variables
  ols_all_lags = lm(X_t[(lagmaks+1):(end)] ~ x_lag)
  #each loop we add the most significant variable from the ols_all_lags model
  #setup start aic value and amount of insignificant p-values for ljung box test
  aic_best = Inf
  amount_LjungBox_best = -Inf
  aic_ljungbox_best = Inf
  removed_variables <-  c(1:lagtjek) #price_train_l1 is very significant.. lm cant run if we remove all
  for (i in 1:lagtjek) {
    #setup model
    if (i < lagtjek) {
      x_model = x_lag[,-removed_variables[i]]
      ols_adl = lm(X_t[(lagmaks+1):(end)] ~ x_model) #this cant run when removed_variables = 0 --> therefore we split into if else statement
    }
    else {
      x_model = x_lag
      ols_adl = lm(X_t[(lagmaks+1):end] ~ x_model)
    }
    #check the aic and take the best model
    if (AIC(ols_adl) < aic_best) {
      aic_best = AIC(ols_adl)
      best_adl_aic = ols_adl
    }
    
    #check residuals - if amount of insignificant models is higher or the same 
    amount_LjungBox_now = sum(LjungBoxTest(res = residuals(ols_adl), k = lagtjek-length(removed_variables), lag.max = 1500, StartLag = lagtjek-length(removed_variables)+1)[,3]>0.05)
    if (amount_LjungBox_now >= amount_LjungBox_best) {
      if (amount_LjungBox_now == amount_LjungBox_best) {
        if (AIC(ols_adl) < aic_ljungbox_best) {
          aic_ljungbox_best = AIC(ols_adl) 
          best_adl_LjungBox = ols_adl
        }
      }
      else {
        amount_LjungBox_best = amount_LjungBox_now
        best_adl_LjungBox = ols_adl
        #check residuals - if amount of insignificant models is the same, we take the one with the best aic
      }
    }
    
    
    if (i == lagtjek) break
    
    #add the most significant variable:
    most_significant_var = names(sort(summary(ols_all_lags)$coeff[-1,4], decreasing = FALSE)[1]) #we already added intercept and price lag 1
    entry_to_add = 1+which(names(ols_all_lags$coefficients[-1]) == most_significant_var)    #entry #1 here is the price lag 2
    #removed_variables = removed_variables[-which(removed_variables == entry_to_add)]
  }
  summary(best_adl_aic)
  AIC(best_adl_aic)
  par(mfrow=c(1,1))
  plot(residuals(best_adl_aic))
  qqnorm(residuals(best_adl_aic))
  hist(residuals(best_adl_aic),breaks = 200)
  auto.arima((residuals(best_adl_aic)),max.p = 10,max.q = 10,max.order = 10,stepwise=FALSE,approximation = FALSE,D=0)
}





fgf <- matrix(1,2,2)








