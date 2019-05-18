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
# install.packages("TSA")
# install.packages("FitAR")
# install.packages("glmnet")
# install.packages("stringr")
#install.packages("rms")
#load library
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
# library(LSTS)
library(dplyr)
library(FitAR)
library(glmnet)
library(TSA)
library(rms)
# xtable(PRICES[1:10,])

mmed <- function(x,n=5){runmed(x,n)}
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
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
  #hydro2 <- cbind(dato2,HYDRO)
  
  
  hydrodayli <- c()
  for (i in 1:(length(HYDRO[,1])-1)) {
    hydrodayli <- c(hydrodayli,(seq(from=HYDRO[i,1],length.out = 7,by=((HYDRO[i+1,1]-HYDRO[i,1])/7 ))) )
  }
  hydrodayli <- c(hydrodayli,seq(from=HYDRO[i,1],length.out = 7,by=((52663-HYDRO[i,1])/7 )))
  #length(hydrodayli)
}





#opdelling til hverdage, weekender og helligdage.
head(pris)

### Vi f?r en vektor p? med tal fra 1-7 hvor mandag=1 s?ndag=7
dagligpris <- as.data.frame(mutate(dplyr::select(pris,dato,Oslo),weekday = wday(dato)))
dagligpris[,2] <-log(dagligpris[,2])


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

##m?nedlig gennemsnits pris weekend mod hverdag
meanmonthhverdag <- c(summarize(group_by(hverdagspris,month=floor_date(dato,"month")),Oslo=mean(Oslo)))
meanmonthweekend <-c(summarize(group_by(weekendpris,month=floor_date(dato,"month")),Oslo=mean(Oslo)))

meanmonthdata <- as.data.frame(cbind(c(1:length(meanmonthhverdag$Oslo)),meanmonthhverdag$Oslo,meanmonthweekend$Oslo))

### Helligdag

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


###sæson cleaning
dummyhelligweekend <- dummyhelligdage+dummyweekend

for (i in 1:length(dummyhelligweekend)) {
  if (dummyhelligweekend[i]==2){
    dummyhelligweekend[i]=1
  }
}
## dummy spiks
model1 <- glm(dagligpris[,2]~
                time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))+
                dummyhelligweekend)
summary(model1)

#vores nye tidsrække før spikes
X_t <- ts(model1$residuals)
#spikes

# {
#   dummyspikes <- c()
#   for (i in 1:length(X_t)) {
#     if (X_t[i]>1.1) {
#       dummyspikes[i] <- 1
#     }else if (X_t[i]< -1.1) {
#       dummyspikes[i] <- -1
#     }
#     else{
#       dummyspikes[i] <- 0
#     }  
#   }
# }
# ########### tilføjer dummy spikes til GLM
# model1 <- glm(dagligpris[,2]~time(dagligpris[,1])+
#                 I(time(dagligpris[,1])^2)+
#                 cos((2*pi/365)*I(time(dagligpris[,1])))+
#                 sin((2*pi/365)*I(time(dagligpris[,1])))+
#                 cos((4*pi/365)*I(time(dagligpris[,1])))+
#                 sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend+dummyspikes)
# #summary(model1)
# 
# #vores nye tidsserie
# X_t <- ts(model1$residuals)



# opstiller data for 2019
data_2019_files <- list.files("2019data", full.names = 1)
PRICES_2019SA <- read.csv2(data_2019_files[3], header = TRUE)[1:110,10]
HYDRO_2019 <- read.csv2(data_2019_files[4], header = TRUE)[1:16,2]
Hydrprep <- (rep(HYDRO_2019,each = 7,length.out=110))
CONSUMPTION_2019 <- read.csv2(data_2019_files[1], header = TRUE,skip = 2)[1:110,2]
WEATHER_2019 <- read.csv2(data_2019_files[5], header = TRUE)[1:110,(3:4)]

hydrodayli_2019 <- c()
for (i in 1:(length(HYDRO_2019)-1)) {
  hydrodayli_2019 <- c(hydrodayli_2019,(seq(from=HYDRO_2019[i],length.out = 7,by=((HYDRO_2019[i+1]-HYDRO_2019[i])/7 ))) )
}
hydrodayli_2019 <- c(hydrodayli_2019,seq(from=HYDRO_2019[i],length.out = 7,by=((30790-HYDRO_2019[i])/7 )))
#length(hydrodayli)

Data2019 <- cbind(PRICES_2019SA[1:110],hydrodayli_2019[1:110],CONSUMPTION_2019,WEATHER_2019[,1],WEATHER_2019[,2])
#Data2019 <- cbind(Hydrprep,CONSUMPTION_2019/1000,WEATHER_2019[,1],WEATHER_2019[3:112,1],WEATHER_2019[,2])

