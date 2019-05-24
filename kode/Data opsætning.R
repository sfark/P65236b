##Pakker brugt
library(gtools)
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
library(zoo)

#Data uploading
setwd("~/P65236b/DATA")

PRICES_list <- list.files("PRICES", full.names = 1)
HYDRO_list <- list.files("HYDRO", full.names = 1)
CONSUMPTION_list <- list.files("CONSUMPTION", full.names = 1)

PRICES <- read.csv2(PRICES_list[1], header = TRUE)[,c(10:15)]
HYDRO <- read.csv2(HYDRO_list[1], header = TRUE)[,c(2:3)]
CONSUMPTION <- read.csv2(CONSUMPTION_list[1], header = TRUE)[,c(2:7)]
WEATHER <- read.csv2("vejrdata.csv", header = TRUE,skip = 1)[1:2191,c(3,6)]

WEATHER$Precipitation <- as.numeric(gsub(",", ".", WEATHER$Precipitation,ignore.case = "."))
for (i in 1:length(WEATHER$Precipitation)) {
  if(is.na(WEATHER$Precipitation[i])==TRUE){
    WEATHER$Precipitation[i] <- 0
  }else{
    
  }
}
for (i in 2:6) {
  PRICES <- rbind(PRICES,read.csv2(PRICES_list[i], header = TRUE)[,c(10:15)])
  HYDRO <- rbind(HYDRO,read.csv2(HYDRO_list[i], header = TRUE)[,c(2:3)])
  CONSUMPTION <- rbind(CONSUMPTION,read.csv2(CONSUMPTION_list[i], header = TRUE)[,c(2:7)])
}

#dato vektor
dato <- seq(c(ISOdate(2013,1,1)), by = "day", length.out = 2191)
dato2 <- seq(ISOdate(2013,1,1),by="week", length.out = 310)

#fjerne data for 2019
WEATHER <- WEATHER[1:2191,]
WEATHER$Date <- dato


# data_NO1

data_NO1 <- cbind(PRICES$Oslo,CONSUMPTION$NO1,hydrodayli,WEATHER$Mean.temperature,WEATHER$Precipitation)
colnames(data_NO1) <- c("Price","Consumption","Hydro reserve","Mean.temperature", "Precipitation")


corefiventerchange <- c()
for (i in seq(from=100,to=2191,by =3)) {
  testtest <-arima(x = X_t[1:(i)], order = c(1, 0, 2))
  corefiventerchange <- rbind(corefiventerchange,testtest$coef)
}

plot.ts(corefiventerchange[,1],ylim=c(c(-0.5,0.2),c(0.7,1)))
lines(corefiventerchange[,2],col="red")
lines(corefiventerchange[,3],col="blue")


min(corefiventerchange[,3])
min(corefiventerchange[,2])

max(corefiventerchange[,3])
max(corefiventerchange[,2])

min(corefiventerchange[,1])
max(corefiventerchange[,1])


