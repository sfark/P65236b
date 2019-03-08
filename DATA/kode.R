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

#data
PRICES_list <- list.files("PRICES", full.names = 1)
HYDRO_list <- list.files("HYDRO", full.names = 1)
CONSUMPTION_list <- list.files("CONSUMPTION", full.names = 1)

PRICES <- read.csv2(PRICES_list[1], header = TRUE)[,c(10:15)]
HYDRO <- read.csv2(HYDRO_list[1], header = TRUE)[,c(2:3)]
CONSUMPTION <- read.csv2(CONSUMPTION_list[1], header = TRUE)[,c(2:7)]

for (i in 2:6) {
  PRICES <- rbind(PRICES,read.csv2(PRICES_list[i], header = TRUE)[,c(10:15)])
  HYDRO <- rbind(HYDRO,read.csv2(HYDRO_list[i], header = TRUE)[,c(2:3)])
  CONSUMPTION <- rbind(CONSUMPTION,read.csv2(CONSUMPTION_list[i], header = TRUE)[,c(2:7)])
}


dato <- seq(c(ISOdate(2013,1,1)), by = "day", length.out = 2191)
ggplot(fortify(PRICES),aes(x= dato, y= Oslo)) + geom_line() + xlab("Date")+ylab("El-spot price Oslo")

ggplot(fortify(PRICES),aes(x= dato, y= Oslo)) + geom_line() + geom_line(aes(x=dato, y=Bergen),col="red", alpha = 0.5)


x <- 1:2191

log(PRICES)
par(mfrow=c(1,1))


###ACF for pris, hydro, og comsumption
acf(log(PRICES[,1]),lag.max = 500)
acf(log(HYDRO[,1]),lag.max=100)
acf(log(CONSUMPTION[,1]),lag.max = 355)

###ARFIMA model
pris <- ts(PRICES[,1])
library(arfima)
y <- log(pris)-mean(log(pris))
acf2(y,max.lag = 355)
acf2pris.fd <- arfima(y)
summary(pris.fd)
d <- summary(pris.fd)$coef[[1]][1]; d
se.d <- summary(pris.fd)$coef[[1]][1,2];se.d
residual <- resid(pris.fd)
plot.ts(residual[[1]])
acf(residual[[1]])

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
