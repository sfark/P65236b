plot(olie)

oliepris <- window(log(olie),start=1986,end=c(2012,12))
outofsampledata <- window(log(olie),start=2013)


acf(oliepris,lag.max = 100)
firstdiff <- diff(oliepris)
acf(firstdiff)

AR_1 <- ar.ols(oliepris,order.max = 1,demean = FALSE,intercept = T)
AR_diff <-ar.ols(oliepris,order.max = 1,demean = TRUE,intercept = T) 


sarima(firstdiff,d=0,p=1,q=0)

acf(firstdiff)

pacf(firstdiff)

detrendolie <- decompose(oliepris)$trend

detrendoliepris <- oliepris-detrendolie
plot(detrendoliepris)
plot(detrendolie)
plot(decompose(oliepris))
plot(detrendoliepris)
plot(oliepris)
AIC(as.ts(detrendoliepris))

#install if necessary
install.packages('gtools')
#load library
library(gtools)
#urn with 3 balls
x <- c(0,1,2,3,4)
#pick 2 balls from the urn with replacement
#get all permutations
per <- permutations(n=5,r=2,v=x,repeats.allowed=T)
armanr <- c()

for (i in 1:25) {
  ARMAmatrix <- arima(detrendoliepris,order=c(per[i,1],0,per[i,2]))
  armanr <- c(armanr,AIC( ARMAmatrix ))
}
per[20,]
min(armanr)

arma32 <- arima(detrendoliepris,order=c(3,0,2))
