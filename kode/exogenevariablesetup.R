### #################### Mean.temp ###################
temp <- WEATHER[,1]


plot.ts(temp)
model2 <- glm(temp~
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))-1)


plot.ts(model2$residuals)
acf(model2$residuals)
pacf(model2$residuals)


{
di_ACF <- acf(model2$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)


diffY_PACF <- pacf(model2$residuals ,plot = FALSE)
pacfdf <- with(diffY_PACF, data.frame(lag, acf))
ggplot(data = pacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("PACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)

}

auto.arima(model2$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)#AR(5)
sarima(model2$residuals,d=0,p=5,q=0,no.constant = T)
fit5 <-arima(model2$residuals,order = c(5,0,0),include.mean = F)
acf(fit5$residuals)
 {
di_ACF <- acf(fit5$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
geom_hline(aes(yintercept = 0)) +
geom_segment(mapping = aes(xend = lag, yend = 0))+
ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)
}

#efter der er fjernet sæson, så fittes der en AR(3) model på, og vi får nogle gode resultater

y <- X_t-residuals(Arima(X_t,model=fit5))
ccf(model2$residuals,y)


{ #CCF Temp
fittemp <- auto.arima(model2$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)#AR(5)

ggCcf(fittemp$residuals,Arima(X_t,model=fittemp)$residuals,main="")


}
########################## Consumption##################
consump <-ts(CONSUMPTION[,1],frequency = 365)

plot.ts(consump)
plot(decompose(consump))
model3 <- glm(consump~time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend)

summary(model3)
plot.ts(model3$residuals)
acf(model3$residuals)
pacf(model3$residuals)

auto.arima(model3$residuals,stepwise=FALSE, approximation=FALSE)#ARMA(3,0,2)
sarima(model2$residuals,d=0,p=3,q=2,no.constant = T)

{
di_ACF <- acf(model3$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)


diffY_PACF <- pacf(model3$residuals ,plot = FALSE)
pacfdf <- with(diffY_PACF, data.frame(lag, acf))
ggplot(data = pacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("PACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
}
{ #CCF con
  fitcon <- auto.arima(model3$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)
  
  ggCcf(fitcon$residuals,Arima(X_t,model=fitcon)$residuals,main="")
  
  
}

##############  Preception#############
regn <- ts(WEATHER[,2],frequency = 365)

plot.ts(regn)
model4 <- glm(regn~regn2+time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1]))))


regn2 <- c()
  for (i in 1:length(regn)) {
    if (regn[i]<5) {
      regn2[i] <- 0
    }    
    else
      regn2[i] <- 1
  }
plot.ts(regn2,ylim=c(0,2))

plot(decompose(regn))
acf(diff(regn))
auto.arima(regn,stepwise=FALSE, approximation=FALSE,allowmean = F)
sarima(regn,1,0,1,no.constant = F)

############ Hydro #####
plot.ts(hydrodayli)
plot(decompose(ts(hydrodayli,frequency = 365)))

model5 <- glm(hydrodayli~
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1]))))

plot.ts(model5$residuals)


acf(model5$residuals,lag.max = 100)
pacf(model5$residuals,lag.max = 100)

acf(diff(model5$residuals),lag.max=100)

auto.arima(model5$residuals,stepwise=FALSE, approximation=FALSE)#ARIMA(2,1,3)

sarima(model5$residuals,d=1,p=2,q=3,no.constant = T)

{ #CCF Hydro
  fithy <- auto.arima(model5$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)#AR(5)
  
  ggCcf(fithy$residuals,Arima(X_t,model=fithy)$residuals,main="")
  
  
}

