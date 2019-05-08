### #################### Mean.temp ###################
temp <- WEATHER[,1]


plot.ts(diffseries(temp,1))
model2 <- glm(temp~
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1]))))


plot.ts(model2$residuals)
acf(model2$residuals)
pacf(model2$residuals)

auto.arima(model2$residuals,stepwise=FALSE, approximation=FALSE)#AR(3)
sarima(model2$residuals,d=0,p=3,q=0)

#efter der er fjernet sæson, så fittes der en AR(3) model på, og vi får nogle gode resultater

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

############ Hydro
plot.ts(hydrolang)


model5 <- glm(hydrolang~
                time(dagligpris[1:2170,1])+
                I(time(dagligpris[1:2170,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[1:2170,1])))+
                sin((2*pi/365)*I(time(dagligpris[1:2170,1])))+
                cos((4*pi/365)*I(time(dagligpris[1:2170,1])))+
                sin((4*pi/365)*I(time(dagligpris[1:2170,1]))))

plot.ts(model5$residuals)
