### #################### Mean.temp ###################
temp <- ts(WEATHER[,1])


plot.ts(temp)
model2 <- glm(temp~
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1]))))



glm(temp~time(dagligpris[,1])+
      I(time(dagligpris[,1])^2))

summary(glm(temp~time(dagligpris[,1])))


xtable(summary(model2)$coef)

length(temp)
length(model2$residuals)

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


autoplot(model2$residuals)
ggplot(temp,aes(x=1:length(temp),y=temp))+
  geom_line()+
  geom_abline(intercept = 6.9017352432,slope =0.0005215422,col="blue" )+
  xlab("Date")+ylab("Mean Temperature")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = c(0,365,730,1095,1460,1825,2190),labels = c("2013","2014","2015","2016","2017","2018","2019"))

ggplot(as.data.frame(model2$residuals),aes(x=1:length(model2$residuals),y=model2$residuals))+
  geom_line()+
  xlab("Time")+ylab("Residuals")
}

auto.arima(model2$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)#AR(3)
summary(arfima(model2$residuals,c(3,0,0),lmodel = c("n"),dmean = F,itmean = F))


sarima(model2$residuals,d=0,p=3,q=0,no.constant = T)
fit5 <-arima(model2$residuals,order = c(3,0,0),include.mean = F)
summary(fit5)
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

y <- residuals(Arima(X_t,model=fit5))
ccf(model2$residuals,y)
tempccf <- ccf(fittemp$residuals,Arima(dagligpris[,2],model=fittemp)$residuals)

{ #CCF Temp
fittemp <- auto.arima(model2$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)#AR(5)

ggCcf(fittemp$residuals,X_t-Arima(X_t,model=fittemp)$residuals,main="")

di_CCF <- ccf(fittemp$residuals,Arima(X_t,model=fittemp)$residuals,plot = FALSE)
di_acf <- with(di_CCF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("CCF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

}




########################## Consumption##################
consump <-ts(CONSUMPTION[,1])
{
ggplot(fortify(CONSUMPTION/1000),aes(x=dato,y=NO1))+
  geom_line(linetype=1)+
  geom_hline(yintercept = 100,col="blue")+
  xlab("Daily")+
  ylab("Consumption in GWh")

}

consump_daligi <- as.data.frame(mutate(dplyr::select(CONSUMPTION,NO1),weekday = wday(dato)))


##Vi laver 2 matricer en for hverdags priser og en for weekend priser
hverdag_consump <- filter(consump_daligi,weekday==2|weekday==3|weekday==4|weekday==5|weekday==6)
weekend_consump <- filter(consump_daligi,weekday==1|weekday==7)




### splitter data om og tager gennemsnit af hverdage og weekender
k <-split(consump_daligi,1:7)
consump_split <- cbind(k$`1`[,1],k$`2`[,1],k$`3`[,1],k$`4`[,1],k$`5`[,1],k$`6`[,1],k$`7`[,1])
gennemsnit <- c()
for (i in 1:7) {
  gennemsnit <- rbind(gennemsnit,mean(consump_split[,i]))  
if (i==7) {
  gennemsnit <- as.data.frame(gennemsnit)
  names(gennemsnit) #<- rbind("Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday","Monday")
  gennemsnit <- cbind(1:7,gennemsnit)
  print(gennemsnit)
  }
}

gennemsnit <- c(gennemsnit)
plot(gennemsnit)

##ggplot afden gennemsnitlige daglige pris
{
  ggplot(gennemsnit,aes(x=gennemsnit[,1],y=V1))+
  geom_col(width = 0.8,fill="steelblue")+
  ylim(0,120000)+ylab("Consumption")+xlab("")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),labels = c("Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday","Monday"))
  
  }




plot.ts(CONSUMPTION[,1])
plot(decompose(consump))

plot(CONSUMPTION[,1]/1000)
abline(glm(CONSUMPTION[,1]/1000~time(dagligpris[,1])))


model3 <- glm(consump~
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend)

summary(model3)

plot.ts(model3$residuals,ylab=expression(hat(z)))
z_hat <- as.data.frame(model3$residuals)

autoplot(ts(model3$residuals),ylab = expression(hat(z)))

ggplot(z_hat,aes(x=1:length(z_hat[,1]),y=z_hat[,1]))+
  geom_line()+
  ylab("Residuals")+
  xlab("Time")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = c(0,365,730,1095,1460,1825,2190),labels = c("2013","2014","2015","2016","2017","2018","2019"))

conccf <- ccf(fitcon$residuals,Arima(dagligpris[,2],model=fitcon)$residuals)

acf(model3$residuals)
pacf(model3$residuals)

auto.arima(model3$residuals,stepwise=FALSE, approximation=FALSE)#ARIMA(2,1,1)

  sarima(model3$residuals,d=1,p=2,q=1,no.constant = T)
summary(arfima(diff(model3$residuals),c(2,0,1),lmodel = c("n"),dmean = F,itmean = F))
  
  
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
  
  ggCcf(fitcon$residuals,X_t-Arima(X_t,model=fitcon)$residuals,main="")
  
  di_CCF <- ccf(fitcon$residuals,Arima(X_t,model=fitcon)$residuals,plot = FALSE)
  di_acf <- with(di_CCF, data.frame(lag, acf))
  ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("CCF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)
  
  
  
}
ccf(fitcon$residuals, Arima(X_t,model=fitcon)$residuals, main="",family="serif")




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
curve(4.474e+04+ 1.425e+01*x+-5.569e-03*x^2,add=TRUE)
plot(decompose(ts(hydrodayli,frequency = 365)))

model5 <- glm(hydrodayli~
                time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1]))))

summary(glm(hydrodayli~
              time(dagligpris[,1])+
              I(time(dagligpris[,1])^2)))

plot.ts(model5$residuals)

acf(model5$residuals,lag.max = 100)
pacf(model5$residuals)


{
ggplot(ts(hydrodayli),aes(x=1:length(hydrodayli),y=hydrodayli))+
    geom_line()+
    geom_smooth(method = "lm",se=F,formula =hydrodayli ~poly(x,2)) +
    ylab("Hydro")+
    xlab("Time")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_continuous(breaks = c(0,365,730,1095,1460,1825,2190),labels = c("2013","2014","2015","2016","2017","2018","2019"))
  
  
  
  di_ACF <- acf(model5$residuals ,plot = FALSE,lag.max = 100)
  di_acf <- with(di_ACF, data.frame(lag, acf))
  ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)
  
  
  diffY_PACF <- pacf(model5$residuals ,plot = FALSE,lag.max = 10)
  pacfdf <- with(diffY_PACF, data.frame(lag, acf))
  ggplot(data = pacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
  
}###ACF ggplot


fit53 <-arima(model2$residuals,order = c(2,0,0),include.mean = F)

acf(fit53$residuals)
{
  
  ggplot(as.data.frame(model5$residuals),aes(x=1:length(model5$residuals),y=model5$residuals))+
    geom_line()+
    xlab("Time")+ylab("Residuals")
  
  di_ACF <- acf(fit53$residuals ,plot = FALSE)
  di_acf <- with(di_ACF, data.frame(lag, acf))
  ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)+
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1))
}### ACF ggplot 


auto.arima(model5$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)#AR(2)

hydroccf <- ccf(fithy$residuals,Arima(dagligpris[,2],model=fithy)$residuals,family="serif")


arima(model5$residuals)
sarima(model5$residuals,d=0,p=2,q=0,no.constant = T)

 #CCF Hydro
  fithy <- auto.arima(model5$residuals,stepwise=FALSE, approximation=FALSE)#AR(5)

sarima(model5$residuals,d=1,p=2,q=3,no.constant = T)

{ #CCF Hydro
  fithy <- auto.arima(model5$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F) #AR(5)
  
  ggCcf(fithy$residuals,Arima(X_t,model=fithy)$residuals,main="")
  
  di_CCF <- ccf(fithy$residuals,X_t-Arima(X_t,model=fithy)$residuals,plot = FALSE)
  di_acf <- with(di_CCF, data.frame(lag, acf))
  ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("CCF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)


}

ccf(fithy$residuals,Arima(X_t,model=fithy)$residuals,family="serif")

}


ccf(fithy$residuals,Arima(X_t,model=fithy)$residuals,family="serif")
}


