### Fitting af ARMA til X_t


auto.arima(X_t,d=1, stepwise = F, approximation = F)#p=1 og q=2
fitarima <- arima(X_t,order=c(4,1,1),include.mean = F)

fitted.values(fitarima)

acf(fitarima$residuals,lag.max = 50)
pacf(fitarima$residuals)

res_arima_x_t <- fitarima$residuals

summary(arfima(X_t,c(4,1,1),lmodel = c("n"),dmean = F,itmean = F))  
sarima(X_t,4,1,1,details = T,no.constant = T)   


ggplot(data=ts(X_t),mapping = aes(1:length(X_t),X_t))+
  geom_line(aes(y=X_t,colour="fitarma"))+
  geom_line(aes(y=fitted.values(fitarima),colour="X_t"))+
  scale_color_manual(name="Models",labels=c("Original","ARIMA(4,1,1)"),
                     values = c("fitarma"= "black","X_t"="red"))+
  xlab("Time")+ylab("Sample")+xlim(c(1,2100))



###ACF og PACF plot

ACf_arima <- acf(res_arima_x_t ,plot = FALSE,lag.max = 100)
di_acf_arima <- with(ACf_arima, data.frame(lag, acf))
ggplot(data = di_acf_arima , mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

di_pACf_ARiMA <- pacf(res_arima_x_t  ,plot = FALSE,lag.max = 50)
di_pacf_ARiMA <- with(di_pACf_ARiMA, data.frame(lag, acf))
ggplot(data = di_pacf_ARiMA, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)


####ACF og PACF af Diff(X_T)
plot.ts(diff(X_t))
acf(diff(X_t))
Pacf(diff(X_t))

acf(X_t,lag.max = 1000)
{
ACf_X_t <- acf(X_t ,plot = FALSE,lag.max = 100)
di_acf <- with(ACf_X_t, data.frame(lag, acf))
ggplot(data = di_acf , mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)


ACf_diff <- acf(diff(X_t) ,plot = FALSE)
di_acf_diff <- with(ACf_diff, data.frame(lag, acf))
ggplot(data = di_acf_diff , mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

di_pACf_diff <- pacf(diff(X_t)  ,plot = FALSE)
di_pacf_diff <- with(di_pACf_diff, data.frame(lag, acf))
ggplot(data = di_pacf_diff, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)
}