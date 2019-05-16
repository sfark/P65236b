### Fitting af ARMA til X_t
auto.arima(X_t,d=0, stepwise = F, approximation = F)#p=1 og q=2
fitarma <- arima(X_t,order=c(1,0,2),include.mean = F)

fitarma7 <- arima(X_t,order=c(7,0,2),include.mean = F)

acf(residuals(fitarma7),lag.max = 20)

acf(residuals(fitarma))
fitted.values(fitarma)

acf(fitarma$residuals)
pacf(fitarma$residuals)

res_arma_x_t <- fitarma$residuals

summary(arfima(X_t,c(1,0,2),lmodel = c("n"),dmean = F,itmean = F))  
#sarima(X_t,1,0,2,details = T,no.constant = T)   


ggplot(data=ts(X_t),mapping = aes(1:length(X_t),X_t))+
    geom_line(aes(y=X_t,colour="fitarma"))+
    geom_line(aes(y=fitted.values(fitarma),colour="X_t"))+
    scale_color_manual(name="Models",labels=c("Original","ARMA(1,2)"),
                       values = c("fitarma"= "black","X_t"="red"))+
    xlab("Time")+ylab("Sample")+xlim(c(1,2100))
    

###ACF og PACF plot

  ACf_arma <- acf(res_arma_x_t ,plot = FALSE,lag.max = 50)
  di_acf_arma <- with(ACf_arma, data.frame(lag, acf))
  ggplot(data = di_acf_arma , mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)
  
  di_pACf_ARMA <- pacf(res_arma_x_t  ,plot = FALSE,lag.max = 50)
  di_pacf_ARMA2 <- with(di_pACf_ARMA, data.frame(lag, acf))
  ggplot(data = di_pacf_ARMA2, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

  

