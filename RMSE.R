###RMSE

#ARFIMA
modelarfima2 <-   arfima(X_t,c(3,0,4),fixed = list(frac=0.19),dmean = F,itmean = F)
fitarfima12 <- as.data.frame(fitted.values(modelarfima2))

#ARIMA
modelarfima1 <- arfima(X_t,c(4,1,1),dmean = F,itmean = F)
fitarfima122 <- as.data.frame(fitted.values(modelarfima1))

# ARMA
modelarma <- arfima(X_t,c(1,0,2),dmean = F,itmean = F)
fitarfima123 <- as.data.frame(fitted.values(modelarma))


rmse(X_t,fitarfima12[,1])
rmse(X_t,fitarfima122[,1])
rmse(X_t,fitarfima123[,1])
