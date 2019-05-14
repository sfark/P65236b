###RMSE

modelarfima2 <-   arfima(X_t,c(3,0,4),fixed = list(frac=0.19),dmean = F,itmean = F)
fitarfima12 <- as.data.frame(fitted.values(modelarfima2))


rmse(X_t,fitarfima12[,1])