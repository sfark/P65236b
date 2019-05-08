# tests

#ADF-Test
adf.test(model1$residuals,k = 0)
adf.test(model1$residuals)


##RMSE
fit34 <- fitted.values(arfima(X_t,c(3,0,4),fixed = list(frac=0.19),dmean = F,itmean = F))
fit12 <- fitted.values(arfima(X_t,c(1,0,2),lmodel = c("n"),dmean = F,itmean = F))

rmse(exp(X_t+fitted.values(model1)),exp(fit12$Mode1+fitted.values(model1)))
rmse(exp(X_t+fitted.values(model1)),exp(fit34$Mode1+fitted.values(model1)))

##MAPE
mape(X_t,fit12$Mode1)
mape(X_t,fit34$Mode1)
