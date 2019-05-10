# tests

#ADF-Test
adf.test(model1$residuals,k = 0)
adf.test(model3$residuals)


qqnorm(X_t)

qqline(X_t)


sarima(X_t,p=3,d=0.19,q=4,details = T,no.constant = T)
