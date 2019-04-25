end = length(X_t)
lagmaks <- 25


lagseq <- seq(2,lagmaks)
laqmod <- seq(lagmaks,2)
{
price_train = c()
  #X_t[2:(end-(lagmaks+1))]
nameprice_train <- c()

hydro_train <- c() 
  #data_NO1[,2][1:(end-lagmaks)]
namehydro_train <- c()

consumption_train = c()
  #data_NO1[,3][1:(end-lagmaks)]
nameconsumption_train<- c()

temp_train = c()
  #data_NO1[,4][1:(end-lagmaks)]
nametemp_train <- c()

rain_train = c()
  #data_NO1[,5][1:(end-lagmaks)]
namerain_train <- c()


for (i in 1:(lagmaks-1)) {
  price_train <-cbind(price_train, X_t[lagseq[i]:(end-laqmod[i])])
  nameprice_train<- c(nameprice_train,paste("price_train_l",toString(i),sep = ""))
  
  hydro_train <- cbind(hydro_train, data_NO1[,2][lagseq[i]:(end-laqmod[i])])
  namehydro_train <- c(namehydro_train,paste("hydro_train_l",toString(i),sep = ""))
  
  consumption_train <- cbind( consumption_train, data_NO1[,3][lagseq[i]:(end-laqmod[i])])
  nameconsumption_train <- c(nameconsumption_train,paste("consumption_train_l",toString(i),sep = ""))
  
  temp_train <-cbind(temp_train, data_NO1[,4][lagseq[i]:(end-laqmod[i])])
  nametemp_train <- c(nametemp_train,paste("temp_train_l",toString(i),sep = ""))
  
  rain_train <-cbind(rain_train, data_NO1[,5][lagseq[i]:(end-laqmod[i])])
  namerain_train <- c(namerain_train,paste("rain_train_l",toString(i),sep = ""))
}
colnames(price_train) <- nameprice_train
colnames(hydro_train) <- namehydro_train
colnames(consumption_train) <- nameconsumption_train
colnames(temp_train) <- nametemp_train
colnames(rain_train) <- namerain_train
datalag <- cbind(price_train,hydro_train,consumption_train,temp_train,rain_train)
}
{
#Normal OLS setup, including all lags of exo. var.
x_lag = model.matrix(X_t[1:(end-(lagmaks))] ~ 0 +price_train +hydro_train + consumption_train +temp_train+rain_train)



lagtjek <- 5*lagmaks
### ARDL model - loop to add the most significant variables
ols_all_lags = lm(X_t[(lagmaks+1):(end)] ~ x_lag)
#each loop we add the most significant variable from the ols_all_lags model
#setup start aic value and amount of insignificant p-values for ljung box test
aic_best = Inf
amount_LjungBox_best = -Inf
aic_ljungbox_best = Inf
removed_variables <-  c()


for (i in 1:lagtjek) {
 
  
  if (i==1) {
    x_model = x_lag
    ols_adl = lm(X_t[1:(end-lagmaks)] ~ x_model)
    
  }
  if(i>1){
    x_model = x_lag[,-removed_variables]
    ols_adl = lm(X_t[1:(end-lagmaks)] ~ x_model) #this cant run when removed_variables = 0 --> therefore we split into if else statement
    
  }
 
  #check the aic and take the best model
  if (AIC(ols_adl) < aic_best) {
    aic_best = AIC(ols_adl)
    best_adl_aic = ols_adl
  }
  
  
  amount_LjungBox_now = sum(LjungBoxTest(res = residuals(ols_adl), k = lagtjek-length(removed_variables), lag.max = 1500, StartLag = lagtjek-length(removed_variables)+1)[,3]>0.05)
  if (amount_LjungBox_now >= amount_LjungBox_best) {
    if (amount_LjungBox_now == amount_LjungBox_best) {
      if (AIC(ols_adl) < aic_ljungbox_best) {
        aic_ljungbox_best = AIC(ols_adl) 
        best_adl_LjungBox = ols_adl
      }
    }
    else {
      amount_LjungBox_best = amount_LjungBox_now
      best_adl_LjungBox = ols_adl
      #check residuals - if amount of insignificant models is the same, we take the one with the best aic
    }
  }
  
  #add the most significant variable:
  most_significant_var = names(sort(summary(ols_all_lags)$coeff[-1,4], decreasing = FALSE)[1]) #we already added intercept and price lag 1
  
  removed_variables <- c(removed_variables,which(colnames(x_lag)==str_sub(most_significant_var, 6, str_length(most_significant_var)))
)
   
}
}
par(mfrow=c(2,1))
plot(decompose(ts(data_NO1[,2],frequency = 365))$random)


model1hydro <- glm(data_NO1[,2]~time(dagligpris[,1])+
                     I(time(dagligpris[,1])^2)+
                     cos((2*pi/365)*I(time(dagligpris[,1])))+
                     sin((2*pi/365)*I(time(dagligpris[,1])))+
                     cos((4*pi/365)*I(time(dagligpris[,1])))+
                     sin((4*pi/365)*I(time(dagligpris[,1]))))
summary(model1hydro)

#vores nye tidsserie
hydro_sason <- ts(model1hydro$residuals)
plot.ts(model1hydro$residuals)

AIC(fracdiff((hydro_sason), nar = 28, nma = 28))
fracdiff((data_NO1[,2]/(1000)))

AICMAtrixhydro <-matrix(0,30,30) 
for (i in 1:30) {
  for (j in 1:30) {
    AICMAtrixhydro[i,j] <-  AIC(fracdiff((data_NO1[,2]/(1000)), nar = i, nma = j))
  }
}

which(AICMAtrixhydro==min(AICMAtrixhydro),arr.ind = TRUE)




auto.arima(hydro_sason,max.p = 10,max.q = 10,max.order = 100)#hydro

aa_hydro <- auto.arima(hydro_sason,max.p = 10,max.q = 10,max.order = 100)#hydro
hydro_coef <- aa_hydro$coef[1:10] 
arma55model = aa_hydro
arma55model
pwx=arma55model$residuals
newpwy = stats::filter(X_t, filter = as.numeric(hydro_coef), sides =1)
ccf (newpwy,pwx,na.action=na.omit,lag.max = 300)
hydropre <- prewhiten(data_NO1[,2],X_t,x.model=arma55model,lag.max=365)


aa_con <- auto.arima(hydro_sason)#consump
consum_coef <- c(1/(-aa_con$coef[5]),aa_con$coef[1]+1/(-aa_con$coef[5]),aa_con$coef[2]+aa_con$coef[1]/(-aa_con$coef[5]),aa_con$coef[3]+aa_con$coef[2]/(-aa_con$coef[5]),aa_con$coef[4]+aa_con$coef[3]/(-aa_con$coef[5]),aa_con$coef[4]/(-aa_con$coef[5]))
arma1model = auto.arima(data_NO1[,3]/(1000))
arma1model
pwx=arma1model$residuals
newpwy = Arima(X_t,model= arma1model)
ccf (newpwy$residuals,pwx,na.action=na.omit)
prewhiten(data_NO1[,3],X_t,x.model=arma1model)
conpre <- prewhiten(data_NO1[,3],X_t,x.model=arma1model)


aa_temp <- auto.arima(data_NO1[,4])#temp
temp_coef <- c(aa_temp$coef[1:5])
arma5model = auto.arima(data_NO1[,4])
arma5model
pwx=arma5model$residuals
newpwy = stats::filter(X_t, filter = as.numeric(temp_coef), sides =1)
ccf (newpwy,pwx,na.action=na.omit)
prewhiten(data_NO1[,4],X_t,x.model=arma5model,lag.max=365)
temppre <- prewhiten(data_NO1[,4],X_t,x.model=arma5model,lag.max=365)

for (i in 1:length(temppre$ccf)) {
  
}





aa_rain <- auto.arima(data_NO1[,5])#rain
rain_coef <- c(-1/(aa_rain$coef[2]),(aa_rain$coef[1]+1)/aa_rain$coef[2],(-aa_rain$coef[1])/aa_rain$coef[2])
arma11model = auto.arima(data_NO1[,5])
arma11model
pwx=arma11model$residuals
newpwy = stats::filter(X_t, filter = as.numeric(rain_coef), sides =1)
ccf (newpwy,pwx,na.action=na.omit)
prewhiten(data_NO1[,5],X_t,x.model=arma11model,lag.max=365)
rainpre <- prewhiten(data_NO1[,5],X_t,x.model=arma11model,lag.max=365)

acf(X_t)
acf(X_t,family="serif")
aa_price <- auto.arima(data_NO1[,1])#price

arma11model = auto.arima(data_NO1[,1])
arma11model
plot(arma11model$residuals)
prewhiten(data_NO1[,1],X_t,x.model=arma11model)
rainpre <- prewhiten(data_NO1[,5],X_t,x.model=arma11model,lag.max=365)
# con lag 2,3,4,5
# temp lag 1, 2, 3, 4, 5
# rain lag 1
rainpre_lag <- c()
temppre_lag <- c()
hydropre_lag <- c()
conpre_lag <- c()
laqseq <- seq(-100,-1)
for (i in 1:length(laqseq)) {
  if (rainpre$ccf[laqseq[i]]< -0.05 || rainpre$ccf[laqseq[i]]> 0.05) {
    rainpre_lag <- c(rainpre_lag,laqseq[i])
  }
  if (temppre$ccf[laqseq[i]]< -0.05 || temppre$ccf[laqseq[i]]> 0.05) {
    temppre_lag <- c(temppre_lag,laqseq[i])
  }
  if (hydropre$ccf[laqseq[i]]< -0.05 || hydropre$ccf[laqseq[i]]> 0.05) {
    hydropre_lag <- c(hydropre_lag,laqseq[i])
  }
  if (conpre$ccf[laqseq[i]]< -0.05 || conpre$ccf[laqseq[i]]> 0.05) {
    conpre_lag <- c(conpre_lag,laqseq[i])
  }
}

rainpre_lag
temppre_lag 
hydropre_lag 
conpre_lag 

rainpre_lag <- rainpre_lag*-1
temppre_lag <- temppre_lag *-1
hydropre_lag <- hydropre_lag *-1
conpre_lag <- conpre_lag *-1

hydro_train[,hydropre_lag]
consumption_train[,conpre_lag]
temp_train[,temppre_lag]
rain_train[,conpre_lag]

testmodel <- lm(X_t[1:(length(X_t)-lagmaks-1)]~-1+price_train[,c(1,2, 3,  6, 13, 14)]+hydro_train[,c(4)]+consumption_train[,c(1,2,10,11,15,16)]+temp_train[,c(1,17)]+rain_train[,c(2,5,6)])
testmodel <- lm(X_t[1:(length(X_t)-lagmaks)]~-1+datalag)
AIC(testmodel)
testsum <- summary(testmodel)
testsum$coefficients[,4]
goodlag <- c()
pvallag <- sort(testsum$coefficients[,4], decreasing = FALSE) 
for (i in 1:(lagmaks-1)) {
    if (pvallag[i]<0.05) {
      goodlag <- c(goodlag,names(pvallag[i]))
    }
}

sort(goodlag)


dim(price_train)
dim(hydro_train)
length(X_t[1:(length(X_t)-lagmaks)])
for (i in 1:lagtjek) {
  #setup model
  if (i < lagtjek) {
    x_model = x_lag[,-removed_variables[i]]
    ols_adl = lm(X_t[1:(end-lagmaks)] ~ x_model) #this cant run when removed_variables = 0 --> therefore we split into if else statement
  }
  if(i== laqtjek) {
    x_model = x_lag
    ols_adl = lm(X_t[1:(end-lagmaks)] ~ x_model)
  }
  #check the aic and take the best model
  if (AIC(ols_adl) < aic_best) {
    aic_best = AIC(ols_adl)
    best_adl_aic = ols_adl
  }
  
  #check residuals - if amount of insignificant models is higher or the same 
  amount_LjungBox_now = sum(LjungBoxTest(res = residuals(ols_adl), k = lagtjek-length(removed_variables), lag.max = 1500, StartLag = lagtjek-length(removed_variables)+1)[,3]>0.05)
  if (amount_LjungBox_now >= amount_LjungBox_best) {
    if (amount_LjungBox_now == amount_LjungBox_best) {
      if (AIC(ols_adl) < aic_ljungbox_best) {
        aic_ljungbox_best = AIC(ols_adl) 
        best_adl_LjungBox = ols_adl
      }
    }
    else {
      amount_LjungBox_best = amount_LjungBox_now
      best_adl_LjungBox = ols_adl
      #check residuals - if amount of insignificant models is the same, we take the one with the best aic
    }
  }
  
  
  if (i == lagtjek) break
  
  #add the most significant variable:
  most_significant_var = names(sort(summary(ols_all_lags)$coeff[-1,4], decreasing = FALSE)[1]) #we already added intercept and price lag 1
  
  
  
}
summary(best_adl_aic)

# price lag c(1, 3,  6, 9, 13, 14, 19, 22)
#hydro lag c(4)
# con lag c(1,2,10,11,15,16)
# temp lag c(1,2,17,23)
# rain lag c(2,5,6)









