end = length(X_t)
lagmaks <- 25


lagseq <- seq(1,lagmaks)
laqmod <- seq(lagmaks,1)
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


for (i in 1:(lagmaks)) {
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
par(mfrow=c(1,1))
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
arma55model = auto.arima(hydro_sason,max.p = 10,max.q = 10,max.order = 100)
arma55model
pwx=arma55model$residuals
newpwy = stats::filter(X_t, filter = as.numeric(hydro_coef), sides =1)
ccf (newpwy,pwx,na.action=na.omit,lag.max = 300)



aa_con <- auto.arima(hydro_sason)#consump
consum_coef <- c(1/(-aa_con$coef[5]),aa_con$coef[1]+1/(-aa_con$coef[5]),aa_con$coef[2]+aa_con$coef[1]/(-aa_con$coef[5]),aa_con$coef[3]+aa_con$coef[2]/(-aa_con$coef[5]),aa_con$coef[4]+aa_con$coef[3]/(-aa_con$coef[5]),aa_con$coef[4]/(-aa_con$coef[5]))
arma1model = auto.arima(data_NO1[,3]/(1000))
arma1model
pwx=arma1model$residuals
newpwy = stats::filter(X_t, filter = as.numeric(consum_coef), sides =1)
ccf (newpwy,pwx,na.action=na.omit)


aa_temp <- auto.arima(data_NO1[,4])#temp
temp_coef <- c(aa_temp$coef[1:5])
arma5model = auto.arima(data_NO1[,4])
arma5model
pwx=arma5model$residuals
newpwy = stats::filter(X_t, filter = as.numeric(temp_coef), sides =1)
ccf (newpwy,pwx,na.action=na.omit)

aa_rain <- auto.arima(data_NO1[,5])#rain
rain_coef <- c(-1/(aa_rain$coef[2]),(aa_rain$coef[1]+1)/aa_rain$coef[2],(-aa_rain$coef[1])/aa_rain$coef[2])
arma11model = auto.arima(data_NO1[,5])
arma11model
pwx=arma11model$residuals
newpwy = stats::filter(X_t, filter = as.numeric(rain_coef), sides =1)
ccf (newpwy,pwx,na.action=na.omit)


# con lag 2,3,4,5
# temp lag 1, 2, 3, 4, 5
# rain lag 1

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













