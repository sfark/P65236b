end = length(X_t)
lagmaks <- 30

##ændre hydro til Twh
data_NO1[,2] <- data_NO1[,2]/1000

lagseq <- seq(2,lagmaks)
laqmod <- seq(lagmaks,2)
#{
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
#{
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
plot(decompose(ts(data_NO1[,1],frequency = 365))$random)

model1hydro <- glm(data_NO1[,2]~time(dagligpris[,1])+
                     I(time(dagligpris[,1])^2)+
                     cos((2*pi/365)*I(time(dagligpris[,1])))+
                     sin((2*pi/365)*I(time(dagligpris[,1])))+
                     cos((4*pi/365)*I(time(dagligpris[,1])))+
                     sin((4*pi/365)*I(time(dagligpris[,1]))))
summary(model1hydro)

#vores nye tidsserie hydro adjusteret for sæson
hydro_sason <- ts(model1hydro$residuals)
plot.ts(model1hydro$residuals)

AIC(fracdiff((hydro_sason), nar = 28, nma = 28))
fracdiff((data_NO1[,2]/(1000)))

AICMAtrixhydro <-matrix(0,10,10) 
for (i in 1:10) {
  for (j in 1:10) {
    AICMAtrixhydro[i,j] <-  AIC(fracdiff((data_NO1[,2]/(1000)), nar = i, nma = j))
  }
}

which(AICMAtrixhydro==min(AICMAtrixhydro),arr.ind = TRUE)

#Sæson adjusteret data ?????????
model1temp <- glm(data_NO1[,3]~time(dagligpris[,1])+
                     I(time(dagligpris[,1])^2)+
                     cos((2*pi/365)*I(time(dagligpris[,1])))+
                     sin((2*pi/365)*I(time(dagligpris[,1])))+
                     cos((4*pi/365)*I(time(dagligpris[,1])))+
                     sin((4*pi/365)*I(time(dagligpris[,1]))))
summary(model1temp)

#vores nye tidsserie hydro adjusteret for sæson
hydro_sason <- ts(model1hydro$residuals)
temp_sason <- ts(model1temp$residuals)

plot(decompose(ts(data_NO1[,3],frequency = 365))$random)


auto.arima(hydro_sason,max.p = 30,max.q = 30,max.order = 100)#hydro
# ARIMA (5,0,5) for hydro efter sæson adjusteret
#prewhitning forsøg
###################

aa_hydro <- auto.arima(hydro_sason,max.p = 10,max.q = 10,max.order = 100)#hydro
hydro_coef <- aa_hydro$coef[1:10] 
arma55model = aa_hydro
arma55model
hypwxhy=arma55model$residuals
hynewpwyhy = stats::filter(X_t, filter = as.numeric(hydro_coef), sides =1)
ccf(hydro_sason,X_t)
ccf (hynewpwyhy,hypwxhy,na.action=na.omit)
hydropre <- prewhiten(hydro_sason,X_t,x.model=arma55model)


aa_con <- auto.arima(hydro_sason)#consump
consum_coef <- c(1/(-aa_con$coef[5]),aa_con$coef[1]+1/(-aa_con$coef[5]),aa_con$coef[2]+aa_con$coef[1]/(-aa_con$coef[5]),aa_con$coef[3]+aa_con$coef[2]/(-aa_con$coef[5]),aa_con$coef[4]+aa_con$coef[3]/(-aa_con$coef[5]),aa_con$coef[4]/(-aa_con$coef[5]))
arma1model = auto.arima(data_NO1[,3]/(1000))
con_coef <- arma1model$coef[1:5]
conpwx=arma1model$residuals
connewpwy = stats::filter(X_t, filter = as.numeric(con_coef), sides =1)
ccf (connewpwy,conpwx,na.action=na.omit)
prewhiten(data_NO1[,3],X_t,x.model=arma1model)
conpre <- prewhiten(data_NO1[,3],X_t,x.model=arma1model)


aa_temp <- auto.arima(data_NO1[,4])#temp
temp_coef <- c(aa_temp$coef[1:5])
arma5model = auto.arima(data_NO1[,4])
arma5model
temppwx=arma5model$residuals
tempnewpwy = stats::filter(X_t, filter = temp_coef, sides =1)
ccf (tempnewpwy,temppwx,na.action=na.omit)
prewhiten(data_NO1[,4],X_t,x.model=arma5model)
temppre <- prewhiten(data_NO1[,4],X_t,x.model=arma5model,lag.max=365)



aa_rain <- auto.arima(data_NO1[,5])#rain
rain_coef <- aa_rain$coef[1:2]
#rain_coef <- c(-1/(aa_rain$coef[2]),(aa_rain$coef[1]+1)/aa_rain$coef[2],(-aa_rain$coef[1])/aa_rain$coef[2])
arma11model = auto.arima(data_NO1[,5])
arma11model
rainpwx=arma11model$residuals
rainnewpwy = stats::filter(X_t, filter = as.numeric(rain_coef), sides =1)
ccf (rainnewpwy,rainpwx,na.action=na.omit)
prewhiten(data_NO1[,5],X_t,x.model=arma11model)
rainpre <- prewhiten(data_NO1[,5],X_t,x.model=arma11model,lag.max=365)


#################


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
hydroccf <- ccf (hynewpwyhy,hypwxhy,na.action=na.omit)
conccf <- ccf (connewpwy,conpwx,na.action=na.omit)
tempccf <- ccf (tempnewpwy,temppwx,na.action=na.omit)
rainccf <- ccf (rainnewpwy,rainpwx,na.action=na.omit)

rain_lag <- c()
temp_lag <- c()
hydro_lag <- c()
con_lag <- c()

rainpre_lag <- c()
temppre_lag <- c()
hydropre_lag <- c()
conpre_lag <- c()
laqseq <- seq(-30,-1)
### tjekker for signifikante lags
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
  
  
  if (rainccf[laqseq[i]]< -0.05 || rainccf[laqseq[i]]> 0.05) {
    rain_lag <- c(rain_lag,laqseq[i])
  }
  if (tempccf[laqseq[i]]< -0.05 || tempccf[laqseq[i]]> 0.05) {
    temp_lag <- c(temp_lag,laqseq[i])
  }
  if (hydroccf[laqseq[i]]< -0.05 || hydroccf[laqseq[i]]> 0.05) {
    hydro_lag <- c(hydro_lag,laqseq[i])
  }
  if (conccf[laqseq[i]]< -0.05 || conccf[laqseq[i]]> 0.05) {
    con_lag <- c(con_lag,laqseq[i])
  }
}

rain_lag <- rain_lag*-1
temp_lag <- temp_lag *-1
hydro_lag <- hydro_lag *-1
con_lag <- con_lag *-1

rainpre_lag <- rainpre_lag*-1
temppre_lag <- temppre_lag *-1
hydropre_lag <- hydropre_lag *-1
conpre_lag <- conpre_lag *-1

rainpre_lag
temppre_lag 
hydropre_lag 
conpre_lag 

rain_lag 
temp_lag
hydro_lag
con_lag


hydro_train[,hydropre_lag]
consumption_train[,conpre_lag]
temp_train[,temppre_lag]
rain_train[,conpre_lag]

Premodel <- lm(X_t[1:(length(X_t)-lagmaks-1)]~-1+price_train[,c(1,2, 3,  6, 13, 14)]+consumption_train[,c(conpre_lag )]+temp_train[,c(temppre_lag)]+rain_train[,c(rainpre_lag)])
Pennmodel <- lm(X_t[1:(length(X_t)-lagmaks-1)]~-1+price_train[,c(1,2, 3,  6, 13, 14,21,22)]+hydro_train[,c(hydro_lag)]+consumption_train[,c(con_lag)]+temp_train[,c(temp_lag)]+rain_train[,c(rain_lag)])
AIC(Pennmodel)
AIC(Premodel)
summary(Premodel)
summary(Pennmodel)


#model
###############
#future xreg
data_2019_files <- list.files("2019data", full.names = 1)

PRICES_2019 <- read.csv2(data_2019_files[2], header = TRUE)[1:110,10]
PRICES_2019 <- log(PRICES_2019)

# fjern sæson i pris 

dato19 <- seq(ISOdate(2019,1,1),by="day", length.out = 110)

helligedage19 <- c("2019-01-01 12:00:00 GMT","2019-04-18 12:00:00 GMT","2019-04-19 12:00:00 GMT","2019-04-22 12:00:00 GMT")
dummy_week19 <-  rep(c(0,0,0,0,0,1,1),16)[1:110]
helligedage19 <-  as.POSIXct(strptime(helligedage19, format = "%Y-%m-%d %H:%M:%S", "GMT"))

dato2019 <- strptime(dato19, format = "%Y-%m-%d %H:%M:%S", "GMT")
dummyhelligdage19 <- numeric(length = length(dato19))
dummyhelligdage19[match(helligedage19,dato19)] <- 1
dummyhelligweekend19 <- dummyhelligdage19+dummy_week19

<<<<<<< HEAD
=======

>>>>>>> ab3d13251a6141dc9a205206406caca7b0b452f3
model19 <- glm(PRICES_2019~time(dato19)+
                I(time(dato19)^2)+
                cos((2*pi/365)*I(time(dato19)))+
                sin((2*pi/365)*I(time(dato19)))+
                cos((4*pi/365)*I(time(dato19)))+
                sin((4*pi/365)*I(time(dato19)))+dummyhelligweekend19)
summary(model19)
PRICES_2019SA <- ts(model19$residuals)




# opstiller data for 2019
HYDRO_2019 <- read.csv2(data_2019_files[3], header = TRUE)[1:15,2]
Hydrprep <- (rep(HYDRO_2019,each = 7,length.out=110))/1000
CONSUMPTION_2019 <- read.csv2(data_2019_files[1], header = TRUE,skip = 2)[1:110,2]
WEATHER_2019 <- read.csv2(data_2019_files[4], header = TRUE)[1:110,c(3:4)]
WEATHER_2019[,1] <- as.numeric(WEATHER_2019)
Data2019 <- cbind(PRICES_2019SA[1:110],Hydrprep,CONSUMPTION_2019/1000,WEATHER_2019[,1],WEATHER_2019[3:112,1],WEATHER_2019[,2])
#Data2019 <- cbind(Hydrprep,CONSUMPTION_2019/1000,WEATHER_2019[,1],WEATHER_2019[3:112,1],WEATHER_2019[,2])

xmod <- forecast::Arima(X_t[1:(length(X_t)-(lagmaks+1))], order=c(0,0,2),xreg = cbind(ts(X_t[2:(length(X_t)-(lagmaks))]),hydro_train[,1],consumption_train[,1]/1000,temp_train[,c(1,3)],rain_train[,1]))
#xmod <- forecast::Arima(X_t[1:(length(X_t)-(lagmaks+1))], order=c(1,0,2),xreg = cbind(hydro_train[,1],consumption_train[,1],temp_train[,c(1,3)],rain_train[,1]))
forecasttest <- forecast(xmod,xreg = Data2019, level=95)
plot(forecast(xmod,xreg = Data2019, level=95),xlim=c(2100,length(c(X_t[1:(length(X_t)-lagmaks-1)],Data2019[,1]))))

fore <- predict(xmod, n.ahead = 110, newxreg = Data2019,se.fit = TRUE)


#plot
ggplot(data=data.frame(X1 = fore$pred[1:109], X2 = dato19[1:109]), aes(y=X1, x=X2)) +
  geom_line(aes(col = "Forecastet values")) +
  geom_ribbon(aes(ymin =  fore$pred[1:109]- fore$se[1:109], ymax = fore$pred[1:109] + fore$se[1:109]), alpha = 0.2) +
  geom_line(data=data.frame(X1=c(X_t[2150:2191],PRICES_2019SA[1:109]),X2 = c(dato[2150:2191],dato19[1:109])), aes(col = "Observed season adjusted values")) + 
  scale_colour_manual(values = c("Black","yellow"))+xlab("Time 2019")+ylab("Season adjusted Price")+
  ggtitle("ARMAX Forecast")+ 
  theme(legend.title = element_blank())+ 
  coord_cartesian(xlim = c(dato[2180:2191],dato19[1:49]), ylim = c(-100, 230))

rmse(PRICES_2019SA[1:108],fore$pred[1:108])
### cross validation
tsCV(PRICES_2019SA[1:109],forecasttest,h=1)

##################
## tjekker order

armaxAICtest <- matrix(nrow = 10,ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    armaxAICtest[i,j] <- AIC(forecast::Arima(X_t[1:(length(X_t)-(lagmaks+1))], order=c(i,0,j),xreg = cbind(hydro_train[,c(hydro_lag)],consumption_train[,c(con_lag)],temp_train[,c(temp_lag)],rain_train[,c(rain_lag)])))

  }
}


which.min(armaxaic[1:8,1:10])
(sarima(X_t[1:(length(X_t)-(lagmaks+1))], p=1,d=0,q=2,xreg = cbind(hydro_train[,c(hydro_lag)],consumption_train[,c(con_lag)],temp_train[,c(temp_lag)],rain_train[,c(rain_lag)])))



###########################################

# # Definer model
# 70 vindlmodel = Arima ( vindfrac , order = c(0 ,0 ,1) , include . mean = FALSE )
# 71
# 72 # Gør spotpriserne stationære
# 73 fracspot = ts( frakdiff (ts( daily . spot . prices $DK1 [’/ 2017 -12 -31 ’]) ,0.2244354) )
# 74 spot = diff ( diff ( fracspot ,7) )
# 75
# 76 # prewhite spot og vind
# 77 spot . pw = resid ( Arima ( spot , model = vindlmodel ) )
# 78 vind . pw = resid ( vindlmodel )

aa_temp <- auto.arima(data_NO1[,4])#temp
temp_coef <- c(aa_temp$coef[1:5])
arma5model = auto.arima(data_NO1[,4])
arma5model
temppwx=arma5model$residuals
tempnewpwy = stats::filter(X_t, filter = temp_coef, sides =1)
ccf (tempnewpwy,temppwx,na.action=na.omit)
prewhiten(data_NO1[,4],X_t,x.model=arma5model)
temppre <- prewhiten(data_NO1[,4],X_t,x.model=arma5model,lag.max=365)




########################################
ccf (hynewpwyhy,hypwxhy,na.action=na.omit)
ccf (connewpwy,conpwx,na.action=na.omit)
ccf (tempnewpwy,temppwx,na.action=na.omit)
ccf (rainnewpwy,rainpwx,na.action=na.omit)


p1 <- ggCcf(hynewpwyhy,hypwxhy, lag.max = 30, type = "correlation",plot = TRUE)+ggtitle("Hydro Reservoir Level")
p2 <- ggCcf(connewpwy,conpwx, lag.max = 30, type = "correlation",plot = TRUE)+ggtitle("Consumption")
p3 <- ggCcf(tempnewpwy,temppwx, lag.max = 30, type = "correlation",plot = TRUE)+ggtitle("Mean Temperature")
p4 <- ggCcf(rainnewpwy,rainpwx, lag.max = 30, type = "correlation",plot = TRUE)+ggtitle("Precipitation")

multiplot(p1, p4, p2, p3, cols=2) 



###################################
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

#lag function STATS::Lag
##################

lagmaks <- 30
laggedHydro <- matrix(0,2191,lagmaks)
laggedCon <- matrix(0,2191,lagmaks)
laggedTemp <- matrix(0,2191,lagmaks)
laggedRain <- matrix(0,2191,lagmaks)

for (i in 1:lagmaks) {
  laggedHydro[,i] <- stats::lag(ts(data_NO1[,2]),k=i)
  laggedCon[,i] <- stats::lag(ts(data_NO1[,3]),k=i)
  laggedTemp[,i] <- stats::lag(ts(data_NO1[,4]),k=i)
  laggedRain[,i] <- stats::lag(ts(data_NO1[,5]),k=i)
}






