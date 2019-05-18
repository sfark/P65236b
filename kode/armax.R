# plot ccf's #####
plot(tempccf,main="mean Temp ccf")
plot(conccf,main="con ccf")
plot(hydroccf,main="hydro ccf")



# lag temp #####
ccftemplist <- seq(from=-30, to =30, along.with = tempccf[[1]])
meantemp <- cbind(ccftemplist,tempccf[[1]])[1:31,1:2]
lagtemp <- meantemp[which(abs(meantemp[,2])>0.042)]

xregtemp <- c()
for (i in 1:length(lagtemp)) {
  xregtemp <- cbind(xregtemp,stats::lag(as.ts(data_NO1[,4]),k=(lagtemp[i])))
}

colnames(xregtemp) <- c("lag 10","lag 1","lag 0")
# model =ARIMA(3,0,0) with zero mean

# lag con #####
ccfconlist <- seq(from=-30, to =30, length.out = 61)
meancon <- cbind(ccfconlist,conccf[[1]])[1:31,1:2]
lagcon <- meancon[which(abs(meancon[,2])>0.042)]

xregcon <- c()
for (i in 1:length(lagcon)) {
  xregcon <- cbind(xregcon,stats::lag(as.ts(data_NO1[,3]),k=(lagcon[i])))
}


colnames(xregcon) <- c("lag 30","lag 23","lag 22","lag 17","lag 16","lag 11","lag 10","lag 9","lag 4","lag 2","lag 0")
 

# lag hydro #####
ccfhydrolist <- seq(from=-30, to =30, length.out = 61)
meanhydro <- cbind(ccfhydrolist,hydroccf[[1]])[1:31,1:2]
laghydro <- meanhydro[which(abs(meanhydro[,2])>0.042)]

xreghydro <- c()
for (i in 1:length(laghydro)) {
  xreghydro <- cbind(xreghydro,stats::lag(as.ts(data_NO1[,2]),k=(laghydro[i])))
}

colnames(xreghydro) <- c("lag 20","lag 19","lag 16")
 
# samlede xreg ####
xvaribale <- cbind(xregcon,xreghydro,xregtemp,as.ts(WEATHER$Precipitation))[1:2191,]

# model =ARIMA(2,0,0) with zero mean 

# Pris lag model (1,0,2) ####
ar1 <- c()
ar1 <- stats::lag(X_t,k=(-1))

colnames(ar1) <- c("lag 1")


# Pris lag model (3,0.19,4) ####
ar3 <- cbind(stats::lag(X_t,k=(-1)),stats::lag(X_t,k=(-2)),stats::lag(X_t,k=(-3)))

colnames(ar3) <- c("lag 1","lag 2","lag 3")

# ARMA #####
arma1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)), include.mean = F)

rmsearma <- rmse(X_t[31:2191],as.data.frame(fitted.values(arma1_2))[31:2191,1])
plot(as.data.frame(fitted.values(arma1_2))[,1],col="red",type="l");lines(X_t,col="blue")

AIC(arma1_2)#-3898.065
BIC(arma1_2)#-3875.297
rmsearma#0.09931654

# ARFIMA #####
arfima3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)), include.mean = F)

rmsekcarma <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(arfima3_019_4))[31:2191,1])
plot(as.data.frame(fitted.values(arfima3_019_4))[,1],col="red",type="l");lines(X_t,col="blue")


AIC(arfima3_019_4)#-3902.928
BIC(arfima3_019_4)#-3857.391
rmsekcarma#0.0990173
# ARMAX model (1,0,2)####

testxreg <- xvaribale

armax1_0_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg = xvaribale, include.mean = F)

di_ACF <- acf(armax1_0_2$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+ggtitle("ARMAX (1,0,2)")+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)


rmse(X_t,fitted.values(armax1_0_2))
plot(as.data.frame(fitted.values(armax1_0_2)),col="red");lines(X_t,col="blue")
AIC(armax1_0_2)#-3985.678

arma1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)), include.mean = F)

rmse(X_t,fitted.values(arma1_2))
plot(as.data.frame(fitted.values(arma1_2)),col="red");lines(X_t,col="blue")
AIC(arma1_2)#-3898.065


# ARFIMAX model (3,0.19,4)####

arfimax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg = xvaribale, include.mean = F)

rmsekcarmax <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(arfimax3_019_4))[31:2191,1])
plot(as.data.frame(fitted.values(arfimax3_019_4))[,1],col="red",type="l");lines(X_t,col="blue")

  
di_ACF <- acf(arfimax3_019_4$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+ggtitle("ARfiMAX (3,0.19,4)")+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

AIC(arfimax3_019_4)#-4007.554
BIC(arfimax3_019_4)#-3859.918
rmsekcarmax#0.09469012
# arfimax(3,019,4) con sumption xreg  #######
conxreg <-as.data.frame(cbind(xregcon)[1:2191,] )

con_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg <- conxreg, include.mean = F)

conrmse <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(con_armax3_019_4))[31:2191,1])
conrmse #0.09881569
AIC(con_armax3_019_4)#-3941.925
BIC(con_armax3_019_4)#-3729.814
# arfimax(3,019,4)  temp xreg ######### 
tempxreg <-as.data.frame(cbind(xregtemp)[1:2191,] )
temp_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg <- tempxreg, include.mean = F)

temprmse <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(temp_armax3_019_4)[31:2191])[,1])

AIC(temp_armax3_019_4)#-4025.898
BIC(temp_armax3_019_4)
temprmse
# arfima(3,019,4) hydro xreg #####
hydroxreg <-as.data.frame(cbind(xreghydro)[1:2191,] )
hydro_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- hydroxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

hydrormse <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(hydro_armax3_019_4)[31:2191])[,1])

AIC(hydro_armax3_019_4)#-4011.651
BIC(hydro_armax3_019_4)#-3943.367
hydrormse#0.09641103
# arfima(3,019,4) rain xreg #####
rainxreg <-as.ts(WEATHER$Precipitation)
rain_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- rainxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

rainormse <- rmse(frakdiff(X_t,0.19),as.data.frame(fitted.values(rain_armax3_019_4))[,1])

AIC(rain_armax3_019_4)#-3910.419
BIC(rain_armax3_019_4)#-3853.088
rainormse# 0.09866371
# armax(1,0,2) consumption xreg  #######
conxreg <-as.data.frame(cbind(xregcon)[1:2191,] )

con_armax1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg <- conxreg, include.mean = F)
conrmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(con_armax1_2 )[31:2191])[,1])
AIC(con_armax1_2 )#-3827.889
BIC(con_armax1_2 )#-3742.714
conrmse
# armax(1,0,2)   temp xreg ######### 
tempxreg <-as.data.frame(cbind(xregtemp)[1:2191,] )
temp_armax1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg <- tempxreg, include.mean = F)

temprmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(temp_armax1_2 )[31:2191])[,1])

AIC(temp_armax1_2 )#-4015.338
BIC(temp_armax1_2 )#-3975.525
temprmse#0.09608023
# armax(1,0,2)  hydro xreg #####
hydroxreg <-as.data.frame(cbind(xreghydro)[1:2191,] )
hydro_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- hydroxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

hydrormse <- rmse(X_t[31:2191],as.data.frame(fitted.values(hydro_armax1_2 )[31:2191])[,1])

AIC(hydro_armax1_2 )#-3988.19
BIC(hydro_armax1_2 )#-3942.668
hydrormse# 0.09701755
# armax(1,0,2)  rain xreg #####
rainxreg <-as.ts(WEATHER$Precipitation)
rain_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- rainxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

rainormse <- rmse(X_t[31:2191],as.data.frame(fitted.values(rain_armax1_2 )[31:2191])[,1])

AIC(rain_armax1_2)#-3909.191
BIC(rain_armax1_2)#-3875.038
rainormse #0.09898314
# armax(1,0,2)  kitchen snik #####
kcarxreg <-as.ts(WEATHER$Precipitation)
kcar_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- xvaribale, seasonal = list(order = c(0, 0, 0), include.mean = F))

kcarrmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(kcar_armax1_2 )[31:2191])[,1])

AIC(kcar_armax1_2)#-3993.17
BIC(kcar_armax1_2)#-3862.568
kcarrmse #0.09499074
# auto arima med x reg ####
aarmax <- auto.arima(X_t,xreg = as.matrix(testxreg))
rmse(X_t,aarmax$fitted)
plot(aarmax$fitted,col="red",type="l");lines(X_t,col="blue")
acf(aarmax$residuals)
qqnorm(aarmax$residuals)
qqline(aarmax$residuals)
aarmax$coef

AIC(aarmax)

# aic values forloop ARMAX model (1,0,2) #####

rolingxreg <-xvaribale

#for (i in 1:dim(rolingxreg)[1]) {
  for (j in 1:dim(rolingxreg)[2]) {
    if(is.na(rolingxreg[i,j])==TRUE){
      rolingxreg[i,j] <- 0
    }
    
  }


AIClagmatrix <- matrix(data=0,nrow = 18,ncol = 20)
#rownavne <- c("c30","c23","con lag 22","con lag 17","con lag 16","con lag 11" ,"con lag 10","con lag 9","con lag 4","con lag 2","con lag 0","hydro lag 20" ,"hydro lag 19","hydro lag 16","temp lag 10","temp lag 1","temp lag 0","Rain lag 1")
row.names(AIClagmatrix) <- colnames(rolingxreg)
  
for (i in 1:18) {
  AIClagmatrix[i,19] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=rolingxreg[,1:i]))
  AIClagmatrix[i,1:i] <- "x" 
}


AIClagmatrix[2:18,20] <- format(round(diff(as.numeric(AIClagmatrix[,19])), 2), nsmall = 2)
AIClagmatrix[1:18,19] <- format(round(as.numeric(AIClagmatrix[1:18,19]), 2), nsmall = 2)
AIClagmatrix
xtable(AIClagmatrix)

nytestxreg <- rolingxreg[,c(1,which(as.numeric(AIClagmatrix[,20])<0))]

nyAIClagmatrix <- matrix(data=0,nrow = dim(nytestxreg)[2],ncol = (dim(nytestxreg)[2]+2))
#rownavne <- c("c30","c23","con lag 22","con lag 17","con lag 16","con lag 11" ,"con lag 10","con lag 9","con lag 4","con lag 2","con lag 0","hydro lag 20" ,"hydro lag 19","hydro lag 16","temp lag 10","temp lag 1","temp lag 0","Rain lag 1")
row.names(nyAIClagmatrix) <- colnames(nytestxreg)

for (i in 1:7) {
  nyAIClagmatrix[i,8] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=nytestxreg[,1:i]))
  nyAIClagmatrix[i,1:i] <- "x" 
}


nyAIClagmatrix[2:7,9] <- format(round(diff(as.numeric(nyAIClagmatrix[,8])), 2), nsmall = 2)
nyAIClagmatrix[1:7,8] <- format(round(as.numeric(nyAIClagmatrix[1:7,8]), 2), nsmall = 2)
nyAIClagmatrix


for (i in 1:50) {
  set.seed(i)
  rand <- sample(ncol(rolingxreg))
  
}



rolingxreg <-as.data.frame(cbind(xregcon[,11:1],xreghydro[,3:1],xregtemp[,3:1],as.ts(WEATHER$Precipitation))[1:2191,] )
#for (i in 1:dim(rolingxreg)[1]) {
for (j in 1:dim(rolingxreg)[2]) {
  if(is.na(rolingxreg[i,j])==TRUE){
    rolingxreg[i,j] <- 0
  }
  
}


# Best AIC model for ARMAX(1,0,2) #######
#bestem start parameter
startAIClag <- c()

#stokastisk optimering #######
modelsforarmax1_0_2 <- c()
modelsforarmax1_0_2 <-c("15",-4015.749)

names(startAIClag) <- colnames(rolingxreg)
for (i in 1:18) {
  startAIClag[i] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=rolingxreg[,i], include.mean = F))
}

for (j in 1:20) {
  rand <- sample(ncol(rolingxreg))
  modellag <- as.numeric(which.min(startAIClag))# temp lag 0 
  best_aic_1_0_2 <- min(startAIClag)
for (i in rand[rand != modellag]) {
  nyaic <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=rolingxreg[,c(modellag,i)]))
  if(nyaic<best_aic_1_0_2){
    best_aic_1_0_2 <- nyaic
    modellag <- c(modellag,i)
  }else{
    next
  }
}
  modelsforarmax1_0_2 <- rbind(modelsforarmax1_0_2,cbind(toString(modellag),best_aic_1_0_2))
}
AIC(arma1_2)


# en af gangen optimering ######
modellag <- as.numeric(which.min(startAIClag))# temp lag 0 
best_aic_1_0_2 <- min(startAIClag)

parameterantal <- c(1:18)
k <- 1
repeat{
for (i in parameterantal[-modellag]) {
  nyaic <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=rolingxreg[,c(modellag,i)], include.mean = F))
  print(i)
  if(nyaic<best_aic_1_0_2){
    best_aic_1_0_2 <- nyaic
    modellag <- c(modellag,i)
  }else{
    next
  }
}
  k <- k+1
if(k==5){
  break
}
}

# Den bedste model for en ARMAX (1_0_2) "xregtemp[, 3:1].lag 0""xregtemp[, 3:1].lag 1""as.ts(WEATHER$Precipitation)" AIC)-4039.285





# arfimax(3_019_4) beste aic  ####
startAIClag <- c()
for (i in 1:18) {
  startAIClag[i] <- AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=rolingxreg[,i], include.mean = F))
}
names(startAIClag) <- colnames(rolingxreg)

modellagarfimax <- as.numeric(which.min(startAIClag))# temp lag 0 
best_aic_3_019_4 <- min(startAIClag)

parameterantal <- c(1:18)
diff_X_t <- frakdiff(X_t,0.19)
minaiclist <- c()
minaic <- 1
for(j in 1:10){
  minaiclist <- c()
  for  (i in parameterantal[-modellagarfimax]) {
    minaiclist <- c(minaiclist,AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=rolingxreg[,c(modellagarfimax,i)], include.mean = F)))
  }
  if(min(minaiclist)<best_aic_3_019_4){
    modellagarfimax <- c(modellagarfimax,parameterantal[-modellagarfimax][as.numeric(which.min(minaiclist))])
    minaic <- AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=rolingxreg[,modellagarfimax], include.mean = F))
    best_aic_3_019_4 <- minaic
    }
}



colnames(rolingxreg)[modellagarfimax ]
ARFIMAXBESTMOD <- TSA::arima(diff_X_t , order = c(3, 0, 4),xreg=xvaribale[,modellagarmax], include.mean = F)
AIC(ARFIMAXBESTMOD)#-4042.903
BIC(ARFIMAXBESTMOD)#-3968.918
ARFIMAXrmse# 0.09565925
ARFIMAXrmse <- rmse(diff_X_t[31:2191],as.data.frame(fitted.values(ARFIMAXBESTMOD ))[31:2191,1])

# armax(1,2) beste aic  ####
armastartvalues <- c()
startAIClag <- c()
biclagarmax <- c()
rmsearmax <- c()
for (i in 1:18) {
  midmodarmax <- TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,i], include.mean = F)
  startAIClag[i] <- AIC(midmodarmax)
  biclagarmax[i] <- BIC(midmodarmax)
  rmsearmax[i]   <- rmse(X_t[31:2191],as.data.frame(fitted.values(midmodarmax )[31:2191])[,1])
  
}
armastartvalues <- cbind(startAIClag,biclagarmax,rmsearmax)
row.names(armastartvalues) <- colnames(xvaribale)

modellagarmax <- as.numeric(which.min(startAIClag))# temp lag 0 
best_aic_1_0_2 <- min(startAIClag)



parameterantal <- c(1:18)
minaiclistar <- c()
minaicar <- 1
for(j in 1:6){
  minaiclistar <- c()
  for  (i in parameterantal[-modellagarmax]) {
    minaiclistar <- c(minaiclistar,AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=rolingxreg[,c(modellagarmax,i)], include.mean = F)))
  }
  print(cbind(   colnames(rolingxreg)[c(parameterantal[-modellagarmax])],  minaiclistar  ))
  print(cbind(min(minaiclistar),as.numeric(which.min(minaiclistar))))
  print(best_aic_1_0_2)
  if(min(minaiclistar)<best_aic_1_0_2){
    modellagarmax <- c(modellagarmax,parameterantal[-modellagarmax][as.numeric(which.min(minaiclistar))])
    minaicar <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=rolingxreg[,modellagarmax], include.mean = F))
    best_aic_1_0_2 <- minaicar
  }
}
colnames(xvaribale)[modellagarmax]

best_aic_1_0_2

bestarmax <- TSA::arima(X_t, order = c(1, 0, 2),xreg=rolingxreg[,c(modellagarmax)], include.mean = F)
bestarmaxrmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(bestarmax )[31:2191])[,1])

AIC(bestarmax)# -4024.326
BIC(bestarmax)#-3978.826
bestarmaxrmse #0.09584562