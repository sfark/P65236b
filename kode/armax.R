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
#model ARIMA(2,1,1) 

# lag hydro #####
ccfhydrolist <- seq(from=-30, to =30, length.out = 61)
meanhydro <- cbind(ccfhydrolist,hydroccf[[1]])[1:31,1:2]
laghydro <- meanhydro[which(abs(meanhydro[,2])>0.042)]

xreghydro <- c()
for (i in 1:length(laghydro)) {
  xreghydro <- cbind(xreghydro,stats::lag(as.ts(data_NO1[,2]),k=(laghydro[i])))
}

colnames(xreghydro) <- c("lag 20","lag 19","lag 16")
xreghydro 
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

# ARMAX model (1,0,2)####

testxreg <- xvaribale

armax1_0_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg = testxreg, include.mean = F)

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


# ARMAX model (3,0.19,4)####

testxreg <-as.data.frame(cbind(xregcon,xreghydro,xregtemp)[1:2191,] )

armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg <- testxreg, include.mean = F)

rmse(frakdiff(X_t,0.19),as.data.frame(fitted.values(armax3_019_4))[,1])
plot(as.data.frame(fitted.values(armax3_019_4))[,1],col="red",type="l");lines(X_t,col="blue")

  
di_ACF <- acf(armax3_019_4$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+ggtitle("ARMAX (3,0.19,4)")+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

AIC(armax3_019_4)#-3986.379
# arfimax(3,019,4) con sumption xreg  #######
conxreg <-as.data.frame(cbind(xregcon)[1:2191,] )

con_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg <- conxreg, include.mean = F)
conrmse <- rmse(frakdiff(X_t,0.19),as.data.frame(fitted.values(con_armax3_019_4))[,1])
AIC(con_armax3_019_4)#-3941.925
# arfimax(3,019,4)  temp xreg ######### 
tempxreg <-as.data.frame(cbind(xregtemp)[1:2191,] )
temp_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg <- tempxreg, include.mean = F)

temprmse <- rmse(frakdiff(X_t,0.19),as.data.frame(fitted.values(temp_armax3_019_4))[,1])

AIC(temp_armax3_019_4)#-4025.898
# arfima(3,019,4) hydro xreg #####
hydroxreg <-as.data.frame(cbind(xreghydro)[1:2191,] )
hydro_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- hydroxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

hydrormse <- rmse(frakdiff(X_t,0.19),as.data.frame(fitted.values(temp_armax3_019_4))[,1])

AIC(hydro_armax3_019_4)#-3897.392
# arfima(3,019,4) rain xreg #####
rainxreg <-as.ts(WEATHER$Precipitation)
rain_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- rainxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

rainormse <- rmse(frakdiff(X_t,0.19),as.data.frame(fitted.values(rain_armax3_019_4))[,1])

AIC(rain_armax3_019_4)#-3897.392
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

rolingxreg <-as.data.frame(cbind(xregcon[,11:1],xreghydro[,3:1],xregtemp[,3:1],as.ts(WEATHER$Precipitation))[1:2191,] )
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





# armax(3_019_4) beste aic  ####
startAIClag <- c()
#rolingxreg <- cbind(xreghydro,xregtemp,xregcon,as.ts(WEATHER$Precipitation))
rolingxreg <-as.data.frame(cbind(xreghydro[,3:1],xregtemp[,3:1],as.ts(WEATHER$Precipitation),xregcon[,11:1])[1:2191,] )
for (i in 1:18) {
  startAIClag[i] <- AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=rolingxreg[,i], include.mean = F))
}
names(startAIClag) <- colnames(rolingxreg)

modellag <- as.numeric(which.min(startAIClag))# temp lag 0 
best_aic_1_0_2 <- min(startAIClag)

parameterantal <- c(1:18)
k <- 1
repeat{
  for (i in parameterantal[-modellag]) {
    nyaic <- AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=rolingxreg[,c(modellag,i)], include.mean = F))
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

colnames(rolingxreg)[modellag]

best_aic_1_0_2












