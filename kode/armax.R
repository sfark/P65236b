# plot ccf's #####
plot(tempccf,main="mean Temp ccf")
plot(conccf,main="con ccf")
plot(hydroccf,main="hydro ccf")
myrmse <- function(x){
  rmse(exp(X_t[31:2191]+fitted.values(model1)[31:2191]),exp(fitted.values(x)[31:2191]+fitted.values(model1)[31:2191]))
}
nymyrmse <- function(x){
  rmse(exp(fitted.values(x)[31:2191]+fitted.values(model1)[31:2191]) ,data_NO1[31:2191,1])
}

meanrmse <-   rmse(exp(X_t+fitted.values(model1)),exp(mean(X_t)+fitted.values(model1)))



myrmsefi <- function(x){
  rmse(data_NO1[31:2191,1],exp(frakdiff(fitted.values(x)[31:2191],-0.19)+fitted.values(model1)[31:2191]))
}
myrmsefi(ARFIMAXBESTMOD)#26.87611

# lag temp #####
#ccftemplist <- seq(from=-30, to =30, along.with = tempccf[[1]])
#meantemp <- cbind(ccftemplist,tempccf[[1]])[1:31,1:2]
#lagtemp <- meantemp[which(abs(meantemp[,2])>0.042)]
lagtemp <- c(-10,-1,0)
xregtemp <- c()
for (i in 1:length(lagtemp)) {
  xregtemp <- cbind(xregtemp,stats::lag(as.ts(data_NO1[,4]),k=(lagtemp[i])))
}
xregtemp <- xregtemp[1:2191,]
colnames(xregtemp) <- c("Mean Temp lag 10","Mean Temp lag 1","Mean Temp lag 0")
# model =ARIMA(3,0,0) with zero mean

# lag con #####
#ccfconlist <- seq(from=-30, to =30, length.out = 61)
#meancon <- cbind(ccfconlist,conccf[[1]])[1:31,1:2]
#lagcon <- meancon[which(abs(meancon[,2])>0.042)]
lagcon <- c(-30 ,-23, -22 ,-17 ,-16 ,-11 ,-10 , -9 , -4 , -2 ,  0)
xregcon <- c()
for (i in 1:length(lagcon)) {
  xregcon <- cbind(xregcon,stats::lag(as.ts(data_NO1[,2]),k=(lagcon[i])))
}


colnames(xregcon) <- c("Consumption lag 30","Consumption lag23","Consumption lag 22" ,"Consumption lag 17" ,"Consumption lag 16" ,"Consumption lag 11","Consumption lag 10","Consumption lag 9","Consumption lag 4","Consumption lag 2","Consumption lag 0")


# lag hydro #####
# ccfhydrolist <- seq(from=-30, to =30, length.out = 61)
# meanhydro <- cbind(ccfhydrolist,hydroccf[[1]])[1:31,1:2]
# laghydro <- meanhydro[which(abs(meanhydro[,2])>0.042)]
# 
# xreghydro <- c()
# for (i in 1:length(laghydro)) {
#   xreghydro <- cbind(xreghydro,stats::lag(as.ts(data_NO1[,2]),k=(laghydro[i])))
# }
xreghydro <- cbind(stats::lag(as.ts(data_NO1[,3]),k=(-20)),stats::lag(as.ts(data_NO1[,3]),k=(-19)),stats::lag(as.ts(data_NO1[,3]),k=(-16)),as.ts(data_NO1[,3]))[1:2191,1:3]
colnames(xreghydro) <-  c("Hydro lag 20 ","Hydro lag 19","Hydro lag 16")
 
# lag rain #####
xregrain <- as.data.frame(cbind(stats::lag(as.ts(WEATHER$Precipitation),k=-1),as.ts(WEATHER$Precipitation)))
names(xregrain) <- c("R1","R0")
xregrain <- xregrain[,1]
# samlede xreg ####
xvaribale <- cbind((xregcon)[1:2191,],xreghydro[1:2191,],xregtemp[1:2191,],xregrain[1:2191])
colnames(xvaribale) <- c("Consumption lag 30","Consumption lag23","Consumption lag 22" ,"Consumption lag 17" ,"Consumption lag 16" ,"Consumption lag 11","Consumption lag 10","Consumption lag 9","Consumption lag 4","Consumption lag 2","Consumption lag 0","Hydro lag 20 ","Hydro lag 19","Hydro lag 16","Mean Temp lag 10","Mean Temp lag 1","Mean Temp lag 0","Precipitation lag 1")

# model =ARIMA(2,0,0) with zero mean 


# ARFIMAX#####
{
# ARFIMA #####
arfima3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), include.mean = F)

rmsekcarma <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(arfima3_019_4))[31:2191,1])
plot(as.data.frame(fitted.values(arfima3_019_4))[,1],col="red",type="l");lines(X_t,col="blue")


stats::AIC(arfima3_019_4)#-3902.928
stats::BIC(arfima3_019_4)#-3857.391
myrmsefi(arfima3_019_4)
rmsekcarma#0.0990173
ljbox <-LjungBoxTest(arfima3_019_4$residuals[31:2191],k=7)

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
  geom_point()+
  geom_hline(yintercept = 0.05,col="blue")+
  xlab("Lag")+
  xlim(c(7,30))+
  ylab("P-Value")


df <- data.frame(arfima3_019_4 $residuals)
p <- ggplot(df, aes(sample = arfima3_019_4$residuals))
p + stat_qq() + stat_qq_line()
# ARFIMAX model (3,0.19,4)####

arfimax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg = xvaribale, include.mean = F)

rmsekcarmax <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(arfimax3_019_4))[31:2191,1])
plot(as.data.frame(fitted.values(arfimax3_019_4))[,1],col="red",type="l");lines(X_t,col="blue")

  
di_ACF <- acf(arfimax3_019_4$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+ggtitle("ARfiMAX (3,0.19,4)")+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

stats::AIC(arfimax3_019_4)#-4007.554
stats::BIC(arfimax3_019_4)#-3859.918
rmsekcarmax#0.09469012
myrmsefi(arfimax3_019_4 )
ljbox <- LjungBoxTest(arfimax3_019_4$residuals[31:2191])

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
  geom_point()+
  geom_hline(yintercept = 0.05,col="blue")+
  xlab("Lag")+
  xlim(c(7,30))+
  ylab("P-Value")

df <- data.frame(arfimax3_019_4 $residuals)
p <- ggplot(df, aes(sample = arfimax3_019_4$residuals))
p + stat_qq() + stat_qq_line()


# arfimax(3,019,4) con sumption xreg  #######
conxreg <-as.data.frame(cbind(xregcon)[1:2191,] )

con_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- conxreg, include.mean = F)

conrmse <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(con_armax3_019_4))[31:2191,1])
conrmse #0.09881569
stats::AIC(con_armax3_019_4)#-3941.925
stats::BIC(con_armax3_019_4)#-3834.009
myrmsefi(con_armax3_019_4 )#28.98416
# arfimax(3,019,4)  temp xreg ######### 
tempxreg <-as.data.frame(cbind(xregtemp)[1:2191,] )
temp_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- tempxreg, include.mean = F)

temprmse <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(temp_armax3_019_4)[31:2191])[,1])

stats::AIC(temp_armax3_019_4)#-4025.898
stats::BIC(temp_armax3_019_4)
myrmsefi(temp_armax3_019_4)#28.26639
temprmse
# arfimax(3,019,4) hydro xreg #####
hydroxreg <-as.data.frame(cbind(xreghydro)[1:2191,] )
hydro_arfimax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg = (xreghydro)[1:2191,], include.mean = F)

hydrormse <- rmse(frakdiff(X_t[31:2191],0.19),as.data.frame(fitted.values(hydro_arfimax3_019_4)[31:2191])[,1])

stats::AIC(hydro_arfimax3_019_4)#-4011.651
stats::BIC(hydro_arfimax3_019_4)#-3943.367
hydrormse#0.09641103
myrmsefi(hydro_arfimax3_019_4)#28.10398
# arfimax(3,019,4) rain xreg #####
rainxreg <-stats::lag(as.ts(WEATHER$Precipitation),k=-1)
rain_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg = rainxreg, include.mean = F)

rainormse <- rmse(frakdiff(X_t,0.19),as.data.frame(fitted.values(rain_armax3_019_4))[,1])

stats::AIC(rain_armax3_019_4)#-3912.016
stats::BIC(rain_armax3_019_4)# -3860.787
rainormse# 0.09866371
myrmsefi(rain_armax3_019_4)#28.86602
# arfimax(3_019_4) beste aic  ####

ARFIMAEcoef <- TSA::arima(diff_X_t , order = c(3, 0, 4), include.mean = F)
arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,NA)

arfimastartvalues <- c()
startAIClag <- c()
biclagarfimax <- c()
rmsearfimax <- c()
for (i in 1:18) {
  midmodarfimax <- TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribale[,i], include.mean = F)
  startAIClag[i] <- stats::AIC(midmodarfimax)
  biclagarfimax[i] <- stats::BIC(midmodarfimax)
  rmsearfimax[i]   <- myrmsefi(midmodarfimax)
  
}
arfimaxstartlagtable <- cbind(colnames(xvaribale),startAIClag)
arfimaxstartlagtable
arfimastartvalues <- cbind(startAIClag,biclagarfimax,rmsearfimax)
row.names(arfimastartvalues) <- colnames(xvaribale)
arfimastartvalues
modellagarfimax <- as.numeric(which.min(startAIClag))# temp lag 0
best_aic_3_019_4 <- min(startAIClag)

parameterantal <- c(1:18)
diff_X_t <- frakdiff(X_t,0.19)
minaiclist <- c()
minaic <- 1
for(j in 1:10){
  minaiclist <- c()
  for  (i in parameterantal[-modellagarfimax]) {
    arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax,i))))
    minaiclist <- c(minaiclist,stats::AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribale[,c(modellagarfimax,i)], include.mean = F)))
  }
  print("#######")
  print(best_aic_3_019_4)
  print(colnames(xvaribale)[as.numeric(which.min(minaiclist))])
  print(min(minaiclist))
  if(min(minaiclist)<best_aic_3_019_4){
    modellagarfimax <- c(modellagarfimax,parameterantal[-modellagarfimax][as.numeric(which.min(minaiclist))])
    arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax))))
    minaic <- stats::AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribale[,modellagarfimax], include.mean = F))
    best_aic_3_019_4 <- minaic
  }
}

diff_X_t <- frakdiff(X_t,0.19)
modellagarfimax <- c(17,18,11,16)
colnames(xvaribale)[modellagarfimax ]
arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax))))

ARFIMAXBESTMOD <- TSA::arima(diff_X_t , order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribale[,modellagarfimax], include.mean = F)
stats::AIC(ARFIMAXBESTMOD)#-4043.295
stats::BIC(ARFIMAXBESTMOD)#-3974.996
myrmsefi(ARFIMAXBESTMOD)#28.25779

scale_con <- cbind(xvaribale[,modellagarfimax][1],xvaribale[,modellagarfimax][2],xvaribale[,modellagarfimax][3]/10000,xvaribale[,modellagarfimax][4])
sarfimax <- sarima(diff_X_t, 3, 0, 4, xreg=xvaribale[,modellagarfimax])
sarfimax_sc <- sarima(diff_X_t, 3, 0, 4, xreg=scale_con)
scalecontablelatex <- xtable(sarfimax_sc$ttable)
contablelatex <- xtable(sarfimax$ttable)
scalecontablelatex
contablelatex 

ljbox <-LjungBoxTest(ARFIMAXBESTMOD$residuals[10:2191],k=7)

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
     geom_point()+
     geom_hline(yintercept = 0.05,col="blue")+
     xlab("Lag")+
     xlim(c(7,30))+
     ylab("P-Value")
 


df <- data.frame(ARFIMAXBESTMOD$residuals)
p <- ggplot(df, aes(sample = ARFIMAXBESTMOD$residuals))
p + stat_qq() + stat_qq_line()

# ARFIMAX BEST model BIC ##########

best_Bic_3_019_4 <- min(arfimastartvalues[,2])
modellagarfimax <- as.numeric(which.min(arfimastartvalues[,2]))
parameterantal <- c(1:18)
diff_X_t <- frakdiff(X_t,0.19)
minbiclist <- c()
minbic <- 1
for(j in 1:10){
  minbiclist <- c()
  for  (i in parameterantal[-modellagarfimax]) {
    minbiclist <- c(minbiclist,stats::BIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=xvaribale[,c(modellagarfimax,i)], include.mean = F)))
  }
  if(min(minbiclist)<best_Bic_3_019_4){
    modellagarfimax <- c(modellagarfimax,parameterantal[-modellagarfimax][as.numeric(which.min(minbiclist))])
    minbic <- stats::BIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=xvaribale[,modellagarfimax], include.mean = F))
    best_Bic_3_019_4 <- minbic
  }
}



# ARFIMAX kitchensink#########

ARFIMAXKS <- TSA::arima(diff_X_t , order = c(3, 0, 4),xreg=xvaribale, include.mean = F)
stats::AIC(ARFIMAXKS)#-4040.281
stats::BIC(ARFIMAXKS)#-3971.981
myrmsefi(ARFIMAXKS)#27.74125

ljbox <-LjungBoxTest(ARFIMAXKS$residuals[31:2191],k=7)

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
  geom_point()+
  geom_hline(yintercept = 0.05,col="blue")+
  xlab("Lag")+
  xlim(c(7,30))+
  ylab("P-Value")



# ARFIMAX best mod ljungbox plot #####
ljbox <-LjungBoxTest(ARFIMAXBESTMOD$residuals[31:2191],k=7)

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
  geom_point()+
  geom_hline(yintercept = 0.05,col="blue")+
  xlab("Lag")+
  ylab("P-Value")+
  xlim(c(7,30))

}
# ARMAX#####
{
# ARMA #####
arma1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)), include.mean = F)

rmsearma <- rmse(exp(X_t[31:2191]+fitted.values(model1)[31:2191]),
                 exp(fitted.values(arma1_2)[31:2191]+fitted.values(model1)[31:2191]))
myrmse(arma1_2)

plot(as.data.frame(fitted.values(arma1_2))[,1],col="red",type="l");lines(X_t,col="blue")

stats::AIC(arma1_2)#-3898.065
stats::BIC(arma1_2)#-3875.297
rmsearma#0.09931654

ljbox <-LjungBoxTest(arma1_2$residuals,k=3,StartLag = 1)

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
  geom_point()+
  geom_hline(yintercept = 0.05,col="blue")+
  xlab("Lag")+
  ylab("P-Value")+xlim(c(4,30))

# GGPLOT ARMA#########

arma1_2$residuals

df <- data.frame(arma1_2$residuals)
p <- ggplot(df, aes(sample = y))
p + stat_qq() + stat_qq_line()




# ARMAX model (1,0,2)####

testxreg <- xvaribale

armax1_0_2 <- TSA::arima(X_t,order=c(1,0,2),xreg = xvaribale, include.mean = F)

di_ACF <- acf(armax1_0_2$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+ggtitle("ARMAX (1,0,2)")+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)


plot(as.data.frame(fitted.values(armax1_0_2)),col="red");lines(X_t,col="blue")
stats::AIC(armax1_0_2)#-3976.044
stats::BIC(armax1_0_2)#-3851.121
rmse(X_t[31:2191],fitted.values(armax1_0_2)[31:2191])
myrmse(armax1_0_2)
arma1_2 <- TSA::arima(X_t,order=c(1,0,2), include.mean = F)


plot(as.data.frame(fitted.values(arma1_2)),col="blue");lines(X_t,col="yellow")
stats::AIC(arma1_2)#-3898.065
stats::BIC(arma1_2)#-3875.297
rmse(X_t,fitted.values(arma1_2))# 0.09918506

ljbox <-LjungBoxTest(armax1_0_2 $residuals[1:2191],k=3)

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
  geom_point()+
  geom_hline(yintercept = 0.05,col="blue")+
  xlab("Lag")+
  xlim(c(4,30))+
  ylab("P-Value")

df <- data.frame(armax1_0_2 $residuals)
p <- ggplot(df, aes(sample = armax1_0_2 $residuals))
p + stat_qq() + stat_qq_line()

# armax(1,0,2) consumption xreg  #######
conxreg <-as.data.frame(cbind(xregcon)[1:2191,] )

con_armax1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg <- conxreg, include.mean = F)
conrmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(con_armax1_2 )[31:2191])[,1])
stats::AIC(con_armax1_2 )#-3905.93
stats::BIC(con_armax1_2 )#-3820.755
conrmse#0.09728524
myrmse(con_armax1_2)
nymyrmse(con_armax1_2)
# armax(1,0,2)   temp xreg ######### 
tempxreg <-as.data.frame(cbind(xregtemp)[1:2191,] )
temp_armax1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg <- tempxreg, include.mean = F)

temprmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(temp_armax1_2 )[31:2191])[,1])

stats::AIC(temp_armax1_2 )#-4015.338
stats::BIC(temp_armax1_2 )#-3975.525
temprmse#0.09608023
myrmse(temp_armax1_2 )
# armax(1,0,2)  hydro xreg #####
hydroxreg <-as.data.frame(cbind(xreghydro)[1:2191,] )
hydro_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- hydroxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

hydrormse <- rmse(X_t[31:2191],as.data.frame(fitted.values(hydro_armax1_2 )[31:2191])[,1])

stats::AIC(hydro_armax1_2 )#-3883.235
stats::BIC(hydro_armax1_2 )#-3837.713
hydrormse# 0.09925392
myrmse(hydro_armax1_2)
# armax(1,0,2)  rain xreg #####
rain_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- xvaribale[,18], seasonal = list(order = c(0, 0, 0), include.mean = F))

rainormse <- rmse(X_t[31:2191],as.data.frame(fitted.values(rain_armax1_2 )[31:2191])[,1])

stats::AIC(rain_armax1_2)#-3891.654
stats::BIC(rain_armax1_2)#-3857.504
rainormse #0.09931375
myrmse(rain_armax1_2)
# armax(1,0,2)  kitchen snik #####
kcar_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg = xvaribale, include.mean = F)

ljbox <-LjungBoxTest(kcar_armax1_2$residuals[31:2191],k=3)
ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
     geom_point()+
     geom_hline(yintercept = 0.05,col="blue")+
     xlab("Lag")+
     xlim(c(4,30))+
     ylab("P-Value")


kcarrmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(kcar_armax1_2 )[31:2191])[,1])

stats::AIC(kcar_armax1_2)#-3974.65
stats::BIC(kcar_armax1_2)#-3844.049
kcarrmse #0.09499074
myrmse(kcar_armax1_2)
{
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

xvaribale <-xvaribale

#for (i in 1:dim(xvaribale)[1]) {
  for (j in 1:dim(xvaribale)[2]) {
    if(is.na(xvaribale[i,j])==TRUE){
      xvaribale[i,j] <- 0
    }
    
  }


AIClagmatrix <- matrix(data=0,nrow = 18,ncol = 20)
#rownavne <- c("c30","c23","con lag 22","con lag 17","con lag 16","con lag 11" ,"con lag 10","con lag 9","con lag 4","con lag 2","con lag 0","hydro lag 20" ,"hydro lag 19","hydro lag 16","temp lag 10","temp lag 1","temp lag 0","Rain lag 1")
row.names(AIClagmatrix) <- colnames(xvaribale)
  
for (i in 1:18) {
  AIClagmatrix[i,19] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,1:i]))
  AIClagmatrix[i,1:i] <- "x" 
}


AIClagmatrix[2:18,20] <- format(round(diff(as.numeric(AIClagmatrix[,19])), 2), nsmall = 2)
AIClagmatrix[1:18,19] <- format(round(as.numeric(AIClagmatrix[1:18,19]), 2), nsmall = 2)
AIClagmatrix
xtable(AIClagmatrix)

nytestxreg <- xvaribale[,c(1,which(as.numeric(AIClagmatrix[,20])<0))]

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
  rand <- sample(ncol(xvaribale))
  
}



xvaribale <-as.data.frame(cbind(xregcon[,11:1],xreghydro[,3:1],xregtemp[,3:1],as.ts(WEATHER$Precipitation))[1:2191,] )
#for (i in 1:dim(xvaribale)[1]) {
for (j in 1:dim(xvaribale)[2]) {
  if(is.na(xvaribale[i,j])==TRUE){
    xvaribale[i,j] <- 0
  }
  
}


# Best AIC model for ARMAX(1,0,2) #######
#bestem start parameter
startAIClag <- c()

#stokastisk optimering #######
modelsforarmax1_0_2 <- c()
modelsforarmax1_0_2 <-c("15",-4015.749)

names(startAIClag) <- colnames(xvaribale)
for (i in 1:18) {
  startAIClag[i] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,i], include.mean = F))
}

for (j in 1:20) {
  rand <- sample(ncol(xvaribale))
  modellag <- as.numeric(which.min(startAIClag))# temp lag 0 
  best_aic_1_0_2 <- min(startAIClag)
for (i in rand[rand != modellag]) {
  nyaic <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,c(modellag,i)]))
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
  nyaic <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,c(modellag,i)], include.mean = F))
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





}# test best model algoritmer
# armax(1,2) beste aic  ####

# armax_coef <- TSA::arima(X_t, order = c(1, 0, 2), include.mean = F)
# fixedarma <- c(0.9692 , -0.2018 , -0.2260,NA)
# armastartvalues <- c()
# startAIClag <- c()
# biclagarmax <- c()
# rmsearmax <- c()
# for (i in 1:18) {
#   midmodarmax <- TSA::arima(X_t, order = c(1, 0, 2),fixed= fixedarma,xreg=xvaribale[,i], include.mean = F)
#   startAIClag[i] <- stats::AIC(midmodarmax)
#   biclagarmax[i] <- stats::BIC(midmodarmax)
#   rmsearmax[i]   <- myrmse(midmodarmax)
# 
# }
# armaxstartlagtable <- cbind(colnames(xvaribale),startAIClag)
# xtable(armaxstartlagtable)
# armastartvalues <- cbind(startAIClag,biclagarmax,rmsearmax)
# row.names(armastartvalues) <- colnames(xvaribale)
# 
# modellagarmax <- as.numeric(which.min(startAIClag))# temp lag 0
# best_aic_1_0_2 <- min(startAIClag)
# 
# 
# 
# parameterantal <- c(1:18)
# minaiclistar <- c()
# minaicar <- 1
# for(j in 1:6){
#   minaiclistar <- c()
#   for  (i in parameterantal[-modellagarmax]) {
#     
#     fixedarma <- c(0.9692 , -0.2018 , -0.2260,rep(NA,length(c(modellagarmax,i))))
#     minaiclistar <- c(minaiclistar,stats::AIC(TSA::arima(X_t, order = c(1, 0, 2),fixed = fixedarma,xreg=xvaribale[,c(modellagarmax,i)], include.mean = F)))
#   }
#   if(min(minaiclistar)<best_aic_1_0_2){
#     modellagarmax <- c(modellagarmax,parameterantal[-modellagarmax][as.numeric(which.min(minaiclistar))])
#     fixedarma <- c(0.9692 , -0.2018 , -0.2260,rep(NA,length(c(modellagarmax))))
#     minaicar <- stats::AIC(TSA::arima(X_t, order = c(1, 0, 2),fixed = fixedarma,xreg=xvaribale[,modellagarmax], include.mean = F))
#     best_aic_1_0_2 <- minaicar
#   }
# }
modellagarmax <- c(17,16,18)
colnames(xvaribale)[modellagarmax]
best_aic_1_0_2

fixedarma <- c(0.9692 , -0.2018 , -0.2260,rep(NA,length(c(modellagarmax))))
bestarmax <- TSA::arima(X_t, order = c(1, 0, 2),fixed = fixedarma,xreg=xvaribale[,c(modellagarmax)], include.mean = F)
bestarmaxrmse <- rmse(X_t[31:2191],as.data.frame(fitted.values(bestarmax )[31:2191])[,1])
myrmse(bestarmax)
stats::AIC(bestarmax)# -4024.326
stats::BIC(bestarmax)#-3978.826
bestarmaxrmse #0.09584562

ljbox <-LjungBoxTest(bestarmax$residuals[31:2191],k=3)

ggplot(fortify(as.data.frame(ljbox)),aes(x=m,y=pvalue))+
  geom_point()+
  geom_hline(yintercept = 0.05,col="blue")+
  xlab("Lag")+
  xlim(c(4,30))+
  ylab("P-Value")

sdds <- sarima(X_t,1,0,2,xreg=xvaribale[,c(modellagarmax)])

sdds$ttable

df <- data.frame(bestarmax$residuals)
p <- ggplot(df,aes(sample=bestarmax$residuals))
p + stat_qq() + stat_qq_line()


# armax models #####
armaxmodels <- c(arma1_2,armax1_0_2,con_armax1_2,hydro_armax1_2,temp_armax1_2,rain_armax1_2,kcar_armax1_2,bestarmax)




# ARMAX lag tabel stor ;) ###########

ARMAXBT <- c()
for (i in xvaribale){
  armax1_0_2 <- TSA::arima(X_t,order=c(1,0,2),xreg = xvaribale, include.mean = F)
  
  for (j in xvaribale[-i]) {
    armax1_0_2 <- TSA::arima(X_t,order=c(1,0,2),xreg = xvaribale, include.mean = F)
  }
}


for (i in c(1:5)){
  print(i)
  for (j in c(1:5)[-i]) {
    print(c(i,j))
    for (k in c(1:5)[-c(i,j)]) {
      print(c(i,j,k))
    }
      }
}
numberofcombinations <- c()
for (i in 1:3) {
  numberofcombinations <- c(numberofcombinations,combn(x=colnames(xvaribale[1:18]),m=i))
}



arfima_ARFIMAX <-  arfima(X_t[2:2191], order=c(3,0,4), fixed = list(phi=c( 0.5472 ,-0.5299,0.9359),theta=c(-0.0070,0.4990,-0.5108,-0.2236),reg =c(-0.0102,-0.0014,0.2e-7,-0.0026),frac=0.19),dmean = F, xreg = xvaribale[2:2191,modellagarfimax])


test <- arfima(X_t[2:2191], order=c(3,0,4), fixed = list(phi=c( 0.498841 ,-0.554024,0.898237),theta=c(-0.015279,-0.529284, 0.491697 ,0.208435 ),frac=0.19))


rmse( exp(fitted.values(test)[[1]]+fitted.values(model1)[2:2191]) ,exp( X_t[2:2191]+fitted.values(model1)[2:2191])  )
plot.ts(data_NO1[2:2191,1])
lines(exp( X_t[2:2191]+fitted.values(model1)[2:2191]),col="yellow" )


plot.ts(frakdiff(frakdiff(fitted.values(test)[[1]],0.19),-0.19)-fitted.values(test)[[1]])
 
}




c("Consumption lag 30","Consumption lag23","Consumption lag 22" ,"Consumption lag 17" ,"Consumption lag 16" ,"Consumption lag 11","Consumption lag 10","Consumption lag 9","Consumption lag 4","Consumption lag 2","Consumption lag 0")
  c("Hydro lag 20 ","Hydro lag 19","Hydro lag 16")
  c("Mean Temp lag 10","Mean Temp lag 1","Mean Temp lag 0")
