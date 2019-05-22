sarma_lort <- TSA::arima(X_t, order = c(3,0,4),seasonal = list(order = c(7, 0.19, 5)))
acf(ts(residuals(sarma_lort)),lag.max = 360)
par(mfrow=c(2,1))
fit <- stl(ts(data_NO1[,1],frequency = 7),s.window="periodic")
plot(forecast(fit),xlim=c(312,316))
lines(ts(c(data_NO1[,1],Data2019[,1]),frequency = 7))

plot(stlf(ts(data_NO1[,1],frequency = 7), lambda=BoxCox.lambda(ts(data_NO1[,1],frequency = 7))),xlim=c(312,316),ylim=c(400,600))
lines(ts(c(data_NO1[,1],Data2019[,1]),frequency = 7))

par(mfrow=c(1,1))
plot.ts(data_NO1[2180:2191,1])

colnavne <- c()
for (i in 0:30) {
  colnavne <- c(colnavne,c(paste("con  lag ", as.character(i))))
}
for (i in 0:30) {
  colnavne <- c(colnavne,c(paste("hydro lag ", as.character(i))))
}
for (i in 0:30) {
  colnavne <- c(colnavne,c(paste("temp lag ", as.character(i))))
}
for (i in 0:30) {
  colnavne <- c(colnavne,c(paste("rain lag ", as.character(i))))
}

# lag temp #####
xregtemp1 <- c()
for (i in 0:30) {
  xregtemp1 <- cbind(xregtemp1,stats::lag(as.ts(data_NO1[,4]),k=(-i)))
}
#lag con #####
xregcon1 <- c()
for (i in 0:30) {
  xregcon1 <- cbind(xregcon1,stats::lag(as.ts(data_NO1[,3]),k=(-i)))
}
#lag hydro #####
xreghydro1 <- c()
for (i in 0:30) {
  xreghydro1 <- cbind(xreghydro1,stats::lag(as.ts(data_NO1[,2]),k=(-i)))
}
# lag rain #####
xregrain1<- c()
for (i in 0:30) {
  xregrain1 <- cbind(xregrain1,stats::lag(as.ts(data_NO1[,5]),k=(-i)))
}
# samlede xreg ####
xvaribalefun <- cbind(xregcon1[1:2191,],xreghydro1[1:2191,],xregtemp1[1:2191,],xregrain1[1:2191,])

arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,NA)

arfimastartvalues <- c()
startAIClag <- c()
biclagarfimax <- c()
rmsearfimax <- c()
for (i in 1:124) {
  midmodarfimax <- TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribalefun [,i], include.mean = F)
  startAIClag[i] <- stats::AIC(midmodarfimax)
  biclagarfimax[i] <- stats::BIC(midmodarfimax)
  rmsearfimax[i]   <- myrmsefi(midmodarfimax)
  
}
arfimaxstartlagtable <- cbind(colnames(xvaribalefun ),startAIClag)
arfimaxstartlagtable
arfimastartvalues <- cbind(startAIClag,biclagarfimax,rmsearfimax)
#row.names(arfimastartvalues) <- colnames(xvaribalefun )
arfimastartvalues
modellagarfimax <- as.numeric(which.min(startAIClag))# temp lag 0
best_aic_3_019_4 <- min(startAIClag)

parameterantal <- c(1:124)
diff_X_t <- frakdiff(X_t,0.19)
minaiclist <- c()
minaic <- 1
for(j in 1:30){
  minaiclist <- c()
  for  (i in parameterantal[-modellagarfimax]) {
    arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax,i))))
    minaiclist <- c(minaiclist,stats::AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribalefun [,c(modellagarfimax,i)], include.mean = F)))
  }
  print("#######")
  print(best_aic_3_019_4)
  print(colnames(xvaribalefun )[as.numeric(which.min(minaiclist))])
  print(min(minaiclist))
  if(min(minaiclist)<best_aic_3_019_4){
    modellagarfimax <- c(modellagarfimax,parameterantal[-modellagarfimax][as.numeric(which.min(minaiclist))])
    arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax))))
    minaic <- stats::AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribalefun [,modellagarfimax], include.mean = F))
    best_aic_3_019_4 <- minaic
  }
}

colnames(xvaribalefun )[modellagarfimax ]
arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax))))

ARFIMAXBESTMOD <- TSA::arima(diff_X_t , order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=xvaribalefun [,modellagarfimax], include.mean = F)
stats::AIC(ARFIMAXBESTMOD)#-4043.295
stats::BIC(ARFIMAXBESTMOD)#-3974.996
myrmsefi(ARFIMAXBESTMOD)#28.25779

