
x <- xvaribale[1:2191,]

bestmodarfimax(xvaribale[1:2191,])
# 
# [1] "Mean Temp lag 0"  "Mean Temp lag 1"   "Precipitation lag 1" "Consumption lag 0"  
# [1] -4034.665
# [1] -3966.366
# [1] 27.93227
bestmodarfimax(xregcon[1:2191,])
# [1] "Consumption lag 0"  "Consumption lag 2"  "Consumption lag 16"
# [4] "Consumption lag 9"  "Consumption lag 17"
# [1] -3974.196
# [1] -3940.09
# [1] 28.07867

bestmodarfimax(xreghydro[1:2191,])
# [1] "Hydro lag 16"
# [1] -3883.122
# [1] -3871.753
# [1] 28.95346
bestmodarfimax(xregtemp[1:2191,])
# [1] "Mean Temp lag 0"  "Mean Temp lag 10"
# [1] -4027.182
# [1] -4010.119
# [1] 28.71048



bestmodarfimax <-   function(x){
ARFIMAEcoef <- TSA::arima(diff_X_t , order = c(3, 0, 4), include.mean = F)
arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,NA)

arfimastartvalues <- c()
startAIClag <- c()
biclagarfimax <- c()
rmsearfimax <- c()
for (i in 1:dim(x)[2]) {
  midmodarfimax <- TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=x[,i], include.mean = F)
  startAIClag[i] <- stats::AIC(midmodarfimax)
  biclagarfimax[i] <- stats::BIC(midmodarfimax)
  rmsearfimax[i]   <- myrmsefi(midmodarfimax)
  
}
arfimaxstartlagtable <- cbind(colnames(x),startAIClag)
arfimaxstartlagtable
arfimastartvalues <- cbind(startAIClag,biclagarfimax,rmsearfimax)
row.names(arfimastartvalues) <- colnames(x)
arfimastartvalues
modellagarfimax <- as.numeric(which.min(startAIClag))# temp lag 0
best_aic_3_019_4 <- min(startAIClag)

parameterantal <- c(1:dim(x)[2])
diff_X_t <- frakdiff(X_t,0.19)
minaiclist <- c()
minaic <- 1
for(j in 1:10){
  minaiclist <- c()
  for  (i in parameterantal[-modellagarfimax]) {
    arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax,i))))
    minaiclist <- c(minaiclist,stats::AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=x[,c(modellagarfimax,i)], include.mean = F)))
  }
  # print("#######")
  # print(best_aic_3_019_4)
  # print(colnames(x)[as.numeric(which.min(minaiclist))])
  # print(min(minaiclist))
  if(min(minaiclist)<best_aic_3_019_4){
    modellagarfimax <- c(modellagarfimax,parameterantal[-modellagarfimax][as.numeric(which.min(minaiclist))])
    arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax))))
    minaic <- stats::AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=x[,modellagarfimax], include.mean = F))
    best_aic_3_019_4 <- minaic
  }
}


#modellagarfimax <- c(17,18,11,16)
print(colnames(x)[modellagarfimax ])
arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,rep(NA,length(c(modellagarfimax))))

ARFIMAXBESTMOD <- TSA::arima(diff_X_t , order = c(3, 0, 4),fixed = arfimaxfixedcoes,xreg=x[,modellagarfimax], include.mean = F)
print(stats::AIC(ARFIMAXBESTMOD))
print(stats::BIC(ARFIMAXBESTMOD))
print(myrmsefi(ARFIMAXBESTMOD))

}



arfimaxfixedcoes =c(0.50798175,-0.54969185,0.90879000,0.07365125,0.55444252,-0.44067697,-0.20974401,NA)

ARFIMAXBESTMOD <- TSA::arima(diff_X_t , order = c(3, 0, 4),xreg=xvaribale[,c(17,18,11,16)], include.mean = F)


rmse(data_NO1[31:2191,1],exp(frakdiff(fitted.values(ARFIMAXBESTMOD)[31:2191],-0.19)+fitted.values(model1)[31:2191]))



acf(ts(X_t[31:2191]-frakdiff(fitted.values(ARFIMAXBESTMOD)[31:2191],-0.19)))

