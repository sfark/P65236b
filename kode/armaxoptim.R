# ARMAX models ##########
{
  
  
bestmodarmax(xregtemp[1:2191,])#####
  # [1] "T0" "T1"
  # [1] "AIClist"
  # [1] -4000.535 -4023.798
  # [1] "minAIC"
  # [1] -4023.798
  # startAIClag biclagarmax rmsearmax
  # T10   -3888.211   -3876.836  28.53850
  # T1    -3930.144   -3918.761  28.61190
  # T0    -4015.484   -4004.100  27.87319
  # [1] "RMSE"
  # [1] 27.8784
  # [1] "BIC"
  # [1] -4006.723
bestmodarmax(xreghydro[1:2191,])#####
  # [1] "H16"
  # [1] "AIClist"
  # [1] -3860.817 -3858.497
  # [1] "minAIC"
  # [1] -3866.475
  # startAIClag biclagarmax rmsearmax
  # H20   -3862.808   -3851.442  28.64668
  # H19   -3858.843   -3847.476  28.66871
  # H16   -3866.475   -3855.106  28.68293
  # [1] "RMSE"
  # [1] 28.68293
  # [1] "BIC"
  # [1] -3855.106

bestmodarmax(xregcon[1:2191,])#####
#CONsumption
# [1] "C0"
# [1] "AIClist"
# [1] -3839.363 -3851.038 -3853.333 -3862.123 -3863.566 -3874.616 -3876.879 -3880.270 -3893.396 -3901.456
# [1] "minAIC"
# [1] -3902.476
# startAIClag biclagarmax rmsearmax
# C30   -3841.134   -3829.778  28.68571
# C23   -3852.807   -3841.444  28.68160
# C22   -3855.085   -3843.721  28.67875
# C17   -3863.911   -3852.542  28.67192
# C16   -3865.392   -3854.023  28.67664
# C11   -3876.557   -3865.183  28.68905
# C10   -3878.855   -3867.480  28.69017
# C9    -3882.267   -3870.891  28.70006
# C4    -3894.702   -3883.322  28.68063
# C2    -3899.407   -3888.025  28.67703
# C0    -3902.476   -3891.092  28.67363
# [1] "RMSE"
# [1] 28.67363
# [1] "BIC"
# [1] -3891.092

#Fuldmodel#####
  bestmodarmax(xvaribale[1:2191,])

#Function bestmodarmax###########
bestmodarmax <-   function(x){
  
  
  armax_coef <- TSA::arima(X_t, order = c(1, 0, 2), include.mean = F)
  fixedarma <- c(0.9692 , -0.2018 , -0.2260,NA)
  armax_SV <- c()
  startAIClag <- c()
  biclagarmax <- c()
  rmsearmax <- c()
  for (i in 1:dim(x)[2]) {
    midmodarmax <- TSA::arima(X_t, order = c(1, 0, 2),fixed= fixedarma,xreg=x[,i], include.mean = F)
    startAIClag[i] <- stats::AIC(midmodarmax)
    biclagarmax[i] <- stats::BIC(midmodarmax)
    rmsearmax[i]   <- myrmse(midmodarmax)
    
  }
  armaxstartlagtable <- cbind(colnames(x),startAIClag)
  armax_SV <- cbind(startAIClag,biclagarmax,rmsearmax)
  row.names(armax_SV) <- colnames(x)
  modellagarmax <- as.numeric(which.min(startAIClag))# temp lag 0
  best_aic_1_0_2 <- min(startAIClag)
  
  parameterantal <- c(1:dim(x)[2])
  minaiclistar <- c(-7000)
  minaicar <- c()
  while (min(minaiclistar)<best_aic_1_0_2){
    minaiclistar <- c()
    for  (i in parameterantal[-modellagarmax]) {
      
      fixedarma <- c(0.9692 , -0.2018 , -0.2260,rep(NA,length(c(modellagarmax,i))))
      minaiclistar <- c(minaiclistar,stats::AIC(TSA::arima(X_t, order = c(1, 0, 2),fixed = fixedarma,xreg=x[,c(modellagarmax,i)], include.mean = F)))
    }
    if(min(minaiclistar)<best_aic_1_0_2){
      modellagarmax <- c(modellagarmax,parameterantal[-modellagarmax][as.numeric(which.min(minaiclistar))])
      fixedarma <- c(0.9692 , -0.2018 , -0.2260,rep(NA,length(c(modellagarmax))))
      minaicar <- stats::AIC(TSA::arima(X_t, order = c(1, 0, 2),fixed = fixedarma,xreg=x[,modellagarmax], include.mean = F))
      best_aic_1_0_2 <- minaicar
    }
  }
  bestmodel <- TSA::arima(X_t, order = c(1, 0, 2),fixed =  c(0.9692 , -0.2018 , -0.2260,rep(NA,length(c(modellagarmax)))),xreg=x[,modellagarmax], include.mean = F)
  bestmod_rmse <- myrmse(bestmodel)
  #modellagarmax_CON <- c(17,16,18)
  print(colnames(x)[modellagarmax])
  print("AIClist")
  print(minaiclistar)
  print("minAIC")
  print(best_aic_1_0_2)
  print(xtable(armax_SV))
  print("RMSE")
  print(bestmod_rmse)
  print("BIC")
  print(stats::BIC(bestmodel))
}

}
