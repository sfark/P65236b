#Long memory

### Estimering af d-parameter med AIC og BIC
AIC_BIC_matrix_d <- c()
AIC_BIC_Resultat <- c()
for (i in 0:5) {
  for (j in 0:5) {
    AIC_value <-  AIC(fracdiff(X_t, nar = i, nma = j))
    BIC_value <- AIC(fracdiff(X_t, nar = i, nma = j),k = log(length(X_t)) )#Ved K=log(length(X_t)), gives BIC.
    
    BIC_AIC_values <- cbind(AIC_value,BIC_value,i,j,fracdiff(X_t,nar=i,nma=j)$d)
    AIC_BIC_Resultat <- rbind(AIC_BIC_Resultat,BIC_AIC_values)
    names <- c("AIC","BIC","p","q","d")
    colnames(AIC_BIC_Resultat) <- names
  }
  if (i==5 &&j==5) {
    print(AIC_BIC_Resultat)
  }
}

### AIC og BIC vælger den samme model ###
which(AIC_BIC_Resultat[,1] == min(AIC_BIC_Resultat[,1]), arr.ind = TRUE) #AIC, p=1,q=2
which(AIC_BIC_Resultat[,2] == min(AIC_BIC_Resultat[,2]), arr.ind = TRUE) #BIC, p=1,q=2

###Estimering af d-parameter
AIC_p <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,1] == min(AIC_BIC_Resultat[,1]), arr.ind = TRUE),3]
AIC_q <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,1] == min(AIC_BIC_Resultat[,1]), arr.ind = TRUE),4]

BIC_p <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,2] == min(AIC_BIC_Resultat[,2]), arr.ind = TRUE),3]
BIC_q <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,2] == min(AIC_BIC_Resultat[,2]), arr.ind = TRUE),4]


d_hat_AIC <- fracdiff(X_t,nar=AIC_p,nma=AIC_q)$d;d_hat_AIC
d_hat_BIC <- fracdiff(X_t,nar=BIC_p,nma=BIC_q)$d;d_hat_BIC

#Den fraktionelle diffede tidsserie
DiffY12 <- ts(frakdiff(X_t,d_hat_BIC))

autoplot(DiffY12,ylab = "Sample")

diffy12_ACf <- acf(DiffY12 ,plot = FALSE,lag.max = 100)
diffy12_acfdf2 <- with(diffy12_ACf, data.frame(lag, acf))
ggplot(data = diffy12_acfdf2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)


#sarima finder AIC og BIC for (3,0.19,4) og (1,0,2) 
sarima(X_t,p=3,d=0.19,q=4,details = F,no.constant = T)#AIC -3.620895 BIC -4.602709
sarima(X_t,p=1,d=0,q=2,details = F,no.constant = T)# AIC -3.619008 BIC -4.611214

#Loop der finder den bedste model til Diffy12 ved både AIC og BIC
BIC_AIC_fmod <- c()
result_diffy12 <- c()
for (i in 0:5) {
  for (j in 0:5) {
    f_model <- sarima(DiffY12,i,0,j,details = F,no.constant = T)
    
    BIC_AIC_fmod <- cbind(f_model$AIC,f_model$BIC,f_model$AICc,i,j)
    
    result_diffy12 <- rbind(result_diffy12,  BIC_AIC_fmod)
    
    names <- c("AIC","BIC","AICc","p","q")
    
    colnames(result_diffy12) <- c("AIC","BIC","AICc","p","q")
  }
  if (i==5&&j==5) {
    print(result_diffy12)
  }
}

################ AIC og BIC vælger 2 forskellige modeller#############################
AIC_p <- result_diffy12[which(result_diffy12[,1] == min(result_diffy12[,1]), arr.ind = TRUE),4]
AIC_q <- result_diffy12[which(result_diffy12[,1] == min(result_diffy12[,1]), arr.ind = TRUE),5]

BIC_p <- result_diffy12[which(result_diffy12[,2] == min(result_diffy12[,2]), arr.ind = TRUE),4]
BIC_q <- result_diffy12[which(result_diffy12[,2] == min(result_diffy12[,2]), arr.ind = TRUE),5]

#AIC vælger den bedste model til at være en ARfIMA(3,d_hat_BIC,4)
#BIC vælger den bedste mdoel til at være en ARfIMA(1,d_hat_BIC,2)

#AIC og BIC for model valgt ud fra AIC
AIC(arfima(X_t,order = c(AIC_p,0,AIC_q),fixed = list(frac=d_hat_BIC)))#AIC=-10120.29
BIC(arfima(X_t,order = c(AIC_p,0,AIC_q),fixed = list(frac=d_hat_BIC)))#BIC=-10063.36

#AIC og BIC for model valgt ud fra BIC
AIC(arfima(X_t,order = c(BIC_p,0,BIC_q),fixed = list(frac=d_hat_BIC)))#AIC=-10109.26
BIC(arfima(X_t,order = c(BIC_p,0,BIC_q),fixed = list(frac=d_hat_BIC)))#BIC=-10075.11

#der fittes en ARIMA model til DiffY12, SARIMA giver forskellige plots
sarima(DiffY12,AIC_p,0,AIC_q,details = T,no.constant = T)
sarima(DiffY12,BIC_p,0,BIC_q,details = T,no.constant = T)

#Residualerne for de 2 forskellige modeller
res.arima.AIC34 <- residuals(arima(DiffY12,order = c(AIC_p,0,AIC_q)))#p=5, q=4
res.arima.BIC12 <- residuals(arima(DiffY12,order = c(BIC_p,0,BIC_q)))#p=1, q=2

fit34 <- fitted.values(arima(DiffY12,order = c(AIC_p,0,AIC_q),include.mean = F))
fit12 <- fitted.values(arima(DiffY12,order = c(BIC_p,0,BIC_q),include.mean = F))

#plots, acf og pacf valgt ud fra AIC
plot(acf(res.arima.AIC34)) #p=5, q=4
{
  di_ACfF <- acf(res.arima.AIC34 ,plot = FALSE,lag.max = 20)
  di_acfdf2 <- with(di_ACfF, data.frame(lag, acf))
  ggplot(data = di_acfdf2, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

  di_pACfF <- pacf(res.arima.AIC34 ,plot = FALSE)
  di_pacfdf2 <- with(di_pACfF, data.frame(lag, acf))
  ggplot(data = di_pacfdf2, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

  diffY_PACF <- pacf(diffY ,plot = FALSE)
  pacfdf <- with(diffY_PACF, data.frame(lag, acf))
  ggplot(data = pacfdf, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
} #ggplots
acf(res.arima.AIC34)  #Acf'en for residualerne
pacf(res.arima.AIC34) #Pacf for residualerne

#plot, acf og pacf for modellen valgt ud fra BIC
plot(res.arima.BIC12) #p=1, q=2
{
  di_ACfF12 <- acf(res.arima.BIC12 ,plot = FALSE,lag.max = 20)
  di_acfdf12 <- with(di_ACfF12, data.frame(lag, acf))
  ggplot(data = di_acfdf12, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

  di_pACfF12 <- pacf(res.arima.BIC12 ,plot = FALSE)
  di_pacfdf12 <- with(di_pACfF12, data.frame(lag, acf))
  ggplot(data = di_pacfdf12, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)
}
acf(res.arima.BIC12)  #Acf'en for residualerne
pacf(res.arima.BIC12) #Pacf for residualerne


########### estimerede værdier imod den rigtige tidsrække 
plot(fitted.values(arima(DiffY12,order = c(3,0,4),include.mean = F)),col="red",ylim=c(-200,230),lty=2)
lines(DiffY12)
lines(fitted.values(arima(DiffY12,order = c(1,0,2),include.mean = F)),col="blue",lty=4)

autoplot(DiffY12)+lines(fitted.values(arima(DiffY_AIC,order = c(5,0,3))),col="red",lty=2)

ggplot(data=DiffY12,mapping = aes(1:length(DiffY12),DiffY12,colour="DiffY12"))+
  geom_line()+
  xlim(c(1,2100))+
  ylim(c(-200,200))+
  geom_line(aes(y=fit12,colour="fit12"))+
  geom_line(aes(y=fit34,colour="fit34"))+
  scale_color_manual(name="Models",labels=c("Original","ARFIMA(1,6.85e-05,2)","ARFIMA(5,6.85e-05,4)"),
                     values = c("DiffY12"="black","fit12"= "red","fit34"="blue"))+
  xlab("Time")+ylab("Sample")

forskel1 <- abs(DiffY12[1:length(DiffY12)])-abs(fit34[1:length(DiffY12)])
forskel <- DiffY12[1:length(DiffY12)]-fit12[1:length(DiffY12)]
plot.ts(forskel)
plot.ts(forskel1)


{
  ##################ed Parametre for p og q op til 10#############################################
  AIC_BIC_matrix_d <- c()
  AIC_BIC_Resultat2 <- c()
  for (i in 0:10) {
    for (j in 0:10) {
      AIC_value <-  AIC(fracdiff(X_t, nar = i, nma = j))
      BIC_value <- AIC(fracdiff(X_t, nar = i, nma = j),k = log(length(X_t)) )#Ved K=log(length(X_t)), gives BIC.
      
      BIC_AIC_values <- cbind(AIC_value,BIC_value,i,j)
      AIC_BIC_Resultat2 <- rbind(AIC_BIC_Resultat2,BIC_AIC_values)
      names <- c("AIC","BIC","p","q")
      colnames(AIC_BIC_Resultat2) <- names
    }
    if (i==10 &&j==10) {
      print(AIC_BIC_Resultat2)
    }
  }
  AIC_long_p <- AIC_BIC_Resultat2[which(AIC_BIC_Resultat2[,1] == min(AIC_BIC_Resultat2[,1]), arr.ind = TRUE),3] 
  AIC_long_q <- AIC_BIC_Resultat2[which(AIC_BIC_Resultat2[,1] == min(AIC_BIC_Resultat2[,1]), arr.ind = TRUE),4] 
  BIC_long_p <- AIC_BIC_Resultat2[which(AIC_BIC_Resultat2[,2] == min(AIC_BIC_Resultat2[,2]), arr.ind = TRUE),3] 
  BIC_long_q <- AIC_BIC_Resultat2[which(AIC_BIC_Resultat2[,2] == min(AIC_BIC_Resultat2[,2]), arr.ind = TRUE),4] 
  
  ###Estimering af d-parameter 
  d_hat_AIC <- fracdiff(X_t,nar = AIC_long_p,nma = AIC_long_q)$d
  d_hat_BIC <- fracdiff(X_t,nar = BIC_long_p,nma = BIC_long_q)$d
  
  #residualerne for de 2 beste modelle valgt ud fra AIC og BIC
  acf(residuals(fracdiff(X_t,nar = AIC_long_p,nma = AIC_long_q)))
  acf(residuals(fracdiff(X_t,nar = BIC_long_p,nma = BIC_long_q)))
  
  #Vi Frakdiffer vores tidsrække med den nye fundne d-paramete
  DiffY_AIC <- ts(frakdiff(X_t,d_hat_AIC))
  DiffY_BIC <- ts(frakdiff(X_t,d_hat_BIC))
  
  #loop der finder den bedste model for Diffy_AIC
  BIC_AIC_fmod <- c()
  result_diffy_AIC <- c()
  for (i in 0:10) {
    for (j in 0:10) {
      f_model <- sarima(DiffY_AIC,i,0,j,details = F,no.constant = T)
      
      BIC_AIC_fmod <- cbind(f_model$AIC,f_model$BIC,i,j)
      
      result_diffy_AIC <- rbind(result_diffy_AIC,  BIC_AIC_fmod)
      
      colnames(result_diffy_AIC) <- c("AIC","BIC","p","q")
    }
    if (i==10 &&j==10) {
      print(result_diffy_AIC)
    }
  }
  
  which(result_diffy_AIC[,1] == min(result_diffy_AIC[,1]), arr.ind = TRUE)#AIC p=5, q=7
  which(result_diffy_AIC[,2] == min(result_diffy_AIC[,2]), arr.ind = TRUE)#BIC p=1, q=2
  
  #loop der finder den bedste model for Diffy_BIC
  BIC_AIC_fmod <- c()
  result_diffy_BIC <- c()
  for (i in 0:10) {
    for (j in 0:10) {
      f_model <- sarima(DiffY_BIC,i,0,j,details = F,no.constant = T)
      
      BIC_AIC_fmod <- cbind(f_model$AIC,f_model$BIC,i,j)
      
      result_diffy_BIC <- rbind(result_diffy_BIC,  BIC_AIC_fmod)
      
      colnames(result_diffy_BIC) <-  c("AIC","BIC","p","q")
    }
    if (i==10 &&j==10) {
      print(result_diffy_BIC)
    }
  }
  
  which(result_diffy_BIC[,1] == min(result_diffy_BIC[,1]), arr.ind = TRUE)#AIC p=5, q=7
  which(result_diffy_BIC[,2] == min(result_diffy_BIC[,2]), arr.ind = TRUE)#BIC p=1, q=2
  
  
  AIC(arfima(X_t,order = c(AIC_long_p,0,AIC_long_q),fixed = list(frac=d_hat_AIC)))
  BIC(arfima(X_t,order = c(AIC_long_p,0,AIC_long_q),fixed = list(frac=d_hat_AIC)))
  
  AIC(arfima(X_t,order = c(BIC_long_p,0,BIC_long_q),fixed = list(frac=d_hat_BIC)))
  BIC(arfima(X_t,order = c(BIC_long_p,0,BIC_long_q),fixed = list(frac=d_hat_BIC)))
  
  
  sarima(DiffY_AIC,1,0,2,details = T,no.constant = T)
  sarima(DiffY_AIC,5,0,7,details = T,no.constant = T)
  sarima(DiffY_AIC,AIC_long_p,0,AIC_long_q,details = T,no.constant = T)
  sarima(DiffY_AIC,BIC_long_p,0,BIC_long_q,details = T,no.constant = T)
}
fit342 <- fitted.values(arima(DiffY_AIC,order = c(5,0,4)))
fit122 <- fitted.values(arima(DiffY_AIC,order = c(1,0,2)))

ggplot(data=DiffY_AIC,mapping = aes(1:length(DiffY_AIC),DiffY_AIC,colour="DiffY_AIC"))+
  geom_line()+
  xlim(c(1,2100))+
  ylim(c(-200,200))+
  geom_line(aes(y=fit122,colour="fit122"))+
  geom_line(aes(y=fit342,colour="fit342"))+
  scale_color_manual(name="Models",labels=c("Original","ARFIMA(1,6.85e-05,2)","ARFIMA(5,6.85e-05,4)"),
                     values = c("DiffY_AIC"="black","fit122"= "red","fit342"="blue"))+
  xlab("Time")+ylab("Sample")



res.arima.AIC <- residuals(arima(DiffY_AIC,order = c(AIC_long_p,0,AIC_long_q),optim.control = list(maxit = 1000)))
res.arima.BIC.long <- residuals(arima(DiffY_AIC,order = c(AIC_long_p,0,AIC_long_q),optim.control = list(maxit = 1000)))
res.arima.BIC <- residuals(arima(DiffY_AIC,order = c(1,0,2)))

par(mfrow=c(3,1))
plot.ts(res.arima.AIC)
plot.ts(res.arima.BIC.long)
plot.ts(res.arima.BIC)

acf(res.arima.AIC)
acf(res.arima.BIC)
acf(res.arima.BIC.long)

pacf(res.arima.AIC)
pacf(res.arima.BIC)
pacf(res.arima.BIC.long)

###################### Estimation a d-parameter med fdGPH ############################
d_hat_fdGPH <- fdGPH(X_t)$d;d_hat_fdGPH #d=0.3917251

#######################vi fraktionel differ tids serien med vores estimerede d_hat#########################
DiffY_fdGPH<-ts(frakdiff(X_t,d_hat_fdGPH)) 


#vi fitter en arima funktion til den frak diffede med alternativ d
BIC_AIC_fmod_fdGPH <- c()
result_diff_fdGPH_BIC <- c()
for (i in 0:5) {
  for (j in 0:5) {
    f_model <- sarima(DiffY_fdGPH,i,0,j,details = F,no.constant = T)
    
    BIC_AIC_fmod_fdGPH <- cbind(f_model$AIC,f_model$BIC,i,j)
    
    result_diff_fdGPH_BIC <- rbind(result_diff_fdGPH_BIC,  BIC_AIC_fmod_fdGPH)
    
    names <- c("AIC","BIC","p","q")
    
    colnames(result_diff_fdGPH_BIC) <- names
  }
  if (i==j ) {
    print(result_diff_fdGPH_BIC)
  }
}


fdGPH_AIC_p <- result_diff_fdGPH_BIC[which(result_diff_fdGPH_BIC[,1] == min(result_diff_fdGPH_BIC[,1]), arr.ind = TRUE),3]
fdGPH_AIC_q <- result_diff_fdGPH_BIC[which(result_diff_fdGPH_BIC[,1] == min(result_diff_fdGPH_BIC[,1]), arr.ind = TRUE),4]
fdGPH_BIC_p <- result_diff_fdGPH_BIC[which(result_diff_fdGPH_BIC[,2] == min(result_diff_fdGPH_BIC[,2]), arr.ind = TRUE),3]
fdGPH_BIC_q <- result_diff_fdGPH_BIC[which(result_diff_fdGPH_BIC[,2] == min(result_diff_fdGPH_BIC[,2]), arr.ind = TRUE),4]


fit453 <- fitted.values(arima(DiffY_fdGPH,order=c(fdGPH_AIC_p,0,fdGPH_AIC_q)))

fit123 <- fitted.values(arima(DiffY_fdGPH,order=c(fdGPH_BIC_p,0,fdGPH_BIC_q),include.mean = F))


ggplot(data=DiffY_fdGPH,mapping = aes(1:length(DiffY_fdGPH),DiffY_fdGPH,colour="DiffY_fdGPH"))+
  geom_line()+
  xlim(c(1,2100))+
  geom_line(aes(y=fit123,colour="fit123"))+
  scale_color_manual(name="Models",labels=c("Original","ARFIMA(1,0.29,2)"),
                     values = c("DiffY_fdGPH"="black","fit123"= "red","fit453"="blue"))+
  xlab("Time")+ylab("Sample")


sarima(DiffY_fdGPH,fdGPH_AIC_p,0,fdGPH_AIC_q,details = T,no.constant = T) #AIC
sarima(DiffY_fdGPH,fdGPH_BIC_p,0,fdGPH_BIC_q,no.constant = T)             #BIC



################### ACF med fdGPH
res.arima.fdGPH <- residuals(arima(DiffY_fdGPH,order = c(fdGPH_BIC_p,0,fdGPH_BIC_q)))#p=1, q=2


###acf for residualerne af arfima(1,0.29,2)
{
  di_ACf_fdgph <- acf(res.arima.fdGPH ,plot = FALSE,lag.max = 20)
  di_acf_fdGPH1 <- with(di_ACf_fdgph, data.frame(lag, acf))
  ggplot(data = di_acf_fdGPH1 , mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)
  
  di_pACf_fdgph <- pacf(res.arima.fdGPH ,plot = FALSE)
  di_pacf_fdgph2 <- with(di_pACf_fdgph, data.frame(lag, acf))
  ggplot(data = di_pacf_fdgph2, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)
  
  
  ##pacf for diffy_fdpgh
  pacf(DiffY_fdGPH)
  diffY_fdgph_PACF <- pacf(DiffY_fdGPH ,plot = FALSE)
  pacf_fdgph <- with(diffY_fdgph_PACF, data.frame(lag, acf))
  ggplot(data = pacf_fdgph, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
  
  ###Acf for diffY_fdGPH
  diffY_fdgph_ACF <- acf(diffY_fdGPH ,plot = FALSE)
  acf_fdgph <- with(diffY_fdgph_ACF, data.frame(lag, acf))
  ggplot(data = acf_fdgph, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
  
  ##ACF og PACF for residualer
  acf(res.arima.fdGPH)
  resid_fdgph_ACF <- acf(res.arima.fdGPH ,plot = FALSE)
  resid_acf_fdgph <- with(resid_fdgph_ACF, data.frame(lag, acf))
  ggplot(data = resid_acf_fdgph, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
  
  pacf(res.arima.fdGPH)
  resid_fdgph_pACF <- pacf(res.arima.fdGPH ,plot = FALSE)
  resid_pacf_fdgph <- with(resid_fdgph_pACF, data.frame(lag, acf))
  ggplot(data = resid_pacf_fdgph, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.041),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.041),col="blue",linetype=2)
  
}


#################Auto arima #################
auto.arima(DiffY_fdGPH,ic=c("bic"))#p=2,q=1
fitfdgph <- fitted.values(arima(DiffY_fdGPH,order=c(2,0,2),include.mean = F))
auto.arima(DiffY_fdGPH,ic=c("aic"))#p=2,q=2

arima(DiffY_fdGPH,order=c(2,0,2),include.mean = F)
sarima(DiffY_fdGPH,2,0,2,details = T,no.constant = T)

#summary(auto.arima(DiffY_fdGPH,ic=c("aic")))


fitfdgph22 <- as.data.frame(fitted.values(arfima(X_t,order = c(1,0,2),fixed = list(frac=0.2943173))))
fitfdgph221 <- ts(fitfdgph22[,1])
plot(X_t)
lines(fitfdgph221,col="red")
{
  ggplot(data=ts(X_t),mapping = aes(1:length(X_t),X_t))+
    geom_line(aes(y=fitfdgph221,colour="sss"))+
    geom_line(aes(y=X_t,colour="fitfdgph221"))+
    scale_color_manual(name="Models",
                       labels=c("Original","ARFIMA(1,0.29,2)"),
                       values = c("sss"="red","fitfdgph221"="black"))+
    xlab("Time")+
    ylab("Sample")
  
  ggplot(data=DiffY_fdGPH,mapping = aes(1:length(DiffY_fdGPH),DiffY_fdGPH,colour="DiffY_fdGPH"))+
    geom_line()+
    xlim(c(1,2100))+
    ylim(c(-200,200))+
    geom_line(aes(y=fit123,colour="fit123"))+
    scale_color_manual(name="Models",
                       labels=c("Original","ARFIMA(1,0.2943173,2)","ARFIMA(4,0.2943173,5)"),
                       values = c("DiffY_fdGPH"="black","fit123"= "red","fit453"="blue"))+
    xlab("Time")+ylab("Sample")
  
  
}


################################ ARMA  #########################
auto.arima(X_t,d=0, stepwise = F, approximation = F)#ARMA(1,2)
### plot med ARMA og X_t
{ggplot(data=ts(X_t),mapping = aes(1:length(X_t),X_t))+
    geom_line(aes(y=X_t,colour="fitarma"))+
    geom_line(aes(y=fitarma[,1],colour="X_t"))+
    scale_color_manual(name="Models",labels=c("Original","ARMA(1,2)"),
                       values = c("fitarma"= "black","X_t"="red"))+
    xlab("Time")+ylab("Sample")+xlim(c(1,2100))+
    ylim(c(-200,200))
}

res.arma <- residuals(arima(X_t,c(1,0,2)))
plot(res.arma)
acf(res.arma)
###ACF og PACF plot
{
  ACf_arma <- acf(res.arma ,plot = FALSE,lag.max = 20)
  di_acf_arma <- with(ACf_arma, data.frame(lag, acf))
  ggplot(data = di_acf_arma , mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)
  
  di_pACf_ARMA <- pacf(res.arma ,plot = FALSE)
  di_pacf_ARMA2 <- with(di_pACf_ARMA, data.frame(lag, acf))
  ggplot(data = di_pacf_ARMA2, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0))+
    ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
    geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)
}


################################ ARIMA #######################
auto.arima(X_t,d=1, stepwise = F, approximation = F)#ARIMA(3,1,1)


###################### AIC og BIC for de forskellige modeller#################
#model speficationer
modelarma <- arfima(X_t,c(1,0,2),lmodel = c("n"),dmean = F,itmean = F)
modelarima <- arfima(X_t,c(3,1,1),lmodel = c("n"),dmean = F,itmean = F)
modelarfima1 <- arfima(X_t,c(1,0,2),fixed = list(frac=0.0000685),dmean = F,itmean = F)
modelarfima3 <- arfima(X_t,c(5,0,4),fixed = list(frac=0.0000685),dmean = F,itmean = F)
modelarfima2 <-   arfima(X_t,c(1,0,2),fixed = list(frac=0.29),dmean = F,itmean = F)

###AIC og BIC###
#summary(arfima(X_t,c(1,0,2),lmodel = c("n"),dmean = F,itmean = F))             #AIC = 14128.1; BIC = 14156.6
#summary(arfima(X_t,c(3,1,1),lmodel = c("n"),dmean = F,itmean = F))             #AIC = 14142.1; BIC = 14176.2
#summary(arfima(X_t,c(1,0,2),fixed = list(frac=0.0000685),dmean = F,itmean = F))#AIC = 14130.1; BIC = 14164.3
#summary(arfima(X_t,c(5,0,4),fixed = list(frac=0.0000685),dmean = F,itmean = F))#AIC = 14134.9; BIC = 14203.2
#summary(arfima(X_t,c(1,0,2),fixed = list(frac=0.29),dmean = F,itmean = F))     #AIC = 14133.8; BIC = 14167.9

fitarma <- as.data.frame(fitted.values(modelarma))
fitarfima12 <- as.data.frame(fitted.values(modelarfima2))

plot(fit)
lines(DiffY_fdGPH,col="red")

#summary(arfima(frakdiff(X_t,0.0000685),c(1,0,2),lmodel = c("n"),dmean = F,itmean = F))#AIC = 14128.1; BIC = 14156.6
#summary(arfima(frakdiff(X_t,0.0000685),c(5,0,4),lmodel = c("n"),dmean = F,itmean = F))#AIC = 14132.9; BIC = 14195.5
#summary(arfima(frakdiff(X_t,0.29),c(1,0,2),lmodel = c("n"),dmean = F,itmean = F))#AIC = 14140.6; BIC = 14169



sarima(DiffY12,5,0,4,details = T,no.constant = T,tol=0.0000000000000000000000000001) #AIC=7.444428 AICc=7.445387 BIC=6.46781
sarima(DiffY12,1,0,2,details = T,no.constant = T)                                    #AIC=7.445694 AICc=7.446615 BIC=6.453488
sarima(DiffY_fdGPH,1,0,2,details = T,no.constant = T)                                #AIC=7.451987 AICc=7.452908 BIC=6.459781
sarima(diff(X_t),3,0,1,details = T,no.constant = T)                                  #AIC=7.455586 AICc=7.456512 BIC=6.465982
sarima(X_t,1,0,2,details = T,no.constant = T)                                        #AIC=7.445692 AICc=7.446614 BIC=6.453486

###############ACF af de fraktionelle diffede tidsrækker####################
#DiffY_fdGPH
#DiffY12

### ACF og PACF for DiffY d=0.00000685
ACf_X_t_DiffY <- acf(DiffY12 ,plot = FALSE,lag.max = 20)
di_acf_X_t_DiffY <- with(ACf_X_t_DiffY, data.frame(lag, acf))
ggplot(data = di_acf_X_t_DiffY , mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

di_pACf_X_t_DiffY <- pacf(DiffY12 ,plot = FALSE)
di_pacf_X_t_DiffY <- with(di_pACf_X_t_DiffY, data.frame(lag, acf))
ggplot(data = di_pacf_X_t_DiffY, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

### ACF og PACF af DIffY_fdGPH
ACf_X_t_DiffY_fdGPH <- acf(DiffY_fdGPH ,plot = FALSE,lag.max = 20)
di_acf_X_t_DiffY_fdGPH <- with(ACf_X_t_DiffY_fdGPH, data.frame(lag, acf))
ggplot(data = di_acf_X_t_DiffY_fdGPH , mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("ACF")+geom_hline(aes(yintercept=0.05),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.05),col="blue",linetype=2)

di_pACf_X_t_DiffY_fdGPH <- pacf(DiffY_fdGPH ,plot = FALSE)
di_pacf_X_t_DiffY_fdGPH <- with(di_pACf_X_t_DiffY_fdGPH, data.frame(lag, acf))
ggplot(data = di_pacf_X_t_DiffY_fdGPH, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+
  ylab("PACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)



# risidual plot detrend desæson

ggplot(data=X_t,mapping = aes(1:length(X_t),X_t,colour="X_t"))+
  xlim(c(1,2000))+
  xlab("Time")+ylab("Sample")



