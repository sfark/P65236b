consump

AIC_BIC_matrix_d <- c()
AIC_BIC_Resultat <- c()
for (i in 0:5) {
  for (j in 0:5) {
    AIC_value <-  AIC(fracdiff(consump
, nar = i, nma = j))
    BIC_value <- AIC(fracdiff(consump
, nar = i, nma = j),k = log(length(consump)) )#Ved K=log(length(X_t)), gives BIC.
    
    BIC_AIC_values <- cbind(AIC_value,BIC_value,i,j,fracdiff(consump,nar=i,nma=j)$d)
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

fdGPH(consump)
