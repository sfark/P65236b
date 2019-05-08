#Sarima eksempel for estimation af p go q parametre med BIC og AIC
BIC_AIC_fmod <- c()
result <- c()
for (i in 0:5) {
  for (j in 0:5) {
    f_model <- sarima(DIFFYRIGTIG,i,0,j,details = F,no.constant = T)
    BIC_AIC_fmod <- cbind(f_model$BIC,f_model$AIC,i,j)
    result <- rbind(result,BIC_AIC_fmod)
    names <- c("BIC","AIC","p","q")
    colnames(result) <- names
    }
if (i==5 &&j==5) {
  print(result)
}
}

which(result[,1] == min(result[,1]), arr.ind = TRUE)
which(result[,2] == min(result[,2]), arr.ind = TRUE)

