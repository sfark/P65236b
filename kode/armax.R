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
  xregtemp <- cbind(xregtemp,stats::lag(as.ts(data_NO1[,4]),k=abs(lagtemp[i])))
}

colnames(xregtemp) <- c("lag 10","lag 1","lag 0")
# model =ARIMA(3,0,0) with zero mean

# lag con #####
ccfconlist <- seq(from=-30, to =30, length.out = 61)
meancon <- cbind(ccfconlist,conccf[[1]])[1:31,1:2]
lagcon <- meancon[which(abs(meancon[,2])>0.042)]

xregcon <- c()
for (i in 1:length(lagcon)) {
  xregcon <- cbind(xregcon,stats::lag(as.ts(data_NO1[,3]),k=abs(lagcon[i])))
}

colnames(xregcon) <- c("lag 30","lag 23","lag 22","lag 17","lag 16","lag 11","lag 10","lag 9","lag 4","lag 2","lag 0")
#model ARIMA(2,1,1) 

# lag hydro #####
ccfhydrolist <- seq(from=-30, to =30, length.out = 61)
meanhydro <- cbind(ccfhydrolist,hydroccf[[1]])[1:31,1:2]
laghydro <- meanhydro[which(abs(meanhydro[,2])>0.042)]

xreghydro <- c()
for (i in 1:length(laghydro)) {
  xreghydro <- cbind(xreghydro,stats::lag(as.ts(data_NO1[,2]),k=abs(laghydro[i])))
}

colnames(xreghydro) <- c("lag 20","lag 19","lag 16")

xvaribale <- cbind(xregcon,xreghydro,xregtemp)

# model =ARIMA(2,0,0) with zero mean 

# Pris lag model (1,0,2) ####
ar1 <- c()
ar1 <- stats::lag(X_t,k=abs(1))

colnames(ar1) <- c("lag 1")


# Pris lag model (3,0.19,4) ####
ar3 <- cbind(stats::lag(X_t,k=abs(1)),stats::lag(X_t,k=abs(2)),stats::lag(X_t,k=abs(3)))

colnames(ar3) <- c("lag 1","lag 2","lag 3")

# ARMAX model (1,0,2)####

testxreg <-as.data.frame(cbind(ar1,xregcon,xreghydro,xregtemp)[31:2221,] )
for (i in 1:dim(testxreg)[1]) {
  for (j in 1:dim(testxreg)[2]) {
    if(is.na(testxreg[i,j])==TRUE){
      testxreg[i,j] <- 0
    }
   
  }
}
armax1_0_2 <- TSA::arima(X_t,order=c(0,0,2), seasonal = list(order = c(0, 0, 0)),xreg <- testxreg, include.mean = F)

di_ACF <- acf(armax1_0_2$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+ggtitle("ARMAX (1,0,2)")+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

# ARMAX model (3,0.19,4)####

testxreg <-as.data.frame(cbind(xregcon,xreghydro,xregtemp)[31:2221,] )
testxreg <-as.data.frame(ar3[3:2193,])
for (i in 1:dim(testxreg)[1]) {
  for (j in 1:dim(testxreg)[2]) {
    if(is.na(testxreg[i,j])==TRUE){
      testxreg[i,j] <- 0
    }
    
  }
}
armax3_019_4 <- TSA::arimax(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xtransf = data_NO1[,2:4],transfer=list(c(2,0),c(2,1),c(3,0)), include.mean = F)

  
di_ACF <- acf(armax3_019_4$residuals ,plot = FALSE)
di_acf <- with(di_ACF, data.frame(lag, acf))
ggplot(data = di_acf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))+ggtitle("ARMAX (3,0.19,4)")+
  ylab("ACF")+geom_hline(aes(yintercept=0.042),col="blue",linetype=2)+
  geom_hline(aes(yintercept=-0.042),col="blue",linetype=2)

armax3_019_4


testmod <- lm(X_t ~ as.matrix(testxreg))
