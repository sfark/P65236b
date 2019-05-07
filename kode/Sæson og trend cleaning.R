
###sæson og trend cleaning
dummyhelligweekend <- dummyhelligdage+dummyweekend

for (i in 1:length(dummyhelligweekend)) {
  if (dummyhelligweekend[i]==2){
    dummyhelligweekend[i]=1
  }
}
## dummy spiks
dummyspikes <- c()
  for (i in 1:length(X_t)) {
    if (X_t[i]>250) {
      dummyspikes[i] <- 1
    }else if (X_t[i]< -250) {
      dummyspikes[i] <- -1
    }
    else{
      dummyspikes[i] <- 0
    }  
  }


model1 <- glm(dagligpris[,2]~time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend+dummyspikes)

# Den nye sæson og trend cleanede tidsrække 
X_t <- ts(model1$residuals)