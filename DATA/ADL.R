end = length(X_t)
lagmaks <- 100

price_train <- cbind(
price_train_l10 = X_t[10:(end-1)],
price_train_l9 = X_t[9:(end-2)],
price_train_l8 = X_t[8:(end-3)],
price_train_l7 = X_t[7:(end-4)],
price_train_l6 = X_t[6:(end-5)],
price_train_l5 = X_t[5:(end-6)],
price_train_l4 = X_t[4:(end-7)],
price_train_l3 = X_t[3:(end-8)],
price_train_l2 = X_t[2:(end-9)])
#price_train_l1 = X_t[1:(end-10)])




lagseq <- seq(1,end)
laqmod <- seq(end,(1+1))

price_train = c()
  #X_t[2:(end-(lagmaks+1))]
nameprice_train <- c()

hydro_train <- c() 
  #data_NO1[,2][1:(end-lagmaks)]
namehydro_train <- c()

consumption_train = c()
  #data_NO1[,3][1:(end-lagmaks)]
nameconsumption_train<- c()

temp_train = c()
  #data_NO1[,4][1:(end-lagmaks)]
nametemp_train <- c()

rain_train = c()
  #data_NO1[,5][1:(end-lagmaks)]
namerain_train <- c()


for (i in 2:(lagmaks-1)) {
  price_train <-cbind(price_train, X_t[lagseq[i]:(end-(lagseq[i]+(lagmaks+1)))])
  nameprice_train<- c(nameprice_train,paste("price_train_l",toString(i),sep = ""))
  
  hydro_train <- cbind(hydro_train, data_NO1[,2][lagseq[i]:(end-(lagseq[i]+(lagmaks+1)))])
  namehydro_train <- c(namehydro_train,paste("hydro_train_l",toString(i),sep = ""))
  
  consumption_train <- cbind( consumption_train, data_NO1[,3][lagseq[i]:(end-(lagseq[i]+(lagmaks+1)))])
  nameconsumption_train <- c(nameconsumption_train,paste("consumption_train_l",toString(i),sep = ""))
  
  temp_train <-cbind(temp_train, data_NO1[,4][lagseq[i]:(end-(lagseq[i]+(lagmaks+1)))])
  nametemp_train <- c(nametemp_train,paste("temp_train_l",toString(i),sep = ""))
  
  rain_train <-cbind(rain_train, data_NO1[,5][lagseq[i]:(end-(lagseq[i]+(lagmaks+1)))])
  namerain_train <- c(namerain_train,paste("rain_train_l",toString(i),sep = ""))
}
colnames(price_train) <- nameprice_train
colnames(hydro_train) <- namehydro_train
colnames(consumption_train) <- nameconsumption_train
colnames(temp_train) <- nametemp_train
colnames(rain_train) <- namerain_train



#Normal OLS setup, including all lags of exo. var.
x_lag = model.matrix(X_t[2:(end-(lagmaks-1))] ~ 0 +price_train +hydro_train + consumption_train +temp_train+rain_train)
