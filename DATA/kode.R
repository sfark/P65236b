#packagesss
library(lubridate)
library(readr)
library(stringr)
library(data.table)
library(tidyverse)
library(rlist)
library(astsa)
library(ggplot2)
library(forecast)
library(tseries)
setwd("C:/Users/soeren/Desktop/P65236b/DATA")
#data
PRICES_list <- list.files("PRICES", full.names = 1)
HYDRO_list <- list.files("HYDRO", full.names = 1)
CONSUMPTION_list <- list.files("CONSUMPTION", full.names = 1)

PRICES <- read.csv(PRICES_list[1], header = FALSE, sep = ";",
                   dec = ",")[,c(1,10:15)]
HYDRO <- read.csv(HYDRO_list[1], header = FALSE, sep = ";",
                   dec = ",")
CONSUMPTION <- read.csv(CONSUMPTION_list[1], header = FALSE, sep = ";",
                   dec = ",")

for (i in 2:length(HYDRO_list)) {
  PRICES <- rbind(PRICES,read.csv(PRICES_list[i], header = FALSE, sep = ";",
                        dec = ",")[2:(dim(read.csv(PRICES_list[i], header = FALSE, sep = ";",
                                                   dec = ","))[1]),c(1,10:15)])
  HYDRO <- rbind(HYDRO,read.csv(HYDRO_list[i], header = FALSE, sep = ";",
                                dec = ","))
  CONSUMPTION <- rbind(CONSUMPTION,read.csv(CONSUMPTION_list[1], header = FALSE, sep = ";",
                          dec = ",")[2:(dim(read.csv(CONSUMPTION_list[i], header = FALSE, sep = ";",
                                                     dec = ","))[1]),])
}



colnames(PRICES) <- as.character(unlist(PRICES[1,]))
PRICES <-  PRICES[-1, ]

samp2 <- PRICES[,-1]
rownames(samp2) <- PRICES[,1]
PRICES <- samp2


HYDRO[1:3,1:3]

CONSUMPTION[1:3,]


ggplot(PRICES, aes(V1, V10))


datapris[1:5,1:5]


