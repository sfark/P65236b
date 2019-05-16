library(httr)
library(jsonlite)
library(XML)
library(methods)
install.packages("xml2")
library("methods")
library(xml2)
options(stringsAsFactors = FALSE)

url  <- "https://api.met.no"
path <- "weatherapi/probabilityforecast/1.1/?lat=60.10&lon=9.58"

result <- xmlParse(GET(url = url, path = path))


xmlfile <- result
xmltop = xmlRoot(xmlfile) #gives content of root


times <- xmlSApply(xmltop[[2]], xmlAttrs)

listoftimes <- substr(times[2,1:10], 1, 10)

#forecastday <- paste(substr(dato19[110], 1, 8),toString(as.numeric(substr(dato19[length(dato2019)], 9, 10))+1),sep = "")
forecastday <- "2019-05-17"
timeswewant <- c()
for (i in 1:10) {
  if(listoftimes[[i]] ==forecastday){
    timeswewant <- c(timeswewant,i)
  }
}

tempfore <- c()

for (i in timeswewant) {
  tempfore <- c(tempfore,xmlSApply(xmltop[[2]][[i]][[1]], xmlAttrs) [5,5])
}

templlag0 <- mean(as.numeric(tempfore))





