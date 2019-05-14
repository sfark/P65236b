library(httr)
library(jsonlite)

options(stringsAsFactors = FALSE)

url  <- "https://api.met.no"
path <- "weatherapi/probabilityforecast/1.1/?lat=60.10&lon=9.58"

raw.result <- GET(url = url, path = path)

library("XML")

# Also load the other required package.
library("methods")

# Give the input file name to the function.
result <- xmlParse(GET(url = url, path = path))
xml_data <- xmlToList(result)
# Print the result.
print(result)

class(raw.result)

result


names(raw.result)
raw.result$status_code
head(raw.result$content)
this.raw.content <- rawToChar(raw.result$content)
nchar(this.raw.content)
substr(this.raw.content, 1, 100)
this.content <- fromJSON(this.raw.content)
class(raw.content)
length(this.content)
this.raw.content[[1]]
