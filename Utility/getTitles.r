rm(list = ls())
clc <- function() cat(rep("\n", 50))
clc()
rm(list = ls())

library(quantmod)
library(fPortfolio)

setwd("./")
source("./Utility/utility.R")

# environmentSettings()

# mystocks <- new.env(hash=TRUE)
# result <- getSymbols(myTitles,src="yahoo", auto.assign = TRUE, env=mystocks)
getTitles <- function(myTitle,source){
  
  numberTitles <- ncol(myTitles)  
  
  getSymbols(myTitles, src=source)
  
  #TODO: capire quale valore utilizzare
  ClosePrices <<- do.call(merge, lapply(myTitles, function(x) Cl(get(x))))
  OpenPrices <<- do.call(merge, lapply(myTitles, function(x) Op(get(x))))
  Volumes <<- do.call(merge, lapply(myTitles, function(x) Vo(get(x))))
  Adjusted <<- do.call(merge, lapply(myTitles, function(x) Ad(get(x))))
  
  return(ClosePrices)
  
}

#Capire cosa fare con i dati

#Puoi usare differenti titoli, quelli che più ti interessano
myTitles <- c("GOOG","AAPL","MSFT","AMZN","INTC")
#Puoi utilizzare servizi differenti oltre a Yahoo
#Se vuoi usare più servizi si può fare una funzione.
source = "yahoo"

getTitles(myTitles,source)

