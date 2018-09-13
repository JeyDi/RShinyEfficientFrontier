rm(list = ls())
clc <- function() cat(rep("\n", 50))
clc()
rm(list = ls())

library(quantmod)
library(fPortfolio)

setwd("./")
source("./Utility/utility.R")

# environmentSettings()

myTitles <- c("GOOG","AAPL","MSFT","AMZN","INTC")
numberTitles <- ncol(myTitles)

# mystocks <- new.env(hash=TRUE)
# result <- getSymbols(myTitles,src="yahoo", auto.assign = TRUE, env=mystocks)

getSymbols(myTitles, src="yahoo")

ClosePrices <- do.call(merge, lapply(myTitles, function(x) Cl(get(x))))
OpenPrices <- do.call(merge, lapply(myTitles, function(x) Op(get(x))))
Volumes <- do.call(merge, lapply(myTitles, function(x) Vo(get(x))))
Adjusted <- do.call(merge, lapply(myTitles, function(x) Ad(get(x))))


#Capire cosa fare con i dati



