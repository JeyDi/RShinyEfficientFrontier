rm(list = ls())
clc <- function() cat(rep("\n", 50))
clc()
rm(list = ls())



library(quantmod)
library(fPortfolio)
library(tidyverse)

# setwd("./")
# source("./Utility/utility.R")

# environmentSettings()
start.date = as.Date("2017-09-09")
end.date = as.Date("2018-09-09")

# mystocks <- new.env(hash=TRUE)
# result <- getSymbols(myTitles,src="yahoo", auto.assign = TRUE, env=mystocks)
getTitles <- function(myTitle,source){
  
  numberTitles <- ncol(myTitles)  
  
  getSymbols(myTitles, src=source, from="2017-09-09", to="2018-09-09")
  
  #ClosePrices <- do.call(merge, lapply(myTitles, function(x) Cl(get(x))))
  #OpenPrices <- do.call(merge, lapply(myTitles, function(x) Op(get(x))))
  #Volumes <- do.call(merge, lapply(myTitles, function(x) Vo(get(x))))
  portfolioPrices <- do.call(merge, lapply(myTitles, function(x) Ad(get(x))))
  
  # result <- as.data.frame(Adjusted)
  
  #Remove NA from the Portfolio Prices
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
  
  #Add the correct names to the dataframe
  colnames(portfolioPrices) <- myTitle
  
  return(portfolioPrices)
  
}




#***********************#
myTitles <- c("GOOG","AAPL","MSFT","AMZN","INTC")
source = "yahoo"

titles <- getTitles(myTitles,source)



