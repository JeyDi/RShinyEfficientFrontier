library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)

tickers <- c("GOOG", "AAPL", "MSFT", "AMZN", "INTC")

portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2017-09-09", auto.assign=FALSE)[,4])

portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]

colnames(portfolioPrices) <- tickers

portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
portfolioReturns <- as.timeSeries(portfolioReturns)

Stock_Data <- tickers %>% lapply(function(x) getSymbols.yahoo(x, from="2016-01-01", auto.assign=FALSE)[,4]) %>%
  lapply(function(x) monthlyReturn(x))

portfolioReturns <- do.call(merge, Stock_Data)

portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
colnames(portfolioReturns) <- tickers

portfolioReturns <- as.timeSeries(portfolioReturns)

effFrontier <- portfolioFrontier(portfolioReturns, constraints = "LongOnly")

plot(effFrontier,c(1,2,3,4))

frontierWeights <- getWeights(effFrontier)
colnames(frontierWeights) <- tickers
risk_return <- frontierPoints(effFrontier)
write.csv(risk_return, "risk_return.csv")


cor_matrix <- cor(portfolioReturns)
cov_matrix <- cov(portfolioReturns)
write.csv(cov_matrix, "covmatrix.csv")

cov_matrix

riskReturnPoints <- frontierPoints(effFrontier)

annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 252)
plot(annualizedPoints)

riskFreeRate <- 0
plot((annualizedPoints[,"targetReturn"]-riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")


