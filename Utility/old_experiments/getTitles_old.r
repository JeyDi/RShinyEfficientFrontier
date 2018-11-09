rm(list = ls())
clc <- function()
  cat(rep("\n", 50))
clc()
rm(list = ls())

library(quantmod)
library(fPortfolio)
library(tidyverse)
library(knitr)
library(lubridate)
library(PortfolioAnalytics)
library(plotly)

# setwd("./")
# source("./Utility/utility.R")
# environmentSettings()
start.date = as.Date("2017-09-09")
end.date = as.Date("2018-09-09")

# mystocks <- new.env(hash=TRUE)

# result <- getSymbols(myTitles,src="yahoo", auto.assign = TRUE, env=mystocks)
getTitles <- function(myTitle, source) {
  numberTitles <- ncol(myTitles)
  
  getSymbols(myTitles,
             src = source,
             from = "2017-09-09",
             to = "2018-09-09")
  
  #ClosePrices <- do.call(merge, lapply(myTitles, function(x) Cl(get(x))))
  #OpenPrices <- do.call(merge, lapply(myTitles, function(x) Op(get(x))))
  #Volumes <- do.call(merge, lapply(myTitles, function(x) Vo(get(x))))
  portfolioPrices <-
    do.call(merge, lapply(myTitles, function(x)
      Ad(get(x))))
  
  # result <- as.data.frame(portfolioPrices)
  
  #Remove NA from the Portfolio Prices (some titles can have NA on columns)
  portfolioPrices <-
    portfolioPrices[apply(portfolioPrices, 1, function(x)
      all(!is.na(x))),]
  
  #Add the correct names to the dataframe
  colnames(portfolioPrices) <- myTitle
  
  return(portfolioPrices)
  
}


getDailyResults <- function(my_titles_dataframe) {
  # result <- sapply(my_titles_dataframe, function(x) dailyReturn(x) )
  result <- data.frame()
  
  for (i in 1:ncol(my_titles_dataframe)) {
    temp_result <- dailyReturn(my_titles_dataframe[, i])
    
    result <- cbind(result, temp_result)
  }
  
  titleNames <- colnames(my_titles_dataframe)
  colnames(result) <- myTitles
  
  return(result)
}


adjustDataframes <- function(daily_results_dataframe) {
  #TODO: calc the mean and variance for every column and create a list of means and variances
  
  # meanReturns <- colmean(daily_results_dataframe)*100
  # varianceReturns <- colvar(daily_results_dataframe)
  
  meanReturns <- apply(daily_results_dataframe, 2, mean) * 100
  varianceReturns <- apply(daily_results_dataframe, 2, var) * 100
  stDevReturns <- apply(daily_results_dataframe, 2, stdev) * 100
  
  # meanReturns <- daily_results_dataframe@statistics$mean * 100
  # varianceReturns <- daily_results_dataframe@statistics$Cov * 100
  
  result <- data.frame()
  
  result <- rbind(meanReturns, varianceReturns, stDevReturns)
  
  return(result)
  
}





covarianceMatrixTimeSeries <- function(timeseries_dataframe) {
  #Create the time series object
  
  assetsSeries <- as.timeSeries(timeseries_dataframe)
  adjusted_dataframe <- portfolioData(data = assetsSeries)

  mv <- tibble(
    Asset = adjusted_dataframe@data$names,
    R = adjusted_dataframe@statistics$mean * 100,
    S = sqrt(diag(adjusted_dataframe@statistics$Cov) * 100)
  )

  mv <- kable(mv,
              caption = "I rendimenti medi (R) e le volatilit? mensili (S) in percentuale",
              format.args = list(decimal.mark = ",", digits = 2))

  #Create the Covariance Matrix
  covMatrix <-
    kable((round(
      adjusted_dataframe@statistics$Cov, digits = 5
    )),
    
    row.names = TRUE,
    caption = "La matrice varianza/covarianza",
    
    format.args = list(
      decimal.mark = ",",
      digits = 4,
      nsmall = 4,
      scientific = FALSE
    )
    )
  
  CovMatrix_Plot <- assetsCorImagePlot(assetsSeries)
  
  print(CovMatrix_Plot)
  
  return(covMatrix)

}


covarianceMatrix <- function(adjusted_dataframe) {
  mean_dataframe <- adjusted_dataframe[1,]
  var_dataframe <- adjusted_dataframe[2,]
  stdev_dataframe <- adjusted_dataframe[3,]

  covMatrix <-
    cov(
      adjusted_dataframe,
      y = NULL,
      use = "everything",
      method = c("pearson")
    )

  covCorMatrix <- cov2cor(covMatrix)

  print(covCorMatrix)

  result <- list(covMatrix, covCorMatrix)

  names(result) <- c("covariance matrix", "correlation matrix")

  return(result)
  
}

groupReturns <- function(input_dataset, type = null) {
  #input_dataset = dataset with returns and date
  
  #type = flag to group = if month [m] or year [y]

  result <- data.frame()

  if (type == "m") {
    for (i in 1:ncol(input_dataset)) {
      temp_result <- na.omit(monthlyReturn(na.omit(input_dataset[, i])))

      result <- cbind(result, temp_result)
    }

    titleNames <- colnames(input_dataset)
    colnames(result) <- myTitles

  }
  else if (type == "y") {
    for (i in 1:ncol(input_dataset)) {
      temp_result <- yearlyReturn(na.omit(input_dataset[, i]))

      result <- cbind(result, temp_result)
    }

    titleNames <- colnames(input_dataset)
    colnames(result) <- myTitles
  }
  
  else{
    print("please specify a tipe of a group: m = month or y = year")
    
  }
  
  return(result)

}





portfolioOptimization <- function(input_dataframe) {
  print("Start creating portfolio and max portfolio")
  
  #Generate Portfolio object
  port <- portfolio.spec(assets = colnames(input_dataframe))
  
  #Box
  # port <- add.constraint(montlyReturns, type = "box", min = 0.05, max = 0.8)
  port <- add.constraint(port,
                         type = "box",
                         min = 0.05,
                         max = 0.5)
  
  #Leverage
  port <- add.constraint(portfolio = port, type = "full_investment")
  
  
  #Generate random portfolios
  rportfolios <-
    random_portfolios(port, permutations = 5000, rp_method = "sample")
  
  #Get minimum variance portfolio
  minvar.port <- add.objective(port, type = "Risk", name = "var")
  
  # Generate maximum return portfolio
  maxret.port <- add.objective(port, type = "return", name = "mean")
  
  # Optimize
  maxret.opt <-
    optimize.portfolio(dailyResults,
                       maxret.port,
                       optimize_method = "random",
                       
                       rp = rportfolios)
  
  result <- maxret.opt
  
  return(result)
}


#################################################
############### TEST FUNCTIONS ##################

myTitles <- c("GOOG", "AAPL", "MSFT", "AMZN", "INTC")
source = "yahoo"

titles <- getTitles(myTitles, source)
dailyResults <- getDailyResults(titles)

adjustedDailyResults <- adjustDataframes(dailyResults)
result <- covarianceMatrix(dailyResults)


# t <- covarianceMatrixTimeSeries(dailyResults)

#1) calcolare rendimenti mensili per i titoli
montlyReturns <- groupReturns(dailyResults, "m")

print(montlyReturns)


#2) calcolare rendimento del portafoglio
optMaxRet <- portfolioOptimization(dailyResults)

#3) calcolare excess return (rendimento portafoglio - risk free)
#4) calcolare sharpe ratio manualmente

#TODO: Non so cosa fare ora

meanReturns <- colMeans(dailyResults)

minret <- 0.06 / 100

maxret <- optMaxRet$weights %*% meanReturns

vec <- seq(minret, maxret, length.out = 100)

eff.frontier <- data.frame(
  Risk = rep(NA, length(vec)),
  
  Return = rep(NA, length(vec)),
  
  SharpeRatio = rep(NA, length(vec))
)

#https://moderndata.plot.ly/portfolio-optimization-using-r-and-plotly/



# Method 1: use the Return.excess function from PerformanceAnalytics,

# then calculate the Sharpe Ratio manually.

# portfolio_excess_returns <- Return.excess(portfolio_monthly_returns, Rf = .0003)

#

# sharpe_ratio_manual <- round(mean(portfolio_excess_returns)/StdDev(portfolio_excess_returns), 4)


# If we wanted to use the original, 1966 formulation of the Sharpe Ratio, there is one small

# change to the code in Method 1.

# sharpe_ratio_manual <- round(mean(portfolio_excess_returns)/StdDev(portfolio_monthly_returns), 4)


# Method 2: use the built in SharpeRatio function in PerformanceAnalytics.

# sharpe_ratio <- round(SharpeRatio(portfolio_monthly_returns, Rf = .0003), 4)


#media <- average(nomedataframe$nomecolonna)

#media <- mean(nomedataframe[,numerocolonna])


#TODO FINAL: correlation matrix between titles (variance, covariance)

#TODO: Pesi del portafoglio (di tangenza)

pspec <- portfolio.spec(colnames(dailyResults))
pspec <-
  add.constraint(
    portfolio = pspec,
    type = "box",
    min = 0,
    max = 1
  )

rp1 <-
  random_portfolios(portfolio = pspec,
                    permutations = 5000,
                    rp_method = "sample")

tmp1.mean <- apply(rp1, 1, function(x)
  mean(pspec %*% x))
tmp1.StdDev <- apply(rp1, 1, function(x)
  StdDev(R = pspec, weights = x))


opt <-
  optimize.portfolio(rp1,
                     portfolio = opt,
                     optimize_method = "ROI",
                     trace = T)
pesiopt <- extractWeights(opt)


plot(
  opt,
  return.col = "mean",
  risk.col = "StdDev",
  chart.assets = T,
  main = "Portafoglio ottimizzato e suoi pesi"
)
