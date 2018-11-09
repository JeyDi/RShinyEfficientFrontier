library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(PortfolioAnalytics)
library(plotly)
######################STEP ONE: Create Returns Time Series#########################################
# environmentSettings()
start.date = as.Date("2017-09-09")
end.date = as.Date("2018-09-09")
# mystocks <- new.env(hash=TRUE)
# result <- getSymbols(myTitles,src="yahoo", auto.assign = TRUE, env=mystocks)

getTitles <- function(myTitle,source){
  
  numberTitles <- ncol(myTitles)  
  
  getSymbols(myTitles, src=source, from=start.date, to=end.date)
  
  #ClosePrices <- do.call(merge, lapply(myTitles, function(x) Cl(get(x))))
  #OpenPrices <- do.call(merge, lapply(myTitles, function(x) Op(get(x))))
  #Volumes <- do.call(merge, lapply(myTitles, function(x) Vo(get(x))))
  portfolioPrices <- do.call(merge, lapply(myTitles, function(x) Ad(get(x))))
  
  # result <- as.data.frame(portfolioPrices)
  
  #Remove NA from the Portfolio Prices (some titles can have NA on columns)
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
  
  #Add the correct names to the dataframe
  colnames(portfolioPrices) <- myTitle
  
  return(portfolioPrices)
  
}

getDailyReturn <- function(my_titles_dataframe){
  # result <- sapply(my_titles_dataframe, function(x) dailyReturn(x) )
  result <- data.frame()
  for(i in 1:ncol(my_titles_dataframe)){
    temp_result <- dailyReturn(my_titles_dataframe[,i])
    result <- cbind(result,temp_result)
  }
  titleNames <- colnames(my_titles_dataframe)
  colnames(result) <- myTitles
  return(result)
}

####TEST 1 start####
myTitles <- c("GOOG","AAPL","MSFT","AMZN","INTC")
source = "yahoo"

titles <- getTitles(myTitles,source)
dailyReturns <- getDailyReturn(titles)
####TEST 1 end####

portfolioReturns <- as.timeSeries(dailyReturns)
portfolioReturns
#Calculate Monthly or Weekly Returns
Stock_Data <- myTitles %>% lapply(function(x) getSymbols.yahoo(x, from = start.date, auto.assign=FALSE)[,4]) %>%
  lapply(function(x) monthlyReturn(x))

portfolioReturns <- do.call(merge, Stock_Data)

# keep only the dates that have closing prices for all tickers
portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
colnames(portfolioReturns) <- myTitles

#################STEP TWO: Calculate and Plot Frontier and Efficient Portfolios##############
portfolioReturns <- as.timeSeries(portfolioReturns)
effFrontier <- portfolioFrontier(portfolioReturns, constraints = "LongOnly")

# plot frontier
#'Options
#'1: Plot Efficient Frontier
#'2: Plot Minimum Variance Portfolio
#'3: Plot Tangency Portfolio
#'4: Plot Risk Returns of Each Asset
#'5: Plot Equal Weights Portfolio
#'6: Plot Two Asset Frontiers (Long)
#'7: Plot Monte Carlo Portfolios
#'8: Plot Sharpe Ratio

plot.fPORTFOLIO(effFrontier, c(1,2,3,4))
grid()
tailoredFrontierPlot(effFrontier, risk = "Sigma")
grid()

frontierWeights <- getWeights.fPORTFOLIO(effFrontier) # get allocations for each instrument for each point on the efficient frontier
colnames(frontierWeights) <- myTitles
frontierWeights
write.csv(frontierWeights, "frontierWeights.csv")

risk_return <- frontierPoints(effFrontier, return = "mean", risk = "Sigma")
write.csv(risk_return, "risk_return1.csv")

#Output Correlation
cor_matrix <- cor(portfolioReturns)
cor_matrix
cov_matrix <- cov(portfolioReturns)
cov_matrix
write.csv(cor_matrix, "cormatrix.csv")
write.csv(cov_matrix, "covmatrix.csv")

#Annualize Data
riskReturnPoints <- frontierPoints(effFrontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 252)
write.csv(annualizedPoints, "annualizedPoints.csv")
plot(annualizedPoints)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0.03
plot((annualizedPoints[,"targetReturn"]-riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")

###grafico che si può togliere###
#Plot Frontier Weights (Need to transpose matrix first)
barplot(t(frontierWeights), main="Frontier Weights", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(frontierWeights))
################################
#Get Minimum Variance Port, Tangency Port, etc.
#port <- portfolio.spec(portfolioReturns)
minvariancePortfolio <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
minvariancePortfolio
tangencyPort <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
tangencyPort
##getweights da capire#################################################
minvariancePortfolioweights <- getWeights.fPORTFOLIO(minvariancePortfolio)
tangencyweights <- getWeights.fPORTFOLIO(tangencyPort)
tangencyweights
minvariancePortfolioweights
write.csv(tangencyweights, "tangencyweights.csv")
write.csv(minvariancePortfolioweights, "minvariancePortfolioweights.csv")

##prova##
x <- fPortfolio::getOptimize.fPORTFOLIO(effFrontier)
x

#Extract value at risk
covRisk(portfolioReturns, minvariancePortfolioweights)
varRisk(portfolioReturns, minvariancePortfolioweights, alpha = 0.05)
cvarRisk(portfolioReturns, minvariancePortfolioweights, alpha = 0.05)

#Plot MVP Weights: Basic Graphs
barplot(minvariancePortfolioweights, main="Minimum Variance Portfolio Weights", xlab="Asset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))
pie(minvariancePortfolioweights, col=cm.colors(ncol(frontierWeights)+2))

#ggplot minvariancePortfolioweights Weights
dfm <- data.frame(minvariancePortfolioweights)
assets <- colnames(frontierWeights)

##grafici con plotly##
minvariancePortfolioweightsGraph <-plot_ly(dfm, x=assets, y=minvariancePortfolioweights*100, type = "bar",
                            marker = list(color = 'green',
                      width = 1.5)) %>%
        layout(title="Minimum Variance Portfolio Optimal Weights",
               xaxis=list(title="Assets"),
               yaxis=list(title="Weight (%)"))
minvariancePortfolioweightsGraph

dft <- data.frame(tangencyweights)
assets <- colnames(frontierWeights)

TangencyPortfolioWeightsGraph <- plot_ly(dft, x=assets, y=tangencyweights*100, type = "bar",
                marker = list(color = "yellow",
                      width = 1.5)) %>%
  layout(title="Tangency Portfolio Weights",
         xaxis=list(title="Assets"),
         yaxis=list(title="Weight (%)"))
TangencyPortfolioWeightsGraph

##grafico con ggplot##
#ggplot(data=df, aes(x=assets, y=minvariancePortfolioweights, fill=assets)) +
  #geom_ba(stat="identity", position=position_dodge(),colour="brown") +
  #geom_text(aes(label=sprintf("%.02f %%",minvariancePortfolioweights*100)),
  #          position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  #ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  #labs(x= "Assets", y = "Weight (%)")

##inserimento constraints##
Spec = portfolioSpec()
setSolver(Spec) = "solveRquadprog"
setTargetRisk(Spec) = .12
constraints <- c("minW[1:length(myTitles)]=.03","maxW[1:length(myTitles)]=.60")

effFrontierShort <- portfolioFrontier(portfolioReturns, Spec, constraints = constraints)
weights <- getWeights.fPORTFOLIO(effFrontierShort)
##weights prova##
weightsprova <- as.data.frame(weights)
#################
write.csv(weights, "weightsShort.csv")
colnames(weights) <- myTitles
effFrontierShort

plot(effFrontierShort, c(1, 2, 3))

effPortShort <- minvariancePortfolio(portfolioReturns, Spec, constraints=constraints)
optWeights <- getWeights.fPORTFOLIO(effPortShort)
tanPortShort <- tangencyPortfolio(portfolioReturns, Spec, constraints=constraints)
tanWeights <- getWeights.fPORTFOLIO(tanPortShort)
maxR <- maxreturnPortfolio(portfolioReturns , Spec, constraints=constraints)
maxWeights <- getWeights.fPORTFOLIO(maxR)
tanPortShort


dfmSHORT <- data.frame(optWeights)
assets <- colnames(frontierWeights)

minvariancePortfolioweightsGraphSHORT <-plot_ly(dfmSHORT, x=assets, y=optWeights*100, type = "bar",
                                           marker = list(color = 'green',
                                                         width = 1.5)) %>%
  layout(title="Minimum Variance Portfolio Optimal Weights SHORT",
         xaxis=list(title="Assets"),
         yaxis=list(title="Weight (%)"))
minvariancePortfolioweightsGraphSHORT

dftShort <- data.frame(tanWeights)
assets <- colnames(frontierWeights)

colors <- c("brown", "yellow", "red", "black", "blue")
TangencyPortfolioWeightsGraphSHORT <- plot_ly(dftShort, x=assets, y=tanWeights*100, type = "bar",
                                         marker = list(color = "brown",
                                                       width = 1.5)) %>%
  layout(title="Tangency Portfolio Weights SHORT",
         xaxis=list(title="Assets"),
         yaxis=list(title="Weight (%)"))
TangencyPortfolioWeightsGraphSHORT

###prova altro codice###
frontierPlot(effFrontier, risk = "Sigma", auto = "FALSE")
tangencyLines(effFrontier, col = "orange")
singleAssetPoints(effFrontier, col = "blue", cex=1.0, pch=20, auto = "False", lwd=2)
grid()
tailoredFrontierPlot(effFrontier, risk = "Sigma")
grid()
effFrontierShortwithName <- tailoredFrontierPlot(effFrontierShort, risk = "Sigma")
effFrontierShortwithName
###fine prova altro codice###

##autenticazione plotly##
Sys.setenv("plotly_username"="simone.violin")
Sys.setenv("plotly_api_key"="030OQDiNzeofpQEcbPkR")
#######
##grafico plotly eff frontier##
SpecProva <- portfolio.spec(effFrontierShort)
create.EfficientFrontier(portfolioReturns, SpecProva, type = "mean-StdDev")
weightsprovagrafico <- plot_ly(weightsprova, x=assets, y=tanWeights*100, type = "scatter",
                               mode="lines",               
                               marker = list(color = "brown",
                                                            width = 1.5)) %>%
  layout(title="Tangency Portfolio Weights SHORT",
         xaxis=list(title="Assets"),
         yaxis=list(title="Weight (%)"))
weightsprovagrafico


