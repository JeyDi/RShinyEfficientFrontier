library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(PortfolioAnalytics)
library(plotly)
######################STEP ONE: Create Returns Time Series#########################################

#Create Vector of Tickers
tickers <- c("AXP", "MSFT", "GOOG", "AMZN", "JNJ", "HD")

#Calculate Returns: Daily
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2016-01-01", auto.assign=FALSE)[,4])

#Delete all dates with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,function(x) all(!is.na(x))),]
#Rename Columns
colnames(portfolioPrices) <- tickers

#Calculate Returns: Daily RoC
portfolioReturns <- na.omit(dailyReturn(portfolioPrices))
#portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
portfolioReturns <- as.timeSeries(portfolioReturns)

portfolioReturns
#Calculate Monthly or Weekly Returns

Stock_Data <- tickers %>% lapply(function(x) getSymbols.yahoo(x, from="2016-01-01", auto.assign=FALSE)[,4]) %>%
  lapply(function(x) monthlyReturn(x))

portfolioReturns <- do.call(merge, Stock_Data)
# keep only the dates that have closing prices for all tickers
portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
colnames(portfolioReturns) <- tickers

#################STEP TWO: Calculate and Plot Frontier and Efficient Portfolios##############
# calculate the efficient frontier
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


plot(effFrontier,c(1,2,3,4))
#Plot Frontier Weights (Can Adjust Number of Points)
frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
#frontierWeights
colnames(frontierWeights) <- tickers
risk_return <- frontierPoints(effFrontier)
risk_return
write.csv(risk_return, "2710risk_return.csv")

#Output Correlation
cor_matrix <- cor(portfolioReturns)
cov_matrix <- cov(portfolioReturns)
cov_matrix
write.csv(cov_matrix, "covmatrix.csv")

#Annualize Data
riskReturnPoints <- frontierPoints(effFrontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn=riskReturnPoints[,"targetReturn"] * 252)
plot(annualizedPoints)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0
plot((annualizedPoints[,"targetReturn"]-riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")

#-----------
#Draftconstraints <- add.constraint(portfolioReturns, type = "box", min = 0.05, max = 0.8)
constraints <- c("minW[(tickers)]=0.07", "maxW[(tickers)]=0.5")


#Get Minimum Variance Port, Tangency Port, etc.
mvp <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
mvp

tangencyPort <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints = constraints)
tangencyPort

mvpweights <- getWeights(mvp)
tangencyweights <- getWeights(tangencyPort)

#ggplot MVP Weights
df <- data.frame(mvpweights)
assets <- colnames(frontierWeights)
ggplot(data=df, aes(x=assets, y=mvpweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",mvpweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

dft <- data.frame(tangencyweights)
assets <- colnames(frontierWeights)
ggplot(data=dft, aes(x=assets, y=tangencyweights, fill=assets)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",tangencyweights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")



###########################Examine Constraints and Stats#####################################
#Example Constraints:
#"minW[asset]=percentage" for box constraints resp.
#"maxsumW[assets]=percentage" for sector constraints.
#eqsumWConstraints(data, spec=portfolioSpec(), constraints="LongOnly")

#Set Specs
Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
setTargetRisk(Spec) = .12
constraints <- c("minW[1:length(tickers)]=-1","maxW[1:length(tickers)]=.60", "Short")
Secondconstraints <- c("minW[tickers]=0.00","maxW[tickers]=0.5")
#frontierpoint <- `setNFrontierPoints<-`(Spec, value=15)

effFrontierShort <- portfolioFrontier(portfolioReturns, constraints = Secondconstraints)
weights <- getWeights(effFrontierShort)

write.csv(weights, "weightsShort_v1.csv")
colnames(weights) <- tickers


plot(effFrontierShort, c(1, 2, 3, 4, 5))

#Plot Frontier Weights (Need to transpose matrix first)
barplot(t(weights), main="Frontier Weights", col=cm.colors(ncol(weights)+2), legend=colnames(weights))

effPortShort <- minvariancePortfolio(portfolioReturns, Spec, constraints=constraints)
optWeights <- getWeights(effPortShort)
tanPortShort <- tangencyPortfolio(portfolioReturns, Spec, constraints=constraints)
tanWeights <- getWeights(tanPortShort)
maxR <- maxreturnPortfolio(portfolioReturns , Spec, constraints=constraints)
maxWeights <- getWeights(maxR)

#try
effport <- efficientPortfolio(portfolioReturns, Spec, constraints = Secondconstraints)

