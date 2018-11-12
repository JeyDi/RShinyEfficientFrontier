library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(PortfolioAnalytics)
library(plotly)


source("./Utility/EfficientFrontierPlot.R")

# environmentSettings()


######################STEP ONE: Create Returns Time Series#########################################


#Download and get the titles
getTitles <- function(myTitle,source,start.date=as.Date("2017-09-09"),end.date=as.Date("2018-09-09")){
  
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


#Trasform titles into daily returns
getDailyReturn <- function(my_titles_dataframe){
  # result <- sapply(my_titles_dataframe, function(x) dailyReturn(x) )
  result <- data.frame()
  
  for(i in 1:ncol(my_titles_dataframe)){
    temp_result <- dailyReturn(my_titles_dataframe[,i])
    result <- cbind(result,temp_result)
  }
  
  titleNames <- colnames(my_titles_dataframe)
  colnames(result) <- myTitles
  
  result <- as.timeSeries(result)
  
  return(result)
}


#Trasform titles into monthly returns
getMontlyReturn <- function(my_titles_dataframe){
  result <- data.frame()
  
  for(i in 1:ncol(my_titles_dataframe)){
    temp_result <- monthlyReturn(my_titles_dataframe[,i])
    result <- cbind(result,temp_result)
  }
  
  titleNames <- colnames(my_titles_dataframe)
  colnames(result) <- myTitles
  
  result <- as.timeSeries(result)
  
  return(result)
}


#Configurate the solver parameter for the efficient frontier calculation
define_spec <- function(solver="solveRquadprog",targetRisk=.12){
  #Create the solver
  Spec = portfolioSpec()
  setSolver(Spec) = solver
  setTargetRisk(Spec) = targetRisk
  
  return(Spec)
}


#Set constraint parameters 
set_constraint <- function(myTitles,minW=.03,maxW=.60){
  
  constraint_parameters_minW <- paste("minW[1:length(myTitles)]=",minW,sep="")
  constraint_parameters_maxW <- paste("maxW[1:length(myTitles)]=",maxW,sep="")
  
  constraints <- c(constraint_parameters_minW,constraint_parameters_maxW)
  
  return(constraints)
}


#Calculate efficient frontier
efficient_frontier_calculation <- function(portfolioReturnDataset,Spec=portfolioSpec(),constraints="LongOnly"){
  
  effFrontier <- portfolioFrontier(portfolioReturnDataset, Spec, constraints = constraints)

  return(effFrontier)
  
}


#Calculate weights for an input dataset
weights_calculation <- function(dataset,writeCsv = FALSE,myTitles,datasetName="dataset"){
  
  frontierWeights <- getWeights.fPORTFOLIO(dataset) # get allocations for each instrument for each point on the efficient frontier
  colnames(frontierWeights) <- myTitles
  
  if(writeCsv){
    filename = paste(datasetName,"-Weights.csv",sep="")
    write.csv(frontierWeights, filename)  
  }
  
  return(frontierWeights)
}

#Calculate risk returns
risk_return_calculation <- function(effFrontier, writeCsv = FALSE){
  
  risk_return <- frontierPoints(effFrontier, return = "mean", risk = "Sigma")
  
  if(writeCsv){
    write.csv(risk_return, "risk_return1.csv")  
  }
  
  return(risk_return)
}

#Calculate correlation and covariance from dataset
corr_cov_calculation <- function(portfolioReturns, writeCsv = FALSE){
  

  #Output Correlation
  cor_matrix <- cor(portfolioReturns)
  cov_matrix <- cov(portfolioReturns)
  
  result <- list(cor_matrix, cov_matrix)
  
  if(writeCsv){
    write.csv(cor_matrix, "cormatrix.csv")
    write.csv(cov_matrix, "covmatrix.csv")  
  }
  
  return(result)
  
}


#Annualize data
annualize_data_calculation <- function(effFrontier_dataset, writeCsv=FALSE){
  
  #Annualize Data
  riskReturnPoints <- frontierPoints(effFrontier_dataset) # get risk and return values for points on the efficient frontier
  annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                                 targetReturn=riskReturnPoints[,"targetReturn"] * 252)
  
  if(writeCsv){
    write.csv(annualizedPoints, "annualizedPoints.csv")
  }
  
  return(annualizedPoints)
}


#Calculate min variance portfolio and weights
#return a list with sublists
min_variance_portfolio_calculation <- function(portfolioReturn_dataframe,Spec=portfolioSpec(),constraints="LongOnly"){
  

  result <- minvariancePortfolio(portfolioReturn_dataframe, Spec, constraints=constraints)  

  return(result)
}

#Calculate tangency portfolio 
#If you want to use the constraint, you have to set the checkConstraint flag = TRUE and also use Spec and constraint as parameters
tangency_portfolio_calculation <- function(portfolioReturn_dataframe,checkConstraint=FALSE,Spec=portfolioSpec(),constraints="LongOnly"){
  
  result <- tangencyPortfolio(portfolioReturn_dataframe, Spec, constraints=constraints)
  
  return(result)
}


#Calculte max return portfolio
max_return_portfolio_calculation <- function(portfolioReturn_dataframe,checkConstraint=FALSE,Spec=portfolioSpec(),constraints="LongOnly"){

  result <- maxreturnPortfolio(portfolioReturn_dataframe , Spec, constraints=constraints)  

  return(result)
}

######################################################################################?

####TEST FUNCTIONS####

#FUNCTIONS LAUNCH ORIGINAL ORDER
#'1: GetTitles
#'2: DailyReturns, MonthlyReturns
#'3: Annualize Data
#'4: Efficient Frontier without constraints
#'5: Minimum Variance
#'6: Tangency Portfolio
#'P: PLOT: Efficient Frontier without constraints
#'7: Calculate contraints and spec
#'8: Efficient Frontier with constraints
#'9: Minimum Variance constraints
#'10: Tangency Portfolio constraints
#'11: Max returns constraints
#'P: PLOT: Efficient Frontier with constraints plot
#'P: PLOT: Barchart with Tangency Portfolio without constraints
#'P: PLOT: Barchart with Tangency Portfolio with constraints

myTitles <- c("GOOG","AAPL","MSFT","AMZN","INTC")
source = "yahoo"

titles <- getTitles(myTitles,source)
dailyReturns <- getDailyReturn(titles)
monthlyReturns <- getMontlyReturn(titles)

effFrontier <- efficient_frontier_calculation(monthlyReturns)


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

plotResult <- plot.fPORTFOLIO(effFrontier, c(1,2,3,4))

grid()
tailoredFrontierPlot(effFrontier, risk = "Sigma")
grid()

annualized_data <- annualize_data_calculation(effFrontier)

spec <- define_spec()
constraint <- set_constraint(myTitles)

#Efficient Frontier Plot with the constraints
#TODO Launch the efficient frontier and plot
effFrontierConstraint <- efficient_frontier_calculation(monthlyReturns,Spec = spec, constraints = constraint)
plot(effFrontierConstraint, c(1, 2, 3))

minimumVariance <- min_variance_portfolio_calculation(monthlyReturns,Spec = spec, constraints = constraint)

tangencyPortfolio <- tangency_portfolio_calculation(monthlyReturns,Spec = spec, constraints = constraint)



frontierWeights <- weights_calculation(effFrontier,FALSE,myTitles)

riskReturn <- risk_return_calculation(effFrontier)

# corrCov <- corr_cov_calculation(dailyReturns)





#################################################################?


##prova##
# x <- fPortfolio::getOptimize.fPORTFOLIO(effFrontier)
# x

#Extract value at risk
# covRisk(portfolioReturns, minvariancePortfolioweights)
# varRisk(portfolioReturns, minvariancePortfolioweights, alpha = 0.05)
# cvarRisk(portfolioReturns, minvariancePortfolioweights, alpha = 0.05)


#######
#TODO:Tentativo di fare con plotly il grafico della frontiera efficiente
##grafico plotly eff frontier##

# SpecProva <- portfolio.spec(effFrontierShort)
# create.EfficientFrontier(portfolioReturns, SpecProva, type = "mean-StdDev")
# weightsprovagrafico <- plot_ly(weightsprova, x=assets, y=tanWeights*100, type = "scatter",
#                                mode="lines",               
#                                marker = list(color = "brown",
#                                                             width = 1.5)) %>%
#   layout(title="Tangency Portfolio Weights SHORT",
#          xaxis=list(title="Assets"),
#          yaxis=list(title="Weight (%)"))
# weightsprovagrafico


##########
##prova settargetreturn##
# SpecReturn = portfolioSpec()
# setSolver(SpecReturn) = "solveRquadprog"
# setTargetReturn(SpecReturn) = .5
# constraintsReturn <- c("minW[1:length(myTitles)]=.03","maxW[1:length(myTitles)]=.60")
