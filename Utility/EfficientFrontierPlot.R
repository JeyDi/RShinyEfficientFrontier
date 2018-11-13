print("Efficient Frontier Plot opened")

#Annualize Data Plto
annualize_data_plot <- function(annualizedPoints){
  
  annualizeDataPlot <- plot(annualizedPoints)
  return(annualizeDataPlot)
  
}


# plot Sharpe ratios for each point on the efficient frontier
sharpe_ratio_plot <- function(annualizedPoints_dataset){
  riskFreeRate <- 0.03
  sharpeRatioPlot <- plot((annualizedPoints_dataset[,"targetReturn"]-riskFreeRate) / annualizedPoints_dataset[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")
  
  return(sharpeRatioPlot)
}


#Plot Frontier Weights
frontier_weights_plot <- function(frontierWeights_dataset){
  
  frontierWeightsPlot <- barplot(t(frontierWeights_dataset), main="Frontier Weights", col=cm.colors(ncol(frontierWeights_dataset)+2), legend=colnames(frontierWeights_dataset))
  
  return(frontierWeightsPlot)
}



#Min Variance Portfolio Barplot Chart
min_variance_portfolio_barplot <- function(minvariancePortfolioweights){
  barplotchart <- barplot(minvariancePortfolioweights, 
                          main="Minimum Variance Portfolio Weights",
                          xlab="Asset",
                          ylab="Weight In Portfolio (%)",
                          col=cm.colors(ncol(frontierWeights)+2),
                          legend=colnames(weights))  
  return(barplotchart)
}


#Min Variance Portfolio Pie Chart
min_variance_portfolio_pie <- function(minvariancePortfolioweights){
  piechart <- pie(minvariancePortfolioweights, col=cm.colors(ncol(frontierWeights)+2))
  
  return(piechart)
}


##Plotly chart for min variance portfolio
min_variance_portfolio_plotly <- function(minvariancePortfolioweights,frontierWeights){
  
  
  dfm <- data.frame(minvariancePortfolioweights)
  assets <- colnames(frontierWeights)
  
  minVariancePortoflioWeightsPlot <-plot_ly(dfm, x=assets, y=minvariancePortfolioweights*100, type = "bar",
                                            marker = list(color = 'green',
                                                          width = 1.5)) %>%
    layout(title="Minimum Variance Portfolio Optimal Weights",
           xaxis=list(title="Assets"),
           yaxis=list(title="Weight (%)"))
  
  return(minVariancePortoflioWeightsPlot)
}


##Plotly chart for tangency portfolio
tangency_portfolio_plotly <- function(tangencyWeights, frontierWeights){
  
  dft <- data.frame(tangencyWeights)
  assets <- colnames(frontierWeights)
  
  tangencyPortfolioWeightsGraph <- plot_ly(dft, x=assets, y=tangencyWeights*100, type = "bar",
                                           marker = list(color = "yellow",
                                                         width = 1.5)) %>%
    layout(title="Tangency Portfolio Weights",
           xaxis=list(title="Assets"),
           yaxis=list(title="Weight (%)"))
  
  return(tangencyPortfolioWeightsGraph)
}


print("Efficient Frontier Plot loaded")