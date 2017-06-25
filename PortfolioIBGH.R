# PortfolioIB.R, Kyle Loomis, Updated 2017-06-07

# ==================================================================
# IBrokers API documentation: https://cran.r-project.org/web/packages/IBrokers/IBrokers.pdf
# TTR Package for technical analysis: https://cran.r-project.org/web/packages/TTR/TTR.pdf

library(IBrokers)
library(quantmod)
library(PerformanceAnalytics)
library(TTR)

########################## Being Functions ###########################
######################################################################

timeframe <- function(months) {
  # date range in form: 'yyyy-mm-dd::yyyy-mm-dd'
  startDate <- toString(Sys.Date() - months(months))
  endDate <- toString(Sys.Date())
  dateRange <- paste0(startDate,"::",endDate)
  return(dateRange)
}

# Connects to TWS api
# *Check clientId and port
conn <- function() {
  tws <- twsConnect(clientId=101, host="localhost", port=7496, verbose=TRUE, timeout=15, filename=NULL)
  return(tws)
}

# account update
accountInfo <- function(tws) { return(reqAccountUpdates(tws)) }

# getters take in accountInfo account update object
# parses and returns specificied datum

# liquidity data
netLiquidation <- function(accInfo) { as.numeric(accInfo[[1]]$NetLiquidation[1]) }
grossPositionVal <- function(accInfo) { as.numeric(accInfo[[1]]$GrossPositionValue[1]) }
excessLiquidity <- function(accInfo) { as.numeric(accInfo[[1]]$ExcessLiquidity[1]) }
buyingPower <- function(accInfo) { as.numeric(accInfo[[1]]$BuyingPower[1]) }
lookAheadFunds <- function(accInfo) { as.numeric(accInfo[[1]]$LookAheadAvailableFunds[1]) }
cushionPercent <- function(accInfo) { as.numeric(accInfo[[1]]$Cushion[1]) }
# realized gains
accruedDiv <- function(accInfo) { as.numeric(accInfo[[1]]$AccruedDividend[1]) }
# margin/options
marginReq <- function(accInfo) { as.numeric(accInfo[[1]]$MaintMarginReq[1]) }
lookAheadMarginReq <- function(accInfo) { as.numeric(accInfo[[1]]$LookAheadMaintMarginReq[1]) }
optionMarketVal <- function(accInfo) { as.numeric(accInfo[[1]]$OptionMarketValue[1]) }


fundLiquidity <- function(accInfo) {
  liquidity <- c(
    netLiquidation(accInfo),
    3000,
    netLiquidation(accInfo) / 3000,
    grossPositionVal(accInfo),
    excessLiquidity(accInfo),
    buyingPower(accInfo),
    lookAheadFunds(accInfo),
    cushionPercent(accInfo),
    accruedDiv(accInfo)
  )
  names(liquidity) <- c("Net Liquidation",
                        "Starting Value",
                        "Percent Gain",
                        "Gross Position Val",
                        "Excess Liquidity",
                        "Buying Power",
                        "Look Ahead Funds",
                        "Cushion Percent",
                        "Accrued Dividend"
  )
  return(liquidity)
}

# equity holdings data getters
symbol <- function(accInfo, i) { accInfo[[2]][[i]]$contract$symbol }
contractID <- function(accInfo, i) { as.numeric(accInfo[[2]][[i]]$contract$conId) }
contractExch <- function(accInfo, i) { accInfo[[2]][[i]]$contract$exchange }
positionNum <- function(accInfo, i) { as.numeric(accInfo[[2]][[i]]$portfolioValue$position) }
marketPrice <- function(accInfo, i) { as.numeric(accInfo[[2]][[i]]$portfolioValue$marketPrice) }
averageCost <- function(accInfo, i) { as.numeric(accInfo[[2]][[i]]$portfolioValue$averageCost) }

# calculates position weights taking into account
# cushion percentage
posWeights <- function(accInfo) {
  w <- as.numeric(positions(accInfo)[7,])
  w / sum(w)
}

positions <- function(accInfo) {
  holdings <- matrix(nrow = 7, ncol = length(accInfo[[2]]))
  
  for (i in 1:length(accInfo[[2]])) {
    holdings[1,i] <- symbol(accInfo,i)
    holdings[2,i] <- contractID(accInfo,i)
    holdings[3,i] <- positionNum(accInfo,i)
    holdings[4,i] <- marketPrice(accInfo,i)
    holdings[5,i] <- averageCost(accInfo,i)
    # percent gain - ex. +12% = 1.12
    holdings[6,i] <- marketPrice(accInfo,i) / averageCost(accInfo,i)
    # percent makeup of net liquidation
    holdings[7,i] <- (marketPrice(accInfo,i)*positionNum(accInfo,i)) / netLiquidation(accInfo)
  }
  
  # names columns according to symbol
  colnames(holdings) <- c(holdings[1,])
  rownames(holdings) <- c("Symbol","contractID","Position","Market Price","Average Cost","Percent Gain", "Percent Invested")
  
  # Multiplies decimal percents by 100
  #holdings[6,] <- as.numeric(holdings[6,])*100
  #holdings[7,] <- as.numeric(holdings[7,])*100
  
  return(holdings)
}

historicalData <- function(tws, ticker, barsize, timeframe, rth) {
  contract <- twsEquity(ticker,'SMART','ISLAND') # equity specification
  reqHistoricalData(tws, contract, barSize = barsize, duration = timeframe, useRTH = rth)
}

portfolioCharts <- function(tws, accInfo, barsize, timeframe, rth) {
  symbolList <- c(positions(accInfo)[1,])
  
  # initialize a list to store the fetched prices
  dataList <- list()
  # loop through symbols, calls historicalData function and stores in dataList
  dataList <-lapply(symbolList, function(x) {
    historicalData(tws,x,barsize,timeframe,rth)
  })
  names(dataList) <- symbolList
  
  # replace path directory
  path <- file.path("/Users/kloomis/Desktop/Code/Quant/Charting/Charts/Portfolio",Sys.Date())
  # creates directory if path doesn't exist
  if (!file.exists(path)) { dir.create(path) }
  
  for (i in symbolList) {
    pdf(file = paste0(paste0(path,"/"), i, " - ", barsize, ", " , timeframe, ".pdf"))
    candleChart(dataList[[i]], type = 'candles', name = i, TA = 'addVo();addEMA();addWMA();addBBands();addRSI();addMACD();',
                theme = chartTheme('white'), up.col = 'green', dn.col = 'red', major.ticks = barsize, show.grid = TRUE)
    dev.off()
  }
  # for testing purposes
  return(TRUE)
}

chartTF <- function(tws, accInfo, barsize, timeframeVector, rth) {
  for (i in 1:length(timeframeVector)) {
    portfolioCharts(tws, accInfo, barsize, timeframeVector[i], rth)
  }
  # for testing purposes
  return(TRUE)
}

timeseriesTechnicals <- function(tws, symbol, barsize, timeframe, rth) {
  data <- historicalData(tws, symbol, barsize, timeframe, rth)[,6]
  data <- merge(data, TTR::EMA(data), TTR::WMA(data))
  names(data) <- c('WAP', 'EMA', 'WMA')
  return(data)
}

calcReturns <- function(tws, symbol, barsize, timeframe, rth) {
  data <- historicalData(tws, symbol, barsize, timeframe, rth)[,6] # WAP of symbol
  R <- Return.calculate(data,"discrete")
  # removes first row from data
  return(R[-1,])
}

techCrossover <- function(dataframe) {
  # takes in timeseriesTechnicals dataframe argument
  # finds crossover points between WAP and Moving Averages
  # 0 indicates MA's are below price, 1 is above
  # look for crossover points from 0 to 1 for buy indication 
  # or 1 to 0 for sell
  xOver = cbind(dataframe, "Price < EMA" = dataframe[,'WAP'] < dataframe[,'EMA'], 
                "Price < WMA" = dataframe[,'WAP'] < dataframe[,'WMA'])
  return(xOver)
}

calcCorrelation <- function(tws, symbolList, barsize, timeframe, rth) {
  data <- c()
  for (i in 1:length(symbolList)) {
    df <- historicalData(tws, symbolList[i], barsize, timeframe, rth)
    data <- cbind(data, df[,6])
  }
  return(stockPortfolio::getCorr(cov(data)))
}

# uses CAPM.beta method from Performance Analytics
calcBeta <- function(tws, symbol, index, barsize, timeframe, rth, rf) {
  equity <- historicalData(tws, symbol, barsize, timeframe, rth)
  idx <- historicalData(tws, index, barsize, timeframe, rth)
  
  # index WAP column
  PerformanceAnalytics::CAPM.beta(Ra = equity[,6], Rb = idx[,6], Rf = rf)
}

# functions similarly to PerformanceAnalytics::CAPM.beta() method
# assumes a risk free rate = 0
portfolioBeta <- function(tws, accInfo, symbolList, index, barsize, timeframe, rth) {
  betaVector <- c()
  
  idx <- historicalData(tws, index, barsize, timeframe, rth)
  # WAP of index
  idx <- c(idx[,6])
  
  for (i in 1:length(symbolList)) {
    df <- historicalData(tws, symbolList[i], barsize, timeframe, rth)
    equity <- c(df[,6])
    COV <- cov(cbind(equity,idx))
    VAR <- var(idx)
    betaVector <- cbind(betaVector, COV[1,2] / VAR)
  }
  betaVector <- c(betaVector)
  names(betaVector) <- symbolList
  
  holdings <- positions(accInfo)[7,]
  
  betaVector <- betaVector*as.numeric(holdings)
  result <- sum(betaVector) / (1- cushionPercent(accInfo))
  return(result)
}

sharpeRatio <- function(tws, accInfo, symbolList, barsize, timeframe, rth, period, weights, rf) {
  returns <- c()
  
  for (i in 1:length(symbolList)) {
    df <- historicalData(tws, symbolList[i],barsize, timeframe, rth)
    returns <- cbind(returns, periodReturn(df[,6], period = period))
  }
  pReturn <- Return.excess(Return.portfolio(R = returns, weights = weights, Rf = rf))
  
  PerformanceAnalytics::SharpeRatio(R = pReturn, FUN = "StdDev")
}

sharpeRatio(tws, info, positions(info)[1,], '1 day', '5 Y', '1', 'monthly', posWeights(info), 0)


########################## End Functions #############################
######################################################################

# connection to tws passed into other functions
tws <- conn()

info <- accountInfo(tws)
# tests account data
netLiquidation(info)
accruedDiv(info)
excessLiquidity(info)
buyingPower(info)
grossPositionVal(info)
lookAheadFunds(info)
marginReq(info)
lookAheadMarginReq(info)
optionMarketVal(info)

# TA used for adding technical indicators in charts
ta <- paste0('addVo();addEMA(n = ',1*10,
             ');addWMA(n = ',1*10,
             ');addBBands(n = ',1*20,
             ');addRSI(n = ',1*14,
             ');addMACD(fast = ', 1*12,
             ', slow = ', 1*26,
             ', signal = ', 1*9,
             ');')

# month timeframe vector to chart
months <- c('2 M','3 M','6 M')

# calculate correlation between fund assets
# not balanced based on % holding

fundLiquidity(info)
positions(info)
posWeights(info)
historicalData(tws, 'NVDA', '1 hour', '1 M', '1')
portfolioCharts(tws, info, '1 hour', '1 M', '1')
chartTF(tws, info, '1 day', months, '1')
timeseriesTechnicals(tws, 'NVDA', '1 min', '1 D', '1')
calcCorrelation(tws, c('NVDA','AMD'),'1 day','2 Y','1')
calcBeta(tws, 'NVDA', 'SPY', '1 day', '1 Y', '1', 0)
portfolioBeta(tws, info, positions(info)[1,], 'SPY', '1 day', '4 M', '1')

# tws disconnected after all functions are run
twsDisconnect(tws)