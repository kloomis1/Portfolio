# AutoCharts.R, Kyle Loomis, Updated 2017-06-07

# ==================================================================
# Charting examples on quantmod: http://www.quantmod.com/examples/charting/
# Imports quantmod library to retrieve stock data from Yahoo! Finance
library(quantmod)
library(lubridate)
library(IBrokers)
require(RCurl)
require(jsonlite)

# returns vector of csv col
csVector <- function(filedir, header, col) { return(as.character(read.csv(file = filedir, header = header)[,col])) }

chartsQuantmod <- function(symbolList, timeframe, rth) {
  # timeframe must be in form: '2016-06-06::2017-03-06'
  # initialize a list to store the fetched prices
  dataList <- list()
  # loop through symbols, calls historicalData function and stores in dataList
  dataList <-lapply(symbolList, function(x) { getSymbols(x,auto.assign=FALSE) })
  names(dataList) <- symbolList
  
  path <- file.path("/Users/kloomis/Desktop/Code/Quant/Charting/Charts/Prospects",Sys.Date())
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

# tws connection object
conn <- function() {
  tws <- twsConnect(clientId=101, host="localhost", port=7496, verbose=TRUE, timeout=15, filename=NULL)
  return(tws)
}

chartsIB <- function(tws, symbolList, barsize, timeframe, rth) {
  symbolList <- c(symbolList)
  # initialize a list to store the fetched prices
  dataList <- list()
  # loop through symbols, calls historicalData function and stores in dataList
  dataList <-lapply(symbolList, function(x) {
    contract <- twsEquity(x,'SMART','ISLAND') # equity specification
    reqHistoricalData(tws, contract, barSize = barsize, duration = timeframe, useRTH = rth)
  })
  names(dataList) <- symbolList
  
  path <- file.path("/Users/kloomis/Desktop/Code/Quant/Charting/Charts/Prospects",Sys.Date())
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

newsUpdates <- function(tws) {
  reqNewsBulletins(tws,TRUE)
}

researchChartsIB <- function(tws, symbolList, barsize, timeframe, rth) {
  symbolList <- c(symbolList)
  # initialize a list to store the fetched prices
  dataList <- list()
  # loop through symbols, calls historicalData function and stores in dataList
  dataList <-lapply(symbolList, function(x) {
    contract <- twsEquity(x,'SMART','ISLAND') # equity specification
    reqHistoricalData(tws, contract, barSize = barsize, duration = timeframe, useRTH = rth)
  })
  names(dataList) <- symbolList
  
  path <- file.path("/Users/kloomis/Desktop/Code/Quant/Charting/Charts/Prospects",Sys.Date())
  # creates directory if path doesn't exist
  if (!file.exists(path)) { dir.create(path) }
  
  for (i in symbolList) {
    path <- file.path("/Users/kloomis/Desktop/Code/Quant/Charting/Charts/Prospects",paste0(Sys.Date(),'/', i))
    # creates directory if path doesn't exist
    if (!file.exists(path)) { dir.create(path) }
    
    pdf(file = paste0(paste0(path,"/"), i, " - " , timeframe, "; ", barsize,".pdf"))
    candleChart(dataList[[i]], type = 'candles', name = i, TA = 'addVo();addEMA();addWMA();addBBands();addRSI();addMACD();',
                theme = chartTheme('white'), up.col = 'green', dn.col = 'red', major.ticks = barsize, show.grid = TRUE)
    dev.off()
  }
  # for testing purposes
  return(TRUE)
}



chartTF <- function(tws, symbolList, barsize, timeframeVector, rth) {
  for (i in 1:length(timeframeVector)) {
    researchChartsIB(tws, symbolList, barsize, timeframeVector[i], rth)
  }
  # for testing purposes
  return(TRUE)
}

# timeframeVector
tws <- conn()
symbolList <- c('GS','AMD','MU','AUO')
tfHours <- c('1 M','2 M')
tfDays <- c('3 M','6 M','1 Y')
tfWeeks <- c('2 Y','3 Y')

chartTF(tws,symbolList,'1 hour',tfHours,'1')
chartTF(tws,symbolList,'1 day',tfDays,'1')
chartTF(tws,symbolList,'1 week',tfWeeks,'1')

url.keyratios <- function(x) {
  return(paste0("http://financials.morningstar.com/ajax/exportKR2CSV.html?t=",x))
}

getFundamentals <- function(symbol) {
  str.keyratios<-getURL(url.keyratios(symbol))
  kr.fin <- sub(".*Financials\n(.*)Key Ratios -> Profitability.*","\\1",str.keyratios)
  kr.margins <- sub(".*Key Ratios -> Profitability\n(.*)Profitability.*","\\1",str.keyratios)
  kr.profit <- sub(".*Key Ratios -> Profitability.*(Profitability.*)Key Ratios -> Growth.*","\\1",str.keyratios)
  kr.growth<-sub(".*Key Ratios -> Growth\n(.*)Key Ratios -> Cash Flow.*","\\1",str.keyratios)
  kr.cashflow<-sub(".*Key Ratios -> Cash Flow\n(.*)Key Ratios -> Financial Health.*","\\1",str.keyratios)
  kr.balance<-sub(".*Key Ratios -> Financial Health\n(Balance Sheet Items.*)Liquidity/Financial Health.*","\\1",str.keyratios)
  kr.liquid<-sub(".*Key Ratios -> Financial Health.*(Liquidity/Financial Health.*)Key Ratios -> Efficiency Ratios.*","\\1",str.keyratios)
  kr.eff<-sub(".*Key Ratios -> Efficiency Ratios\n(.*)","\\1",str.keyratios)
  
  
  path <- file.path("/Users/kloomis/Desktop/Code/Quant/Charting/Charts/Prospects",Sys.Date())
  # creates directory if path doesn't exist
  if (!file.exists(path)) { dir.create(path) }
  
  path <- file.path("/Users/kloomis/Desktop/Code/Quant/Charting/Charts/Prospects",paste0(Sys.Date(),'/', symbol))
  # creates directory if path doesn't exist
  if (!file.exists(path)) { dir.create(path) }
  
  setwd(path)
  
  write.csv(read.csv(textConnection(kr.fin)), 'Financials.csv')
  write.csv(read.csv(textConnection(kr.margins)), 'Margins.csv')
  write.csv(read.csv(textConnection(kr.profit)), 'Profitability.csv')
  write.csv(read.csv(textConnection(kr.growth)), 'Growth.csv')
  write.csv(read.csv(textConnection(kr.cashflow)), 'CashFlow.csv')
  write.csv(read.csv(textConnection(kr.balance)), 'BalanceSheet.csv')
  write.csv(read.csv(textConnection(kr.liquid)), 'Liquidity.csv')
  write.csv(read.csv(textConnection(kr.eff)), 'Efficiency.csv')
  
  return(TRUE)
}


rankDebtEquity <- function(symbolList) {
  dataList <- list()
  # loop through symbols, calls historicalData function and stores in dataList
  dataList <-lapply(symbolList, function(x) {
    tryCatch({
      str.keyratios<-getURL(url.keyratios(x))
      kr.liquid<-sub(".*Key Ratios -> Financial Health.*(Liquidity/Financial Health.*)Key Ratios -> Efficiency Ratios.*","\\1",str.keyratios)
      as.numeric((read.csv(textConnection(kr.liquid)))[4,12])
    }, warning = function(w) {0}, error = function(e) {0}, finally = {0})
  })
  
  names(dataList) <- symbolList
  dataList <- sort.int(unlist(dataList))
  return(dataList)
}

rankCurrentRatio <- function(symbolList) {
  missingVal <- 0
  dataList <- list()
  # loop through symbols, calls historicalData function and stores in dataList
  dataList <-lapply(symbolList, function(x) {
    tryCatch({
      str.keyratios<-getURL(url.keyratios(x))
      kr.liquid<-sub(".*Key Ratios -> Financial Health.*(Liquidity/Financial Health.*)Key Ratios -> Efficiency Ratios.*","\\1",str.keyratios)
      ratio <- as.numeric((read.csv(textConnection(kr.liquid)))[1,12])
      if (is.na(ratio)) { missingVal } else { ratio } }, 
      warning = function(w) {missingVal}, error = function(e) {missingVal}, finally = {missingVal})
  })
  
  names(dataList) <- symbolList
  dataList <- sort(unlist(dataList))
  return(dataList)
}

symbols <- csVector('/Users/kloomis/Desktop/Code/Quant/Data/Datasets/Finance.csv',TRUE,1)

debtEquity <- rankDebtEquity(symbols)
currentRatio <- rankCurrentRatio(symbols)

debtEquity
currentRatio
