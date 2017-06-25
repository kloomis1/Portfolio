library("quantmod")

SPY <- getSymbols("SPY", src = "google", from = Sys.Date() - lubridate::years(1), auto.assign = FALSE)
SPY <- adjustOHLC(SPY)

SPY.SMA.10 <- SMA(Cl(SPY), n = 5)
SPY.SMA.200 <- SMA(Cl(SPY), n = 100)
SPY.RSI.14 <- RSI(Cl(SPY))
SPY.RSI.SellLevel <- xts(rep(70, NROW(SPY)), index(SPY))
SPY.RSI.BuyLevel <- xts(rep(30, NROW(SPY)), index(SPY))


highchart(type = "stock") %>% 
  # create axis :)
  hc_yAxis_multiples(
    create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
  ) %>% 
  # series :D
  hc_add_series(SPY, yAxis = 0, name = "SPY") %>% 
  hc_add_series(SPY.SMA.10, yAxis = 0, name = "Fast MA") %>% 
  hc_add_series(SPY.SMA.200, yAxis = 0, name = "Slow MA") %>% 
  hc_add_series(SPY$SPY.Volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
  hc_add_series(SPY.RSI.14, yAxis = 2, name = "Osciallator", color = hex_to_rgba("green", 0.7)) %>%
  hc_add_series(SPY.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                yAxis = 2, name = "Sell level") %>% 
  hc_add_series(SPY.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                yAxis = 2, name = "Buy level")