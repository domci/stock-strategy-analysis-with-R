rm(list=ls())
library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
library(TTR, warn.conflicts = FALSE, quietly = TRUE)
library(xts)
library(XLConnect)
library(tcltk)


# Get all availible Ticker Symbols
#SymbolList <- stockSymbols()



endDate <- Sys.Date()





# Ticker Lookup: https://finance.yahoo.com/lookup

#Download the stock history (for all tickers)
S1      <- suppressWarnings(getSymbols("^GDAXI", src='yahoo', auto.assign = F))
S2      <- suppressWarnings(getSymbols("NESM.DE", src='yahoo', auto.assign = F))
S3      <- suppressWarnings(getSymbols("SPLV", src='yahoo', auto.assign = F))

SP500   <- suppressWarnings(getSymbols("^GSPC", src='yahoo', auto.assign = F))
       

startDate <- max(min(index(S1)), min(index(S2)), min(index(S3)), min(index(SP500)))






S1        <- S1[index(S1)>=startDate]
S2        <- S2[index(S2)>=startDate]
S3        <- S3[index(S3)>=startDate]
SP500     <- SP500[index(SP500)>=startDate]


all <- merge(S1, S2, S3, SP500)
head(all)

all <- na.locf(all, fromLast=T)



nrow(SP500)
nrow(S1)
nrow(S2)
nrow(S3)




S1    <- all[,1:6]
S2    <- all[,7:12]
S3    <- all[,13:18]
SP500 <- all[,19:24]


length(S1)
length(S2)
length(S3)
length(SP500)



# Daily Returns
R1        <- periodReturn(S1, period='daily')
R2        <- periodReturn(S2, period='daily')
R3        <- periodReturn(S3, period='daily')
r.SP500   <- periodReturn(SP500, period='daily')


names(R1)       <- "R1"
names(R2)       <- "R2"
names(r.SP500)  <- "r.SP500"
names(R3)       <- "R3"




#Lets plot the data
chartSeries(S1)
addBBands(n=50, sd=2)

chartSeries(S2)
addBBands(n=50, sd=2)

chartSeries(SP500)
addBBands(n=50, sd=2)

chartSeries(S3)
addBBands(n=50, sd=2)





# Relative Performace vs. Benchmark
    # Nestle vs S&P 500
chart.RelativePerformance(R1, as.vector(r.SP500), main = "Relative Performace vs. Benchmark", xaxis = TRUE)
chart.RelativePerformance(R2, as.vector(r.SP500), main = "Relative Performace vs. Benchmark", xaxis = TRUE)
chart.RelativePerformance(R3, as.vector(r.SP500), main = "Relative Performace vs. Benchmark", xaxis = TRUE)


#Active Premium The return on an investment's annualized return minus the benchmark's annualized return.
act.premium <- data.frame(S1=ActivePremium(R2, r.SP500, scale = 252),
                          S2=ActivePremium(R3, r.SP500, scale = 252), 
                          S3=ActivePremium(R3, r.SP500, scale = 252),
                          SP500=ActivePremium(r.SP500, r.SP500, scale = 252))
row.names(act.premium) <- "act.premium"



returns <- cbind(S1=R1, S1=R2, S3=R3, sp500=r.SP500)



charts.BarVaR(returns, width = 20, gap = 0, methods = "StdDev", p = 0.95, 
              clean = "none", all = TRUE, show.clean = FALSE, show.horizontal = TRUE, 
              show.symmetric = FALSE, show.endvalue = TRUE, show.greenredbars = TRUE, 
              ylim = NA, colorset = 1:12, lty = 1, ypad = 0, legend.cex = 0.8)



## CAPM KPIs
CAPM <- data.frame(table.CAPM(R1, r.SP500, scale = NA, Rf = -.01, digits = 4), 
        table.CAPM(R2, r.SP500, scale = NA, Rf = -.01, digits = 4),
        table.CAPM(R3, r.SP500, scale = NA, Rf = -.01, digits = 4))

RiskPremium <- data.frame(CAPM.RiskPremium(R1, Rf = -.01), CAPM.RiskPremium(R2, Rf = -.01), CAPM.RiskPremium(R3, Rf = -.01), CAPM.RiskPremium(r.SP500, Rf = -.01))



#historical and parametric VaR estimates:
chart.VaRSensitivity(R1, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))
chart.VaRSensitivity(R2, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))
chart.VaRSensitivity(R3, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))
chart.VaRSensitivity(r.SP500, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))


#Performance Summary
charts.PerformanceSummary(R1, Rf = -0.01)
charts.PerformanceSummary(R2, Rf = -0.01)
charts.PerformanceSummary(R3, Rf = -0.01)
charts.PerformanceSummary(r.SP500, Rf = -0.01)



#Histogramm der Rendite mit VaR 95%
chart.Histogram(R2, main = "Risk Measures", methods = c("add.risk"))


VaR <- data.frame(VaR(R2, p = 0.95), VaR(R2, p = 0.95), VaR(R3, p = 0.95),VaR(r.SP500, p = 0.95))

#(geometric) cumulative return
cumReturn <- data.frame(Return.cumulative(R1), Return.cumulative(R2), Return.cumulative(R3), Return.cumulative(r.SP500))


#SharpeRatio 
Sharpe <- data.frame(SharpeRatio(R1, Rf = -0.01), SharpeRatio(R2, Rf = -0.01), SharpeRatio(R3, Rf = -0.01), SharpeRatio(r.SP500, Rf = -0.01))

names(RiskPremium) <- c("S1", "S2", "S3", "S&P500")
names(Sharpe) <- c("S1", "S2", "S3", "S&P500")
names(act.premium) <- c("S1", "S2", "S3", "S&P500")
names(cumReturn) <- c("S1", "S2", "S3", "S&P500")
names(VaR) <- c("S1", "S2", "S3", "S&P500")
names(act.premium)

PerformanceSummary <- rbind(RiskPremium, Sharpe, act.premium, cumReturn, VaR)
PerformanceSummary
