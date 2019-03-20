#Clear Workspace
rm(list=ls())



#http://www.r-bloggers.com/an-attempt-at-replicating-david-varadis-percentile-channels-strategy/
require(quantmod)
require(caTools)
require(PerformanceAnalytics)
require(TTR)


invested <- 3800


symbols <- c("SHY", "CON.DE", "MUV2.DE", "MRK.DE", "TUI1.DE", "DAI.DE", "O1BC.DE", "NESR.F", "FRE.DE")
benchmarkSymbol <- "^GSPC"
benchmark <- getSymbols(benchmarkSymbol, src='yahoo', auto.assign = F, from="2010-01-01")



data <- list()

#Set Progress Bar
pb <- txtProgressBar(min = 0, max = length(symbols), style = 3)


# Get Historical Data fpr all Symbols
for (i in 1:length(symbols)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    data[[i]] <- suppressWarnings(getSymbols(symbols[i], src='yahoo', auto.assign = F, from="2010-01-01")),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  #REAL WORK
  data[[i]] <- suppressWarnings(getSymbols(symbols[i], src='yahoo', auto.assign = F, from="2010-01-01"));
  #Sys.sleep(3)
  
  setTxtProgressBar(pb, i)  
}  #end for
rm(i)
close(pb)



names(data) <- gsub(".Open", "", as.character(unlist(lapply(data, function(x) names(x[,1])))))
  

# Clean up MRK.DE Split 2:1
data$MRK.DE["2014-06-24",6] <- data$MRK.DE["2014-06-24",6] * 2
data$MRK.DE["2014-06-25",6] <- data$MRK.DE["2014-06-25",6] * 2
data$MRK.DE["2014-06-26",6] <- data$MRK.DE["2014-06-26",6] * 2
data$MRK.DE["2014-06-27",6] <- data$MRK.DE["2014-06-27",6] * 2

# Clean up FRE.DE Split 3:1
data$FRE.DE["2014-07-31",6] <- data$FRE.DE["2014-07-31",6] * 3

data$FRE.DE["2014-08-01",6] <- data$FRE.DE["2014-08-01",6] * 3




#Merge Adusted into one DF
prices <- do.call("merge", data)
prices <- na.approx(Ad(prices))
prices <- na.omit(prices)
names(prices) <- symbols

prices <- prices[!is.na(prices[,2]),]

returns <- Return.calculate(prices)
cashPrices <- prices[, 1]
assetPrices <- prices [, -1]


require(caTools)
pctChannelPosition <- function(prices,
                               dayLookback = 60,
                               lowerPct = .25, upperPct = .75) {
  leadingNAs <- matrix(nrow=dayLookback-1, ncol=ncol(prices), NA)
  
  upperChannels <- runquantile(prices, k=dayLookback, probs=upperPct, endrule="trim")
  upperQ <- xts(rbind(leadingNAs, upperChannels), order.by=index(prices))
  
  lowerChannels <- runquantile(prices, k=dayLookback, probs=lowerPct, endrule="trim")
  lowerQ <- xts(rbind(leadingNAs, lowerChannels), order.by=index(prices))
  
  positions <- xts(matrix(nrow=nrow(prices), ncol=ncol(prices), NA), order.by=index(prices))
  positions[prices > upperQ & lag(prices) < upperQ] <- 1 #cross up
  positions[prices < lowerQ & lag(prices) > lowerQ] <- -1 #cross down
  positions <- na.locf(positions)
  positions[is.na(positions)] <- 0
  
  colnames(positions) <- colnames(prices)
  return(positions)
}


/*

# pctChannelPosition <- function(prices, rebal_on=c("months", "quarters"),
#                                dayLookback = 60,
#                                lowerPct = .25, upperPct = .75) {
#   upperQ <- rollapply(prices, width=dayLookback, quantile, probs=upperPct)
#   lowerQ <- rollapply(prices, width=dayLookback, quantile, probs=lowerPct)
#   positions <- xts(matrix(nrow=nrow(prices), ncol=ncol(prices), NA), order.by=index(prices))
#   positions[prices > upperQ] <- 1
#   positions[prices < lowerQ] <- -1
#   
#   ep <- endpoints(positions, on = rebal_on[1])
#   positions <- positions[ep,]
#   positions <- na.locf(positions)
#   positions[is.na(positions)] <- 0
# 
#   colnames(positions) <- colnames(prices)
#   
#   return(positions)
#   
# }









#find our positions, add them up
d60 <- pctChannelPosition(assetPrices)
d120 <- pctChannelPosition(assetPrices, dayLookback = 120)
d180 <- pctChannelPosition(assetPrices, dayLookback = 180)
d252 <- pctChannelPosition(assetPrices, dayLookback = 252)
compositePosition <- (d60 + d120 + d180 + d252)/4







#find 20-day rolling standard deviations, subset them on identical indices
#to the percentile channel monthly positions
sd20 <- xts(sapply(returns[,-1], runSD, n=20), order.by=index(assetPrices))
monthlySDs <- sd20[index(compositePosition)]







#compute inverse volatilities
inverseVols <- 1/monthlySDs



#multiply inverse volatilities by composite positions
invVolPos <- inverseVols*compositePosition



#take absolute values of inverse volatility multiplied by position
absInvVolPos <- abs(invVolPos)



#normalize the above quantities
normalizedAbsInvVols <- absInvVolPos/rowSums(absInvVolPos)



#keep only positions with positive composite positions (remove zeroes/negative)
nonCashPos <- normalizedAbsInvVols * sign(compositePosition > 0)
nonCashPos[is.na(nonCashPos)] <- 0 #no positions before we have enough data



#add cash position which is complement of non-cash position
finalPos <- nonCashPos
finalPos$cashPos <- 1-rowSums(finalPos)






#compute returns
stratRets <- Return.portfolio(R = returns, weights = finalPos)
charts.PerformanceSummary(stratRets)
stats <- cbind(rbind(table.AnnualizedReturns(stratRets), maxDrawdown(stratRets)), rbind(table.AnnualizedReturns(dailyReturn(Ad(benchmark))), maxDrawdown(dailyReturn(Ad(benchmark)))))
rownames(stats)[4] <- "Worst Drawdown"
names(stats) <- c("Strategy", "Benchmark")
stats

# Proposed Weights:
round(t(tail(finalPos,1))*100,2)



#charts.PerformanceSummary(dailyReturn(Ad(benchmark)))



#  apply.yearly(stratRets, Return.cumulative)


portfolio <- data.frame(StockValue=invested * t(tail(finalPos,2)), lastPrice=t(tail(prices,2)), round(invested * (t(tail(finalPos,2))) / t(tail(prices,2))))
names(portfolio) <- c("OptimalInvest (Previous)", "OptimalInvest", "lastPrice (Previous)", "lastPrice", "# (Previous)", "#")
portfolio
row.names(portfolio) <- names(finalPos)
portfolio



crossprod(portfolio$lastPrice, portfolio$"#")
crossprod(portfolio$"lastPrice (Previous)", portfolio$"# (Previous)")


# calculate order fees
sum(ifelse(portfolio$"# (Previous)"==portfolio$"#", 0, 1)) * 5.00




# Plot Strategy Performance vs. Benchmark
chart.RelativePerformance(stratRets, dailyReturn(Ad(benchmark)))

all <- cbind(stratRets, dailyReturn(Ad(benchmark)))
colnames(all) <- c("Strategy", "Benchmark")
charts.PerformanceSummary(all)

