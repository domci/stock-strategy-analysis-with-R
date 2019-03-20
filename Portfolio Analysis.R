#load("C:/Users/Dom/Dropbox/R-Data/Stock Analysis/Auto Stock Analysis.RData")

#Clear Workspace
rm(list=ls())


#Set rJava Home
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre7')


#Load Packages
library(quantmod, warn.conflicts = FALSE, quietly = TRUE)
library(PerformanceAnalytics, warn.conflicts = FALSE, quietly = TRUE)
library(TTR, warn.conflicts = FALSE, quietly = TRUE)
library(xts)
library(ggplot2)
library(corrplot)
library(reshape)
library(tseries)




#Set End Date 
endDate <- Sys.Date()

#Set Benchmark Index
benchmark <- "^GDAXI"



# Portfolio Analysis
###################################################################################################################################

#Enter Portfolio Data      
portfolio.symbols <- data.frame(Symbol=c(benchmark, "CON.DE", "MUV2.DE", "MRK.DE", "TUI1.DE", "DAI.DE", "O1BC.DE", "NESR.F", "FRE.DE"),
                        Anzahl=          c(0,         4,          1,        6,       40,         8,         1,         2,        8)
                        , stringsAsFactors = F
                        )



portfolio <- list()


#Set Progress Bar
pb <- txtProgressBar(min = 0, max = length(symbols), style = 3)

# Get Historical Data fpr all Portfolio Items
for (i in 1:nrow(portfolio.symbols)) {
  
  #ERROR HANDLING
  possibleError <- tryCatch(
    portfolio[[i]] <- suppressWarnings(getSymbols(portfolio.symbols[i,1], src='yahoo', auto.assign = F)),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  #REAL WORK
  portfolio[[i]] <- suppressWarnings(getSymbols(portfolio.symbols[i,1], src='yahoo', auto.assign = F));

  setTxtProgressBar(pb, i)  
} 
rm(i, possibleError, pb)

# Set Names for List Elements
names(portfolio) <- portfolio.symbols[,1]


# Add Daily Returns
#rd <- lapply(portfolio, function(x) dailyReturn(Ad(x)))
#portfolio <- Map(cbind, portfolio, daily.returns = rd)

#head(portfolio[[1]])


#Merge Adusted into one DF
Ad.portfolio <- do.call("merge", portfolio)
Ad.portfolio <- na.approx(Ad(Ad.portfolio))
Ad.portfolio <- na.omit(Ad.portfolio)


#only use last 90 days / 1 Year
Ad.portfolio.90 <- Ad.portfolio[(nrow(Ad.portfolio)-89):nrow(Ad.portfolio),]
Ad.portfolio.1Y <- Ad.portfolio[(nrow(Ad.portfolio)-251):nrow(Ad.portfolio),]


#Calculate Daily Returns
dr.portfolio <- na.fill(diff(Ad.portfolio), fill=0)
dr.portfolio.90 <- na.fill(diff(Ad.portfolio.90), fill=0)
dr.portfolio.1Y <- na.fill(diff(Ad.portfolio.1Y), fill=0)




# Create Portfolio Performance Summary
portfolio.KPI <- (data.frame(Symbol=portfolio.symbols[,1]))

#Add Risk Premium to Performance Summary
portfolio.KPI$Risk.Premium.Rf0 <- t(data.frame(Risk.Premium.Rf0 = CAPM.RiskPremium(dr.portfolio.90, Rf = 0)))

#Add 90 Day Return to Performance Summary
portfolio.KPI$Return.90 <- unlist(lapply(portfolio, function(x) ((as.numeric(Ad(x)[nrow(x),]) - as.numeric(Ad(x)[(nrow(x)-90),])) / as.numeric(Ad(x)[(nrow(x)-90),]) )))


#Add 90 Day Volatility to Performance Summary
portfolio.KPI$Volatility.90 <- (Volatility.90 = unlist(lapply(portfolio, function(x) volatility(x, n=90, N=252)[nrow(x),])))

#Add Weights to Performance Summary
portfolio.KPI$Anzahl <- portfolio.symbols[,2]

# Add latest Stock Price to Performance Summary
portfolio.KPI$StockPrice <- t(Ad.portfolio[nrow(Ad.portfolio),])

#Calculate Invested Capital per Stock
portfolio.KPI$Invested <- portfolio.KPI$StockPrice * portfolio.KPI$Anzahl

#Calculate Stock Weights
portfolio.KPI$Weight <- portfolio.KPI$Invested / sum(portfolio.KPI$Invested)

#Add Betas to Performance Summary
portfolio.KPI$Beta.1Y <- unlist(lapply(portfolio, function(x) CAPM.beta(dailyReturn(Ad(x[(nrow(x)-251):nrow(x),])), dailyReturn(tail(Ad(portfolio[[1]]),252)), Rf = 0)))

#Add Portfolio Beta to Performance Summary
portfolio.KPI$combined.Beta.90 <- sum(portfolio.KPI$Beta.1Y * portfolio.KPI$Weight)

#Set Names for Performance Summary
row.names(portfolio.KPI) <- NULL
colnames(portfolio.KPI[,2]) <- "Risk.Premium.Rf0"
colnames(portfolio.KPI[,5]) <- "Stock Price"
colnames(portfolio.KPI[,6]) <- "Capital Invested"
colnames(portfolio.KPI[,7]) <- "weight"

portfolio.KPI



#Plot Portfolio KPIs
plot(portfolio.KPI$Volatility.90, portfolio.KPI$Risk.Premium.Rf0)
plot(portfolio.KPI$Beta.1Y, portfolio.KPI$Risk.Premium.Rf0)































# Watchlist Analysis
###################################################################################################################################



#Load Ticker Data from Excel
ticker.list <- read.csv("Top_100_ETF_yahoo.csv", sep="\t")
symbols <- c(benchmark, as.vector(ticker.list$Ticker))

#Remove Wrong Ticker Symbol
symbols <- symbols[symbols!="A0MN1Y "]



# Create Stock List Variable
data <- list()



#Set Progress Bar
pb <- txtProgressBar(min = 0, max = length(symbols), style = 3)


# Get Historical Data fpr all Symbols
for (i in 1:length(symbols)) {
  #ERROR HANDLING
  possibleError <- tryCatch(
    data[[i]] <- suppressWarnings(getSymbols(symbols[i], src='yahoo', auto.assign = F)),
    error=function(e) e
  )
  
  if(inherits(possibleError, "error")) next
  
  #REAL WORK
  data[[i]] <- suppressWarnings(getSymbols(symbols[i], src='yahoo', auto.assign = F));
  #Sys.sleep(3)
  
  setTxtProgressBar(pb, i)  
}  #end for
rm(i)
close(pb)

names(data) <- symbols




# Remove Empty List Elements
data <- data[!sapply(data, is.null)]




# Get minimum common start Date
startDate <- max(as.Date(unlist(lapply(data, function(x) min(index(x))))))


# remove historial data before startdate
data <- lapply(data, function(x) x[index(x)>=startDate])


length(symbols)


#Merge Adusted into one DF
Ad.data <- do.call("merge", data)
Ad.data <- na.approx(Ad(Ad.data))
Ad.data <- na.omit(Ad.data)
head(Ad.data)
ncol(head(Ad.data))


#only use last 90 days / 1 Year
Ad.data.90 <- Ad.data[(nrow(Ad.data)-89):nrow(Ad.data),]
Ad.data.1Y <- Ad.data[(nrow(Ad.data)-251):nrow(Ad.data),]


#Calculate Daily Returns
dr.data.90 <- na.fill(diff(Ad.data.90), fill=0)
dr.data.1Y <- na.fill(diff(Ad.data.1Y), fill=0)





# Create Portfolio Performance Summary
data.KPI <- (data.frame(Symbol=symbols))

#Add Risk Premium to Performance Summary
data.KPI$Risk.Premium.Rf0 <- t(data.frame(Risk.Premium.Rf0 = CAPM.RiskPremium(dr.data.90, Rf = 0)))

#Add 90 Day Return to Performance Summary
data.KPI$Return.90 <- unlist(lapply(data, function(x) ((as.numeric(Ad(x)[nrow(x),]) - as.numeric(Ad(x)[(nrow(x)-90),])) / as.numeric(Ad(x)[(nrow(x)-90),]) )))

#Add 90 Day Volatility to Performance Summary
data.KPI$Volatility.90 <- (Volatility.90 = unlist(lapply(data, function(x) volatility(x, n=90, N=252)[nrow(x),])))


#Add Betas to Performance Summary
data.KPI$Beta.1Y <- unlist(lapply(data, function(x) CAPM.beta(dailyReturn(Ad(x[(nrow(x)-251):nrow(x),])), dailyReturn(tail(Ad(data[[1]]),252)), Rf = 0)))


data.KPI



#Plot Portfolio KPIs
plot(data.KPI$Volatility.90, data.KPI$Risk.Premium.Rf0)
plot(data.KPI$Beta.1Y, data.KPI$Risk.Premium.Rf0)




















































#Clustering Watchlist 
#################################################################################################



stats <- data.KPI

km2 <- kmeans(data.KPI[,c(3,5)], 2, iter.max = 1000, nstart = 25)
km3 <- kmeans(data.KPI[,c(3,5)], 3, iter.max = 1000, nstart = 25)
km4 <- kmeans(data.KPI[,c(3,5)], 4, iter.max = 1000, nstart = 25)
km5 <- kmeans(data.KPI[,c(3,5)], 5, iter.max = 1000, nstart = 25)
km6 <- kmeans(data.KPI[,c(3,5)], 6, iter.max = 1000, nstart = 25)


wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss), sum(km5$withinss), sum(km6$withinss))
names(wss) <- 2:6
barplot(wss)


km6$centers
plot(data.KPI$Volatility.90, data.KPI$Volatility.90, col = km6$centers)


data.KPI$Cluster <- km6$cluster
plot(data.KPI[data.KPI$Cluster==6,]$Volatility.90, data.KPI[data.KPI$Cluster==6,]$Return.90)


?SharpeRatio







# PLOTS
#################################################################################################





#historical and parametric VaR estimates:
chart.VaRSensitivity(data[[3]][,7], methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))
chart.VaRSensitivity(R2, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))
chart.VaRSensitivity(R3, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))
chart.VaRSensitivity(r.SP500, methods = c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))


#Performance Summary
charts.PerformanceSummary(data[[4]][,7], Rf = -0.01)
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










# PLOTS

#Relative Performance
lapply(data, function(x) chart.RelativePerformance(x$daily.returns, 
                                                   as.vector(data[[1]]$daily.returns), 
                                                   main = paste(substr(names(x[,1]), 1, nchar(names(x[,1]))-6), 
                                                                " Relative Performace vs. Benchmark"), 
                                                   xaxis = TRUE))



# VaR Bar Charts
lapply(data, function(x) charts.BarVaR(x[,7], 
                                       main = substr(names(x[,1]), 1, nchar(names(x[,1]))-6),
                                       width = 20, gap = 0, methods = "StdDev", p = 0.95, 
                                       clean = "none", all = TRUE, show.clean = FALSE, show.horizontal = TRUE, 
                                       show.symmetric = FALSE, show.endvalue = TRUE, show.greenredbars = TRUE, 
                                       ylim = NA, colorset = 1:12, lty = 1, ypad = 0, legend.cex = 0.8))



