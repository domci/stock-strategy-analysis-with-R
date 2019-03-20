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



#Load Ticker Data from Excel
ticker.list <- read.csv("Top_100_ETF_yahoo.csv", sep="\t")
symbols <- as.vector(ticker.list[,2])




#Set End Date 
endDate <- Sys.Date()




#Sample Sybols
#symbols <- (symbols[1:10])


symbols <- (c("^GSPC", symbols))


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




head(data[[1]])


# Remove Empty List Elements
data <- data[!sapply(data, is.null)]




#Plot All Closing Prices
#lapply(data, function(x) plot(Ad(x), main=names(x[,4])))


# Get minimum common start Date
startDate <- max(as.Date(unlist(lapply(data, function(x) min(index(x))))))


# remove historial data before startdate
data <- lapply(data, function(x) x[index(x)>=startDate])


# Check Dates
data.frame(as.Date(unlist(lapply(data, function(x) min(index(x))))))
data.frame((unlist(lapply(data, function(x) nrow(x)))))


## Remove row that is only in data[[1]] somehow....
data[[1]] <- data[[1]][index(data[[1]])!="2009-11-26"]


   
           


# Add Daily Returns

daily.returns <- lapply(data, function(x) dailyReturn(Ad(x)))
data <- Map(cbind, data, daily.returns = daily.returns)

# Test for Stock Split at MIDU
data$MIDU["2010-05"]



# Create KPI Data.Frame for Asset Comparison
KPI <- data.frame(Ticker=names(data))




#Active Premium The return on an investment's annualized return minus the benchmark's annualized return.
#KPI$Acive.Premium <- unlist(lapply(data, function(x) ActivePremium(x$daily.returns, data[[1]]$daily.returns, scale = 252)))

# Test single APs
#ActivePremium(data[[100]]$daily.returns, data[[1]]$daily.returns, scale = 252)



# Relative Performace vs. Benchmark
# 


## CAPM KPIs
CAPM <- data.frame(lapply(data, function(x) (table.CAPM(x$daily.returns, data[[1]]$daily.returns, scale = NA, Rf = -.01, digits = 4))))

names(CAPM) <- KPI[,1]


KPI <- cbind( KPI, t(CAPM))



#Risk Premium
KPI$CAPM.RiskPremium <- data.frame(CAPM.RiskPremium=unlist(lapply(data, function(x) CAPM.RiskPremium(x$daily.returns, Rf = -.01))))



#VaR
KPI$VAR95 <- data.frame(VaR95=unlist(lapply(data, function(x) VaR(x[,7], p = 0.95))))





#Market Cap
KPI$MarketCap <- data.frame(MarketCap=unlist(lapply(data, function(x) x[nrow(x),4] * x[nrow(x),5])))
nrow(data[[2]])


head(KPI)




x <- cbind(data.frame((lapply(data, function(x) unlist(Ad(x))))))
head(x)


names(x) <- gsub(".Adjusted", "", names(x))
names(x)


corrplot(cor(x))






#Clustering
#################################################################################################



stats <- KPI[,c(2,4,9,15,16)]

kmeans(stats)

tmp <- data.frame(stats[,1:3], as.matrix(stats[,4]), as.matrix(stats[,5]), as.matrix(stats[,6]))
names(tmp) <- names(stats)
stats <- tmp
rm(tmp)

stats[stats$Beta < 3,]$Beta


plot(stats[stats$Beta < 3,]$Beta, stats[stats$Beta < 3,]$CAPM.RiskPremium, labels=T)



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



