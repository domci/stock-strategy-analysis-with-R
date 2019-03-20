###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/2013/03/22/maximum-sharpe-portfolio/
###############################################################################
#Clear Workspace
rm(list=ls())

invisible(Sys.setlocale("LC_MESSAGES", "C"))
invisible(Sys.setlocale("LC_TIME", "C"))

setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)




library(stringr)



#*****************************************************************
# Create Efficient Frontier
#****************************************************************** 	
# yahoo tickers
#symbols <- c("CON.DE", "MUV2.DE", "MRK.DE", "TUI1.DE", "DAI.DE", "O1BC.DE", "NESR.F", "FRE.DE")

# google tickers
symbols <- c("FRA:CON", "FRA:CON", "FRA:CON", "FRA:TUI1", "FRA:DAI", "FRA:O1BC", "FRA:NESR", "FRA:FRE")


get.stock.data <- function(symbols)
{
  load.packages('quantmod,quadprog')
  symbols = symbols
  symbol.names = symbols
  getSymbols(symbols, src = "google", auto.assign = TRUE)
  hist.prices = na.omit(na.approx(do.call("merge", mget(symbols))))
  month.ends = endpoints(hist.prices, 'months')
  hist.prices = Cl(hist.prices)[month.ends, ]
  # Adjust FRE.DE Stock Split 1:3
  #hist.prices["2014-07-31",]$FRE.DE.Adjusted <- hist.prices["2014-07-31",]$FRE.DE.Adjusted * 3
  #hist.prices["2014-08-01",]$FRE.DE.Adjusted <- hist.prices["2014-08-01",]$FRE.DE.Adjusted * 3
  colnames(hist.prices) = symbols
  hist.prices = na.omit(hist.prices['2010::'])
  hist.returns = na.omit( ROC(hist.prices, type = 'discrete') )
  ia = create.historical.ia(hist.returns, 12, symbols, symbols)
  return(ia)
}


# create sample historical input assumptions
ia = get.stock.data(symbols)



# create long-only, fully invested efficient frontier
n = ia$n		

# 0 <= x.i <= 1
constraints = new.constraints(n, lb = 0, ub = 1)
constraints = add.constraints(diag(n), type='>=', b=0, constraints)
constraints = add.constraints(diag(n), type='<=', b=1, constraints)

# SUM x.i = 1
constraints = add.constraints(rep(1, n), 1, type = '=', constraints)		

# create efficient frontier
ef = portopt(ia, constraints, 50, 'Efficient Frontier') 

#*****************************************************************
# Create Plot
#****************************************************************** 	
# plot efficient frontier
plot.ef(ia, list(ef), transition.map=F)	 

# find maximum sharpe portfolio
max(portfolio.return(ef$weight,ia) /  portfolio.risk(ef$weight,ia))

# plot minimum variance portfolio
weight = min.var.portfolio(ia,constraints)	
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='red')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

# plot maximum Sharpe or tangency portfolio
weight = max.sharpe.portfolio()(ia,constraints)	
points(100 * portfolio.risk(weight,ia), 100 * portfolio.return(weight,ia), pch=15, col='orange')
portfolio.return(weight,ia) /  portfolio.risk(weight,ia)

plota.legend('Minimum Variance,Maximum Sharpe','red,orange', x='topright')	








#*****************************************************************
# Load historical data
#******************************************************************
load.packages('quantmod')

tickers = spl('CON.DE, MRK.DE, TUI1.DE, DAI.DE, O1BC.DE, NESR.F, FRE.DE')


data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)                           
bt.prep(data, align='keep.all', dates='2004:12::')

#*****************************************************************
# Code Strategies
#******************************************************************
prices = data$prices 
n = ncol(prices)

models = list()

#*****************************************************************
# Code Strategies
#******************************************************************
# find period ends
period.ends = endpoints(prices, 'months')
period.ends = period.ends[period.ends > 0]

n.mom = 180
n.vol = 60
n.top = 4       
momentum = prices / mlag(prices, n.mom) 

# Adjust FRE.DE Stock Split 1:3
data$prices["2014-07-31",]$" FRE.DE" <- data$prices["2014-07-31",]$" FRE.DE" * 3
data$prices["2014-08-01",]$" FRE.DE" <- data$prices["2014-08-01",]$" FRE.DE" * 3


obj = portfolio.allocation.helper(data$prices, period.ends=period.ends,
                                  lookback.len = n.vol, universe = ntop(momentum[period.ends,], n.top) > 0,
                                  min.risk.fns = list(EW=equal.weight.portfolio,
                                                      #RP=risk.parity.portfolio,
                                                      MV=min.var.portfolio,
                                                      MD=max.div.portfolio,
                                                      MC=min.corr.portfolio,
                                                      MC2=min.corr2.portfolio,
                                                      MCE=min.corr.excel.portfolio,
                                                      MS=max.sharpe.portfolio())
)

models = create.strategies(obj, data)$models

#*****************************************************************
# Create Report
#******************************************************************   
strategy.performance.snapshoot(models, T)

plotbt.custom.report.part2(models$MS)

# Plot Portfolio Turnover for each strategy
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')