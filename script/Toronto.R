source('script/fetch.R')
source('script/screen.R')
source('script/plot.R')
source('script/portfolio.R')
source('script/util.R')

scrape.symbols <- function() {
  TSX <- read.csv("resources/TSX.csv", header=TRUE)  # TODO scrape from TSX site
  TSX$Symbol <- as.character(TSX$Symbol)
  TSX$Name <- as.character(TSX$Name)
  TSX <- subset(TSX[, c("Symbol", "Name")],!grepl("\\.", Symbol))    # without a dot                
  TSX$Symbol <- paste(TSX$Symbol, "TO", sep=".")
  return(TSX)
}

update.vanguard.expense <- function(D) {
  exp <- read.csv("resources/vanguard_canada.csv", header=FALSE, col.names=c("Symbol", "fund.expense"))
  exp <- transform(exp, fund.expense = fund.expense / 100,
                        Symbol = paste(Symbol, "TO", sep="."))
  D$fund.expense[match(exp$Symbol, D$Symbol)] <- exp$fund.expense
  return(D)
}

TSX <- load.symbols("data/Toronto_symbols.RData", scrape.symbols)
TSX <- update.vanguard.expense(TSX)
TSX <- subset(TSX, type == "Exchange Traded Fund" & avg.trading.value > 0)

plot.profiles(TSX, "Canadian ETFs")

vg.T <- subset(TSX, fund.family == "Vanguard Investments Canada Inc")

T <- screen.etf(TSX)
plot.profiles(T, "TSX Screened Funds")

load.quotes(T$Symbol)
load.quotes(vg.T$Symbol)
load.quotes("XIU.TO")

xiu.mu <- expected.return(c("XIU.TO"))
xiu.sd <- sqrt(cov.return(c("XIU.TO")))

pf.vg <- globalMin.portfolio(expected.return(vg.T$Symbol), cov.return(vg.T$Symbol), shorts=FALSE)
cat("Global MinVar Vanguard Funds:\n")
print(sort(pf.vg$weights, decreasing=TRUE)[1:3])
epf.vg <- efficient.portfolio(expected.return(vg.T$Symbol), cov.return(vg.T$Symbol), 0.07, shorts=FALSE)
cat("Efficient MinVar Vanguard Funds:\n")
print(sort(epf.vg$weights, decreasing=TRUE)[1:3])

pf.T <- globalMin.portfolio(expected.return(T$Symbol), cov.return(T$Symbol), shorts=FALSE)
cat("Global MinVar Reguar Funds:\n")
print(sort(pf.T$weights, decreasing=TRUE)[1:3])
epf.T <- efficient.portfolio(expected.return(T$Symbol), cov.return(T$Symbol), 0.07, shorts=FALSE)
cat("Efficient MinVar Regular Funds:\n")
print(sort(epf.T$weights, decreasing=TRUE)[1:3])

# Union top symbols
sym.opt <- names(sort(pf.vg$weights, decreasing=TRUE)[1:3])
sym.opt <- union(sym.opt,  names(sort(epf.vg$weights, decreasing=TRUE)[1:3]))
sym.opt <- union(sym.opt,  names(sort(pf.T$weights, decreasing=TRUE)[1:3]))
sym.opt <- union(sym.opt,  names(sort(epf.T$weights, decreasing=TRUE)[1:3]))
get.sym(TSX, sym.opt)[, c("Symbol", "Name", "category")]

# Last screen out of top symbols
epf <- efficient.portfolio(expected.return(sym.opt), cov.return(sym.opt), 0.07, shorts=FALSE)

# Plot portfolios
portfolio.rets <- portfolio.returns("XIU.TO", 
                                    names=c("Vanguard Equal", "Pre-Screened Equal", "Final 3"),
                                    vg.T$Symbol, 
                                    T$Symbol,
                                    names(sort(epf$weights, decreasing=TRUE)[1:3]))

charts.PerformanceSummary(portfolio.rets)
chart.RiskReturnScatter(portfolio.rets, Rf=0.015, add.sharpe=c(1,2,3), scale=52) ## axes tick labels are wrong
