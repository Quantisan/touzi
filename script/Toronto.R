source('script/fetch.R')
source('script/screen.R')
source('script/plot.R')
source('script/portfolio.R')
source('script/util.R')

# Scrape ETF symbols from TSX
scrape.symbols <- function() {
  TSX <- read.csv("resources/TSX.csv", header=TRUE)  # TODO scrape from TSX site
  TSX$Symbol <- as.character(TSX$Symbol)
  TSX$Name <- as.character(TSX$Name)
  TSX <- subset(TSX[, c("Symbol", "Name")],!grepl("\\.", Symbol))    # without a dot                
  TSX$Symbol <- paste(TSX$Symbol, "TO", sep=".")
  return(TSX)
}

# Manually update expense ratio for Vanguard funds because they are not on Yahoo
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
plot.profiles(T, "TSX Pre-Screened Funds")

## Load quotes data
load.quotes(T$Symbol)
load.quotes(vg.T$Symbol)
load.quotes("XIU.TO")

## Get TSX market stats
xiu.mu <- expected.return(c("XIU.TO"))
xiu.sd <- sqrt(cov.return(c("XIU.TO")))

# Mean variance optimisation #
pf.vg <- globalMin.portfolio(expected.return(vg.T$Symbol), cov.return(vg.T$Symbol), shorts=FALSE)
cat("Global MinVar Vanguard Funds:\n")
print(sort(pf.vg$weights[pf.vg$weights > 0], decreasing=TRUE))
epf.vg <- efficient.portfolio(expected.return(vg.T$Symbol), cov.return(vg.T$Symbol), 0.07, shorts=FALSE)
cat("Efficient MinVar Vanguard Funds:\n")
print(sort(epf.vg$weights[epf.vg$weights > 0], decreasing=TRUE))

pf.T <- globalMin.portfolio(expected.return(T$Symbol), cov.return(T$Symbol), shorts=FALSE)
cat("Global MinVar Reguar Funds:\n")
print(sort(pf.T$weights[pf.T$weights > 0], decreasing=TRUE))
epf.T <- efficient.portfolio(expected.return(T$Symbol), cov.return(T$Symbol), 0.07, shorts=FALSE)
cat("Efficient MinVar Regular Funds:\n")
print(sort(epf.T$weights[epf.T$weights > 0], decreasing=TRUE))

## Union top symbols
sym.opt <- union.many(names(sort(pf.vg$weights, decreasing=TRUE)[1:3]),
                      names(sort(epf.vg$weights, decreasing=TRUE)[1:3]),
                      names(sort(pf.T$weights, decreasing=TRUE)[1:3]),
                      names(sort(epf.T$weights, decreasing=TRUE)[1:3]))
get.sym(TSX, sym.opt)[, c("Symbol", "Name", "category")]  # print final funds

## Further variance minimisation out of top symbols
epf <- efficient.portfolio(expected.return(sym.opt), cov.return(sym.opt), 0.08, shorts=FALSE)
sort(epf$weights, decreasing=TRUE)
final.syms <- c("CPD.TO", "CLF.TO", "VEE.TO")
final.weights <- c(0.50, 0.30, 0.20)

# Plot portfolios
portfolio.rets <- portfolio.returns("XIU.TO", 
                                    names=c("Vanguard Equal", "Pre-Screened Equal", "Final 3"),
                                    weights=list(rep(1, length(vg.T$Symbol))/length(vg.T$Symbol),
                                              rep(1, length(T$Symbol))/length(T$Symbol),
                                              final.weights),
                                    vg.T$Symbol, T$Symbol, final.syms)

charts.PerformanceSummary(portfolio.rets)
chart.RiskReturnScatter(portfolio.rets, Rf=0.015, add.sharpe=c(1,2,3), scale=52) ## axes tick labels are wrong

## Portfolio stats for final portfolio
getPortfolio(expected.return(final.syms), cov.return(final.syms), final.weights)