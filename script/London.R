require(XML)
require(RCurl)

source('script/fetch.R')
source('script/screen.R')
source('script/plot.R')
source('script/portfolio.R')
source('script/util.R')

fetch.symbols <- function(url) {
  max.paginate <- 34
  curl <- getCurlHandle(cookiefile = '/tmp/Rcookie.txt' , 
                        useragent =  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en - US; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6",
                        header = F,
                        verbose = TRUE,
                        netrc = TRUE,
                        maxredirs = as.integer(20),
                        followlocation = TRUE)
  
  page <- getURL(url, curl=curl)
  tree <- htmlTreeParse(page, useInternalNodes=TRUE)
  xpath <- "//table[@class = 'table_dati']/tbody"
  node <- getNodeSet(tree, xpath)
  
  return(readHTMLTable(node[[1]]))
}

scrape.symbols <- function() {
  url <- "http://www.londonstockexchange.com/exchange/prices-and-markets/ETFs/ETFs.html?&page=1"
  Table <- data.frame()
  for(i in 1:max.paginate) {
    Sys.sleep(0.2)
    url.root <- "http://www.londonstockexchange.com/exchange/prices-and-markets/ETFs/ETFs.html?&page="
    url <- paste(url.root, i, sep="")
    Table <- rbind(Table, fetch.symbols(url))
  }
  fields <- c("Symbol", "Name", "currency", "Price", "Change")
  colnames(Table) <- fields
  Table <- transform(Table, Symbol=paste(as.character(Symbol), "L", sep="."),
                     Name=as.character(Name),
                     Price=as.numeric(sub(",", "", as.character(Price))),
                     Change=as.numeric(as.character(Change)))
  return(Table[,fields])
}

LSE <- load.symbols("data/London_symbols.RData", scrape.symbols)
LSE <- subset(LSE, type == "Exchange Traded Fund" & avg.trading.value > 0 # skip inactive ETFs
              & currency %in% c("GBX", "GBP"))
plot.profiles(LSE, "London ETFs")
vg.L <- subset(LSE, fund.family == "Vanguard Group (Ireland) Limited")

L <- screen.etf(LSE)
plot.profiles(L, "London Pre-Screened Funds")

## Load quotes data
load.quotes(L$Symbol)
load.quotes(vg.L$Symbol)
load.quotes("ISF.L")

## Get FTSE stats
isf.mu <- expected.return(c("ISF.L"))
isf.sd <- sqrt(cov.return(c("ISF.L")))

# Mean variance optimisation #
pf.L <- globalMin.portfolio(expected.return(L$Symbol), cov.return(L$Symbol), shorts=FALSE)
cat("Global MinVar Funds:\n")
print(sort(pf.L$weights[pf.L$weights > 0], decreasing=TRUE))
epf.L <- efficient.portfolio(expected.return(L$Symbol), cov.return(L$Symbol), 0.08, shorts=FALSE)
cat("Efficient MinVar Funds:\n")
print(sort(epf.L$weights[epf.L$weights > 0], decreasing=TRUE))

pf.vg <- globalMin.portfolio(expected.return(vg.L$Symbol), cov.return(vg.L$Symbol), shorts=FALSE)
cat("Global MinVar Vanguard Funds:\n")
print(sort(pf.vg$weights[pf.vg$weights > 0], decreasing=TRUE))
epf.vg <- efficient.portfolio(expected.return(vg.L$Symbol), cov.return(vg.L$Symbol), 0.08, shorts=FALSE)
cat("Efficient MinVar Vanguard Funds:\n")
print(sort(epf.vg$weights[epf.vg$weights > 0], decreasing=TRUE))

sym.opt <- union.many(names(sort(pf.L$weights, decreasing=TRUE)[1:5]),
                      names(sort(epf.L$weights, decreasing=TRUE)[1:5]))
get.sym(LSE, sym.opt)[, c("Symbol", "Name", "category")]  # print final funds

epf <- efficient.portfolio(expected.return(sym.opt), cov.return(sym.opt), 0.08, shorts=FALSE)
sort(epf$weights[epf$weights > 0], decreasing=TRUE)
get.sym(LSE, names(sort(epf$weights[epf$weights > 0], decreasing=TRUE)))[, c("Symbol", "Name", "category")]  # print final funds

final.syms <- c("IGLS.L", "MIDD.L")
final.vg.syms <- c("VGOV.L", "VWRL.L")
final.weights <- c(0.80, 0.20)

# Plot portfolios
portfolio.rets <- portfolio.returns("ISF.L", 
                                    names=c("Screened Equal", "Final", "Vangards"),
                                    weights=list(rep(1, length(L$Symbol))/length(L$Symbol),
                                                 final.weights, final.weights),
                                    L$Symbol, final.syms, final.vg.syms)

charts.PerformanceSummary(portfolio.rets)
chart.RiskReturnScatter(portfolio.rets, Rf=0.015, add.sharpe=c(1,2,3), scale=52) ## axes tick labels are wrong

## Portfolio stats for final portfolio
getPortfolio(expected.return(final.vg.syms), cov.return(final.vg.syms), final.weights)
getPortfolio(expected.return(final.syms), cov.return(final.syms), final.weights)