source('script/fetch.R')
source('script/screen.R')
source('script/plot.R')
source('script/main.R')

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

TSX <- load.symbols("Toronto")
TSX <- update.vanguard.expense(TSX)
TSX <- subset(TSX, type == "Exchange Traded Fund" & avg.trading.value > 0)

plot.profiles(TSX, "Canadian ETFs")

vg.T <- subset(TSX, fund.family == "Vanguard Investments Canada Inc")

T <- screen.etf(TSX)
plot.profiles(T, "TSX Screened Funds")

load.quotes(T$Symbol)
load.quotes(vg.T$Symbol)
load.quotes("XIU.TO")