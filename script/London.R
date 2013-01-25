require(XML)
require(RCurl)

source('script/fetch.R')

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
#LSE <- subset(LSE, type == "Exchange Traded Fund" & avg.trading.value > 0)