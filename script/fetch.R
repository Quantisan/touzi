require('quantmod')
require(XML)

# Bootstrap dataset by going off to scrape pages and data from web
bootstrap.symbols <- function(script.file, data.file) {
  source(script.file, local=TRUE)
  D <- scrape.symbols()
  source('script/fetch.R', local=TRUE)
  D <- merge(D, fetch.trade.summary(D$Symbol), by="Symbol")
  D <- merge(D, fetch.yahoo.profiles(D$Symbol), by="Symbol")
  save(D, file=data.file)
  return(D)
}

# Tries to load data from file first otherwise go fetch data.
load.symbols <- function(exchange) {
  data.file <- paste("data/", exchange, "_symbols.RData", sep="")
  script.file <- paste("script/", exchange, ".R", sep="")
  
  tried <- try(load(data.file), silent = TRUE)
  if(inherits(tried, "try-error")) {
    return(bootstrap.symbols(script.file, data.file))    ## ERROR infinite loop
  } else {
    return(local(get(load(data.file))))
  }
}

## Quotes ##
bootstrap.quote.data <- function(symbol, file) {
  D <- getSymbols(symbol, from="2000-01-01", auto.assign=FALSE)
  D <- adjustOHLC(D, use.Adjusted=TRUE)
  colnames(D) <- c("open", "high", "low", "close", "volume", "adjusted")
  save(D, file = file)
  return(D)
}

# Tries to load quotes data from file first otherwise go fetch data.
load.quotes <- function(symbols) {
  for(sym in symbols) {
    file <- paste("data/", sym, ".RData", sep="")
    tried <- try(load(file), silent = TRUE)
    
    if(inherits(tried, "try-error")) {
      cat("No data file for ", sym,". Fetching and saving data now ...\n")
      D <- bootstrap.quote.data(sym, file)
    } else {
      D <- local(get(load(file)))
    }
    
    assign(sym, D, envir = .GlobalEnv)
  }
}

##### Get Yahoo profile for a fund #####

# Takes in a percent string and returns decimal
as.decimal <- function(s) {
  as.numeric(sub("%", "", s)) / 100
}

parse.overview <- function(overview) {
  make.overview <- function(category, fund.family, net.assets, yield, inception, type) {
    data.frame(category = category,
               fund.family= fund.family,
               net.assets = net.assets,
               yield = yield,
               inception = inception,
               type = type)
  }
  
  if(length(overview) == 1 && is.na(overview)) return(make.overview(NA,NA,NA,NA,NA,NA))
  
  overview$V2 <- as.character(overview$V2)
  fields <- gsub("[[:punct:]]", "", overview$V1)
  fields.expected <- c("Category", "Fund Family", "Net Assets", "Yield",
                       "Fund Inception Date", "Legal Type")
  if (length(fields) != length(fields.expected) || !all(fields == fields.expected)) {
    warning("Fund Overview table structure not as expected.")
    return(make.overview(NA,NA,NA,NA,NA,NA))
  }
  
  parse.asset <- function(s) {
    len <- nchar(s)
    last.char <- substr(s, len, len)
    as.numeric(substr(s, 1, len - 1)) * 
      if(last.char == 'B') {1e9}
    else if (last.char == 'M') {1e6}
    else NA                                      
  }
  
  make.overview(category = overview[1,2],
                fund.family= overview[2,2],
                net.assets = parse.asset(overview[3,2]),
                yield = as.decimal(overview[4,2]),
                inception = as.Date(overview[5,2], "%b %d, %Y"),
                type = overview[6,2])
}

parse.operation <- function(op) {
  make.operation <- function(fund.expense, fund.turnover, category.expense, category.turnover) {
    data.frame(fund.expense = fund.expense,
               fund.turnover = fund.turnover,
               category.expense = category.expense,
               category.turnover = category.turnover)
  }
  
  if (length(op) == 1 && is.na(op)) return(make.operation(NA,NA,NA,NA))
  
  op <- apply(op, c(1,2), as.character)
  fields.expected <- c("Annual Report Expense Ratio (net)",
                       "Annual Holdings Turnover", "Total Net Assets")
  if (length(op[,1]) != length(fields.expected) || !all(op[,1] == fields.expected)) {
    warning("Fund Operation table structure not as expected")
    return(make.operation(NA,NA,NA,NA))
  }
  
  make.operation(fund.expense = as.decimal(op[1,2]),
                 fund.turnover = as.decimal(op[2,2]),
                 category.expense = as.decimal(op[1,3]),
                 category.turnover = as.decimal(op[2,3]))
}

scrape.yahoo.profile <- function(symbol) {  
  url <- paste("http://finance.yahoo.com/q/pr?s=", symbol, "+Profile", sep="")
  tree <- htmlTreeParse(url, useInternalNodes=TRUE)
  xpath <- "//table[contains(concat(' ', @class, ' '), ' yfnc_datamodoutline1 ')]/tr/td/table"
  node <- getNodeSet(tree, xpath)
  
  operation <- tryCatch(readHTMLTable(node[[2]]), error = function(e) NA)
  overview <- tryCatch(readHTMLTable(node[[1]]), error = function(e) NA)
  
  D <- cbind(parse.overview(overview), parse.operation(operation))
  D$Symbol <- symbol
  return(D)
}

fetch.yahoo.profiles <- function(symbols) {
  ### Get profile page data ###
  profiles <- data.frame()
  cat("scaping profiles:\n")
  for (i in 1:length(symbols)) {
    cat(symbols[i], "")
    if((i %% 10) == 0) {
      cat("\n")
      Sys.sleep(8)
    }
    profiles <- rbind(profiles, scrape.yahoo.profile(symbols[i]))
  }
  cat("...done\n")
  return(profiles)
}

# Populate data frame with each symbol's profile information
fetch.trade.summary <- function(symbols) {
  ## getQuote2 circumvent getQuote bug when length(Symbol) > 200
  getQuote2 <- function(Symbols,src='yahoo',what=standardQuote(), ...) {
    length.of.symbols <- length(Symbols)
    if(length.of.symbols > 200) {
      # we will recursively call getQuote2 to handle each block of 200
      Symbols <- unlist(strsplit(Symbols,";"))
      all.symbols <- lapply(seq(1,length.of.symbols,200),
                            function(x) na.omit(Symbols[x:(x+199)]))
      df <- NULL
      cat("downloading set: ")
      for(i in 1:length(all.symbols)) {
        Sys.sleep(0.5)
        cat(i,", ")
        df <- rbind(df, getQuote2(all.symbols[[i]], src=src, what=what))
      }
      cat("...done\n")
      return(df)
    } else
      return(getQuote(Symbols, src=src, what=what))
  }
  
  ### Get trading data ###
  trades <- getQuote(symbols, src = "yahoo",
                     what=yahooQF(c("Average Daily Volume",
                                    "50-day Moving Average")))
  trades$Symbol <- rownames(trades)
  trades$"50-day MA" <- as.numeric(trades$"50-day MA")
  trades$avg.trading.value <- trades$"Ave. Daily Volume" * trades$"50-day MA"
  
  return(trades)
}