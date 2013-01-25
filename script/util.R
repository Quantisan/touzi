get.sym <- function(D, symbol) {
  D[D$Symbol == symbol,]
}

align.xts <- function(symbols, FUN=ClCl) {
  d <- FUN(get(symbols[1]))  ## init first
  names(d) <- symbols[1]
  
  for(sym in symbols[-1]) {
    cur <- FUN(get(sym))
    names(cur) <- sym
    d <- merge(d, cur)
  }
  return(na.omit(d))
}

cov.xts <- function(symbols, FUN=ClCl) {
  d <- align.xts(symbols, FUN)
  cov(d)
}