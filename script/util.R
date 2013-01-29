get.sym <- function(D, symbol) {
  D[D$Symbol %in% symbol,]
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

fun.xts <- function(FUN, symbols, metric.fun=ClCl, ...) {
  d <- align.xts(symbols, metric.fun)
  FUN(d, ...)
}

union.many <- function(...) {
  args <- list(...)
  out <- args[[1]]
  for(i in 2:length(args)) {
    out <- union(out, args[[i]])
  }
  return(out)
}