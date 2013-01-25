screen.etf <- function(D, age = 3, atv.threshold = 0.66, expense.rank.threshold = 3) {  
  q.atv <- quantile(D$avg.trading.value, prob=atv.threshold, na.rm = TRUE)
  
  D <- within(D, expense.rank <- ave(fund.expense, category, FUN=function(x) { rank(x, ties.method="min") }))
  scr <- subset(D, (Sys.Date() - inception) > (age * 365) &  ## minimum fund age
                  expense.rank <= expense.rank.threshold &  ## top expense ratio in each category
                  avg.trading.value > q.atv)
  return(scr[,!(colnames(scr) %in% "expense.rank")])
}