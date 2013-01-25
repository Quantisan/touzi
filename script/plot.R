require(ggplot2)

plot.profiles <- function(D, title) {
  p.mer <- ggplot(D[!is.na(D$fund.expense),], aes(x=category, y=fund.expense * 100))
  p.mer <- p.mer + geom_boxplot() + xlab("Fund Category") + ylab("Fund Expense Ratio [%]") + ggtitle(title) +
    theme(axis.text.x=element_text(angle = 90, hjust = 1, size = 14))
  print(p.mer)
  
  p.issuer <- ggplot(D[!is.na(D$fund.expense),], aes(x=fund.family, y=fund.expense * 100))
  p.issuer <- p.issuer + geom_boxplot() + xlab("Fund Issuer") + ylab("Fund Expense Ratio [%]") + ggtitle(title) +
    theme(axis.text.x=element_text(angle = 90, hjust = 1, size = 14))
  print(p.issuer)
  return()
}
