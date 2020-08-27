rm(list=ls(all=TRUE))

stock = c(rep(100, 3), rep(50, 5), rep(10, 2))

share = function(i) {
  stockSample = sample(stock, 1000, rep = T)
}

results = sapply(FUN = share, X = c(1:10000))
print(mean(results))
print((100*(3/10))+(50*(1/2))+(10*(1/5)))