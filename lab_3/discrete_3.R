rm(list=ls(all=TRUE))

p = .5
q = .75

func = function(i){
  
  y=rbinom(n = 100, size = 1, prob = p)
  z=rbinom(n = 100, size = 1, prob = q)
  
  x = y + z
  xSamp = sample(x, 1000, rep = T)
}

results = sapply(FUN = func, X = c(1:10000))
print(mean(results))