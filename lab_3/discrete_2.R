rm(list=ls(all=TRUE))

coin = c(1:2)
die = c(1:6)

func = function(i) {
  
  coin = sample(2, 1)
  
  if(coin == 1){
    die = sample(6, 1)
  }
  
  else if(coin == 2){
    die = sample(4, 1)
  }
}

results = sapply(FUN = func, X = c(1:10000))
print(mean(results))
