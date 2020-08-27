#deletes all previously created objects and variables
rm(list=ls(all=TRUE))
average = 0
##
##
#Discrete 1
#create a vector with a proportional number of each X value to represent their probabilities
stock = c(rep(100, 3), rep(50, 5), rep(10, 2))

#take 10 samples from the stock vector
stockSample = sample(stock, 10, rep = T)
#take the average value of stockSample to get the expected value
average = mean(stockSample)
print(paste("Discrete 1:Expected value with 10 trials:", average))

#take 10000 samples from the stock vector
stockSample = sample(stock, 10000, rep = T)
#take the average value of stockSample to get the expected value
average = mean(stockSample)
print(paste("Discrete 1:Expected value with 10000 trials:", average))

#pen and paper method of finding expected value
print(paste("Pen and paper solution:", (100*(3/10))+(50*(1/2))+(10*(1/5))))

plot(c(1:length(average)),average)
abline(h=57, col='red')
title('Discrete 1')
##
##
#Discrete 2
#vectors to represent and two sided coin, 4-sided die, and 6-sided die 
coin = c(1:2)
dieSix = c(1:6)
dieFour = c(1:4)

disc2 = function(i) {
  #flip a coin
  coin = sample(2, 1)
  
  #die used depends on result of coin flip
  if(coin == 1){
    die = sample(dieSix, 1)
  }
  
  else if(coin == 2){
    die = sample(dieFour, 1)
  }
}

#execute disc2 10 times
results2a = sapply(FUN = disc2, X = c(1:10))
#print average of results2 to find expected value
average = mean(results2a)
print(paste("Discrete 2:Expected value with 10 trials:", average))

#execute disc2 10000 times
results2b = sapply(FUN = disc2, X = c(1:10000))
#print average of results2 to find expected value
average = mean(results2b)
print(paste("Discrete 2:Expected value with 10000 trials:", average))

print(paste('Pen and paper expected value: ',0.5 * ((sum(dieSix)/6)+(sum(dieFour)/4))))

plot(c(1:length(results2b)),results2b)
abline(h=2.7, col='red')
title('Discrete 2')
##
##
#Discrete 3
#probabilities p and q as described in the pdf
p = .5
q = .75

disc3 = function(i){
  #creates vectors y and z with binomial distribution
  y=rbinom(n = 100, size = 1, prob = p)
  z=rbinom(n = 100, size = 1, prob = q)
  
  #sum vectors y and z into vector x
  x = y + z
  xSamp = sample(x, 1000, rep = T)
}

#run disc3 10 times
results3a = sapply(FUN = disc3, X = c(1:10))
#print average of results3 to find expected value
average = mean(results3a)
print(paste("Discrete 3:Expected value with 10 trials:", average))

#run disc3 10000 times
results3b = sapply(FUN = disc3, X = c(1:10000))
#print average of results3 to find expected value
average = mean(results3b)
print(paste("Discrete 3:Expected value with 10000 trials:", average))

print(paste('Pen and paper expected value = ',p+q))

plot(c(1:length(results3b)),results3b)
abline(h=(p+q), col='red')
title('Discrete 3')
##
##
#Continuous
#create a vector with 10 random values between -5 and 10
cont = runif(n = 10, min = -5, max = 10)
average = mean(cont)
print(paste("Continuous:Expected value with 10 trials:", average))

#create a vector with 10000 random values between -5 and 10
cont = runif(n = 10000, min = -5, max = 10)
average = mean(cont)
print(paste("Continuous:Expected value with 10000 trials:", average))
print(paste("Pen and paper expected value:", ((-5+10)/2)))

plot(c(1:length(cont)),cont)
abline(h=2.5, col='red')
title('Continous')