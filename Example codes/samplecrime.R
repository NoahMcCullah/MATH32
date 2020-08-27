#10-fold cross validation
#randomly partition data into 10 subsets
#1000 rows
#10 groups of 100 randomly chosen rows each

#each row in the original data set belongs to only 1 set

#cycle through each of the 10 groups
#if group i is the current one, use that as the test set
#train using all the groups other than i

#iter 1: calc test error using group 1
#iter 2: calc test error using group 2
# .
# .
# .
#iter 10: calc test error using group 10

rm(list=ls(all=TRUE))

library("ggplot2")
library("GGally")
load('crime.RData')

myx = x[,-c(1:5)]
#make test set

#nrow(x)

#shufflerows = sample(c(1:nrow(x)))
#rowsperfold = floor(nrow(x)/10)
#mse = c()

#for(i in c(1:10)){
  
#  si = rowsperfold*(i-1)+1
#  ei = rowsperfold*i
  
  #when i = 1, si:ei = 1:199
  #when i = 2, si:ei = 200 : 399
  
#  testrows = shufflerows[c(si:ei)]
#  trainrows = shufflerows[-c(si:ei)]
#  xtrain = x[trainrows,]
#  xtest = x[testrows,]
#  mymod = lm(ViolentCrimesPerPop ~ PctKids2Par + medIncome, data = xtrain)
#  ypred = predict(mymod, newdata=xtest)
#  mse[i] = mean((ypred - xtest$ViolentCrimesPerPop)^2)
#}
#sqrt(mean(mse))*200
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
# danger of correlation
set.seed(42)

# create two predictor variables that are correlated with each other
#n = 50
#x1 = rnorm(n)
#x2 = 0.9*x1 + 0.1*rnorm(n)

# create third variable that is different
#x3 = rnorm(n)

# now create response
#y = x1 + x2 + x3 + rnorm(n)

# set up 10-fold cross-validation

numfolds = 10
nummods = 31
rowsperfold = floor(nrow(x)/numfolds)
shufflerows = sample(nrow(x), replace=FALSE)
msevals = matrix(nrow=nummods,ncol=numfolds)

# fit two linear models
for (i in c(1:numfolds))
{
  si = (i-1)*rowsperfold + 1
  ei = i*rowsperfold
  testrows = shufflerows[c(si:ei)]
  traindf = data.frame(PctIlleg = x$PctIlleg[-testrows]
                       ,racepctblack = x$racepctblack[-testrows]
                       ,pctWPubAsst = x$pctWPubAsst[-testrows]
                       ,FemalePctDiv = x$FemalePctDiv[-testrows]
                       ,TotalPctDiv = x$TotalPctDiv[-testrows]
                       ,ViolentCrimesPerPop = x$ViolentCrimesPerPop[-testrows])
  
  testdf = data.frame(PctIlleg = x$PctIlleg[testrows]
                      ,racepctblack = x$racepctblack[testrows]
                      ,pctWPubAsst = x$pctWPubAsst[testrows]
                      ,FemalePctDiv = x$FemalePctDiv[testrows]
                      ,TotalPctDiv = x$TotalPctDiv[testrows]
                      ,ViolentCrimesPerPop = x$ViolentCrimesPerPop[testrows])
  
  mymods = list(NULL)
  #mymods[[1]] = lm(y ~ x1, data=traindf)
  #mymods[[2]] = lm(y ~ x2, data=traindf)
  #mymods[[3]] = lm(y ~ x3, data=traindf)
  #mymods[[4]] = lm(y ~ x1 + x2, data=traindf)
  #mymods[[5]] = lm(y ~ x1 + x3, data=traindf)
  #mymods[[6]] = lm(y ~ x2 + x3, data=traindf)
  #mymods[[7]] = lm(y ~ x1 + x2 + x3, data=traindf)
  
  mymods[[1]] = lm(ViolentCrimesPerPop ~ PctIlleg, data = traindf)
  mymods[[2]] = lm(ViolentCrimesPerPop ~ racepctblack, data = traindf)
  mymods[[3]] = lm(ViolentCrimesPerPop ~ pctWPubAsst, data = traindf)
  mymods[[4]] = lm(ViolentCrimesPerPop ~ FemalePctDiv, data = traindf)
  mymods[[5]] = lm(ViolentCrimesPerPop ~ TotalPctDiv, data = traindf)
  mymods[[6]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack, data = traindf)
  mymods[[7]] = lm(ViolentCrimesPerPop ~ PctIlleg + pctWPubAsst, data = traindf)
  mymods[[8]] = lm(ViolentCrimesPerPop ~ PctIlleg + FemalePctDiv, data = traindf)
  mymods[[9]] = lm(ViolentCrimesPerPop ~ PctIlleg + TotalPctDiv, data = traindf)
  mymods[[10]] = lm(ViolentCrimesPerPop ~ racepctblack + pctWPubAsst, data = traindf)
  mymods[[11]] = lm(ViolentCrimesPerPop ~ racepctblack + FemalePctDiv, data = traindf)
  mymods[[12]] = lm(ViolentCrimesPerPop ~ racepctblack + TotalPctDiv, data = traindf)
  mymods[[13]] = lm(ViolentCrimesPerPop ~ pctWPubAsst + FemalePctDiv, data = traindf)
  mymods[[14]] = lm(ViolentCrimesPerPop ~ pctWPubAsst + TotalPctDiv, data = traindf)
  mymods[[15]] = lm(ViolentCrimesPerPop ~ FemalePctDiv + TotalPctDiv, data = traindf)
  mymods[[16]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack + pctWPubAsst, data = traindf)
  mymods[[17]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack + FemalePctDiv, data = traindf)
  mymods[[18]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack + TotalPctDiv, data = traindf)
  mymods[[19]] = lm(ViolentCrimesPerPop ~ PctIlleg + pctWPubAsst + FemalePctDiv, data = traindf)
  mymods[[20]] = lm(ViolentCrimesPerPop ~ PctIlleg + pctWPubAsst + TotalPctDiv, data = traindf)
  mymods[[21]] = lm(ViolentCrimesPerPop ~ PctIlleg + FemalePctDiv + TotalPctDiv, data = traindf)
  mymods[[22]] = lm(ViolentCrimesPerPop ~ racepctblack + pctWPubAsst + FemalePctDiv, data = traindf)
  mymods[[23]] = lm(ViolentCrimesPerPop ~ racepctblack + pctWPubAsst + TotalPctDiv, data = traindf)
  mymods[[24]] = lm(ViolentCrimesPerPop ~ racepctblack + FemalePctDiv + TotalPctDiv, data = traindf)
  mymods[[25]] = lm(ViolentCrimesPerPop ~ pctWPubAsst + FemalePctDiv + TotalPctDiv, data = traindf)
  mymods[[26]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack + pctWPubAsst + FemalePctDiv, data = traindf)
  mymods[[27]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack + pctWPubAsst + TotalPctDiv, data = traindf)
  mymods[[28]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack + FemalePctDiv + TotalPctDiv, data = traindf)
  mymods[[29]] = lm(ViolentCrimesPerPop ~ PctIlleg + pctWPubAsst + FemalePctDiv + TotalPctDiv, data = traindf)
  mymods[[30]] = lm(ViolentCrimesPerPop ~ racepctblack + pctWPubAsst + FemalePctDiv + TotalPctDiv, data = traindf)
  mymods[[31]] = lm(ViolentCrimesPerPop ~ PctIlleg + racepctblack + pctWPubAsst + FemalePctDiv + TotalPctDiv, data = traindf)
  
  
  
  for (j in c(1:nummods))
  {
    predictions = predict(mymods[[j]],newdata=testdf)
    msevals[j, i] = mean((predictions - testdf$ViolentCrimesPerPop)^2)
  }
}

print(rowMeans(msevals))
