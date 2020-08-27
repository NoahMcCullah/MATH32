rm(list=ls(all=TRUE))

load('mlb2015.RData')

#remove non-numerical and bad predictor columns
mlb = mlb2015[,-c(33, 34, 1)]

#set up LOOCV to avoid overfitting and test the model
allmodels = list(NULL)
n = nrow(mlb)
indiverrors = numeric(length=n)
truewinpct = c()
predwinpct = c()

#increment using each team as a test case against the other 29 as a training set
for(i in c(1:30)){
  
  #linear model using Runs per game, Saves, and Total Bases with intercept 0
  allmodels[[i]] = lm(W.Lpct ~ 0+ R.G + SV + TB, data=mlb[-i,])
  truewinpct[i] = mlb[i,]$W.Lpct
  predwinpct[i] = predict(allmodels[[i]], newdata = mlb[i,])
  indiverrors[i] = (truewinpct[i] - predwinpct[i])^2
}
#average error for all 30 models
print(mean(indiverrors))

#best model's error = 0.001413943 with R.G + SV + TB and intercept = 0