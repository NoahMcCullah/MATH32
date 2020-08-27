#Project Final Draft
#Noah McCullah
#Adrian Gomez
#
rm(list=ls(all=TRUE))

library("ggplot2")
library("GGally")
load('crime.RData')

#####################################################################################
#         Replace NA values in each column with the mean of the column           
####################################################################################3
naCols = c()

for(i in c(1:ncol(x))){
  if(sum(is.na(x[,i] > 10))){
    naCols = c(naCols, i)
  }
  
  #myX[is.na(x[,i]),i] = mean(x[,i], n.rm = TRUE)
}

#make set without overwhelmingly NA columns
myX = x[,-naCols]
#########################################################################################3

#Control model, average of ViolentCrimesPerPop in Train vs Test sets
#########################################################################################################  

numfolds = 10
nummods = 3
rowsperfold = floor(nrow(myX)/numfolds)
shufflerows = sample(nrow(myX), replace=FALSE)
msevals = matrix(nrow=nummods,ncol=numfolds)
predictions = c()

# fit two linear models
for (i in c(1:numfolds))
{
  si = (i-1)*rowsperfold + 1
  ei = i*rowsperfold
  testrows = shufflerows[c(si:ei)]
  traindf = data.frame(PctIlleg = myX$PctIlleg[-testrows]
                       ,racepctblack = myX$racepctblack[-testrows]
                       ,pctWPubAsst = myX$pctWPubAsst[-testrows]
                       ,FemalePctDiv = myX$FemalePctDiv[-testrows]
                       ,TotalPctDiv = myX$TotalPctDiv[-testrows]
                       , MalePctDivorce = myX$MalePctDivorce[-testrows]
                       , PctNotHSGrad = myX$PctNotHSGrad[-testrows]
                       , PctUnemployed = myX$PctUnemployed[-testrows]
                       , PctHousNoPhone = myX$PctHousNoPhone[-testrows]
                       , PctKids2Par = myX$PctKids2Par[-testrows]
                       , PctFam2Par = myX$PctFam2Par[-testrows]
                       , racePctWhite = myX$racePctWhite[-testrows]
                       , PctYoungKids2Par = myX$PctYoungKids2Par[-testrows]
                       , PctTeen2Par = myX$PctTeen2Par[-testrows]
                       , pctWInvInc = myX$pctWInvInc[-testrows]
                       , PctPersOwnOccup = myX$PctPersOwnOccup[-testrows]
                       , medFamInc = myX$medFamInc[-testrows]
                       , medIncome = myX$medIncome[-testrows]
                       , PctHousOwnOcc = myX$PctHousOwnOcc[-testrows]
                       , racePctWhite = myX$racePctWhite[-testrows]
                       , PctVacantBoarded = myX$PctVacantBoarded[-testrows]
                       , PctHousLess3BR = myX$PctHousLess3BR[-testrows]
                       , NumIlleg = myX$NumIlleg[-testrows]
                       , MedNumBR = myX$MedNumBR[-testrows]
                       , PctOccupManu = myX$PctOccupManu[-testrows]
                       ,ViolentCrimesPerPop = myX$ViolentCrimesPerPop[-testrows])

  
  testdf = data.frame(PctIlleg = myX$PctIlleg[testrows]
                      ,racepctblack = myX$racepctblack[testrows]
                      ,pctWPubAsst = myX$pctWPubAsst[testrows]
                      ,FemalePctDiv = myX$FemalePctDiv[testrows]
                      ,TotalPctDiv = myX$TotalPctDiv[testrows]
                      , MalePctDivorce = myX$MalePctDivorce[testrows]
                      , PctNotHSGrad = myX$PctNotHSGrad[testrows]
                      , PctUnemployed = myX$PctUnemployed[testrows]
                      , PctHousNoPhone = myX$PctHousNoPhone[testrows]
                      , PctKids2Par = myX$PctKids2Par[testrows]
                      , PctFam2Par = myX$PctFam2Par[testrows]
                      , racePctWhite = myX$racePctWhite[testrows]
                      , PctYoungKids2Par = myX$PctYoungKids2Par[testrows]
                      , PctTeen2Par = myX$PctTeen2Par[testrows]
                      , pctWInvInc = myX$pctWInvInc[testrows]
                      , PctPersOwnOccup = myX$PctPersOwnOccup[testrows]
                      , medFamInc = myX$medFamInc[testrows]
                      , medIncome = myX$medIncome[testrows]
                      , PctHousOwnOcc = myX$PctHousOwnOcc[testrows]
                      , racePctWhite = myX$racePctWhite[testrows]
                      , PctVacantBoarded = myX$PctVacantBoarded[testrows]
                      , PctHousLess3BR = myX$PctHousLess3BR[testrows]
                      , NumIlleg = myX$NumIlleg[testrows]
                      , MedNumBR = myX$MedNumBR[testrows]
                      , PctOccupManu = myX$PctOccupManu[testrows]
                      ,ViolentCrimesPerPop = myX$ViolentCrimesPerPop[testrows])
  
#PctOccupManu = PctOccupManu[-testrows]
#MedNumBR = myX$MedNumBR[-testrows]
#PctVacantBoarded = myX$PctVacantBoarded[testrows]
#PctHousLess3BR = myX$PctHousLess3BR[testrows]
#NumIlleg = myX$NumIlleg[testrows]
  
  mymods = list(NULL)
  
  #Model 1, constructed with top ten positively correlated variables
  ##########################################################################################################  
  mymods[[1]] = lm(ViolentCrimesPerPop 
                    ~ PctIlleg
                    + racepctblack
                    + pctWPubAsst
                    + PctVacantBoarded
                    + TotalPctDiv
                    + PctHousLess3BR
                    + PctHousNoPhone
                    , data = traindf)
  
  #Model 2, constructed with top ten negatively correlated variables
  ########################################################################################################
  mymods[[2]] = lm(ViolentCrimesPerPop 
                    ~ PctKids2Par
                    + PctOccupManu
                    + racePctWhite
                    + PctYoungKids2Par
                    + MedNumBR
                    + pctWInvInc
                    + medFamInc
                    + medIncome
                    , data = traindf)
  
  #Model 3, constructed with nonlinearity and low p-values in mind
  ##################################################################################################3
  mymods[[3]] = lm(ViolentCrimesPerPop 
                  ~ I(1/(PctFam2Par+1))
                  + racePctWhite
                  , data = traindf)
  
  for (j in c(1:nummods))
  {
    predictions = predict(mymods[[j]],newdata=testdf)
    msevals[j, i] = mean((predictions - testdf$ViolentCrimesPerPop)^2)
  }
}
dumbModel = mean(traindf$ViolentCrimesPerPop)
dumbMSE = mean((dumbModel - testdf$ViolentCrimesPerPop)^2)

pdf("posCorrModel vs ViolentCrimesPerPop.pdf")
plot(predict(mymods[[1]],newdata=testdf), testdf$ViolentCrimesPerPop, xlab = "smartModel predictions")
abline(mymods[[1]], col = "red")
abline(0, 1, col='blue')
dev.off()

pdf("negCorrModel vs ViolentCrimesPerPop.pdf")
plot(predict(mymods[[2]],newdata=testdf), testdf$ViolentCrimesPerPop, xlab = "smartModel predictions")
abline(mymods[[2]], col = "red")
abline(0, 1, col='blue')
dev.off()

pdf("smartModel vs ViolentCrimesPerPop.pdf")
plot(predict(mymods[[3]],newdata=testdf), testdf$ViolentCrimesPerPop, xlab = "smartModel predictions")
abline(mymods[[3]], col = "red")
abline(0, 1, col='blue')
dev.off()

#Print mean squared error values for the four models
##################################################################################################
message(sprintf("Dumb model MSE: %f", mean(dumbMSE)))

message(sprintf("posCorrModel MSE: %f", mean(msevals[1,])))
message(sprintf("Positive Model Percent Error to the Control: %f%%", ((dumbMSE-mean(msevals[1,]))/dumbMSE)))

message(sprintf("negCorrModel MSE: %f", mean(msevals[2,])))
message(sprintf("Negative Model Percent Error to the Control: %f%%", ((dumbMSE-mean(msevals[2,]))/dumbMSE)))

message(sprintf("smartModel MSE: %f", mean(msevals[3,])))
message(sprintf("Smart Model Percent Error to the Control: %f%%", ((dumbMSE-mean(msevals[3,]))/dumbMSE)))

