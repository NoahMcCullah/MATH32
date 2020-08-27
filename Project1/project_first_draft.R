#Project First Draft
#Noah McCullah
#Adrian Gomez
#
rm(list=ls(all=TRUE))

library("ggplot2")
library("GGally")
load('crime.RData')

#make test set
myX = x

#####################################################################################
#         Replace NA values in each column with the mean of the column                    
####################################################################################3
#x[is.na(x[,i]),i] = mean(x[,i], n.rm=TRUE)

myX$LemasSwornFT[is.na(myX$LemasSwornFT)] = mean(myX$LemasSwornFT, na.rm = TRUE)
myX$LemasSwFTPerPop[is.na(myX$LemasSwFTPerPop)] = mean(myX$LemasSwFTPerPop, na.rm = TRUE)
myX$LemasSwFTFieldOps[is.na(myX$LemasSwFTFieldOps)] = mean(myX$LemasSwFTFieldOps, na.rm = TRUE)
myX$LemasSwFTFieldPerPop[is.na(myX$LemasSwFTFieldPerPop)] = mean(myX$LemasSwFTFieldPerPop, na.rm = TRUE)
myX$LemasTotalReq[is.na(myX$LemasTotalReq)] = mean(myX$LemasTotalReq, na.rm = TRUE)
myX$LemasTotReqPerPop[is.na(myX$LemasTotReqPerPop)] = mean(myX$LemasTotReqPerPop, na.rm = TRUE)
myX$PolicReqPerOffic[is.na(myX$PolicReqPerOffic)] = mean(myX$PolicReqPerOffic, na.rm = TRUE)
myX$PolicPerPop[is.na(myX$PolicPerPop)] = mean(myX$PolicPerPop, na.rm = TRUE)
myX$RacialMatchCommPol[is.na(myX$RacialMatchCommPol)] = mean(myX$RacialMatchCommPol, na.rm = TRUE)
myX$PctPolicWhite[is.na(myX$PctPolicWhite)] = mean(myX$PctPolicWhite, na.rm = TRUE)
myX$PctPolicBlack[is.na(myX$PctPolicBlack)] = mean(myX$PctPolicBlack, na.rm = TRUE)
myX$PctPolicAsian[is.na(myX$PctPolicAsian)] = mean(myX$PctPolicAsian, na.rm = TRUE)
myX$PctPolicHisp[is.na(myX$PctPolicHisp)] = mean(myX$PctPolicHisp, na.rm = TRUE)
myX$PctPolicMinor[is.na(myX$PctPolicMinor)] = mean(myX$PctPolicMinor, na.rm = TRUE)
myX$OfficAssgnDrugUnits[is.na(myX$OfficAssgnDrugUnits)] = mean(myX$OfficAssgnDrugUnits, na.rm = TRUE)
myX$NumKindsDrugsSeiz[is.na(myX$NumKindsDrugsSeiz)] = mean(myX$NumKindsDrugsSeiz, na.rm = TRUE)
myX$PolicAveOTWorked[is.na(myX$PolicAveOTWorked)] = mean(myX$PolicAveOTWorked, na.rm = TRUE)
myX$PolicCars[is.na(myX$PolicCars)] = mean(myX$PolicCars, na.rm = TRUE)
myX$PolicOperBudg[is.na(myX$PolicOperBudg)] = mean(myX$PolicOperBudg, na.rm = TRUE)
myX$LemasPctPolicOnPatr[is.na(myX$LemasPctPolicOnPatr)] = mean(myX$LemasPctPolicOnPatr, na.rm = TRUE)
myX$LemasGangUnitDeploy[is.na(myX$LemasGangUnitDeploy)] = mean(myX$LemasGangUnitDeploy, na.rm = TRUE)
myX$PolicBudgPerPop[is.na(myX$PolicBudgPerPop)] = mean(myX$PolicBudgPerPop, na.rm = TRUE)
#########################################################################################3

# make training and test sets
trainfrac = 0.80
nrows = ceiling(trainfrac*nrow(myX))

# randomly choose "nrows" number of rows for the training set
trainrows = sample(c(1:nrow(myX)), nrows)
myXTrain = myX[trainrows, ]
myXTest = myX[-trainrows, ]

#Control model, average of ViolentCrimesPerPop in Train vs Test sets
#########################################################################################################  
dumbModel = mean(myXTest$ViolentCrimesPerPop)
dumbMSE = mean((dumbModel - myXTest$ViolentCrimesPerPop)^2)

#Model 1, constructed with top ten positively correlated variables
##########################################################################################################  
posCorrModel = lm(ViolentCrimesPerPop 
            ~ PctIlleg
            + racepctblack
            + pctWPubAsst
            + FemalePctDiv
            + TotalPctDiv
            + PctPolicBlack
            + MalePctDivorce
            + PctPopUnderPov
            + PctUnemployed
            + PctHousNoPhone
            , data = myXTrain)

posPredictions = predict(posCorrModel, newdata = myXTest)
posMSE = mean((posPredictions - myXTest$ViolentCrimesPerPop)^2)

#Model 2, constructed with top ten negatively correlated variables
########################################################################################################
negCorrModel = lm(ViolentCrimesPerPop 
                  ~ PctKids2Par
                  + PctFam2Par
                  + racePctWhite
                  + PctYoungKids2Par
                  + PctTeen2Par
                  + pctWInvInc
                  + PctPersOwnOccup
                  + medFamInc
                  + medIncome
                  + PctHousOwnOcc
                  , data = myXTrain)

negPredictions = predict(negCorrModel, newdata = myXTest)
negMSE = mean((negPredictions - myXTest$ViolentCrimesPerPop)^2)

#Model 3, constructed with nonlinearity and low p-values in mind
##################################################################################################3
smartModel = lm(ViolentCrimesPerPop 
            ~ I(1/(PctFam2Par+1))
            + racePctWhite
            , data = myXTrain)

smartPredictions = predict(smartModel, newdata = myXTest)
plot(x = smartPredictions, y = myXTest$ViolentCrimesPerPop)
abline(smartModel, col = "red")
abline(0, 1, col='blue')
smartMSE = mean((smartPredictions - myXTest$ViolentCrimesPerPop)^2)
#Print mean squared error values for the four models
##################################################################################################
message(sprintf("Dumb model MSE: %f", mean(dumbMSE)))

message(sprintf("posCorrModel MSE: %f", mean(posMSE)))
message(sprintf("Positive Model Percent Error to the Control: %f%%", ((dumbMSE-posMSE)/dumbMSE)))

message(sprintf("negCorrModel MSE: %f", mean(negMSE)))
message(sprintf("Negative Model Percent Error to the Control: %f%%", ((dumbMSE-negMSE)/dumbMSE)))

message(sprintf("smartModel MSE: %f", mean(smartMSE)))
message(sprintf("Smart Model Percent Error to the Control: %f%%", ((dumbMSE-smartMSE)/dumbMSE)))

