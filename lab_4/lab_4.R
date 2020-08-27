#collaborated with Nick Carreon
rm(list=ls(all=TRUE)) #clears memory

#load concrete.Rdata file
current_wd = "C:/Repos/Math 32/lab_4"
#if(!current_wd != "C:/Repos/Math 32/lab_4")
setwd("C:/Repos/Math 32/lab_4")
load("C:/Repos/Math 32/lab_4/concrete.Rdata")

#Task 1
#create a boxplot for each x column and save it to a pdf
pdf("boxplot.pdf", height = 7, width = 7)
boxplot(x)
dev.off()

#create a normalized histogram of x$age and save it to a pdf
pdf("normhist.pdf", height = 7, width = 7)
hist(x$age, probability = T)
dev.off()

#create a matrix of plots for each combination of x variables and save it to a pdf
pdf("pairs.pdf", height = 7, width = 7)
pairs(x)
dev.off()

#print a summary of the mean, quartiles, etc for the variables of x
print(summary(x))

#Task 2
#create vectors to store the MSE of each model checked against y and ytest
yMSE = c(1:6)
ytestMSE = c(1:6)

#create a linear model using blast_furnace_slag, then calculate the MSE against y and ytest
mylmSlag = lm(y ~ water + cement + blast_furnace_slag, data = x) #7.07e-29, 170.80
ypredSlag = predict(mylmSlag,newdata=xtest)
yMSE[1] = mean(y - predict(mylmSlag))^2
ytestMSE[1] = mean((ytest-ypredSlag)^2)

#create a linear model using fly_ash, then calculate the MSE against y and ytest
mylmFly = lm(y ~ water + cement + fly_ash, data = x) #1.19e-28, 212.01
ypredFly = predict(mylmFly,newdata=xtest)
yMSE[2] = mean(y - predict(mylmFly))^2
ytestMSE[2] = mean((ytest-ypredFly)^2)

#create a linear model using superplasticizer, then calculate the MSE against y and ytest
mylmSuper = lm(y ~ water + cement + superplasticizer, data = x) #8.87e-30, 198.41 <- best for y-MSE
ypredSuper = predict(mylmSuper,newdata=xtest)
yMSE[3] = mean(y - predict(mylmSuper))^2
ytestMSE[3] = mean((ytest-ypredSuper)^2)

#create a linear model using coarse_agg, then calculate the MSE against y and ytest
mylmCoarse = lm(y ~ water + cement + coarse_agg, data = x) #3.75e-28, 205.63
ypredCoarse = predict(mylmCoarse,newdata=xtest)
yMSE[4] = mean(y - predict(mylmCoarse))^2
ytestMSE[4] = mean((ytest-ypredCoarse)^2)

#create a linear model using fine_agg, then calculate the MSE against y and ytest
mylmFine = lm(y ~ water + cement + fine_agg, data = x) #2.92e-29, 188.6
ypredFine = predict(mylmFine,newdata=xtest)
yMSE[5] = mean(y - predict(mylmFine))^2
ytestMSE[5] = mean((ytest-ypredFine)^2)

#create a linear model using age, then calculate the MSE against y and ytest
mylmAge = lm(y ~ water + cement + age, data = x) #1.17e-28, 161.87 <- best for ytest-MSE
ypredAge = predict(mylmAge,newdata=xtest)
yMSE[6] = mean(y - predict(mylmAge))^2
ytestMSE[6] = mean((ytest-ypredAge)^2)

#print the MSE of each model to demonstrate which works best
print("Column: Blast Furnace Slag")
print(yMSE[1])
print(ytestMSE[1])
print("Column: Fly Ash")
print(yMSE[2])
print(ytestMSE[2])
print("Column: Superplasticizer")
print(yMSE[3])
print(ytestMSE[3])
print("Column: Coarse Agg")
print(yMSE[4])
print(ytestMSE[4])
print("Column: Fine Agg")
print(yMSE[5])
print(ytestMSE[5])
print("Column: Age")
print(yMSE[6])
print(ytestMSE[6])

