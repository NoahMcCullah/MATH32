rm(list=ls(all=TRUE))
current_wd = "C:/Repos/Math 32/lab_4"
setwd("C:/Repos/Math 32/lab_4")

load("C:/Repos/Math 32/lab_4/concrete.Rdata")

yMSE = c(1:6)
ytestMSE = c(1:6)

mylmSlag = lm(y ~ water + cement + blast_furnace_slag, data = x) #7.07e-29, 170.80
ypredSlag = predict(mylmSlag,newdata=xtest)
yMSE[1] = mean(y - predict(mylmSlag))^2
ytestMSE[1] = mean((ytest-ypredSlag)^2)

mylmFly = lm(y ~ water + cement + fly_ash, data = x) #1.19e-28, 212.01
ypredFly = predict(mylmFly,newdata=xtest)
yMSE[2] = mean(y - predict(mylmFly))^2
ytestMSE[2] = mean((ytest-ypredFly)^2)

mylmSuper = lm(y ~ water + cement + superplasticizer, data = x) #8.87e-30, 198.41 <- best for y-MSE
ypredSuper = predict(mylmSuper,newdata=xtest)
yMSE[3] = mean(y - predict(mylmSuper))^2
ytestMSE[3] = mean((ytest-ypredSuper)^2)

mylmCoarse = lm(y ~ water + cement + coarse_agg, data = x) #3.75e-28, 205.63
ypredCoarse = predict(mylmCoarse,newdata=xtest)
yMSE[4] = mean(y - predict(mylmCoarse))^2
ytestMSE[4] = mean((ytest-ypredCoarse)^2)

mylmFine = lm(y ~ water + cement + fine_agg, data = x) #2.92e-29, 188.6
ypredFine = predict(mylmFine,newdata=xtest)
yMSE[5] = mean(y - predict(mylmFine))^2
ytestMSE[5] = mean((ytest-ypredFine)^2)

mylmAge = lm(y ~ water + cement + age, data = x) #1.17e-28, 161.87 <- best for ytest-MSE
ypredAge = predict(mylmAge,newdata=xtest)
yMSE[6] = mean(y - predict(mylmAge))^2
ytestMSE[6] = mean((ytest-ypredAge)^2)

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

