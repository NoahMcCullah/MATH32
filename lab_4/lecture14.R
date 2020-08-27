rm(list=ls(all=TRUE))

setwd("C:/Repos/MATH 32")
load("C:/Repos/MATH 32/ctg.RData")

model1 = glm(q ~ ./ data=m, family=binomial())

myconfusion = function(pfit, actual)

