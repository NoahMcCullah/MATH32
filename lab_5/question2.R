rm(list=ls(all=TRUE)) #clears memory

m = 0.60
s = sqrt(0.0175)
#
#a
#
x = seq(from=0, to=1, by=0.01)
pdf = dnorm(x, mean = m, sd = s)
#
#b
# P(neg) = 0
# P(X > 1) = 0
gradeNeg = pnorm(0, mean=m, sd=s)
print(gradeNeg)
gradeExtra = 1-pnorm(1, mean=m, sd=s)
print(gradeExtra)

#
#c
#
z = pnorm(0.9, mean = m, sd = s)
message(sprintf("90th percentile: %f", z))

#
#d
#
message(sprintf("Probability student get an A: %f%%", 100*(1-z)))

#
#e
#
passing = pnorm(0.5, mean=m, sd=s)
message(sprintf("Probability of a passing grade: %f%%", 100*(1-passing)))
