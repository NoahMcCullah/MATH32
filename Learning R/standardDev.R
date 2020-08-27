# sample 100 values from standard deviation
stdev = rnorm(1000, mean = 0, sd = 1)

#PDF set-up
graphics.off()
pdf(file="hist.pdf",
    paper = "special",
    width = 4,
    height = 4,
    pointsize = 11)

#histogram plot of sample
hist(stdev)

#finalize PDF
dev.off()