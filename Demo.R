setwd('D:/Documents/R/Statistics and Econometrics/DescriptiveStatistic')

library(devtools)
load_all()

des <- descriptive.stats(students)
print(des)

des <- descriptive.stats(students['Math'])
print(des)
