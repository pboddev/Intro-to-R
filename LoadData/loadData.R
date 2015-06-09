#install.packages("ggplot2")
#install.packages("reshape2")
chinagdpdata.csv <- read.csv("http://www.quandl.com/api/v1/datasets/WORLDBANK/CHN_NY_GDP_MKTP_KD_ZG.csv")

activitydata.csv <- read.csv("~/Documents/Work/Analytics/Direct/mydata copy.csv")

colnames(chinagdpdata.csv)
head(chinagdpdata.csv)
tail(chinagdpdata.csv)

dim(chinagdpdata.csv)
str(chinagdpdata.csv)

dim(activitydata.csv)
str(activitydata.csv)

activityIncomeGT10k <- subset(activitydata.csv, Income > 10000)
summary(activityIncomeGT10k)

by(activitydata.csv[, c(4,5,7,9)], activitydata.csv$Post.Code, colMeans)
by(activitydata.csv[, c(4,5,7,9)], activitydata.csv$Main.Activity, colMeans)

ggplot(activitydata.csv, aes(x = Income)) + geom_histogram()

ggplot(activitydata.csv, aes(x = Income)) + geom_density()

ggplot(activitydata.csv, aes(x = Income)) + geom_histogram() + facet_wrap(~Main.Activity)
