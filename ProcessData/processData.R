#! /usr/bin/env Rscript

# function to calculate aggregated summary statistics for income by activity
analyse.IncomeByActivity <- function(quotes) {
  #print(quotes)
  incomeByActivity <- NULL
  quotes.mainactivities <- unique(quotes$MainActivity)
  for(activity in quotes.mainactivities) {
    # loop through each activity and calculate aggregated summary statistics
    data <- analyse.Income(quotes[quotes$MainActivity == activity, 4])   
    incomeByActivity <- rbind(incomeByActivity, data.frame(activity, data))
  }
  return (incomeByActivity)
}

# function to read and process csv file containing download of Direct Qlickview dashboard.  
process.data <- function(filename) {
  # read tab separated Gatling log file with no header row
  directdata.data <- read.csv(filename, header = TRUE, sep = ",")
  
  # quote data for records that contain a business name 
  quotes <- directdata.data[directdata.data[,2]!="", c(1,2,3,4,6)]
  # requests[,7] <- requests[,5] - requests[,2]
  names(quotes) <- c("QuoteNumber","BusinessName","MainActivity","income", "ProductCode") #OFH","ProductCode","MOI","Town","NoofQuotes","County","PostCode","QuoteDate")
  
  output <- analyse.IncomeByActivity(quotes) 
  
  return (output)
}


# Calculate aggregated summary statistics (e.g. min, mean, quantiles, etc.) for incomes
analyse.Income <- function(quotes.income) {
  print(quotes.income)
  min1 <- min(quotes.income)
  #print(min1)
  max1 <- max(quotes.income)
  #print(max1)
  mean <- mean(quotes.income)
  sd <- sd(quotes.income)
  # Calculate 1%, 5%, 25%, 50%, 75%, 95% and 99% quantiles
  quantiles <- quantile(quotes.income, c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99))
  
  stats <- cbind(mean, sd, min1, t(quantiles), max1)
  
  return (stats)
}



plot.timings <- function(data) {
  # Expand right side of clipping rect to make room for the legend
  par(xpd=T, mar=par()$mar+c(0,0,0,4))
  
  # Columns of data.frame to include in graph
  columns <- c(5, 7, 8, 9, 11)
  
  # Graph summary data (transposing the matrix) using heat colors,  
  # put 10% of the space between each bar, and make labels  
  # smaller with horizontal y-axis labels
  barplot(t(data[columns]), main="Income By Activity", ylab="Income", 
          col=rainbow(length(columns)), space=0.1, cex.axis=0.8, las=1,
          names.arg=data[,1], cex=0.8) 
  
  # Place the legend at (6,30) using heat colors
  #legend(6, 30, names(data[,4:12]), cex=0.8, fill=heat.colors(7));
  legend("topright", names(data[,columns]), cex=0.8, fill=rainbow(length(columns)));
  
  # Restore default clipping rect
  par(mar=c(5, 4, 4, 2) + 0.1)
}

results <- process.data("~/Documents/Work/Analytics/Direct/mydata.csv")
plot.timings(results)
print(results)


#group.statistics <- analyse.groups(requests)
