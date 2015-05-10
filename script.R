# 1. Loading and preprocessing the data
# 
# Check the data file existence
datafile <- "activity.csv"
if (!file.exists(datafile)) {
  stop("Data file activity.csv is not in the working directory")
}
# Read in raw data
data <- read.csv(datafile, sep=",", colClasses=c("numeric", "Date", "numeric"), header=TRUE, stringsAsFactors=FALSE)
# convert interval to zero-padded string
# sprintf("%04d",data$interval)
# strptime(sprintf("%04d",data$interval), "%H%M")

# 2. What is mean total number of steps taken per day?
## dayly_total <- tapply(data$steps, as.factor(data$date),FUN=sum)
## hist(dayly_total)
## should consider to include NA as zero or not 
library(dplyr)
daily_stat <- summarise(group_by(data,date=as.factor(date)), total=sum(steps,na.rm=TRUE), avg=mean(steps), med=median(steps))
hist(daily_stat$total, breaks = 20)
## command to display mean and median here

# 3. What is the average daily activity pattern?
intv_stat <- summarise(group_by(data,interval), average=mean(steps,na.rm=TRUE))
## draw time series
with(intv_stat, plot(strptime(sprintf("%04d",interval),"%H%M"),average, type="l"))
## max
intv_stat[(intv_stat$average==max(intv_stat$average)),"interval"]

# 4. Imputing missing values
## number of rows with NA
nrow(data[is.na(data$steps),])
## devise the strategy to fill in the value?
## check to see any row with NA from intv_stat
## replace NA in data with average value in int_stat, can use apply() or by()
## we use for
data1 <- data
for (i in 1:nrow(data1)) {
  if (is.na(data1[i,]$steps)) {data1[i,]$steps <- intv_stat[(intv_stat$interval == data1[i,]$interval),"average"]}
}



