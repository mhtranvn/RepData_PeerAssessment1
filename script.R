library(dplyr)
library(ggplot2)
library(scales)

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
data$interval <- sprintf("%04d",data$interval)

# 2. What is mean total number of steps taken per day?
## dayly_total <- tapply(data$steps, as.factor(data$date),FUN=sum)
## hist(dayly_total)
## should consider to include NA as zero or not 
## date_stat <- summarise(group_by(data,date=as.factor(date)), total=sum(steps, na.rm=TRUE), avg=mean(steps), med=median(steps))
dsum <- summarise(group_by(na.omit(data), date=as.factor(date)), total=sum(steps))
hist(dsum$total, breaks = 20, main = "Histogram of Total number of steps taken each day", xlab="Total Steps")
## command to display mean and median here
## View(dsum)
summarise(dsum, Mean=mean(total), Median=median(total))

# 3. What is the average daily activity pattern?

iavg <- summarise(group_by(data,interval), average=mean(steps,na.rm=TRUE))
## draw time series
with(iavg, plot(strptime(interval,"%H%M"), average, type="l", xlab="Time of day", 
                ylab="Steps", main="Average Daily Activity by Interval"))
## Interval with maximum number of steps
iavg[(iavg$average==max(iavg$average)),"interval"]
## paste("Interval with Maximum Number of Steps: ",iavg[(iavg$average==max(iavg$average)),"interval"])

# 4. Imputing missing values
## number of rows with NA
nrow(data[is.na(data$steps),])
## devise the strategy to fill in the value?
## replace NA in data with average value in iavg, can use apply() or by()
## we use for loop
data1 <- data
for (i in 1:nrow(data1)) {
    if (is.na(data1[i,]$steps)) {
        data1[i,]$steps <- as.numeric(iavg[(iavg$interval == data1[i,]$interval),"average"])
    }
}

## get new daily sum of steps
dsum1 <- summarise(group_by(data1,date=as.factor(date)), total=sum(steps))
## hist of total steps per day after imputing NA values
hist(dsum1$total, breaks = 20, main = "Histogram of Total number of steps taken each day", xlab="Total Steps")
summarise(dsum1, Mean=mean(total), Median=median(total))

# 5. Are there differences in activity patterns between weekdays and weekends?
## Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
data1 <- mutate(data1, day=as.factor(ifelse(weekdays(data1$date) %in% c("Saturday","Sunday"), "weekend", "weekday")))

## the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days
iavg1 <- summarise(group_by(data1, day, interval), average=mean(steps))
g <- ggplot(iavg1, aes(strptime(interval,"%H%M"), average)) + 
    geom_line() + facet_wrap(~ day, ncol = 1) + 
    xlab("Interval") +
    ylab("Number of steps") +
    scale_x_datetime(labels = date_format("%R")) + 
    ggtitle("Average Number of Steps per Interval in Weekday and Weekend")
print(g)