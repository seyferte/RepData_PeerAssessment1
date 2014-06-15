## Processing
## Load data, ensure date is formatted

raw.data <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
dates = unique(raw.data$date)
intervals = unique(raw.data$interval)

## Find interval means and medians
clean.interval.means <- as.numeric()
i <- 0
for(i in 1:length(intervals)){
  working.steps <- subset(raw.data, interval==intervals[i], select = steps)
  clean.interval.means[i] <- mean(working.steps$steps, na.rm=TRUE)
}
clean.interval.means <- cbind(intervals,clean.interval.means)
colnames(clean.interval.means) <- c("interval", "mean")

## Find date sums
clean.date.sums <- as.integer()
i <- 0
for(i in 1:length(dates)){
  working.steps <- subset(raw.data, date==dates[i], select = steps)
  clean.date.sums[i] <- sum(working.steps$steps, na.rm=TRUE)
}
clean.date.sums <- cbind.data.frame(dates,clean.date.sums)
colnames(clean.date.sums) <- c("date","total")

## What is mean total number of steps taken per day?
## 1 Histogram
hist(clean.date.sums$total)
plot(clean.date.sums)
## 2 Average and median for all dates
daily.mean = mean(clean.date.sums$total, na.rm = TRUE)
daily.median = median(clean.date.sums$total, na.rm = TRUE)

## What is the average daily activity pattern?
## Time Series plot
plot(clean.interval.means, type = "l")
max.interval <- clean.interval.means[which.max(clean.interval.means[,2])]
## Imputing missing values
## 1. NAs
length(which(is.na(raw.data$steps)))
## 2. & 3. Fillin NAs using average interval coerced to integer
simulated.data <- raw.data
observations <- length(simulated.data[,1])

i <- 0
for(i in 1:observations){
  if(is.na(simulated.data[i,1])){
    working.interval = simulated.data[i,3]
    simulated.data$steps[i] = 
      as.integer(round(clean.interval.means[(which(intervals == working.interval)),2], digits = 0))
  }
}

## 4. new histogram, mean and median
simulated.date.sums <- as.integer()
i <- 0
for(i in 1:length(dates)){
  working.steps <- subset(simulated.data, date==dates[i], select = steps)
  simulated.date.sums[i] <- sum(working.steps$steps, na.rm=TRUE)
}
simulated.date.sums <- cbind.data.frame(dates,simulated.date.sums)
colnames(simulated.date.sums) <- c("date","total")

hist(simulated.date.sums$total)
daily.mean.simulated = mean(simulated.date.sums$total, na.rm = TRUE)
daily.median.simulated = median(simulated.date.sums$total, na.rm = TRUE)

## Are there differences in activity patterns between weekdays and weekends?
i <- 0
weekday.value <- as.character()
for(i in 1:observations){
  weekday.value[i] <- weekdays(simulated.data$date[i], abbreviate=TRUE)
}

simulated.data <- cbind.data.frame(simulated.data,weekday.value)
## Weekend
simulated.data.weekend <- subset(simulated.data, weekday.value == c("Sat", "Sun"))
##weekend.dates = unique(simulated.data.weekend$date)
##simulated.date.sums.weekend <- as.integer()
##i <- 0
##for(i in 1:length(weekend.dates)){
##  working.steps <- subset(simulated.data.weekend, date==weekend.dates[i], select = steps)
##  simulated.date.sums.weekend[i] <- sum(working.steps$steps, na.rm=TRUE)
##}
##simulated.date.sums.weekend <- cbind.data.frame(weekend.dates,simulated.date.sums.weekend)
##colnames(simulated.date.sums.weekend) <- c("date","total")
## Weekend Intervals

simulated.data.weekend <- subset(simulated.data, weekday.value == c("Sat", "Sun"))
weekend.interval.means <- as.numeric()
i <- 0
for(i in 1:length(intervals)){
  working.steps <- subset(simulated.data.weekend, interval==intervals[i], select = steps)
  weekend.interval.means[i] <- mean(working.steps$steps, na.rm=TRUE)
}
weekend.interval.means <- cbind(intervals,weekend.interval.means)
colnames(weekend.interval.means) <- c("interval", "mean")
plot(weekend.interval.means, type = "l")

## Workweek
simulated.data.mon <- subset(simulated.data, weekday.value == "Mon")
simulated.data.tue <- subset(simulated.data, weekday.value == "Tue")
simulated.data.wed <- subset(simulated.data, weekday.value == "Wed")
simulated.data.thu <- subset(simulated.data, weekday.value == "Thu")
simulated.data.fri <- subset(simulated.data, weekday.value == "Fri")
simulated.data.workweek <- rbind(simulated.data.mon,simulated.data.tue,simulated.data.wed,simulated.data.thu,simulated.data.fri)
workweek.interval.means <- as.numeric()
i <- 0
for(i in 1:length(intervals)){
  working.steps <- subset(simulated.data.workweek, interval==intervals[i], select = steps)
  workweek.interval.means[i] <- mean(working.steps$steps, na.rm=TRUE)
}
workweek.interval.means <- cbind(intervals,workweek.interval.means)
colnames(workweek.interval.means) <- c("interval", "mean")
plot(workweek.interval.means, type = "l")

set.panel(2,1)
plot(weekend.interval.means, type = "l")
plot(workweek.interval.means, type = "l")
##workweek.dates = unique(simulated.data.workweek$date)
##simulated.date.sums.workweek <- as.integer()
##i <- 0
##for(i in 1:length(dates)){
##  working.steps <- subset(simulated.data.workweek, date==workweek.dates[i], select = steps)
##  simulated.date.sums.workweek[i] <- sum(working.steps$steps, na.rm=TRUE)
##}
##simulated.date.sums.workweek <- cbind.data.frame(dates,simulated.date.sums.workweek)
##colnames(simulated.date.sums.workweek) <- c("date","total")
