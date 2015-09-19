# Reproducible Research Project Assignment 1
Duong Vu  
September 19, 2015  




##Loading and preprocessing the data  
1. Download, unzip the file 

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileName <- "repdata-data-activity.zip"

if(!file.exists(fileName)){
  download.file(fileUrl,fileName, mode = "wb")
}
unzip(fileName)
```
2. Load, re-format and clean the data   

```r
data <- read.csv("activity.csv", header = TRUE, stringsAsFactors = F,sep = ",")
data$date <- as.Date(data$date, format="%Y-%m-%d")
data$interval <- as.numeric(data$interval)
data$steps <- as.numeric(data$steps)
datanew <- na.omit(data)
```

##What is mean total number of steps taken per day?  

```r
stepsperDay <- sqldf("select date, sum(steps) steps from datanew group by date order by date")
png(paste("figure", "StepsPerDayHistOriginal.png", sep = "/"))
hist(stepsperDay$steps, col="red", main = "Number of steps taken per day", xlab = "Steps")
dev.off()
```
![Total number of steps taken per day](figure/StepsPerDayHistOriginal.png) 

Mean of the total number of steps taken per day

```r
m <- mean(stepsperDay$steps)
print(m)
```

```
## [1] 10766.19
```

Media of the total number of steps taken per day

```r
me <- median(stepsperDay$steps)
print(me)
```

```
## [1] 10765
```

##What is the average daily activity pattern?
1. Plot a time series of  the average number of steps taken every 5-minute interval

```r
aveSteps <- sqldf("select interval, avg(steps) steps from datanew group by interval order by interval")
png(paste("figure", "AverageStepCount.png", sep = "/"))
plot(aveSteps$interval, aveSteps$steps, type="l", xlab = "Interval", ylab="Average steps count")
dev.off()
```
![Average steps count per 5-minute interval](figure/AverageStepCount.png) 

2. 5-minute interval with the maximum number of steps

```r
maxSteps <- aveSteps$interval[aveSteps$steps==max(aveSteps$steps)]
print(maxSteps)
```

```
## [1] 835
```

##Imputing missing values
1. Report total number of missing values in the dataset

```r
numberNA <- sqldf("select count(*) count from data where steps is null")
print(numberNA)
```

```
##   count
## 1  2304
```
2. Replacing all NA steps values and Update total steps histogram


```r
dataUpdate <- sqldf(c("update data set steps = (select steps from aveSteps where aveSteps.interval=data.interval) where steps is null", "select * from main.data"))

stepsperDayupdate <- sqldf("select date, sum(steps) steps from dataUpdate group by date order by date")
png(paste("figure","stepsperDayupdate.png", sep = "/"))
hist(stepsperDayupdate$steps, col="red", main = "Number of steps taken per day", xlab = "Steps")
dev.off()
```
![Total number of steps taken per day after update dataset](figure/stepsperDayupdate.png) 

Update Mean of the total number of steps taken per day

```r
m2 <- mean(stepsperDayupdate$steps)
print(m2)
```

```
## [1] 10766.19
```
Update Media of the total number of steps taken per day

```r
me2 <- median(stepsperDayupdate$steps)
print(me2)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

```r
datanew$"daytype" <- weekdays(datanew$"date")
dataDaytype <- sqldf(c("update datanew set daytype = 'Weekend' where daytype in ('Saturday','Sunday')", "select * from main.datanew"))
dataDaytype <- sqldf(c("update dataDaytype set daytype = 'Weekday' where daytype in ('Monday','Tuesday','Wednesday','Thursday','Friday')", "select * from main.dataDaytype"))

groupedData <- aggregate(steps ~ interval + daytype, data = dataDaytype, FUN=mean)
png(paste("figure","weekday.png", sep = "/"))
xyplot(steps ~ interval | daytype, data = groupedData, layout=c(1,2), type = "l")
dev.off()
```
![Differences in activity patterns between weekdays and weekends](figure/weekday.png) 

