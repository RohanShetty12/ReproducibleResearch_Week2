Analysis of Personal Movement Using Activity Monitoring Devices 
========================================================

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This project makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Reading and Preprepocessing the data
1.Downloading the zip file into current working directory

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","data.zip")
```

2.Unzip and read the data from "activity.csv" file

```r
datasetRaw <- read.csv(unz("data.zip","activity.csv"),header=T)
```

3.Summary of the data

```r
summary(datasetRaw)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

4.Structure of the data

```r
str(datasetRaw)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

5.Display the first 6 records

```r
head(datasetRaw)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?
1.Ignoring all the missing values from the dataset 

```r
datasetWithoutNA <- datasetRaw[!is.na(datasetRaw$steps),]
```

2.Preview of the first 6 rows

```r
head(datasetWithoutNA)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

3.Total number of steps taken per day

```r
stepCountPerDay <- setNames(aggregate(x=datasetWithoutNA$steps,by=list(datasetWithoutNA$date),FUN=sum,na.rm=T),c("date","steps"))
```

4.Histogram of the total number of steps taken each day

```r
hist(stepCountPerDay$steps,breaks=25,col="blue",xlab="Total Steps",main="HISTOGRAM OF TOTAL NUMBER OF STEPS TAKEN EACH DAY")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

5.Mean and median of the total number of steps taken per day

```r
meanSteps <- as.integer(mean(stepCountPerDay$steps))
meanSteps
```

```
## [1] 10766
```

```r
medianSteps <- as.integer(median(stepCountPerDay$steps))
medianSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1.Average number of steps taken,averaged across all days

```r
meanStepsPerInterval <- setNames(aggregate(datasetWithoutNA$steps,by=list(datasetWithoutNA$interval),FUN=mean,na.rm=T),c("interval","steps"))
```

2.Time series plot

```r
plot(meanStepsPerInterval$interval,meanStepsPerInterval$steps,type="l",col="red",lwd=1.5,xlab="Interval",ylab="Average Number Of Steps",main="Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

3.The 5-minute interval that contains the maxinum of steps, averaged across all the days

```r
maxInterval <- meanStepsPerInterval[meanStepsPerInterval$steps==max(meanStepsPerInterval$steps),]
maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```
The 5-minute interval that contains the maximum of steps, averaged across all the days, is **835**

## Imputing missing values
1.Total number of missing values in the dataset

```r
missingData <- datasetRaw[is.na(datasetRaw$steps),]
countMissingData <- nrow(missingData)
countMissingData
```

```
## [1] 2304
```
Hence, the total number of missing values in the dataset is **2304**.

2.Strategy for filling in all of the missing values in the dataset. Missing values are replaced by the mean of that 5-minute interval.

```r
#Create a dataset containing the original dataset
datasetNoNA <- read.csv(unz("data.zip","activity.csv"),header=T)

#Indexes of all the records that contain NA values
index <- which(is.na(datasetRaw$steps))

#Calculate the Mean steps averaged across all the days
meanStepsPerInterval <- setNames(aggregate(datasetWithoutNA$steps,by=list(datasetWithoutNA$interval),FUN=mean,na.rm=T),c("interval","steps"))

#Substitute the NA value in the original data set with the mean of the 5-minute interval
datasetNoNA$steps[index] <- meanStepsPerInterval[match(missingData$interval,meanStepsPerInterval$interval),2]

#Display the first 6 records of the substituted dataset
head(datasetNoNA)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

3.Verify the presence of any missing value in the substituted dataset

```r
missingDataSub <- datasetRaw[is.na(datasetNoNA$steps),]
countMissingSub <- nrow(missingDataSub)
```
Hence there are **0** records in the new substituted dataset

4.Total number of steps taken per day for the new substituted data set

```r
stepCountPerDayNoNA <- setNames(aggregate(x=datasetNoNA$steps,by=list(datasetNoNA$date),FUN=sum,na.rm=T),c("date","steps"))
```

5.Histogram of the total number of steps taken each day in the new substituted dataset

```r
hist(stepCountPerDayNoNA$steps,breaks=25,col="blue",xlab="Total Steps",main="HISTOGRAM OF TOTAL NUMBER OF STEPS TAKEN EACH DAY")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

6.Mean and median of the total number of steps taken per day for the substituted data set

```r
newMeanSteps <- as.integer(mean(stepCountPerDayNoNA$steps))
newMeanSteps
```

```
## [1] 10766
```

```r
newMedianSteps <- as.integer(median(stepCountPerDayNoNA$steps))
newMedianSteps
```

```
## [1] 10766
```
We notice that the mean of the substituted data set (**10766**) is equal to the mean of the data set without missing values (**10766**). The median of the substituted data set has shifted from **10765** to **10766**. Therefore, the mean and median for the substituted data set are almost identical to the data set without missing values.

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the substituted data set

```r
#Create a new variable contatining the weekdays equivalent of the date variable in the substituted data set 
weekday <- weekdays(as.Date(datasetNoNA$date,format="%Y-%m-%d"))

#Create a vector containing the values for the weekends
weekends <- c("Saturday","Sunday")

#Create a new factor variable in substituted data indicating whether a date is weekday/weekend
datasetNoNA["weekdays"] <- factor((weekday %in% weekends),labels = c("Weekday","Weekend"))

#Calculate the mean steps across all the dates grouped by weekdays/weekends
meanStepWeekDay <- setNames(aggregate(datasetNoNA$steps,by=list(datasetNoNA$interval,datasetNoNA$weekdays),FUN=mean),c("interval","weekday","meansteps"))
```
2.Panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days

```r
library(lattice)
xyplot(meanStepWeekDay$meansteps ~ meanStepWeekDay$interval|meanStepWeekDay$weekday,type="l",layout=c(1,2),xlab="Interval",ylab="Number of steps",col="red")
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)
### Observation:
From the above plot,we can infer that the activities over the weekdays are more concentrated during the morning and the evening times (predominently during morning), whereas, over the weekend, the activities are spreadout across the day. Also,one more interesting observation is that the activities start early in the morning, wheras over the weekends, the activities start late in the morning
