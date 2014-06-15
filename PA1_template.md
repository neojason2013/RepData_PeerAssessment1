# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
* Load the data (i.e. read.csv())

```r
activity <- read.csv(unz("activity.zip","activity.csv"),
                  header=T, sep=",", na.strings="NA")
```
* Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity$date <- as.Date(activity$date , format = "%Y-%m-%d") 

byDay <- aggregate(steps ~ date, data=activity, sum)
names(byDay)[1] <-"day"
names(byDay)[2] <-"steps"

byInterval <- aggregate(steps ~ interval, data=activity, sum, na.rm=T)
names(byInterval)[1] <-"interval"
names(byInterval)[2] <-"steps"
```

## What is mean total number of steps taken per day?
* Make a histogram of the total number of steps taken each day

```r
hist(byDay$steps, main="Histogram of the total number of steps taken each day", xlab="Step", ylab="Freq")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

* Calculate and report the mean and median total number of steps taken per day

Mean number of steps per day:

```r
mean(byDay$steps, na.rm = TRUE)
```

```
## [1] 10766
```
Median number of steps per day:

```r
median(byDay$steps, na.rm = TRUE)
```

```
## [1] 10765
```
    
## What is the average daily activity pattern?

* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInterval <- aggregate(steps ~ interval, data=activity, mean, na.rm=T)
plot(steps ~ interval, data=stepsInterval, type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
stepsInterval[which.max(stepsInterval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Using the mean for the interval as a replacement for missing values.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newActivity <- activity   

for(i in 1:nrow(newActivity)){
    if(is.na(newActivity[i,]$steps)){
        newActivity[i,]$steps <- stepsInterval[stepsInterval$interval==newActivity[i,]$interval,]$steps
    }
}
```
* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newByDay <- aggregate(steps ~ date, data=newActivity, sum)
hist(newByDay$steps)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(newByDay$steps)
```

```
## [1] 10766
```

```r
median(newByDay$steps)
```

```
## [1] 10766
```

Since the NA is filled with mean value, the mean value of new dataset is the same as before, and the median was pulled to the mean.

## Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
activity$weekdays <- factor(format(activity$date,'%A'))

levels(activity$weekdays) <- list("weekday" = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "weekend" = c("Saturday", "Sunday"))
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
stepsInterval2 <- aggregate(steps~interval+weekdays, data=activity, mean)

library(lattice)

xyplot(steps~interval | factor(weekdays),
       data=stepsInterval2, layout=c(1,2), type="l",
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
