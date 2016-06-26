# Reproducible Research: Peer Assessment 1


```r
library(plyr)
library(ggplot2)
```

## Loading and preprocessing the data

load activity data into steps_data, convert dates into Date format, and add a column for the weekday.

```r
unzip("activity.zip")
steps_data <- read.csv("activity.csv")
steps_data$date <- as.Date(steps_data$date)
steps_data$weekday <- weekdays(steps_data$date)
```


## What is mean total number of steps taken per day?

```r
steps_sum <- tapply(steps_data$steps,steps_data$date,FUN = sum)
# 1. Calculate the total number of steps taken per day
steps_sum
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015         NA      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414         NA      10600      10571         NA      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219         NA         NA      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336         NA         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##         NA
```

```r
# 2. Make a histogram of the total number of steps taken each day
hist(steps_sum)
```

![](PA1_template_files/figure-html/mean_steps-1.png)<!-- -->

```r
# 3. Calculate and report the mean and median of the total number of steps taken per day
mean(steps_sum,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_sum,na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
# add a column to the data set for the mean number of steps across all days for each interval
steps_data <- ddply(steps_data,.(interval),transform,mean_steps_interval = mean(steps,na.rm = TRUE ))

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
plot.ts(steps_data$interval,steps_data$mean_steps_interval)
```

![](PA1_template_files/figure-html/activity-1.png)<!-- -->

```r
# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
steps_data$interval[which.max(steps_data$mean_steps_interval)]
```

```
## [1] 835
```

## Imputing missing values

```r
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(steps_data))
```

```
## [1] 2304
```

```r
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# We will use the mean steps for that interval a´across the same day of the week to replace NA values.  Adding a coluumn mean_steps_interval_weekday for this value
steps_data <- ddply(steps_data,.(interval,weekday),transform,mean_steps_interval_weekday = mean(steps,na.rm = TRUE ))

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

steps_data <- transform(steps_data, replaced_steps = ifelse(!is.na(steps),steps,mean_steps_interval_weekday))
replaced_steps_sum <- 
  
# 4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
tapply(steps_data$replaced_steps,steps_data$date,FUN = sum)


hist(replaced_steps_sum)
```

![](PA1_template_files/figure-html/imputing-1.png)<!-- -->

```r
mean(replaced_steps_sum,na.rm = TRUE)
```

```
## [1] 10821.21
```

```r
median(replaced_steps_sum,na.rm = TRUE)
```

```
## [1] 11015
```

```r
# imputing missing data increases the mean and median of the total daily number of steps.  However it also reduces the variance of the mean number of steps per day, as evidences by the histogram, which sows only the middle bar increase.
```



## Are there differences in activity patterns between weekdays and weekends?


```r
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
weekendDays <- c("Saturday","Sunday","Samstag","Sonntag")
 steps_data$day_type <- factor((steps_data$weekday %in% weekendDays),levels = c(TRUE, FALSE), labels=c("weekend","weekday"))
 
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

steps_for_plot <- ddply(steps_data, .(interval,day_type), summarize,  mean = mean(replaced_steps,na.rm = TRUE))
ggplot() + geom_line(data = steps_for_plot, aes(x = interval, y = mean)) + facet_grid(facets = day_type ~ .)
```

![](PA1_template_files/figure-html/weekday-1.png)<!-- -->
