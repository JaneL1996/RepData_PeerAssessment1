---
title: "Activity monitoring data"
output: 
    html_document:
        keep_md: true
---



## download and unzip data


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./repdata%2Fdata%2Factivity.zip")
unzip("./repdata%2Fdata%2Factivity.zip")
activity <- read.csv("./activity.csv")
```

## Total number of steps taken each day

#### Calculate the total number of steps taken per day and make a histogram.

```r
step_d_t <- with(activity, tapply(steps, date, sum, na.rm=TRUE))
hist(step_d_t,xlab="steps",main="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
 
#### Calculate and the mean and median of the total number of steps taken per day.

```r
mean <- mean(step_d_t)
median <- median(step_d_t)
```
Mean of the total number of steps taken per day is 9354.2295082.
Median of the total number of steps taken per day is 10395.

## The average daily activity pattern

#### Make a time series plot of the average number of steps taken across the 5-minute intervals.

```r
step_i_m <- with(activity, tapply(steps, interval, mean, na.rm=TRUE))
plot(names(step_i_m),step_i_m, type= "l",xlab = "interval", ylab = "average steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### Calculate the 5-minute interval on average across contains the maximum number of steps.

```r
max <- names(which.max(step_i_m))
```
The 5-minute interval that on average contains the maximum number of steps is 835.

## Imputing missing values

#### Calculate the total number of missing values in the dataset.

```r
na <- sum(!complete.cases(activity))
```
There are 2304 missing values in the dataset.
 
#### Fill in all of the missing values in the dataset with the mean for that 5-minute interval.

```r
activity$step_m <- step_i_m
for (i in 1:17568){
    if (is.na(activity[i,1])==TRUE){
        activity[i,1] <- activity[i,4]
    }
}
activity <- activity[,1:3]
head(activity)
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

#### Make a histogram of the total number of steps taken each day after missing values are imputed.

```r
step_d_t2 <- with(activity, tapply(steps, date, sum))
hist(step_d_t2,xlab="steps",main="Total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### Calculate the mean and median total number of steps taken per day after missing values are imputed.

```r
mean2 <- mean(step_d_t2)
median2 <- median(step_d_t2)
```
Mean of the total number of steps taken per day is 1.0766189\times 10^{4}.
Median of the total number of steps taken per day is 1.0766189\times 10^{4}.

## Panel plot of weekdays and weekends

#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
Sys.setlocale("LC_ALL","English")
```

```
## [1] "LC_COLLATE=English_United States.1252;LC_CTYPE=English_United States.1252;LC_MONETARY=English_United States.1252;LC_NUMERIC=C;LC_TIME=English_United States.1252"
```

```r
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
activity$weekdays <- as.factor(weekdays(activity$date))
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity <- activity %>% mutate(w = ifelse(weekdays == "Saturday"|weekdays == "Sunday", "weekend", "weekday"))
head(activity)
```

```
##       steps       date interval weekdays       w
## 1 1.7169811 2012-10-01        0   Monday weekday
## 2 0.3396226 2012-10-01        5   Monday weekday
## 3 0.1320755 2012-10-01       10   Monday weekday
## 4 0.1509434 2012-10-01       15   Monday weekday
## 5 0.0754717 2012-10-01       20   Monday weekday
## 6 2.0943396 2012-10-01       25   Monday weekday
```

#### Make a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

```r
activity1 <- filter(activity, w == "weekday")
activity2 <- filter(activity,w == "weekend")
step_i_m1 <- with(activity1, tapply(steps, interval, mean))
step_i_m2 <- with(activity2, tapply(steps, interval, mean)) 
par(mfrow = c(2,1),mar = c(4, 5, 3, 5))
plot(names(step_i_m1),step_i_m1, type= "l",xlab = "interval", ylab = "average steps", main = "weekdays")
plot(names(step_i_m2),step_i_m2, type= "l",xlab = "interval", ylab = "average steps", main = "weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
