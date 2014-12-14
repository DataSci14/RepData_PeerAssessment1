---
title: "Reproducible Research: Peer Assessment 1"
author: "Michael Mateling"
date: "December 14, 2014"
output: 
  html_document:
    keep_md:true
---

# Reproducible Research: Peer Assessment 1

## Analysis of total steps taken by day

This report follows up on data in the activity.zip file which contains the activity.csv dataset.  The file contains personal movements, voluntarily recorded using an activity monitoring device and includes the number of steps taken in 5-minute intervals each day.

### Load the activity monitoring data from the csv file:

```r
csv <- read.csv("activity.csv")
```

### Solve the sum of total steps taken per day:  
Using the summary command to get the mean and median total steps taken by day (The mean number of steps is 10766 and median number of steps is 10765.  These calculations take into account all recorded steps across all recorded days in October and November. 8 NA values/rows were found and ignored when calculating the mean and median):

```r
library("plyr")
meanmedsum <- ddply(csv, .(date), summarise, sum = sum(as.numeric(as.character(steps))))
meanmedsum
```

```
##          date   sum
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

```r
summary(meanmedsum)
```

```
##          date         sum       
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 8841  
##  2012-10-03: 1   Median :10765  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:13294  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55   NA's   :8
```
## Mean: 10766 
## Median: 10765

### Use ggplot2 to produce histogram of total steps taken each day:

```r
library("ggplot2")
g <- ggplot(meanmedsum, aes(date, sum))
g + geom_histogram(stat = "identity", aes(fill = sum)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, size = 7)) + labs(title = "Total steps taken each day") + labs(y = "Total steps")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

### Create a time series plot with 5-minute intervals:
The maximum average steps taken during a 5-minute interval are reported below: 


```r
x <- ddply(csv, .(interval), summarise, mean = mean(steps, na.rm=TRUE))
plot(x, type = "l", ylab="Average steps", xlab="minutes/day, starting @ 12am (values recorded in 5-min intervals)", main="Average steps taken from 10-01-14 to 11-30-14")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
summary(x)
```

```
##     interval           mean        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
y <- subset(x, x$mean==max(x$mean))
y
```

```
##     interval     mean
## 104      835 206.1698
```
## Max average steps: 206

## 5-minute interval with, on average, the max number of steps: 835 (ie: 8:35am)


### Imputation of missing values into the dataset:
Missing values were assigned the mean value of each 5-minute interval to NAs occuring in their respective 5-minute interval.  Total missing step values in the dataset is reported.

```r
summary(csv)
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

```r
adjNA <- data.frame()
for(i in levels(factor(csv[,3]))) {
  z <- subset(csv, csv[,3]==i)
  zz <- mean(z[,1], na.rm=TRUE)
  z[is.na(z)] <- zz
  adjNA <- rbind(adjNA, z) }
  averages <- ddply(adjNA, .(interval), summarise, mean = mean(steps))
```

## Total missing step values in the dataset: 2304

### Use ggplot2 to produce a histogram of total steps taken each day:
This histogram uses the new dataset that has replaced NAs with their respective 5-minute interval average:

```r
adjNAsum <- ddply(adjNA, .(date), summarise, sum = sum(as.numeric(as.character(steps))))
adjNAsum
```

```
##          date      sum
## 1  2012-10-01 10766.19
## 2  2012-10-02   126.00
## 3  2012-10-03 11352.00
## 4  2012-10-04 12116.00
## 5  2012-10-05 13294.00
## 6  2012-10-06 15420.00
## 7  2012-10-07 11015.00
## 8  2012-10-08 10766.19
## 9  2012-10-09 12811.00
## 10 2012-10-10  9900.00
## 11 2012-10-11 10304.00
## 12 2012-10-12 17382.00
## 13 2012-10-13 12426.00
## 14 2012-10-14 15098.00
## 15 2012-10-15 10139.00
## 16 2012-10-16 15084.00
## 17 2012-10-17 13452.00
## 18 2012-10-18 10056.00
## 19 2012-10-19 11829.00
## 20 2012-10-20 10395.00
## 21 2012-10-21  8821.00
## 22 2012-10-22 13460.00
## 23 2012-10-23  8918.00
## 24 2012-10-24  8355.00
## 25 2012-10-25  2492.00
## 26 2012-10-26  6778.00
## 27 2012-10-27 10119.00
## 28 2012-10-28 11458.00
## 29 2012-10-29  5018.00
## 30 2012-10-30  9819.00
## 31 2012-10-31 15414.00
## 32 2012-11-01 10766.19
## 33 2012-11-02 10600.00
## 34 2012-11-03 10571.00
## 35 2012-11-04 10766.19
## 36 2012-11-05 10439.00
## 37 2012-11-06  8334.00
## 38 2012-11-07 12883.00
## 39 2012-11-08  3219.00
## 40 2012-11-09 10766.19
## 41 2012-11-10 10766.19
## 42 2012-11-11 12608.00
## 43 2012-11-12 10765.00
## 44 2012-11-13  7336.00
## 45 2012-11-14 10766.19
## 46 2012-11-15    41.00
## 47 2012-11-16  5441.00
## 48 2012-11-17 14339.00
## 49 2012-11-18 15110.00
## 50 2012-11-19  8841.00
## 51 2012-11-20  4472.00
## 52 2012-11-21 12787.00
## 53 2012-11-22 20427.00
## 54 2012-11-23 21194.00
## 55 2012-11-24 14478.00
## 56 2012-11-25 11834.00
## 57 2012-11-26 11162.00
## 58 2012-11-27 13646.00
## 59 2012-11-28 10183.00
## 60 2012-11-29  7047.00
## 61 2012-11-30 10766.19
```

```r
summary(adjNAsum)
```

```
##          date         sum       
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10766  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
h <- ggplot(adjNAsum, aes(date, sum))
h + geom_histogram(stat = "identity", aes(fill = sum)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, size = 7)) + labs(title = "Total steps taken each day") + labs(y = "Total steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
## New mean: 10766 
## New median: 10766
The mean is unchanged because average 5-minute intervals sum to the mean.  The median has increased slightly to 10766 as a result of converting the NA values.


### Weekday vs weekend pattern recognition:
Using dataset with filled-in missing values, a new factor variable is created with two levels "weekend" and "weekday".  Using ggplot2, a panel plot containing time series plots of 5-minute intervals for weekends and weekdays is created.  This offers an alternative way to see the data by separating activity based on workdays versus days off; assuming volunteers have a standard Monday-Friday job and/or classes.

```r
day <- weekdays(as.Date(adjNA$date))
day <- gsub("Saturday","weekend",day)
day <- gsub("Sunday","weekend",day)
day <- gsub("Monday","weekday",day)
day <- gsub("Tuesday","weekday",day)
day <- gsub("Wednesday","weekday",day)
day <- gsub("Thursday","weekday",day)
day <- gsub("Friday","weekday",day)
adjNA <- cbind(adjNA, day)
xx <- ddply(adjNA, .(interval, day), summarise, mean = round(mean(steps), 7))
t <- ggplot(xx, aes(interval, mean))
t + geom_line() + facet_grid(day~.) + labs(y = "Average steps") + labs(title = "Average steps taken (weekdays vs. weekends)")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

