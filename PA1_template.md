---
title: "Reproducible Research: Peer Assessment 1"
author: 'Atharva Rmagirkar'
date: 'July 7th, 2020'
output: 
  html_document:
    keep_md: true
---




```r
library(plyr)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```

## Loading and preprocessing the data


```r
unzip(zipfile = "activity.zip", exdir = ".")
activity <- read.csv(file = "activity.csv", header = TRUE)
activity$date<-as.Date(x = activity$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
Perday<-ddply(activity,
              .(date),
              summarize,
              totalsteps=sum(steps))
qplot(Perday$totalsteps,
      binwidth=1000,
      geom = "histogram")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/meansteps-1.png)<!-- -->

```r
mean(Perday$totalsteps)
```

```
## [1] NA
```

```r
median(Perday$totalsteps)
```

```
## [1] NA
```

## What is the average daily activity pattern?

```r
interval_avg <- ddply(activity,
                      .(interval),
                      summarize,
                      avgsteps=mean(steps,
                                    na.rm = TRUE))

plot(x = interval_avg$interval,
     y = interval_avg$avgsteps,
     type = "l")
```

![](PA1_template_files/figure-html/interval_avgsteps-1.png)<!-- -->
## Imputing missing values


```r
print("total number of missing values are:-")
```

```
## [1] "total number of missing values are:-"
```

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
int_avgs<-rep(interval_avg$avgsteps,length(activity$steps)/length(interval_avg$interval))
activity$avgs<-int_avgs
inputNas= function(steps, avgs) 
{
  if (is.na(steps)) 
  { 
    return(avgs)
  }
  return(steps)
}


filled_data = activity
filled_data$steps = mapply(inputNas,
                           filled_data$steps,
                           filled_data$avgs)

print("total number of missing values are:-")
```

```
## [1] "total number of missing values are:-"
```

```r
sum(is.na(filled_data$steps))
```

```
## [1] 0
```

```r
head(filled_data)
```

```
##       steps       date interval      avgs
## 1 1.7169811 2012-10-01        0 1.7169811
## 2 0.3396226 2012-10-01        5 0.3396226
## 3 0.1320755 2012-10-01       10 0.1320755
## 4 0.1509434 2012-10-01       15 0.1509434
## 5 0.0754717 2012-10-01       20 0.0754717
## 6 2.0943396 2012-10-01       25 2.0943396
```

## Are there differences in activity patterns between weekdays and weekends?


