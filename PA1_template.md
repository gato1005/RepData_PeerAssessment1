---
title: "Reproducible Research: Peer Assessment 1"
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
sum(is.na(activity$steps))
```

```
## [1] 2304
```

## Are there differences in activity patterns between weekdays and weekends?
