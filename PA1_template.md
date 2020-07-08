---
title: "Reproducible Research: Peer Assessment 1"
author: 'Atharva Rmagirkar'
date: 'July 7th, 2020'
output: 
  html_document:
    keep_md: true
---



#### load the necessary libraries


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
                geom = "histogram",
                xlab = "total number of steps",
                ylab = "frequency",
                main = "Total steps daily before replacing NA values")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### the Mean and the Median are:-


```r
mean(Perday$totalsteps,na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(Perday$totalsteps,na.rm = TRUE)
```

```
## [1] 10765
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

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



## Imputing missing values

#### First check the number of missing values


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
```

#### Check the number of missing values again


```r
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

#### Make a histogram without any missing values 


```r
perday.filled<-ddply(filled_data,.(date),summarize,totalsteps=sum(steps))
qplot(perday.filled$totalsteps,
                  binwidth=1000,
                  geom = "histogram",
                  xlab = "total number of steps",
                  ylab = "frequency",
                  main = "Total steps daily after replacing NA values")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



## Are there differences in activity patterns between weekdays and weekends?


```r
which_day<-function(day)
  {
  
    if(day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
      {
        return("Weekday")
      }
    else if(day %in% c("Saturday","Sunday"))
      {
        return("Weekend")
      }
    else
      {
        stop("Invalid Day")
      }

  }

activity$week<-sapply(X = weekdays(activity$date),FUN = which_day,simplify = TRUE)
filled_data$week<-activity$week

interval_avg.filled<-ddply(filled_data,.(interval,week),summarise,totalsteps=mean(steps))

ggplot(interval_avg.filled, aes(interval, totalsteps)) + geom_line() + facet_grid(week ~ .) + xlab("5 minute interval") + ylab("total steps")  
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


