---
title: "Reproducible Research: Peer Assessment 1"
author: "ACrst"
date: "17 April 2019"
output:  
  html_document:
   keep_md: true
---



## Loading and preprocessing the data


```r
if(!exists("activity.csv")){
  unzip("activity.zip")
}
if(!exists("activity.csv")){
act<-read.csv("activity.csv")
}

library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
act$date<-as.Date(act$date)
```
#Histogram of the total number of steps taken each day

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
library(reshape2)
 
actMeltDate<-melt(act,id.vars="date",measure.vars="steps",na.rm=TRUE)
actCastDate<-dcast(actMeltDate,date~variable,sum)
```

```r
plot(actCastDate$date,actCastDate$steps,type="h",main="Histogram of Daily steps",xlab="Date",ylab="Steps per day",col="green",lwd=8)
abline(h=mean(actCastDate$steps,na.rm=TRUE),col="red",lwd=2)
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

## What is mean total number of steps taken per day?

```r
meansteps<-mean(actCastDate$steps, na.rm = TRUE)
mediansteps<-median(actCastDate$steps,na.rm = TRUE)
```
The mean and median number of steps taken each day are 1.076619\times 10^{4} and 1.0765\times 10^{4} respectively.

## What is the average daily activity pattern?


```r
require(dplyr)
require(data.table)
```

```
## Loading required package: data.table
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:reshape2':
## 
##     dcast, melt
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:lubridate':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday,
##     week, yday, year
```

```r
actmeltInterval<-melt(act,id.vars="interval",measure.vars="steps",na.rm=TRUE)
actCastInterval<-dcast(actmeltInterval,interval~variable,mean)
```

```r
plot(actCastInterval$interval,actCastInterval$steps,type="l",main="Frequency of steps taken at each interval",xlab="Interval ID", ylab="Steps",col="blue",lwd=2)
abline(h=mean(actCastInterval$steps,na.rm = TRUE),col="red",lwd=2)
```

![](PA1_template_files/figure-html/daily-1.png)<!-- -->
#The 5-minute interval that, on average, contains the maximum number of steps

```r
paste("Interval with max value=",actCastInterval$interval[which(actCastInterval$steps==max(actCastInterval$steps))])
```

```
## [1] "Interval with max value= 835"
```

```r
paste("Maximum interval mean steps=",max(actCastInterval$steps))
```

```
## [1] "Maximum interval mean steps= 206.169811320755"
```


## Imputing missing values

```r
sum(is.na(act$steps))
```

```
## [1] 2304
```
Since there are a considerable number of missing values, we can replace the NAs with the mean for the particular interval number. For example, if the average number of steps taken during the interval x is y, we will replace each each NA with the corresponding y value for that row. Then we will recalculate the steps per day to see how much it differs  from the original result(with NAs included).


```r
stepsperInterval<-actCastInterval
actNoNA<-act
actMerge=merge(actNoNA,stepsperInterval,by="interval",suffixes=c(".act",".spi"))
naIndex=which(is.na(actNoNA$steps))
actNoNA[naIndex,"steps"]=actMerge[naIndex,"steps.spi"]
```
Melt new data frame to prep for casting by date

```r
actMeltDateNoNA<-melt(actNoNA,id.vars="date",measure.vars="steps",na.rm=FALSE)
actCastDateNoNA<-dcast(actMeltDateNoNA,date~variable,sum)
```
#Histogram of the total number of steps taken each day after missing values are imputed


```r
plot(actCastDateNoNA$date,actCastDateNoNA$steps,type="h",main="Histogram of Daily steps (Imputed NA values)",xlab="Date",ylab="Steps",col="gray",lwd=8)
abline(h=mean(actCastDateNoNA$steps),col="red",lwd=2)
```

![](PA1_template_files/figure-html/histogram_no_NAs-1.png)<!-- -->

```r
paste("Mean daily steps=",mean(actCastDateNoNA$steps,na.rm = TRUE))
```

```
## [1] "Mean daily steps= 10889.7992576554"
```

```r
paste("Median daily steps=",median(actCastDateNoNA$steps,na.rm = TRUE))
```

```
## [1] "Median daily steps= 11015"
```
On a percentage basis, the difference between the results in oroginal and new data is very little.The maximum daily value set replacing the NAs vs replacing NAs with mean was much more significant.

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
for(i in 1:nrow(actNoNA)){
  if(weekdays(actNoNA$date[i])=="Saturday" | weekdays(actNoNA$date[i])=="Sunday"){
    actNoNA$dayOfWeek[i]="weekend"
  } else{
     actNoNA$dayOfWeek[i]="weekday"
  }
}


actWeekday<-subset(actNoNA,dayOfWeek=="weekday")
actWeekend<-subset(actNoNA,dayOfWeek=="weekend")

actMeltWeekday<-melt(actWeekday,id.vars="interval",measure.vars="steps")
actMeltWeekend<-melt(actWeekend,id.vars="interval",measure.vars="steps")
actCastWeekday<-dcast(actMeltWeekday,interval~variable,mean)
actCastWeekend<-dcast(actMeltWeekend,interval~variable,mean)
```



## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
plot1<-qplot(actCastWeekday$interval,actCastWeekday$steps,geom="line",data=actCastWeekday,type="bar",main="Steps by Interval-Weekday",xlab="Interval ID", ylab="Number of Steps")
```

```
## Warning: Ignoring unknown parameters: type
```

```r
plot2<-qplot(actCastWeekend$interval,actCastWeekend$steps,geom="line",data=actCastWeekend,type="bar",main="Steps by Interval-Weekend",xlab="Interval ID", ylab="Number of Steps")
```

```
## Warning: Ignoring unknown parameters: type
```

```r
grid.arrange(plot1,plot2,nrow=2)
```

![](PA1_template_files/figure-html/weekend_comparison-1.png)<!-- -->
