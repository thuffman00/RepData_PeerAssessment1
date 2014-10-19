---
title: "PA_template"
author: "T Huffman"
date: "Sunday, September 14, 2014"
output: html_document
---
# Peer Assignment 1

This assigment is to demonstrate how reproducible process is essential to data science.

## Loading and processing the data

This first thing to start will be loading of the necessary packages used in the assigment and load the data file used.

```r
library(reshape)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
library(plyr)
```

```
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:reshape':
## 
##     rename, round_any
```

```r
options(warn=-1)
activityData<-read.csv("activity.csv", sep=",", stringsAsFactors=F)
```

The dataset has a few zeros and NA's associated with the steps variable. We'll first get an index o the location of the zero entires. 

```r
zeroIdx<-which(activityData$steps == 0)
head(zeroIdx)
```

```
## [1] 289 290 291 292 293 294
```
Then we'll set them to NA's. create a new data frame and eliminate NA's using na.omit.

```r
activityData$steps[zeroIdx]<-c(NA)
cleanActivityData<-na.omit(activityData)
```
Now that we have a clean dataset lets create a histogram of the data showing the frequency of step.

```r
hist(cleanActivityData$steps,breaks=70, xlab="Steps", main="Histogram of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

## What is the mean total number of steps taken per day

From this point we'll calculate the mean and median of the steps withn the data frame.
First the mean

```r
cleanActivityDataMean<-tapply(cleanActivityData$steps,cleanActivityData$date, mean, na.rm=T)
head(cleanActivityDataMean, n=10)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##      63.00     140.15     121.16     154.58     145.47     101.99 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##     134.85      95.19     137.39     156.59
```
Then the median

```r
cleanActivityDataMedian<-tapply(cleanActivityData$steps,cleanActivityData$date, median, na.rm=T)
head(cleanActivityDataMedian, n=10)
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##       63.0       61.0       56.5       66.0       67.0       52.5 
## 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##       48.0       56.5       35.0       46.0
```

## What is the average daily activity pattern

Used tapply to create generate the means associated with the data set

```r
cleanInterval<-tapply(cleanActivityData$steps,cleanActivityData$interval, mean, na.rm=T)
cleanMeanDF<-data.frame(as.numeric(cleanInterval), dimnames(cleanInterval), stringsAsFactors=F)
names(cleanMeanDF)<-c("steps","interval")
plot(cleanMeanDF$interval,cleanMeanDF$steps, type="l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 
### Inputing missing values

Using the initial data frame, input mean values where NA's exists.

The folowing reporst the nember of missing values


```r
sum(!complete.cases(activityData))
```

```
## [1] 13318
```

Now we'll fill in those missing values with the mean value for the observation


```r
cleanMeans<-ddply(cleanActivityData,.(date,interval), summarise, mean = mean(steps))
for(i in 1:length(activityData$steps))
{
       if(is.na(activityData$steps[i])){
                if(length(cleanMeans$mean[cleanMeans$interval == activityData$interval[i]]) >0){
                        activityData$steps[i]<-cleanMeans$mean[cleanMeans$interval == activityData$interval[i]]
                }else{
                        activityData$steps[i]<-as.numeric(0)
                }        
       }
}
```
Now to recreate a histogram with the missing data added


```r
newCleanActivity<-activityData[which(activityData$steps !=0),]
newCleanMeans<-tapply(newCleanActivity$steps,newCleanActivity$date, mean, na.rm=T)
newCleanMedian<-tapply(newCleanActivity$steps,newCleanActivity$date, median, na.rm=T)
meansDF<-data.frame(as.numeric(newCleanMeans),dimnames(newCleanMeans))
names(meansDF)<-c("step_means", "date")
hist(meansDF$step_means,breaks=70)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
## Are there differences in activity patterns between weekdays and weekends?

Finally create a plot that shows the differences between weekdays and weekends


```r
activityDataFactor<-activityData
activityDataFactor$minutes<-as.factor(activityData$interval)
activityDataFactor$weekdays<-weekdays(as.Date(activityData$date))
activityDataFactor$weekdays<-gsub("Monday|Tuesday|Wednesday|Thursday|Friday","Weekday",activityDataFactor$weekdays)
activityDataFactor$weekdays<-gsub("Saturday|Sunday","Weekend",activityDataFactor$weekdays)

g<-ggplot(activityDataFactor, aes(interval,steps))
g+geom_line()+facet_grid(.~weekdays)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
