Reproducible Research - Peer Assessment 1
==============================================

####Naman Jain

09 Jan 2015

###Introduction

This document presents the results of peer assessments 1 of course Reproducible Research on coursera. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

This document presents the results of the Reproducible Research's Peer Assessment 1 in a report using a single R markdown document that can be processed by knitr and be transformed into an HTML file.

####Load required library

```{r}
library(ggplot2)
```

####Loading and preprocessing the data

**Set the working directory**

```{r,echo=TRUE}
setwd("C:/Coursera_Data_Science/Reproducible Research/Project-1")
```

**Load the required data**
```{r}
data <- read.csv("activity.csv",header=T,colClasses=c('integer','Date'))
```

Now let's check the data using str() method:
```{r}
str(data)
```

####What is mean total number of steps taken per day?

1. Histogram of the total number of steps taken each day ; ignoring missing values
```{r,echo=TRUE,fig.height=4}
rdata<-data[complete.cases(data),]
attach(rdata)      ## Attaching the data
head(rdata)
total.steps <- tapply(steps,date,sum)
head(total.steps)
## Plotting the histogram
qplot(total.steps,binwidth=1000,fill="Red",xlab="No. of steps per day",
      ylab="No. of times in a day(count)",main="Histogram of no. of steps take per day")
```

2. **Mean** and **Median** of total number of steps taken per day
```{r}
mean(total.steps)
median(total.steps)
```

####What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r,echo=TRUE,fig.height=4}
steps.interval <- aggregate(steps~interval,rdata,mean)
head(steps.interval)
attach(steps.interval)
g <- ggplot(steps.interval,aes(interval,steps)) 
g + geom_line(color="purple") + theme_bw() + xlab("5-minute Interval") + ylab("Average no. of steps taken") + ggtitle("Average daily activity pattern")
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
steps.interval[which.max(steps.interval$steps),]
```

####Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
sum(is.na(data))       ## Total no. of missing values
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 
Create a new dataset that is equal to the original dataset but with the missing data filled in
```{r}
## Imputing missing values by using mean for the 5-minute interval.
average_by_interval <- aggregate(steps~interval,data,mean)
head(average_by_interval)
impData <- data        ## Creating a new dataset 'impData'
for(i in 1:nrow(impData))
{
  if (is.na(impData$steps[i])) {
    impData$steps[i] <- average_by_interval[which(impData$interval[i] == average_by_interval$interval),]$steps
      } 
}
head(impData)
tail(impData)

```
Proof that all missing values have been filled in.
```{r}
sum(!complete.cases(impData))
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r,echo=TRUE,fig.height=4}
attach(impData)
filled.data <- tapply(steps,date,sum) 
head(filled.data)
## Plotting the histogram
qplot(filled.data,binwidth=1000,fill='Red',xlab="No. of steps per day",
      ylab="No. of times in a day(count)",main="Histogram of no. of steps take per day")
```
**Mean** and **Median** of total number of steps taken per day with the imputed data.
```{r}
mean(filled.data)
median(filled.data)
```
Yes, these values do differ slightly.

Before filling the data:

Mean : 10766.189


Median: 10765

After filling the data:

Mean : 10766.189


Median: 10766.189
We see that the values after filling the data mean and median are equal.

####Are there differences in activity patterns between weekdays and weekends?
```{r}
head(impData)
str(impData)
daytype <- function(date){
  if(weekdays(date) %in% c("Saturday","Sunday")){
    "Weekdend"
  } else{
    "weekday"
  }
}
  impData$daytype <- as.factor(sapply(impData$date,daytype)) 
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r,echo=TRUE,fig.height=4}
new.averages <- aggregate(steps~interval+daytype,impData,mean)
head(new.averages)
qplot(interval,steps,data=new.averages,geom="line",color=daytype,facets=daytype~.,xlab="Interval",
      ylab="Number of Steps")
```
We can see at the graph above that activity on the weekday has the greatest peak from all steps intervals. But, we can see too that weekends activities has more peaks over a hundred than weekday. This could be due to the fact that activities on weekdays mostly follow a work related routine, where we find some more intensity activity in little a free time that the employ can made some sport. In the other hand, at weekend we can see better distribution of effort along the time.
