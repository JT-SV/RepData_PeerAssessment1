# Reproducible Research Peer Assessment 1
JT  
June 25, 2017  

## Loading and preprocessing the data


```r
setwd("~/ds/ReproducibleResearch/Week2/RepData_PeerAssessment1")
data<-read.csv(unz("activity.zip","activity.csv"),stringsAsFactors = FALSE)
d_clean<-data[!is.na(data$steps),]
```
## What is mean total number of steps taken per day?
"For this part of the assignment, you can ignore the missing values in the dataset."

```r
# Calculate the total number of steps taken per day - aggregate by day (i.e. date)
d_date<-aggregate(d_clean$steps,list(d_clean$date),mean)
names(d_date)<-c("date","steps")
# Plot
hist(d_date$steps,main="Average Steps Per Day",xlab="Steps")
```

![](PA1_template_files/figure-html/steps_per_day-1.png)<!-- -->

Summary measures for the average number of steps per day:

```r
m<-mean(d_date$steps)
md<-median(d_date$steps)
```

The mean number of average steps per day is 37.3825996.  
The median number of average steps per day is 37.3784722.  

## What is the average daily activity pattern?  


```r
# Aggregate across days by interval, then plot the time series
d_int<-aggregate(d_clean$steps,list(d_clean$interval),mean)
names(d_int)<-c("interval","steps")
with(d_int,plot(interval,steps,type="l",xlab="5-minute Interval",ylab="Steps",
                main="Average Number of Steps per Interval"))
```

![](PA1_template_files/figure-html/average_daily_pattern-1.png)<!-- -->

So, which interval contains the maximum number of steps?

```r
max_index<-which.max(d_int$steps)
max_interval<-d_int[max_index,"interval"]
max_steps<-d_int[max_index,"steps"]
```

Interval 835 contains the maximum average number of steps: 206.1698113. I.e the 5-minute interval starting at 8:35 in the morning.

## Imputing missing values


```r
d_na<-data[is.na(data$steps),]
n_na<-dim(d_na)[1]
```

There are 2304 rows with NA in the data set.

Impute the missing step values using the average steps for the interval, performing some checks as well:


```r
# For each row d in d_na, set steps to average steps for the interval (stored in d_int)
d_na["steps"]<- d_int$steps[match(d_na$interval,d_int$interval)]
# Re-construct the data set by merging clean with imputed 
d_complete<-rbind(d_clean,d_na)
# Reorder
d_complete<-d_complete[order(as.numeric(row.names(d_complete))),]
```


```r
# Confirm it all looks good. First the original data
head(data,n=3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```

```r
# Then the aggregated-by interval data which is used to fill in the NAs
d_int[d_int$interval %in% c(0,5,10),]
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
```

```r
# Here is the result in the "complete" dataset
head(d_complete,n=3)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
```

```r
# Now check one data point in original vs "complete" for un-imputed data - looks good
check<-rbind(data[800,],d_complete[800,])
row.names(check)<-c("Original","Imputed")
check
```

```
##          steps       date interval
## Original    26 2012-10-03     1835
## Imputed     26 2012-10-03     1835
```

Now the summary measures and the  histogram

```r
#Aggregate the "complete" dataset by day (date), then plot a histogram
d_complete_date<-aggregate(d_complete$steps,list(d_complete$date),mean)
names(d_complete_date)<-c("date","steps")
hist(d_complete_date$steps,
     main="Average steps per day (Missing Data Imputed)",xlab="Steps")
```

![](PA1_template_files/figure-html/complete_hist-1.png)<!-- -->


```r
m<-mean(d_complete_date$steps)
md<-median(d_complete_date$steps)
```

The mean number of average steps per day is 37.3825996.  
The median number of average steps per day is 37.3825996.  

The means are identical in the unimputed and imputed datasets, not so the median, which has changed a tiny bit.  I.e. the impact of imputing missing data seems negligible.

As a double check, let's look at the data underlying the calculated means for original vs imputed.  They sums of steps are different and the number of days in the calculation are different; however, the calculated means are equal to four decimal places:

```r
check<-data.frame(matrix(c(sum(d_date$steps),sum(d_complete_date$steps),
                       dim(d_date)[1],dim(d_complete_date)[1],
                       sum(d_date$steps)/dim(d_date)[1],
                       sum(d_complete_date$steps)/dim(d_complete_date)[1]),
                      c(2,2)))
names(check)<-c("Sum of steps","Number of days","Mean")
row.names(check)<-c("Original data set","Imputed data set")
check
```

```
##                   Sum of steps Number of days    Mean
## Original data set     1981.278             53 37.3826
## Imputed data set      2280.339             61 37.3826
```

## Are there differences in activity patterns between weekdays and weekends?

It appears so... The number of steps is higher throughout the day during the weekends.  The opposite is true in the mornings.

```r
# Prepare for a quick panel plot
library(lattice)
# Define a weekday vs. weekend function and a new column in the data
wd<-function(d){ d<-weekdays(as.Date(d))
    if (d=="Saturday"|d=="Sunday"){"Weekend"}else{"Weekday"}}
d_complete["weekday"]<-sapply(d_complete$date,wd)

# Aggregate by weekday/weekend and interval.  Plot.
d_final<-aggregate(d_complete$steps,list(d_complete$weekday,d_complete$interval),mean)
names(d_final)<-c("weekday","interval","steps")
xyplot(steps~interval|weekday,type="l",layout=c(1,2),ylab="Steps",xlab="Interval",
       data=d_final)
```

![](PA1_template_files/figure-html/weekends-1.png)<!-- -->
