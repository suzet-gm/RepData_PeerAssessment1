# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data



>Show any code that is needed to

>1. Load the data (i.e. read.csv())
 
 
```r
activity<-read.csv("activity.csv",header=TRUE,na.string=c("NA"," ","","   .","  /  /"))
```

Check a quick summary of the activity dataset by using the str() function and use the head() function to see the top preview of the data.


```r

str(activity)
head(activity)
```

    ```
    ##'data.frame':   17568 obs. of  3 variables:
    ## $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ## $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ## $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ```

    ```
        steps       date interval
    ##1    NA 2012-10-01        0
    ##2    NA 2012-10-01        5
    ##3    NA 2012-10-01       10
    ##4    NA 2012-10-01       15
    ##5    NA 2012-10-01       20
    ##6    NA 2012-10-01       25

    ```


>2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum)

```


A preview of the top of the dataset is as follows:

```r
head(steps_per_day)

```

    ```
              date steps
    ##1 2012-10-02   126
    ##2 2012-10-03 11352
    ##3 2012-10-04 12116
    ##4 2012-10-05 13294
    ##5 2012-10-06 15420
    ##6 2012-10-07 11015

    ```


##What is mean total number of steps taken per day?

>For this part of the assignment, you can ignore the missing values in the dataset.

>1.	Make a histogram of the total number of steps taken each day

```r
with(steps_per_day, {
      par(oma=c(2,0,0,0), mar=c(7,7,3,0),mgp=c(5.5,0.75,0),las=2)
      barplot(
      height=steps,
      main="Graph of Total Steps Per Day",
      xlab="Dates",
      ylab="Number of Steps Per Day",
      col="cyan",
      names.arg=date,
      space=c(0)
    )
})
```

  ![plot of chunk figure1](figure/figure1.png) 



>2.	Calculate and report the mean and median total number of steps taken per day:

Mean

```r
mean_steps<-mean(steps_per_day$steps, na.rm=TRUE)
mean_steps
```
      
      ```
      ## [1] 10766
      ```

Median

```r
median_steps<-median(steps_per_day$steps, na.rm=TRUE)
median_steps
```

      ```
      ## [1] 10765
      ```


##What is the average daily activity pattern?


>1.	Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


Same to the previous section, these are the following steps to reach this:


A. Generate the mean (average) number of steps taken (ignoring NA values) for each 5-minute interval, itself averaged across all days.

```r
steps_interval <- aggregate(steps ~ interval, data = activity, FUN = mean)

colnames(steps_interval) <- c("Interval", "AvrgStepsAcrossYrs")
```


B.Now we can check a preview of this new constructed data:


```r
head(steps_interval)
```


    ```
        Interval AvrgStepsAcrossYrs
    ##1        0            1.71698
    ##2        5            0.33962
    ##3       10            0.13208
    ##4       15            0.15094
    ##5       20            0.07547
    ##6       25            2.09434

    ```

C. Now we can create a Time-Series plot from the above dataset:


```r
with(steps_interval, {
    plot(
      x=Interval,
      y=AvrgStepsAcrossYrs,
      type="l",
      main="Time-Series of Interfal and Average Steps Across Years",
      xlab="5 Minute Interval",
      ylab="Average Steps and Average Across All Days"
      )
})
```

  ![plot of chung figure2](figure/figure2.png)




>2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
intervalMax <- steps_interval[steps_interval$AvrgStepsAcrossYrs==max(steps_interval$AvrgStepsAcrossYrs),]
```

      ```
            Interval AvrgStepsAcrossYrs
      ##104      835              206.2
      
      ```
From the output above we can see that the interval between **835** and  **840** minutes has the maximum number of steps.




##Imputing missing values


Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


>1.	Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missings <- nrow(subset(activity, is.na(activity$steps)))
missings
```

      ```
      ##[1] 2304
      ```
      

>2.	Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
missings <- data.frame(activity$steps)
missings[is.na(missings),] <- ceiling(tapply(X=activity$steps,INDEX=activity$interval,FUN=mean,na.rm=TRUE))
newdata <- cbind(missings, activity[,2:3])
colnames(newdata) <- c("steps", "Date", "Interval")
```

Now preview the top of the newdata dataset:

```r
head(newdata)
```

      ```
        steps       Date Interval
      ##1     2 2012-10-01        0
      ##2     1 2012-10-01        5
      ##3     1 2012-10-01       10
      ##4     1 2012-10-01       15
      ##5     1 2012-10-01       20
      ##6     3 2012-10-01       25
      ```


>3.	Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_steps_per_day <- aggregate(newdata$steps, list(newdata$Date), sum)
```

Now preview this new dataset:

```r
head(new_steps_per_day)
```
      ```
           Group.1     x
    ##1 2012-10-01 10909
    ##2 2012-10-02   126
    ##3 2012-10-03 11352
    ##4 2012-10-04 12116
    ##5 2012-10-05 13294
    ##6 2012-10-06 15420
    ```

Rename column x to steps_no_na and column Group.1 to dates:


```r
steps_no_na<-(new_steps_per_day$x)

dates<-(new_steps_per_day$Group.1)
```




>4.	Make a histogram of the total number of steps taken each day and Calculate and report the meanand median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
with(new_steps_per_day, {
      par(oma=c(2,0,0,0), mar=c(7,7,3,0),mgp=c(5.5,0.75,0),las=2)
      barplot(
      height=steps_no_na,
      main="Graph of Total Steps Per Day",
      xlab="Dates",
      ylab="Number of Steps Per Day",
      col="cyan",
      names.arg=dates,
      space=c(0)
    )
})
```


![plot of chunk figure3](figure/figure3.png)



Now check the new values for the mean and median for these new dataset


```r
mean_new_steps<-mean(steps_no_na)
mean_new_steps
```
      ```
      [1] 10785
      ```
      

```r
median_new_steps<-median(steps_no_na)
median_new_steps
```

      ```
      [1] 10909
      ```
      

We can see now that adding the missing values to the original data has caused both the mean and median values to increase.


  1. Mean:
  
      10766 to 10785
      
  2. Median:
  
      10765 to 10909
      
      

##Are there differences in activity patterns between weekdays and weekends?


For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


>1.	Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
type_of_day <- data.frame(sapply(X = newdata$Date, FUN = function(day) {
    if (weekdays(as.Date(day)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", 
        "Friday")) {
        day <- "weekday"
    } else {
        day <- "weekend"
    }
}))

newdata_with_typeofday <- cbind(newdata, type_of_day)

colnames(newdata_with_typeofday) <- c("Steps", "Date", "Interval", "Type_of_Day")
```

Now preview this new data set:

```r
head(newdata_with_typeofday)
```

      ```
          Steps       Date Interval Type_of_Day
      ##1     2 2012-10-01        0     weekday
      ##2     1 2012-10-01        5     weekday
      ##3     1 2012-10-01       10     weekday
      ##4     1 2012-10-01       15     weekday
      ##5     1 2012-10-01       20     weekday
      ##6     3 2012-10-01       25     weekday
      ```

Now we can separate the data into weekday or weekend and the mean number of steps  taken each 5 minute interval across all weekdays and weekends.


dayTypeIntervalSteps <- aggregate(
    data=newdata_with_typeofday,
    Steps ~ Type_of_Day + Interval,
    FUN=mean
)

Now preview this new dataset with the new variable Type of Day

```r
head(dayTypeIntervalSteps)
```

      ```
          Type_of_Day Interval  Steps
      ##1     weekday        0 2.2889
      ##2     weekend        0 0.2500
      ##3     weekday        5 0.5333
      ##4     weekend        5 0.1250
      ##5     weekday       10 0.2889
      ##6     weekend       10 0.1250
      ```


>2.	Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```r
library("lattice")

xyplot(
    type="l",
    data=dayTypeIntervalSteps,
    Steps ~ Interval | Type_of_Day,
    xlab="Interval",
    ylab="The Number of Steps",
    layout=c(1,2)
)
```

![plot of chunk figure4](figure/figure4.png)








