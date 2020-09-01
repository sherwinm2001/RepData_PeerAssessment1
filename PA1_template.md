Steps for performing the assignment
-----------------------------------

1.Code for reading in the dataset and/or processing the data 2.Histogram
of the total number of steps taken each day 3.Mean and median number of
steps taken each day 4.Time series plot of the average number of steps
taken 5.The 5-minute interval that, on average, contains the maximum
number of steps 6.Code to describe and show a strategy for imputing
missing data 7.Histogram of the total number of steps taken each day
after missing values are imputed 8.Panel plot comparing the average
number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots,
etc.) in the report

\#\#Step 1: Code for reading in the dataset and/or processing the data

      activity<-read.csv("activity.csv")
      str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

      activity$date<-as.Date(activity$date,"%Y-%m-%d")

\#\#Step 2: Histogram of the total number of steps taken each day

      step_by_day<-aggregate(steps~date,activity,sum)
      hist(step_by_day$steps, main = "Steps taken per Day", xlab = "Steps", col="red")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

      dev.copy(png,"plot1.png",480,480)

    ## png 
    ##   3

      dev.off()

    ## png 
    ##   2

\#\#Step 3: Mean and median number of steps taken each day

      mean_steps_day<-mean(step_by_day$steps)
      median_steps_day<-median(step_by_day$steps)

What is mean total number of steps taken per day?
=================================================

The mean of the total number of steps taken per day is *10766.19* and
the median of the total number of steps taken per day is *10765*.

\#\#Step 4: Time series plot of the average number of steps taken

      step_by_interval<-aggregate(steps~interval,activity,mean)
      plot(step_by_interval$interval, step_by_interval$steps, type="l", main = "Average steps over number of days", xlab = "Interval", ylab="Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

      dev.copy(png,"plot2.png",480,480)

    ## png 
    ##   3

      dev.off()

    ## png 
    ##   2

\#\#Step 5: The 5-minute interval that, on average, contains the maximum
number of steps

      max_steps_row <- which.max(step_by_interval$steps)
      step_by_interval[max_steps_row,]

    ##     interval    steps
    ## 104      835 206.1698

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
=============================================================================================================

The interval *835* contains the highest number of steps i.e. *206.167*.

\#\#Step 6: Code to describe and show a strategy for imputing missing
data There are multiple strategies to deal with multiple value
imputations. The common strategies include: 1. Constant value
imputations 2. Regression model value imputations 3. Mean/mode value
substitutions For the purpose of simplicity, in this question, I will
use the mean/mode value substitution strategy to impute missing values.
That is, using the mean values to substitute out the missing values in
the original data set Before doing any sort of imputation, it is helpful
to understand what are the distributions of missing values by date and
interval

    activity_imputed<-activity
    for(i in 1:nrow(activity_imputed)){
      if(is.na(activity_imputed$steps[i])){
        interv<-activity_imputed$interval[i]
        activity_imputed$steps[i]<-step_by_interval$steps[step_by_interval$interval==interv]
      }
    }

Step 7: Histogram of the total number of steps taken each day after missing values are imputed
----------------------------------------------------------------------------------------------

      step_by_day<-aggregate(steps~date,activity_imputed,sum)
      hist(step_by_day$steps, main = "Steps taken per Day", xlab = "Steps", col="red")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png)

      dev.copy(png,"plot3.png",480,480)

    ## png 
    ##   3

      dev.off()

    ## png 
    ##   2

Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
===============================================================================================================================================================================================================================================================

      mean_steps_day<-mean(step_by_day$steps)
      median_steps_day<-median(step_by_day$steps)

Yes, these values do differ slightly from the first part of the
assignment. There is a slight impact on imputing the missing data.

Step 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
-----------------------------------------------------------------------------------------------------------------

      activity_imputed["day"]=NA
      weekday<-c("Monday","Tuesday","Wednesday","Thursday","Friday")
      weekend<-c("Saturday","Sunday")
      for (i in 1:nrow(activity_imputed)){
        if(weekdays(activity_imputed$date[i]) %in% weekend){
          activity_imputed$day<-"weekend"
        }
        else{
          activity_imputed$day[i]<-"weekday"
        }
      }
      step_by_day<-aggregate(steps~interval+day,activity_imputed,mean)
      library(lattice)
      xyplot(step_by_day$steps ~ step_by_day$interval|step_by_day$day, main="Average 
             Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

      dev.copy(png,"plot4.png",480,480)

    ## png 
    ##   3

      dev.off()

    ## png 
    ##   2

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

Seeing from the graphs we are able to notice slight changes in the
average number of steps over weekdays and weekends.
