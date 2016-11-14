Loading and preprocessing the data
----------------------------------

    # Load the data.

    activity <- read.csv("activity.csv", na.strings = "NA")

    # Process/transform the data (if necessary) into a format suitable for your analysis.

    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    activity$date <- ymd(activity$date)

    head(activity); tail(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

What is mean total number of steps taken per day?
-------------------------------------------------

    attach(activity)

    # Calculate the total number of steps taken per day.

    total_steps <- tapply(steps, date, sum, na.rm = TRUE)

    total_steps

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
    ##          0        126      11352      12116      13294      15420 
    ## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
    ##      11015          0      12811       9900      10304      17382 
    ## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
    ##      12426      15098      10139      15084      13452      10056 
    ## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
    ##      11829      10395       8821      13460       8918       8355 
    ## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
    ##       2492       6778      10119      11458       5018       9819 
    ## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
    ##      15414          0      10600      10571          0      10439 
    ## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
    ##       8334      12883       3219          0          0      12608 
    ## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
    ##      10765       7336          0         41       5441      14339 
    ## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
    ##      15110       8841       4472      12787      20427      21194 
    ## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
    ##      14478      11834      11162      13646      10183       7047 
    ## 2012-11-30 
    ##          0

    # Make a histogram of the total number of steps taken each day.

    hist(total_steps, main = "Histogram of the Total Number of Steps", xlab = "Total number of steps taken each day", las = 1)

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

    # Calculate and report the mean and the median of the total number of steps taken per day.

    summary(total_steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    6778   10400    9354   12810   21190

What is the average daily activity pattern?
-------------------------------------------

    # Make a time series plot of the 5-minute interval and the average number of steps taken.

    average_steps <- round(tapply(steps, interval, mean, na.rm = TRUE), digits = 2)

    interval.f <- activity$interval

    interval.f <- unique(interval.f)

    plot(interval.f, average_steps, type = "l", main = "Time Series Plot", xlab = "5-minute interval", ylab = "Average number of steps", las = 1)

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

    # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    max(average_steps)

    ## [1] 206.17

    both <- data.frame(interval.f, average_steps)

    both[which.max(both$average_steps), ]

    ##     interval.f average_steps
    ## 835        835        206.17

Imputing missing values
-----------------------

    # Calculate and report the total number of missing values in the dataset.

    sum(is.na(activity))

    ## [1] 2304

    # Devise a strategy for filling in all of the missing values in the dataset.

    sum(is.na(steps))

    ## [1] 2304

    detach(activity)

    activity[is.na(activity)] <- round(average_steps, 0)

    # Create a new dataset that is equal to the original dataset but with the missing data filled in.

    activity2 <- activity

    str(activity2)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  2 0 0 0 0 2 1 1 0 1 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    head(activity2); tail(activity2)

    ##   steps       date interval
    ## 1     2 2012-10-01        0
    ## 2     0 2012-10-01        5
    ## 3     0 2012-10-01       10
    ## 4     0 2012-10-01       15
    ## 5     0 2012-10-01       20
    ## 6     2 2012-10-01       25

    ##       steps       date interval
    ## 17563     3 2012-11-30     2330
    ## 17564     5 2012-11-30     2335
    ## 17565     3 2012-11-30     2340
    ## 17566     1 2012-11-30     2345
    ## 17567     0 2012-11-30     2350
    ## 17568     1 2012-11-30     2355

    # Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.

    attach(activity2)

    total_steps2 <- tapply(steps, date, sum)

    hist(total_steps2, main = "Histogram of the New Total Number of Steps", xlab = "New Total number of steps taken each day", las = 1)

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

    summary(total_steps2)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10760   10770   12810   21190

    detach(activity2)

**Do these values differ from the estimates from the first part of the
assignment?**

The mean increased a little bit (9354 vs 10770) because I used the
average number of steps in order to replace the missing data; on the
other hand, the median almost remained the same (10400 vs 10760).

**What is the impact of imputing missing data on the estimates of the
total daily number of steps?**

The impact was noticeable for the mean.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    # Create a new factor variable in the dataset  with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

    activity2$date <- weekdays(activity2$date)

    activity2$date2[activity2$date == "Monday"] <- "weekday"

    activity2$date2[activity2$date == "Tuesday"] <- "weekday"

    activity2$date2[activity2$date == "Wednesday"] <- "weekday"

    activity2$date2[activity2$date == "Thursday"] <- "weekday"

    activity2$date2[activity2$date == "Friday"] <- "weekday"

    activity2$date2[activity2$date == "Saturday"] <- "weekend"
        
    activity2$date2[activity2$date == "Sunday"] <- "weekend"

    str(activity2$date2)

    ##  chr [1:17568] "weekday" "weekday" "weekday" "weekday" ...

    activity2$date2 <- factor(activity2$date2)

    str(activity2$date2)

    ##  Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...

    # Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday or weekend days.

    weekday <- activity2[activity2$date2 == "weekday", ]

    weekday_average_steps <- round(tapply(weekday$steps, weekday$interval, mean), digits = 2)

    weekend <- activity2[activity2$date2 == "weekend", ]

    weekend_average_steps <- round(tapply(weekend$steps, weekend$interval, mean), digits = 2)

    par(mfrow = c(2,1), oma = c(1, 1, 2, 1))

    plot(interval.f, weekday_average_steps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps", main = "weekdays", las = 1)

    plot(interval.f, weekend_average_steps, type = "l", xlab = "5-minute interval", ylab = "Average number of steps", main = "weekends", las = 1)

    mtext("Time Series Plot by Weekdays and Weekends", outer = TRUE, cex = 1.5)

<img src="PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

**Conclusion:** Yes, there are some differences in activity patterns
between weekdays and weekends.
