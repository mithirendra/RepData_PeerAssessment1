---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

The following project is a Course Project 1 assignment for the Reproducible Research Module by John Hopkins University. This assignment will be described in multiple parts. We will need to write a report that answers the questions detailed below. Ultimately, we will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

## Loading and preprocessing the data

First, the data is read from a CSV file called "activity.csv". Then date column is processed to a Date. A new column is created to determine the actual day of the week.  


```r
# Read data from file
df <- read.csv("activity.csv")

# Preprocess date column to Date format
df$date <- as.Date(df$date, "%Y-%m-%d")

# Determine the actual day of the week
df$day <- weekdays(df$date)
```

## What is mean total number of steps taken per day?

2 steps will be done here:  
a. Make a histogram of the total number of steps taken each day  
b. Calculate and report the mean and median total number of steps taken per day  


```r
library(dplyr)

# Group data by date and total steps
df1 <- df %>% group_by(date) %>% summarise(steps = sum(steps))

# Make a histogram for total number of steps
hist(df1$steps,
     breaks = 20,
     xlab = "Day",
     ylab = "Total number of steps taken per day",
     main = "Total number of steps taken each day",
     col = "green")
```

![plot of chunk mean-steps](figure/mean-steps-1.png)

```r
# Find the mean and median for total number of steps
mean(df1$steps, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(df1$steps, na.rm = T)
```

```
## [1] 10765
```

Before the imputation of missing values, it can be seen that the mean and median values have a small difference between each other.


## What is the average daily activity pattern?

In this section, a time series plot of 5-min intervals vs average number of steps taken will be plotted. From there, we will also find which 5-min interval has the highest maximum number of steps. 


```r
df2 <- na.omit(df)

# Plot time series of 5-min interval vs. average number of steps taken
df3 <- df2 %>% group_by(interval) %>% summarise(average = mean(steps))
summary(df3)
```

```
##     interval         average       
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
plot(df3$interval, df3$average, 
     type = "l",
     xlab = "5-min intervals",
     ylab = "Average steps taken per interval",
     main = "5-min intervals vs average number of steps taken",
     col = "red")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
# Find which 5-min interval has the highest/maximum number of steps
df3$interval[which.max(df3$average)]
```

```
## [1] 835
```

In the case above, it shows that the 835 interval has the highest number of steps of 206.  


## Imputing missing values

There are a number of days/intervals where there are missing values. The presence of missing days may introduce bias into some calculations or summaries of the data. We will do a few things here to impute the missing values:  
1. Calculate the number of missing values in the dataset - summary shows missing values are only found in steps. There are a stop of 2304 NAs in steps.
2. Fill missing values with mean of 5-min interval.
3. Create a new dataset with missing data filled.
4. Make a histogram of steps taken each day.
5. Calculate the mean and median of total number of steps taken per day


```r
library(dplyr)
summary(df)
```

```
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
```

```r
# Calculate number of missing values in the dataset
sum(is.na(df$steps))
```

```
## [1] 2304
```

```r
# Fill in missing values with mean of 5-min interval
df4 <- df
for (i in 1:nrow(df4)) {
    if (is.na(df4$steps[i])) {
        df4$steps[i] <- df3$average[df3$interval == df4$interval[i]]
    }
}

# Checking for NAs in dataset
summary(df4)
```

```
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
df5 <- df4 %>% group_by(date) %>% summarise(steps = sum(steps))

# Make a histogram for total number of steps
hist(df5$steps,
     breaks = 20,
     xlab = "Day",
     ylab = "Total number of steps taken per day",
     main = "Total number of steps taken each day",
     col = "orange"
)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
# Find the mean and median for total number of steps
mean(df5$steps)
```

```
## [1] 10766.19
```

```r
median(df5$steps)
```

```
## [1] 10766.19
```

After the imputation of missing values, it can be seen that the mean and median values are very close and similar to each other in values.


## Are there differences in activity patterns between weekdays and weekends?

This sections tries to see if there is any difference in activity patterns between weekdays and weekends.
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l" type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice) 
df4$daytype <- ifelse((df4$day == "Saturday" | df4$day == "Sunday"), "Weekend", "Weekday")
df6 <- df4 %>% group_by(interval, daytype) %>% summarise(average = mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
xyplot(average ~ interval | daytype, 
       data = df6, 
       type = "l", 
       grid=T, 
       layout=c(1,2),
       col = "blue",
       xlab = "5-min intervals",
       ylab = "Average steps taken per interval",
       main = "5-min intervals vs average number of steps taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

From the graph above, it can be seen that there is definitely a difference in activity pattenrs on weekdays and weekends.
