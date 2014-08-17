Steps Data Tracking
========================================================

Imprting data into R:


```r
activity_data <- read.csv("activity.csv")
activity_dates <- levels(activity_data$dates)
activity_data_complete <- na.omit(activity_data)
activity_dates_complete <- levels(activity_data_complete$date)
```


```r
summary(activity_data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```


Plotting a histogram of total steps:


```r
nonempties <- c()
for(dates in activity_dates_complete){
    one_day_data <- subset(activity_data_complete, date == dates)
    if(nrow(one_day_data) != 0){
        nonempties <- c(nonempties, dates)
    }
}

total_steps <- c()

for(dates in activity_dates_complete){
    one_day_data <- subset(activity_data_complete, date == dates)
    if(nrow(one_day_data) != 0){
      total_steps <- c(total_steps, sum(one_day_data[[1]]))
    }
}

hist(total_steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
summary(total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```
We see that both **mean** and **median** are 10800. 


Now we plot the total numebr of steps as a function of the five minute interval:


```r
one_day <- subset(activity_data_complete, activity_data_complete$date == "2012-10-02")
five_minute_intervals <- one_day[[3]]

total_steps_interval <- c()
for(cintervals in five_minute_intervals){
    interval_data <- subset(activity_data_complete, interval == cintervals)
        total_steps_interval <- c(total_steps_interval, sum(interval_data[[1]]))
}

plot(five_minute_intervals, total_steps_interval, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
max(total_steps_interval)
```

```
## [1] 10927
```

```r
five_minute_intervals[which.max(total_steps_interval)]
```

```
## [1] 835
```

We also see that the maximum total number of steps occurs for the 835 five minute interval.

Calculating the nuber of rows with missing data:


```r
nrow(activity_data) - nrow(activity_data_complete)
```

```
## [1] 2304
```

Subsituting all occurances of NA with the mean for that five minute interval:


```r
for (i in 1:nrow(activity_data)){
     if(is.na(activity_data[i,1])){
         activity_data[i,1] <- as.integer(total_steps_interval[match(activity_data[i,3], five_minute_intervals)]/53)
     }
 }

summary(activity_data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.3   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 27.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##                  (Other)   :15840
```

Repeating the histogram plot of the total steps with the new data replacing the missing values:


```r
nonempties <- c()
for(dates in activity_dates_complete){
    one_day_data <- subset(activity_data_complete, date == dates)
    if(nrow(one_day_data) != 0){
        nonempties <- c(nonempties, dates)
    }
}

total_steps <- c()

for(dates in activity_dates_complete){
    one_day_data <- subset(activity_data_complete, date == dates)
    if(nrow(one_day_data) != 0){
      total_steps <- c(total_steps, sum(one_day_data[[1]]))
    }
}

hist(total_steps)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


```r
summary(total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```

Splitting the data into the weekend and weekday data frames:


```r
activity_data$day <- unname(weekdays(as.Date(activity_data$date)))
workdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
weekend <- c("Sunday", "Saturday")
weekend_data <- subset(activity_data, activity_data$day %in% weekend)
weekday_data <- subset(activity_data, activity_data$day %in% workdays)
```

Plotting the weekend and the weekday intervals:

```r
total_steps_interval_weekend <- c()
for(cintervals in five_minute_intervals){
    interval_data <- subset(weekend_data, interval == cintervals)
        total_steps_interval_weekend <- c(total_steps_interval_weekend, sum(interval_data[[1]]))
}

total_steps_interval_weekday <- c()
for(cintervals in five_minute_intervals){
    interval_data <- subset(weekday_data, interval == cintervals)
        total_steps_interval_weekday <- c(total_steps_interval_weekday, sum(interval_data[[1]]))
}

par(mfrow=c(2,1))
plot(five_minute_intervals, total_steps_interval_weekend, type="l")
plot(five_minute_intervals, total_steps_interval_weekday, type="l")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
