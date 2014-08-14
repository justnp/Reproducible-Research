Reproducible Research: Peer Assessment 1
========================================
created by Kumar on August 14, 2014

### Basic settings

```r
echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers
setwd("C:/Rwd-coursera/Course-Reproducible Research/PeerAssessment1") # Set work directory
library(knitr)
opts_chunk$set(fig.path ="figure/", tidy=TRUE) # Set folder to save plots
```
Introduction
============
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken



The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


##### Below libraries packages are required for this assignment, packages can installed using command > install.packages("_packagename_")


* plyr
* ggplot2
* knitr

### Loading and preprocessing the data


* Load the activity data


```r
activityRwDt <- read.csv("./Data/activity.csv")  # activity raw data
```
* View few records from the raw activity data

```r
kable(head(activityRwDt))
```

```
## 
## 
## | steps|date       | interval|
## |-----:|:----------|--------:|
## |    NA|2012-10-01 |        0|
## |    NA|2012-10-01 |        5|
## |    NA|2012-10-01 |       10|
## |    NA|2012-10-01 |       15|
## |    NA|2012-10-01 |       20|
## |    NA|2012-10-01 |       25|
```

 *  Perform summary statistics  on activity raw data (activtyRWDt) to understand  content of  variables steps, date and interval. And you would see their are missing values (NAs) in variable **steps**

```r
summary(activityRwDt)
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

* Process the data i.e. transform the date variable to data class

```r
# Make a copy activity raw data for data cleansing and analysis purposes
activityData <- activityRwDt
activityData$date <- as.Date(activityData$date)
```

### What is mean total number of steps taken per day?

##### 1. Make a histogram of the total number of steps taken each day

```r
# Remove NA's from activityData
activitynoNA <- na.omit(activityData)
activitynoNA$yearmon <- format(activitynoNA$date, "%B %Y")

# load library ggplot2
library(ggplot2)
ggplot(data = activitynoNA, aes(x = date, y = steps, fill = yearmon, width = 0.8)) + 
    geom_bar(stat = "identity", position = position_dodge()) + guides(fill = F) + 
    ggtitle("Histogram of Total Number of Steps Taken Each Day") + ylab("Total number of steps") + 
    xlab("Date") + theme(legend.position = "none") + facet_grid(. ~ yearmon, 
    scales = "free", space = "free")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

##### 2. Calculate and report the mean and median total number of steps taken per day

```r
# load library plyr
library(plyr)
# Calculate mean steps taken per day
meanSteps <- round(mean((ddply(activitynoNA, ~date, summarise, sum = sum(steps))$sum)))
# Calculate median steps taken per day
medSteps <- median((ddply(activitynoNA, ~date, summarise, sum = sum(steps))$sum))
```
 * **Mean** steps taken per day is  **10766**

 * **Median** steps taken per day is  **10765**

### What is the average daily activity pattern?

##### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgSteps <- ddply(activitynoNA, ~interval, summarise, meanOfSteps = mean(steps))
ggplot(avgSteps, aes(interval, meanOfSteps)) + geom_line(color = "#0066CC", 
    size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", 
    x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# Find maxium of steps from avgSteps dataset
maxStep <- max(avgSteps$meanOfSteps)
print(maxStep)
```

```
## [1] 206.2
```

```r
# Find the line containing maxStep and find the interval for this line
maxInterval <- avgSteps[avgSteps$meanOfSteps == maxStep, ]$interval

print(maxInterval)
```

```
## [1] 835
```
###### The maximum number of steps (on average across all the days) is **206.2** and it is contained in interval 835

### Imputing missing values

##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
# Total Number of missing valuses in activity dataset is:
sum(is.na(activityRwDt))
```

```
## [1] 2304
```
##### 2. Devise a strategy for filling in all of the missing values in the dataset.
 * The strategy used to fill each missing(NA) values is the mean of that 5-minute interval
 
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in _(with above strategy)_ .
 

```r
# Create a new dataset
newactivity <- activityRwDt
# Find missing values i.e NAs and fill it from avgSteps$meanOfSteps column
# (steps) where newactivity$interval == avgSteps$meanOfSteps
for (i in 1:nrow(newactivity)) {
    if (is.na(newactivity$steps[i])) {
        newactivity$steps[i] <- avgSteps[which(newactivity$interval[i] == avgSteps$interval), 
            ]$meanOfSteps
    }
}
```
 
 * Perform summary statistics  on newactivity data (newactivty) and compare with statistics of activity raw data and you would see that there are no **NAs** in new dataset

```r
summary(newactivity)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 27.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##                  (Other)   :15840
```

##### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
newactivity$date <- as.Date(newactivity$date)
newactivity$yearmon <- format(newactivity$date, "%B %Y")

ggplot(data = newactivity, aes(x = date, y = steps, fill = yearmon, width = 0.8)) + 
    geom_bar(stat = "identity", position = position_dodge()) + guides(fill = F) + 
    ggtitle("Histogram of Total Number of Steps Taken Each Day \n                          (no missing steps)") + 
    ylab("Total number of steps") + xlab("Date") + theme(legend.position = "none") + 
    facet_grid(. ~ yearmon, scales = "free", space = "free")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

 * What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Calculate mean total number of steps taken per day on new dataset
mean((ddply(newactivity, ~date, summarise, sum = sum(steps))$sum))
```

```
## [1] 10766
```

```r
# Calculate median total number of steps taken per day on new dataset
median((ddply(newactivity, ~date, summarise, sum = sum(steps))$sum))
```

```
## [1] 10766
```
 * After imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean; however new median of total steps taken per day is increased by one than that of old median.
 
### Are there differences in activity patterns between weekdays and weekends?

##### 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
# Add a new column containing day of week
newactivity$weekday = weekdays(newactivity$date)
# Add a new column containing either Weekday OR Weekend
newactivity$weekdayType <- ifelse(newactivity$weekday == "Saturday" | newactivity$weekday == 
    "Sunday", "Weekend", "Weekday")
# convert column to factor
newactivity$weekdayType <- factor(newactivity$weekdayType)
```

##### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
# Create a new dataset grouping data by interval and weekday.type
avgWeekDy <- ddply(newactivity, ~interval + weekdayType, summarise, meanSteps = mean(steps))
avgWeekDy$interval <- as.numeric(as.character(avgWeekDy$interval))

# Plot using ggplot2
ggplot(data = avgWeekDy, aes(x = interval, y = meanSteps, color = weekdayType)) + 
    geom_line() + facet_grid(weekdayType ~ .) + scale_x_continuous("Day Time Interval", 
    breaks = seq(min(avgWeekDy$interval), max(avgWeekDy$interval), 200)) + scale_y_continuous("Average Number of Steps") + 
    ggtitle("Average Number of Steps Taken by Interval")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

  * In the above the plot, the patterns are different on weekends and weekdays. On weekdays there is a clear peak (raise of steps)  in the morning between 8:00 AM and 9:00 AM (_this could be people going to work around this time_). There peak on weekends in morning between 8:00 AM and 9:00 AM is much smaller and number of steps during the other hours between 10:00 AM and 4:00 PM of the day is higher than on the weekdays
 
##### _Remove all variables and data frames to free the memory._

```r
rm(activityRwDt, activityData, activitynoNA, meanSteps, medSteps, avgSteps, 
    maxStep, maxInterval, newactivity, avgWeekDy)
```

##### Below are the details of R environment used for this analysis

```r
sessionInfo()
```

```
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## 
## locale:
## [1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252   
## [3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
## [5] LC_TIME=English_Australia.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] plyr_1.8.1    ggplot2_1.0.0 knitr_1.6    
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-4 digest_0.6.4     evaluate_0.5.5   formatR_0.10    
##  [5] grid_3.1.1       gtable_0.1.2     htmltools_0.2.4  labeling_0.2    
##  [9] MASS_7.3-33      munsell_0.4.2    proto_0.3-10     Rcpp_0.11.2     
## [13] reshape2_1.4     rmarkdown_0.2.54 scales_0.2.4     stringr_0.6.2   
## [17] tools_3.1.1      yaml_2.1.13
```
