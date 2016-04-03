# Reproducible Research
EDLP  
April 3, 2016  

### About
This is Assignment 1 for the Coursera Data Scientist course - Reproducible Research. This assignment explores data related to activity and includes the following:

* loading and preprocessing data
* imputing missing values
* interpreting data to answer research questions

###Data
The data for this assignment was downloaded from the course web site:  

Dataset: [https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip](Activity monitoring data [52K])  
The variables included in this dataset are:  

*steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

*date: The date on which the measurement was taken in YYYY-MM-DD format

*interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

###Loading and Pre-processing the data

```r
## Load libraries to be used
library(lubridate)
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.2.4
```

```r
library(lattice)
```


```r
## Set echo = TRUE
opts_chunk$set(echo = TRUE)
```

#### Read the file/load the data

```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}
data <- read.csv("activity.csv", header = TRUE, sep=',', colClasses =c("numeric", "character", "integer"))
```
#### Check the data

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

#### Process/transform the data (if necessary) into a format suitable for your analysis
##### Change the date

```r
data$date <- ymd(data$date)
```
#####Check to make sure date is in the right format.

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

###What is mean total number of steps taken per day?
####Calculate the total number of steps taken per day

```r
Total_Steps <- aggregate(steps ~ date, data, sum)
```
####Make a histogram of the total number of steps taken each day

```r
hist(Total_Steps$steps, 
   
     main = "Histogram of steps taken each day",
     xlab = 'Total Number of Steps',
     col = 'grey')
```

![](/instructions_fig/unnamed-chunk-8-1.png)

###Calculate and report the mean and median total number of steps taken per day
####Calculate the mean

```r
mean_steps <- round(mean(Total_Steps$steps, na.rm = TRUE),2)
```

#### Calculate the median

```r
median_steps <- round(median(Total_Steps$steps, na.rm = TRUE),2)
```
#### Draw mean and median lines

```r
hist(Total_Steps$steps, 
  
     main = "Histogram of steps taken each day",
     xlab = 'Total Number of Steps',
     col = 'grey')


abline(v = mean_steps, lwd = 3, col = 'green')
abline(v = median_steps, lwd = 3, col = 'black')

#place legend
legend('topright',
       lty = 1,
       lwd = 3,
       col = c("green", "black"),
       cex = .8, 
       legend = c(paste('Mean: ', mean_steps),
                  paste('Median: ', median_steps))
)
```

![](/instructions_fig/unnamed-chunk-11-1.png)
  
###What is the average daily activity pattern?
#### Make a timeseries plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
IntervalAvgSteps <- aggregate(steps ~ interval, data, mean)

### Check to make sure the intended aggregation worked
head(IntervalAvgSteps)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

###Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
####Get the max steps for the label

```r
max_steps = IntervalAvgSteps[which.max(IntervalAvgSteps$steps), ]
max_label = paste0("Interval ", max_steps$interval, " has \nthe maximum number of steps: \n", round(max_steps$steps,2))
```
####Plot the average

```r
plot(IntervalAvgSteps$interval, IntervalAvgSteps$steps, type="l",
     main = "Time Series Plot",
     xlab="5-minute Intervals",
     ylab="Average Number of Steps")

legend("topright",
       legend = max_label,
       text.col = 'black',
       bty = 'n'
)
```

![](/instructions_fig/unnamed-chunk-14-1.png)
  
###Imputing missing values
####Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
print(paste0("There is ", sum(is.na(data$steps)) , " NA's in the dataset."))
```

```
## [1] "There is 2304 NA's in the dataset."
```
###Devise a strategy for filling in all of the missing values in the dataset.
####Methodology for filling in missing values
Missing values were imputed by inserting the average for each interval. Thus, if interval 10 was missing on 10-02-2012, the average for that interval for all days (0.1320755), replaced the NA.

####Create the new data set

```r
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), IntervalAvgSteps$steps[match(data$interval, IntervalAvgSteps$interval)], data$steps))
```
  
Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data.

```r
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```
####Graph the imputed data and then show the difference

```r
data_i <- aggregate(steps ~ date, imputed_data, sum)
hist(data_i$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps")

#Create Histogram to show difference. 
hist(Total_Steps$steps, main = paste("Total Steps Each Day"), col="grey", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("red", "grey"), lwd=10)
```

![](/instructions_fig/unnamed-chunk-18-1.png)
  
####Calculate new mean and median for imputed data.

```r
mean_steps_i <- mean(data_i$steps)
median_steps_i  <- median(data_i$steps)

##Calculate difference between imputed and non-imputed data.
mean_diff <- mean_steps_i - mean_steps
med_diff <- median_steps_i - median_steps

##Calculate total difference.

total_diff <- sum(data_i$steps) - sum(Total_Steps$steps)
```
    

* The imputed data mean is 10,589.69
* The imputed data median is 10,766.19
* The difference between the non-imputed mean and imputed mean is -176.4949
* The difference between the non-imputed mean and imputed mean is 1.1887
* The difference between total number of steps between imputed and non-imputed data is 75,363.32. There were 75,363.32 more steps in the imputed data.

###Are there differences in activity patterns between weekdays and weekends?
####Create a plot to compare steps between the week and weekend. 



```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

IntervalAvgSteps_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

xyplot(IntervalAvgSteps_i$steps ~ IntervalAvgSteps_i$interval|IntervalAvgSteps_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](/instructions_fig/unnamed-chunk-20-1.png)
  
There is a higher peak earlier on weekdays, and more overall activity on weekends.
