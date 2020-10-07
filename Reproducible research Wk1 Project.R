## Week 1 Coursera Reproducible research


##0.1 Check if all libraries are installed
x <- c("zip" )
new<- x[!(x %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
rm(x,new)

## 0.2 Read libraries
library(zip)
library(lattice)
library(ggplot2)
library(dplyr)

## 0.3 Check if data set is already downloaded in the current working directory and download it if is not available.

if (file.exists("activity.csv")){print("Already exists")
} else {
      file1 <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(file1, destfile = "x.zip", method = "curl")
      unzip("x.zip") 
}

## 0.4 Read information
act <- read.csv("activity.csv")


## 1.0 Total number of steps taken per day?
act_clean <-  act[complete.cases(act), ]
steps<- tapply(act_clean$steps, act_clean$date, FUN=sum)


## 2.0 2.0 Create histogram with total number of steps taken each day

histogram(~ steps, data = act_clean,
          main = "Histogram wo NA",
          xlab = "Steps",
          ylab = "Frecuency",
          col = "chocolate3")
dev.copy(png, file = "plot1.png", width=480, height=480)
dev.off()

## 3.0 Mean and median total number of steps taken per day

steps<- tapply(act_clean$steps, act_clean$date, FUN=sum)
mean_steps<- mean(steps, na.rm = T)
mean_steps
median_steps<- median(steps, na.rm = T)
median_steps


## 4.0 Create plot with average steps by period of 5 minutes interval
steps5min <- as.table(tapply(act_clean$steps, act_clean$interval, mean, na.rm = TRUE))
plot(as.numeric(names(steps5min)),steps5min,
     xlab = "Interval", ylab = "Steps",
     main = "Average Daily Activity Pattern", type = "l")

## 5.0 Obtain maximum number of steps 5-min period
m <- max(steps5min)
maxsteps <- steps5min[steps5min==m]
maxsteps[]

## 6.0 Total number of rows with NAs
missingNA <- sum(!complete.cases(act))
missingNA

## 7.0 Strategy for filling the missing values
f_replace<- function(x){ replace(x, is.na(x), mean(x, na.rm = TRUE))}

## 8.0 New dataset with the NA values replaced by the mean
new_data <- act %>%
      group_by(interval) %>%
      mutate(steps = f_replace(steps))

## 9.0 Reviewing if there is any NA value 
missingNA <- sum(!complete.cases(new_data))
missingNA

## 10.0 Create a new dataset that  equal to the original dataset but with the missing data filled in.
steps2<- tapply(new_data$steps, new_data$date, FUN=sum)
n_mean_steps<- mean(steps2, na.rm = T)
n_mean_steps
n_median_steps<- median(steps2, na.rm = T)
n_median_steps

## 11.0 Histogram total number of steps taken each day (NA filled)
histogram(~ steps, data = new_data,
          main = "Histogram with NA",
          xlab = "Steps",
          ylab = "Frecuency",
          col = "blue1")
dev.copy(png, file = "plot2.png", width=480, height=480)
dev.off()

##12.0 Calculate and report the mean/median total number of steps taken per day.
steps<- tapply(new_data$steps, new_data$date, FUN=sum)
n_mean_steps<- mean(steps, na.rm = T)
n_mean_steps
n_median_steps<- median(steps, na.rm = T)
n_median_steps

##13.0 What is the impact NA on the estimates of the total daily number of steps?
Difference_mean= n_mean_steps - mean_steps
Difference_mean
Difference_median = median_steps- n_median_steps
Difference_median

##14.0 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
new_data$wd <- weekdays( as.Date(new_data$date))
new_data$wd <- ifelse (new_data$wd=="Saturday" | new_data$wd=="Sunday", "Weekday", "Weekend" )
new_data2 <- aggregate(new_data$steps, by = list(new_data$wd, new_data$interval), mean)
colnames (new_data2) <-(c("day", "interval", "number_steps"))

## 15.0 Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
xyplot(number_steps ~ interval| day, data = new_data2, layout = c(1, 2), type= "l")
dev.copy(png, file = "plot3.png", width=480, height=480)
dev.off()

## End of code

