
title: 'Reproducible Research: Peer Assessment 1'
=======================================================


## Loading and preprocessing the data


```r
# unzip the data if file doesn't exist
if(!file.exists("acitivity.csv"))
  unzip("activity.zip")

# read the data and adjust the classes
df <- read.csv("activity.csv", header = T,  stringsAsFactors = F,) 
df$date <- as.Date(df$date)
```
## What is mean total number of steps taken per day?

```r
library("dplyr")
df_summary <- df%>%
  group_by(date)%>%
  summarize(total = sum(steps,na.rm = TRUE) )
hist(df_summary$total, main = "Histogram of total number of steps", xlab = "Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
mean(df_summary$total)
```

```
## [1] 9354.23
```

```r
median(df_summary$total)
```

```
## [1] 10395
```
## What is the average daily activity pattern?

```r
df_daily <- df%>%
  group_by(interval)%>%
  summarize(mean = mean(steps, na.rm = TRUE))

plot(x = df_daily$interval, y = df_daily$mean, type = "l" , xlab = "interval", ylab = "mean" )
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
df_daily[which.max(df_daily$mean),]$interval
```

```
## [1] 835
```


## Imputing missing values

```r
sum(is.na(df$steps))
```

```
## [1] 2304
```

```r
#NA is replaced by the mean value
df_new <- df %>% 
  group_by(interval)%>%
  mutate(steps=ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

#Make a histogram and calculate a new mean and median 
df_new_data <- df_new%>%
  group_by(date)%>%
  summarize(total = sum(steps))
hist(df_new_data$total, main = "Histogram of total number of steps", xlab = "Steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
mean(df_new_data$total)
```

```
## [1] 10766.19
```

```r
median(df_new_data$total)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

```r
library(ggplot2)
df_new <- df_new %>% 
  mutate(day = weekdays(date))%>%  
  mutate(day = case_when(
    day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Weekday", 
    day %in% c("Saturday", "Sunday") ~ "Weekend"))

df_daily <- df_new%>%
  group_by(interval, day)%>%
  summarize(mean = mean(steps))

ggplot(data=df_daily, aes(x = interval , y = mean))+
  geom_line()+
  facet_wrap(~day)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

