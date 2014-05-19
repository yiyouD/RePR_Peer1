Reproducible Research/ Peer Assessment 1
========================================================


## 1.Loading and preprocessing the data



```r
setwd("../Downloads")
```

```
## Error: 작업디렉토리를 변경할 수 없습니다
```

```r
data <- read.csv("./activity.csv", sep = ",")
data$date <- as.Date(data$date, "%Y-%m-%d")
```


## 2.What is mean total number of steps taken per day?



```r
install.packages("reshape2")
```

```
## Installing package into 'C:/Users/lenovo/Documents/R/win-library/3.0'
## (as 'lib' is unspecified)
```

```
## Error: trying to use CRAN without setting a mirror
```

```r
library(reshape2)
datamelt <- melt(data, id = c("date"), measure.vars = c("steps"), na.rm = TRUE)
data_sum <- dcast(datamelt, date ~ variable, sum)
```


Now with our new data frame we can create our histogram of the total number of steps taken each day:


```r
hist(data_sum$steps, col = "blue", xlab = "")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


The mean and the median total number of steps taken per day are calculated as below:

```r
mean(data_sum$steps)
```

```
## [1] 10766
```



```r
median(data_sum$steps)
```

```
## [1] 10765
```


## 3.What is the average daily activity pattern?




```r
datamelt2 <- melt(data, id = c("interval"), measure.vars = c("steps"), na.rm = TRUE)
interval <- dcast(datamelt2, interval ~ variable, mean)
head(interval)
```

```
##   interval   steps
## 1        0 1.71698
## 2        5 0.33962
## 3       10 0.13208
## 4       15 0.15094
## 5       20 0.07547
## 6       25 2.09434
```



```r
plot(interval$interval, interval$steps, type = "l", xlab = "Intervals", ylab = "Average", 
    col = "red", lwd = 2)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 




```r
interval[interval$steps == max(interval$steps), 1]
```

```
## [1] 835
```

```r
(interval[interval$steps == max(interval$steps), 1])/5
```

```
## [1] 167
```



## 4.Imputing missing values


```r
data1 <- subset(data, !is.na(steps))
nrow(data)
```

```
## [1] 17568
```

```r
nrow(data1)
```

```
## [1] 15264
```

```r
missing <- nrow(data) - nrow(data1)
No_of_missing
```

```
## Error: 객체 'No_of_missing'를 찾을 수 없습니다
```




```r
newData <- data
for (i in 1:nrow(newData)) {
    if (is.na(newData[i, 1])) {
        newData[i, 1] <- interval[(interval[, 1] == newData[i, 3]), 2]
    }
}
head(newData)
```

```
##     steps       date interval
## 1 1.71698 2012-10-01        0
## 2 0.33962 2012-10-01        5
## 3 0.13208 2012-10-01       10
## 4 0.15094 2012-10-01       15
## 5 0.07547 2012-10-01       20
## 6 2.09434 2012-10-01       25
```

Now we use the same method to creat the histogram for the new data set where the NA values were replaced :

```r
new_datamelt <- melt(newData, id = c("date"), measure.vars = c("steps"), na.rm = TRUE)
new_data_sum <- dcast(new_datamelt, date ~ variable, sum)
```



```r
hist(new_data_sum$steps, col = "green")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 



```r
mean(new_data_sum$steps)
```

```
## [1] 10766
```

```r
median(new_data_sum$steps)
```

```
## [1] 10766
```


If we compare the new values with those from section 2:

```
## [1] 10766
```

```
## [1] 10765
```


## 5.Are there differences in activity patterns between weekdays and weekends?


```r
factorData <- data
factorData$date <- weekdays(factorData$date)
```


```r
for (i in 1:nrow(factorData)) {
    if (factorData[i, 2] == "일요일" | factorData[i, 2] == "토요일") {
        factorData[i, 2] <- "weekend"
    } else {
        factorData[i, 2] <- "weekdays"
    }
}
head(factorData)
```

```
##   steps     date interval
## 1    NA weekdays        0
## 2    NA weekdays        5
## 3    NA weekdays       10
## 4    NA weekdays       15
## 5    NA weekdays       20
## 6    NA weekdays       25
```


```r
library(lattice)
plot <- xyplot(factorData$steps ~ factorData$interval | factorData$date, data = factorData, 
    layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number")
print(plot)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 

