## Reproducible research
## Loading and preprocessing the data
#### Required libraries
library(dplyr)

#### Reading the data set

data<-read.csv( "activity.csv", sep = ",")
names(data)

#### Exloring data
str(data)
head(data)
tail(data)

## What is mean total number of steps taken per day?

#### 1-The total number of steps taken per day.
data <- mutate(data, hour = interval %/% 100, minute = interval %% 100)

daily<-c()  
for (i in 1:61){ 
  start<-(i-1)*288+1  
  last<-(i-1)*288+288
  temp<-data[start:last,1]    
  daily<-c(daily,sum(temp))    
}
print(daily)

#### 2-Histogram of the total number of steps taken each day

daily_noNA<-daily[!is.na(daily)]

png("plot1.png")
hist(daily_noNA, xlab="steps",ylab="Frequency",col="gray",
     main="Histogram of the total number of steps taken each day")
dev.off()
#### 3-Calculate and report the mean and median of the total number of steps taken per day

mean<-mean(daily,na.rm=T)
median<-median(daily,na.rm=T)

## What is the average daily activity pattern?

#### 1-Time series plot of the average number of steps taken

x<-data[,1]         
y<-matrix(x,288,61)   

five_average<-apply(y,1,mean,na.rm=TRUE)  

png("plot2.png")
plot(data$interval[1:288],five_average, type='l',col='blue',
     xlab='Intervals',lwd=1,
     ylab='Average number of steps',
     main ='Average number of steps taken in 5-minute interval')
dev.off()

#### 2-Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

hour<-data$hour[1:288]
minite<-data$minute[1:288]

hour_max<-hour[which(five_average==max(five_average))]
minite_max<-minite[which(five_average==max(five_average))]

cat('The maximum number of steps occurs at',hour_max,':',minite_max,'AM')

## Imputing missing values

#### 1-Calculate and report the total number of missing values in the dataset. 

sums<-sum(is.na(data[,1]))

#### 2-Devise a strategy for filling in all of the missing values in the dataset.

five_average_rep<- rep(five_average,61)

data1<-data   

for (i in 1:length(data1[,1])){  
  
  if(is.na(data1[i,1])==TRUE){
    data1[i,1]= five_average_rep[i]  
  }}

#### 3-Create a new dataset that is equal to the original dataset but with the missing data filled in.

data2<-write.csv(data1, "activity.new.csv") 

#### 4-Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

daily1<-c()

for (i in 1:61){              
start<-(i-1)*288+1        
last<-(i-1)*288+288
temp<-data1[start:last,1]    
daily1<-c(daily1,sum(temp))   
}

png("plot3.png")
hist(daily1, xlab="steps",ylab="Frequency",
main="Data with NA's filled in",col="green")
dev.off()

png("plot4.png")
hist(daily[!is.na(daily)], xlab="steps",ylab="Frequency",
main="NA's not filled in",col="blue",)
dev.off()

mean<-mean(daily1)
median<-median(daily1)

## Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

## Yes, there is a differnce in the median and in the histograms. Imputing missing data on the estimates of the total daily number of steps changes the median and the distribution. Based on the method used for filling in missing values, there is a difference in the mean and median values. The histogram can also be different based on the strategy we used to fill in the missing values.

## Are there differences in activity patterns between weekdays and weekends?

#### 1-Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

data1$date<-as.Date(data1$date)

data1$day<-weekdays(data1$date)

data1_weekdays<-data1[(!data1$day %in% c("Saturday","Sunday")),]

data1_weekend<-data1[(data1$day %in% c("Saturday","Sunday")),]   

weekday_steps<-data1_weekdays[,1]

temp<-matrix(weekday_steps,nrow=288)

weekday_steps_average<-apply(temp,1,mean)


weekend_steps<-data1_weekend[,1]

temp<-matrix(weekend_steps,nrow=288)

weekend_steps_average<-apply(temp,1,mean)

#### 2-Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)
png("plot5.png")
par(mfrow=c(2,1))

plot(data$interval[1:288],weekday_steps_average, type="l",xlab='Intervals',ylab="Number of steps",
col='blue',lwd=2, main="Weekday")

plot(data$interval[1:288],weekend_steps_average, type="l", xlab='Intervals',ylab="number of steps",
col='blue',lwd=2,main="Weekend")
dev.off()
