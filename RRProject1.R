

Ractivity <- read.csv("activity.csv")

origData <- complete.cases(Ractivity)
Rsteps <- subset(Ractivity, complete.cases(Ractivity))

ByDay <- split(Rsteps,Rsteps$date, drop=TRUE)                           # split the complete cases by date  
dailySteps <- sapply(ByDay, function(x) sum(x$steps))                   # build a numeric vector w/ daily sum of steps  
hist(dailySteps, main="Hist Total Steps per Day", xlab="# Steps", 
     col="gray", labels = TRUE)                                         # plot a histogram and lable values
abline(v=mean(dailySteps), lty=5, col="blue")                           # draw a blue line thru the mean  
abline(v=median(dailySteps), lty=6, col="red")                          # draw a red line thru the median  
text(mean(dailySteps),25,labels="mean", pos=4, col="blue")              # label the mean  
text(mean(dailySteps),23,labels="median", pos=4, col="red")             # label the median

rug(dailySteps, col="green") 


# The mean is 'mean(dailysteps)'
# The median is 'median(dailysteps)'
# The sum is 'sum(dailysteps)'

splitMI <- split(Rsteps,Rsteps$interval, drop=TRUE)                     # split the complete cases by date  
intervalAvg <- sapply(splitMI, function(x) mean(x$steps))               # vector of Avg. steps per interval  
plot(intervalAvg, type="l",  
     main="5' Minute Time Series Plot", 
     ylab="Average # Steps", 
     xlab="Interval", col="blue3")                                      # plot the 5' time series
abline(v=which.max(intervalAvg), lty=3, col="red")                      # draw a red line thru the median  
text(which.max(intervalAvg),max(intervalAvg),  
     labels=paste("max = ",as.character(round(max(intervalAvg)))), 
     pos=4, col="black")     


splitmax <- names(which.max(intervalAvg))
splitavg <- round(max(intervalAvg))
splitname <- which.max(intervalAvg)

# The name index where the max number of step taken is `splitname, echo = FALSE`
# The max number of steps is `splitavg, echo = FALSE`

dataNA <- sum(is.na(Ractivity$steps))
#The number of missing values is `dataNA, echo = FALSE`

data1 <- sum(!is.na(Ractivity$steps))
#The number of values is `data1, echo = FALSE`

five_avg <- rep(intervalAvg, 61)
data2 <- Ractivity

for(i in 1:length (data2[,1])){
        
        if(is.na(data2 [i,1])==TRUE){
        data2[i,1] = five_avg [i]
        }}

daily1<-c()


for (i in 1:61){                                #the total number of days in October and November is 31+30=61
        start<-(i-1)*288+1                      #there are 288 five-minute steps in a day; 24*60/5=288
        last<-(i-1)*288+288
        temp<-data2[start:last,1]               #extracting all 5-minute steps for each day
        daily1<-c(daily1,sum(temp))             #concatenating the daily totals 
}

hist(daily1, main="Total Steps per Day with NA Imputed", xlab="# Steps", 
     col="gray", labels = TRUE)                                                     # plot a histogram and lable values
abline(v=mean(daily1), lty=5, col="blue")                                           # draw a blue line thru the mean  
abline(v=median(daily1), lty=6, col="red")                                          # draw a red line thru the median  
text(mean(daily1),25,labels="mean", pos=4, col="blue")                              # label the mean  
text(mean(daily1),23,labels="median", pos=4, col="red") 

summary(dailySteps)
summary(daily1)

data2$date<-as.Date(data2$date)
data2$day<-weekdays(data2$date)

data2_weekdays<-data2[(!data2$day %in% c("Saturday","Sunday")),]                    # weekdays
data2_weekend<-data2[(data2$day %in% c("Saturday","Sunday")),]                      #  weekend

weekday_steps<-data2_weekdays[,1]
temp<-matrix(weekday_steps,nrow=288)
weekday_steps_average<-apply(temp,1,mean)


weekend_steps<-data2_weekend[,1]
temp<-matrix(weekend_steps,nrow=288)
weekend_steps_average<-apply(temp,1,mean)

par(mfrow=c(2,1))

plot(Ractivity$interval[1:288],weekday_steps_average, type="l",xlab='Intervals',ylab="Number of steps",
     col='black',lwd=2, main="Weekday")

plot(Ractivity$interval[1:288],weekend_steps_average, type="l", xlab='Intervals',ylab="number of steps",
     col='red',lwd=2,main="Weekend")

