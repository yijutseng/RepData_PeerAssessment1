library(ggplot2)
library(data.table)
library(xtable)
library(knitr)
Activity<-fread("activity.csv")

StepDay<-Activity[!is.na(steps),.(Sum=sum(steps),Mean=mean(steps),Median=median(steps)),by=date]
qplot(data =StepDay ,x = date,y=Sum,geom = "histogram",stat="identity",xlab = 'Date',ylab='Average number of steps')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
StepDay.table<-xtable(StepDay[,list(date,Mean,Median)])
print(StepDay.table)

StepTime<-Activity[!is.na(steps),.(Sum=sum(steps),Mean=mean(steps),Median=median(steps)),by=interval]
qplot(data =StepTime ,x = interval,y=Mean,geom = "line",xlab = '5-minute interval',ylab='Average number of steps')
StepTime[Mean==max(StepTime$Mean),interval]


#number of missing values in the dataset
nrow(Activity[is.na(steps)])
#Create a new dataset that is equal to the original dataset but with the missing data filled in
ActivityImpu<-Activity
ActivityImpu$steps<-ifelse(is.na(ActivityImpu$steps),StepDay[date==ActivityImpu$date,Mean],ActivityImpu$steps)
StepDayAfterImpu<-ActivityImpu[,.(Sum=sum(steps),Mean=mean(steps),Median=median(steps)),by=date]
#Make a histogram of the total number of steps taken each day.
qplot(data =StepDayAfterImpu ,x = date,y=Sum,geom = "histogram",stat="identity",xlab = 'Date',ylab='Average number of steps')+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Calculate and report the mean and median total number of steps taken per day
StepDayAfterImpu.table<-xtable(StepDayAfterImpu[,list(date,Mean,Median)])
print(StepDayAfterImpu.table)

#Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?


#Are there differences in activity patterns between weekdays and weekends?
Activity$Weekday<-weekdays(as.Date(Activity$date))
Activity$Type<-ifelse(Activity$Weekday=="Sunday"|Activity$Weekday=="Saturday","Weekend","Weekday")
#Make a panel plot containing a time series plot of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
StepTimeWeek<-Activity[!is.na(steps),.(Sum=sum(steps),Mean=mean(steps),Median=as.numeric(median(steps))),by=list(Type,interval)]
qplot(data =StepTimeWeek ,x = interval,y=Mean, facets = Type~.,geom = "line",xlab = '5-minute interval',ylab='Average number of steps')
StepTime[Mean==max(StepTime$Mean),interval]


knit2html("PA1_template.Rmd")
