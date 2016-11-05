
## Loading and preprocessing the data

library(ggplot2)

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", dest="activity.zip", mode="wb") 
unzip ("activity.zip")
activity <- read.csv("activity.csv",stringsAsFactors = F)
activity$date = as.Date(activity$date)
# Clean NA's & steps = 0
activity = activity[complete.cases(activity),]
activity = activity[activity$steps>0,]

## What is mean total number of steps taken per day?
# Calculate mean total number of steps
x=summary(activity$steps)
print(x[4])

## Calculate total number of steps & plot Histogram of the total number of steps taken each day
SUM_STEPS = aggregate(steps~date, data=activity,FUN=sum)
ggplot(SUM_STEPS,aes(x=date,y=steps))+geom_bar(stat = 'identity')+
       theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Calculate and report the mean and median total number of steps taken per day
# Calculate mean
xmean= aggregate(steps~date, data=activity,FUN=mean)
SUM_STEPS$mean = xmean$steps 
ggplot(SUM_STEPS,aes(x=date,y=mean))+geom_bar(stat = 'identity')+
       theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate median
xmedian= aggregate(steps~date, data=activity,FUN=median)
SUM_STEPS$median = xmedian$steps
ggplot(SUM_STEPS,aes(x=date,y=median))+geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Merges Activity & SUM_STEPS
activity=merge(activity,SUM_STEPS,by.x = "date",by.y="date")
colnames(activity) = c('date', 'steps', 'interval', 'SUMSteps','AVGSteps','MEDSteps')

## The 5-minute interval that, on average, contains the maximum number of steps
x=activity [which(activity$steps==max(activity$steps)),3]
print(x)

## What is the average daily activity pattern?
ggplot(activity,aes(x=interval,y=AVGSteps))+geom_line()


## Are there differences in activity patterns between weekdays and weekends?
activity$dow <- ifelse(weekdays(activity$date,abbreviate = T) %in% c("Sat","Sun"), yes ="weekend", no = "weekday")
activity$dow=factor(activity$dow)
ggplot(activity,aes(x=interval,y=steps))+geom_line()+facet_wrap(~dow,nrow=2)

