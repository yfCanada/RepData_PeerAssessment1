library(dplyr)
library(ggplot2)

# 0. Load the data;
# 1. What is mean total number of steps taken per day?
setwd("/Users/xiao/RepData_PeerAssessment1")
Loaded<- read.csv("activity.csv")
Grouped<- group_by(Loaded, date)
steps_total<- summarize(Grouped, sum(steps))
ggplot(data=steps_total, aes(steps_total$sum)) + geom_histogram()

# Summary shows Median = 10760, Mean = 10770
summary(steps_total$sum)
# it shows:   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#             41    8841   10760   10770   13290   21190       8 

#2. What is the average daily activity pattern?
GroupedbyInterval<- group_by(Loaded, interval)
steps_ave<- summarize(GroupedbyInterval, m = mean(steps, na.rm = TRUE))
png(file = "plot1.png")
with(steps_ave, plot(interval,m,type = "l", xlab="Interval (min)",ylab="Average number of steps"))
abline(v=835,lwd=2,lty=2)
dev.off()

# which.max(steps_ave$m)
# [1] 104  max is at 104th element 
# steps_ave[104,]  Show data at 104th row: 
# interval        m
# (int)    (dbl)
# 1      835 206.1698  .... then I know max is at Interval = 835

#3. Imputing missing values

# total number of missing values in data (i.e. in "steps"); No missing values are found in "date" and "inteval" 
# sum(is.na(Loaded$steps))
sapply(Loaded, function(x) sum(is.na(x))) # results show below:
#    steps     date interval 
#  2304        0        0 

################
# Replace NA by mean of the day (there are days all NA in one day, so..)
# Replace NA by mean of interval
Grouped_interval<- group_by(Loaded, interval)
steps_ave<- summarize(Grouped_interval, Mean_interval = mean(steps, na.rm = TRUE))
# add 3rd variable represents the mean steps of a certain interval
data<- merge(Loaded,steps_ave, by = "interval" )
# Replace NA by the mean value of the interval
for (i in 1:length(data$steps)){
    if (is.na(data$steps[i]) == "TRUE"){data$steps[i]<- data$Mean_interval[i] }
}
Grouped_date<- group_by(data, date)
steps_total2<- summarize(Grouped_date, sum(steps))
png(file = "plot2.png")
ggplot(data=steps_total, aes(steps_total2$sum)) + geom_histogram()
dev.off()
summary(steps_total2$sum)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  41    9819   10770   10770   12810   21190 
################


#4. Are there differences in activity patterns between weekdays and weekends?
data$date<-as.POSIXct(strptime(data$date, "%Y-%m-%d"))

# create a factor varaible of two levels: weekday and weekend
# asign each row to either level accordingly 
data<-mutate(data, weekday= weekdays(date))
for (i in 1:length(Loaded$date)){
    if (data$weekday[i] == "Saturday" | data$weekday[i] == "Sunday"){data$weekday[i]<-"Weekends"}
    else{data$weekday[i]<-"Weekdays"}
}
data$weekday<-as.factor(data$weekday)
weekdayData<-data[which(data$weekday == "Weekdays"),]
weekendData<-data[which(data$weekday == "Weekends"),]
Grouped_weekday<- group_by(weekdayData, interval)
Grouped_weekend<- group_by(weekendData, interval)
WeekdayStepsAve<- summarize(Grouped_weekday, m2 = mean(steps, na.rm = TRUE))
WeekendStepsAve<- summarize(Grouped_weekend, m3 = mean(steps, na.rm = TRUE))

png(file = "plot4.png")
par(mfrow = c(2,1))
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(WeekdayStepsAve$interval, WeekdayStepsAve$m2,type="l",xlab="Interval (min)",ylab="Average steps",main ="Weekday")
plot(WeekendStepsAve$interval, WeekendStepsAve$m3,type="l",xlab="Interval (min)",ylab="Average steps", main = "Weekend")
dev.off()


#ggplot(data=steps_total, aes(steps_total2$sum)) + geom_histogram()
#g<- ggplot(steps_ave2, aes(intrevals, steps_ave2))
#g + geom_line() +  geom_smooth(method = "lm")+ facet_grid(.~fips)



# Convert Factor to Date and time format 
#Loaded$date<-as.POSIXct(strptime(data4$DateTime, "%d/%m/%Y %H:%M:%S"))