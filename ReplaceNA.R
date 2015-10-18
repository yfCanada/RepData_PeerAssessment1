library(dplyr)
library(ggplot2)

# Replace NA by mean of the day (there are days all NA in one day, so..)
   #Grouped<- group_by(Loaded, date)
   #steps_ave<- summarize(Grouped, Mean_EachDay = mean(steps, na.rm = TRUE))

# Replace NA by mean of interval
Grouped_interval<- group_by(Loaded, interval)
steps_ave<- summarize(Grouped_interval, Mean_interval = mean(steps, na.rm = TRUE))
# add 3rd variable represents the mean steps of a certain interval
data<- merge(Loaded,steps_ave, by = "interval" )
data_original <-data
# Replace NA by the mean value of the interval
for (i in 1:length(data$steps)){
    if (is.na(data$steps[i]) == "TRUE"){data$steps[i]<- data$Mean_interval[i] }
    }

Grouped_date<- group_by(data, date)
steps_total2<- summarize(Grouped_date, sum(steps))
ggplot(data=steps_total, aes(steps_total2$sum)) + geom_histogram()
summary(steps_total2$sum)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  41    9819   10770   10770   12810   21190 