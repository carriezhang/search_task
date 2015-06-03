setwd("/Users/bingjiezhang/search_task")
# Read data
data = read.table("search_dataset.tsv",sep = "\t",header = TRUE, 
                  colClasses = c("character","character","numeric"))

# Create date variable from timestamp
date = strptime(data[,1],"%Y%m%d%H%M%S")
data_date = data.frame(year = date$year + 1900, month = date$mon + 1, 
                       mday = date$mday, wday = date$wday, hour = date$hour,
                       action = data[,2], displayTime = data[,3])

table(data_date$month) # only 8 observations in November, decide to discard them
data_date = data_date[data_date$month != 11,]
# Subset the data set
start = subset(data_date, action == "start")[,1:5]
click = subset(data_date, action == "click")[,1:5]
results = subset(data_date, action == "results")[,c(1:5,7)]

# Total Number of Counts 
require(plyr)
start = count(start)
click = count(click)

#  Calculate average display Time by hour
avgtime = aggregate(displayTime ~ ., data = results,mean)
avgtime = avgtime[order(avgtime$year,avgtime$month,avgtime$mday,avgtime$hour),]
row.names(avgtime) = NULL;

# Start is one observation less than click and avgtime, set this time to be 0
start = rbind(start[1:2,],data.frame(click[3,1:5],freq = 0),start[3:3942,])

# Summary Statistics and Visualization
require(ggplot2)
# Plot boxplot of start action by hour
hstart = aggregate(freq ~ hour, data = start, median)
p = qplot(factor(hour),freq,data = start, geom = "boxplot") 
p = p + xlab("hour")+ ylab("Total Number of Search Starts") + ggtitle("BoxPlot of Total Searches By Hour")
p = p + layer(data = hclick,mapping = aes(x = factor(hstart[,1]), 
                                          y= hstart[,2] + 2, label=round(hstart[,2])), geom = "text", 
              color="NavyBlue", size=3.5)
p
# Plot boxplot of click action by hour
hclick = aggregate(freq ~ hour, data = click, median)
p = qplot(factor(hour),freq,data = click, geom = "boxplot") 
p = p + xlab("hour")+ ylab("Total Number of Clicks") + ggtitle("Box Plot of Total Clicks By Hour")
p = p + layer(data = hclick,mapping = aes(x = factor(hclick[,1]), 
        y= hclick[,2] + 2, label=round(hclick[,2])), geom = "text", 
        color="NavyBlue", size=3.5)
p
# Plot boxplot of Average Page Display Time
htime = aggregate(displayTime ~ hour, data = avgtime, median)
p = qplot(factor(hour),displayTime,data = avgtime, geom = "boxplot") 
p = p + xlab("hour")+ ylab("Average Page Display Time") + ggtitle("Box Plot of Average Display Time By Hour")
p

# Plot Boxplot of click action by month and weekday
p = ggplot(data = click, aes(factor(month),freq))
p = p + geom_boxplot(aes(fill = factor(wday)))
p = p + scale_fill_discrete(name = "Week Day") + xlab("Month") + ylab("Total Number Of Clicks")
p = p + ggtitle("Box Plot of Total Clicks by Month and Weekday")
p

# Plot Boxplot of start action by month and weekday
p = ggplot(data = start, aes(factor(month),freq))
p = p + geom_boxplot(aes(fill = factor(wday)))
p = p + scale_fill_discrete(name = "Week Day") + xlab("Month") + ylab("Total Number Of Search Starts")
p = p + ggtitle("Box Plot of Total Number of Search Starts by Month and Weekday")
p

# Plot Box Plot of average search time by month and weekday
p = ggplot(data = avgtime, aes(factor(month),displayTime))
p = p + geom_boxplot(aes(fill = factor(wday)))
p = p + scale_fill_discrete(name = "Week Day") + xlab("Month") + ylab("Total Number Of Search Starts")
p = p + ggtitle("Box Plot of Total Number of Search Starts by Month and Weekday")
p

par(mfrow=c(3,2))
# Check seasonality of Starts with acf and pacf
acf(start$freq,main = "ACF of Total Number of Search Starts")
pacf(start$freq, main = "PACF of Total Number of Search Starts")

# Check seasonality of click with acf and pacf
acf(click$freq,main = "ACF of Total Number of Clicks")
pacf(click$freq, main = "PACF of Total Number of Clicks")

# Check seasonality of Average Search Time
acf(avgtime$displayTime, main = "ACF of Average Display Time")
pacf(avgtime$displayTime, main = "PACF of Average Display Time")

par(mfrow = c(1,1))

# Look into Extreme Observations
results_count = count(results[,1:5])
summary(avgtime$displayTime)
plot(results_count$freq,avgtime$displayTime)
tmp = data.frame(count = results_count$freq,time = avgtime$displayTime)
p1 = ggplot(tmp,aes(x = count, y = time)) + geom_point()
p1 = p1 + labs(title = "Total Number of Results Action VS Average Display Time", y = "Average Display Time")
p1

tmp = tmp[tmp[,2] >3000,]
p2 = ggplot(tmp,aes(x = count, y = time)) + geom_point()
p2 = p2 + labs(title = "Total Number of Results Action VS Long Average Display Time", y = "Average Display Time")
p2
# Show results action larger than 500
avgtime[results_count$freq > 500,]
# Show time with large average display time
tmp = avgtime[avgtime$displayTime > 2000,]
tmp
# Plot the total counts of starts and clicks
plot(click$freq, type = "l")
plot(start$freq, type = "l")
# Show the peak points
tmp = data.frame(click, start = start$freq)
tmp[tmp$start > 100,]
# Plot the peak points
tmp = subset(tmp,month == 4 & mday %in% c(3,4,5))
tmp = data.frame(time = 1:72,click = tmp$freq,start = tmp$start)
ggplot(tmp, aes(time, y = value, color = variable)) + 
  geom_point(aes(y = click, col = "click")) + 
  geom_point(aes(y = start, col = "start")) +
  geom_line(aes(y = click, col = "click")) +
  geom_line(aes(y = start, col = "start")) +
  labs(title = "April 3rd to April 5th", y = "Total Number of Actions")
tmp = subset(avgtime,month == 4 & mday %in% c(3,4,5))
ggplot(tmp,aes(1:72, y = displayTime)) + geom_point() + geom_line() +
  labs(title = "April 3rd to April 5th", x = "time",y = "Average Display Time")
 