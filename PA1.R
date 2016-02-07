library(ggplot2)
library(dplyr)
library(scales)
# load the data
activity <- read.csv("../activity.csv", stringsAsFactors = F)

# change into format of date
activity$date <- as.Date(activity$date,"%Y-%m-%d")
str(activity)

# calcul the total step per day
by_date <- group_by(activity,date)
step_date <- summarise(by_date, sum(steps, na.rm = T))
names(step_date)[2] <- "steps"

# make a histogram of steps every day
hist(step_date$steps, main = "Histogram of the total number of steps per day
", xlab = "Total number of steps per day")

# check the mean and median
summary(step_date$steps)

# Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
by_interval <- group_by(activity, interval)
step_interval <- summarize(by_interval, mean(steps, na.rm = T))
names(step_interval)[2] <- "steps"
step_interval$time <- as.character(format(step_interval$interval/100,nsmall = 2))
step_interval$time <- strptime(step_interval$time,"%H.%M")
with(step_interval, plot(time,steps, type = "l"))

# Find the maximum steps in step_interval
format(step_interval$time[which.max(step_interval$steps)],"%H:%M")

# total NAs
summary(activity)

# Imputing missing values
im.na <- merge(activity,step_interval, by = "interval")
im.na$steps <- ifelse(is.na(im.na$steps.x), im.na$steps.y, im.na$steps.x)
im.na <- select(im.na, -contains("."),date)
im.na <- im.na[order(im.na$date),]

# Make a new histogram
by_date$steps <- im.na$steps
step_date2 <- summarise(by_date, sum(steps))
names(step_date2)[2] <- "steps"
hist(step_date2$steps, main = "Histogram of the total number of steps per day
", xlab = "Total number of steps per day")
summary(step_date2$steps)

# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
im.na$wkd <- weekdays(im.na$date,abbreviate =  T)
im.na$wkd <- ifelse(im.na$wkd %in% c("Sat","Sun"), "Weekend","Weekday")
im.na$ind <- paste(format(im.na$time,"%H:%M"),im.na$wkd,sep = "_")
by_wkd <- group_by(select(im.na,-time),ind)
step_wkd <- summarise(by_wkd,mean(steps))
time2<-matrix(unlist(strsplit(step_wkd$ind,"_")),ncol = 2 , byrow = TRUE )
step_wkd <- data.frame(as.data.frame(time2),step_wkd$`mean(steps)`)
names(step_wkd) <- c('time','weekday','steps')
step_wkd$time <-  strptime(step_wkd$time,"%H:%M")
p <- ggplot(step_wkd,aes(time, steps, group =1))+geom_line() + facet_grid(weekday~.)
p+scale_x_datetime(labels = date_format("%H:%M"))+theme_bw(base_size = 14)
