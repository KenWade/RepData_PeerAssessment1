####
#
#   ActivityMonitoring.R
#
#   Johns Hopkins School of Public Health - Data Science 5 - Reproducible Research
#                   (https://www.coursera.org/learn/reproducible-research)
#
#   Course Project 1 - Personal Activity Monitoring
#
#       This assignment makes use of data from a personal activity monitoring device. This device collects data at
#       5 minute intervals through out the day. The data consists of two months of data from an anonymous individual
#       collected during the months of October and November, 2012 and include the number of steps taken in 5 minute
#       intervals each day.
#
#   Requirement:  Write a report that answers the questions detailed in the project description. Ultimately, to
#                 complete the entire assignment in a single R markdown document that can be processed by knitr and be
#                 transformed into an HTML file.

####
#
#   Setup Libraries, Directories, and URLs
#

setwd("C:/Users/Ken/Documents/Ken/Continuing Education/Johns Hopkins School of Public Health - Data Science 5 - Reproducible Research/RepData_PeerAssessment1")

dataFolderName <- "data"
plotFolderName <- "figures"

zipFileName    <- "activity.zip"
dataFileName   <- "activity.csv"

maxMeanSteps   <- 250
####
#
#    Read in the Course Project data files

## Create the data folder
if(!file.exists(dataFolderName)) {
    dir.create(dataFolderName)
    print(paste("Created ", dataFolderName, " folder.", sep=""))
}
    
if(!file.exists(paste("./", dataFolderName, "/", dataFileName, sep=""))) {
    print(paste("Unzipping ", zipFileName, "...", sep=""))
    unzip(zipFileName, exdir=paste("./", dataFolderName, sep=""))
    print(paste("Unzipping ", zipFileName, " completed.", sep=""))
}


####
#
# Loading and preprocessing the data
#

print("Reading data...")
activity <- read.csv(paste("./", dataFolderName, "/", dataFileName, sep=""))
print("Reading data completed.")

# Preprocessing - Create datetime from date and interval.
activity$datetime <- strptime(paste(activity$date, formatC(activity$interval, width=4, flag="0")), "%Y-%m-%d %H%M")

####
#
# What is mean total number of steps taken per day?
#
#   For this part of the assignment, you can ignore the missing values in the dataset.
#

# First plot the data just because I want to see it.
png(filename = paste("./", plotFolderName, "/", "Steps.png", sep=""), width = 480, height = 480, units = "px")
plot(activity$datetime, activity$steps, type="b", main="Steps Per Interval",
     xlab="Date", ylab="Steps", ylim=c(0, max(activity$steps, na.rm=TRUE)), col = "blue")
dev.off()

# Calculate the total number of steps taken per day
totalStepsPerDay <- aggregate(activity$steps, by = list(activity$date), FUN=sum, na.rm=FALSE)
names(totalStepsPerDay) <- c("date", "steps")

# Make a histogram of the total number of steps taken each day
png(filename = paste("./", plotFolderName, "/", "StepsPerDay.png", sep=""), width = 480, height = 480, units = "px")
hist(totalStepsPerDay$steps, main="Total Steps Per Day", xlab="Total Number of Steps", ylab="Frequency in Days",
     ylim=c(0,40), breaks=5)
dev.off()

# CCalculate and report the mean and median of the total number of steps taken per day
meanTotalStepsPerDay = round(mean(totalStepsPerDay$steps, na.rm=TRUE))
medianTotalStepsPerDay <- median(totalStepsPerDay$steps, na.rm=TRUE)

print(paste("Mean Steps Per Day   = ", meanTotalStepsPerDay, sep=""))
print(paste("Median Steps Per Day = ", medianTotalStepsPerDay, sep=""))


####
#
# What is the average daily activity pattern?
#
#   For this part of the assignment, you can ignore the missing values in the dataset.
#

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,
#   averaged across all days (y-axis)
meanStepsPerInterval <- aggregate(activity$steps, by = list(activity$interval), FUN=mean, na.rm=TRUE)
names(meanStepsPerInterval) <- c("interval", "steps")

png(filename = paste("./", plotFolderName, "/", "StepsPerInterval.png", sep=""), width = 480, height = 480, units = "px")
plot((strptime(formatC(meanStepsPerInterval$interval, width=4, flag="0"), "%H%M")$hour * 60
      +strptime(formatC(meanStepsPerInterval$interval, width=4, flag="0"), "%H%M")$min),
     meanStepsPerInterval$steps, type="l",
     main="Mean Steps Per Interval", xlab="Interval in Minutes From Midnight, NOT 24-hour Time", ylab="Mean Steps",
     ylim=c(0, max(maxMeanSteps, meanStepsPerInterval$steps, na.rm=TRUE)), col = "blue")
abline(h = mean(meanStepsPerInterval$steps), col = "red")
legend("topright", c("Steps Per Interval", "Daily Average Steps"),lty=c(1,1),col=c("blue","red"))
dev.off()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
meanIntervalWithMostStepsValue <- round(max(meanStepsPerInterval$steps))
meanIntervalWithMostSteps <- meanStepsPerInterval$interval[which.max(meanStepsPerInterval$steps)]
print(paste("Interval With Most Steps is ", formatC(meanIntervalWithMostSteps, width=4, flag="0"),
            " with ", meanIntervalWithMostStepsValue, " steps.", sep=""))


####
#
# Imputing missing values
#
#

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
naCount <- sum(is.na(activity$steps))
print(paste("Number of Intervals containing NA's is ", naCount, sep=""))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be
#   sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
#
#     Model imputedActivity_di = Replace NA values with the average of the daily mean and interval mean
#     Model imputedActivity_i  = Replace NA values with the interval mean
#     Model imputedActivity_d  = Replace NA values with the daily mean

imputedActivity_di <- activity
imputedActivity_d  <- activity
imputedActivity_i  <- activity
checkcount <- 0

for (i in 1:nrow(activity)) {
  if (is.na(activity$steps[i])) {
    day <- as.integer(difftime(activity$datetime[i], activity$datetime[1], units="days")) + 1
    interval <- ((strptime(formatC(activity$interval[i], width=4, flag="0"), "%H%M")$hour * 60
                 +strptime(formatC(activity$interval[i], width=4, flag="0"), "%H%M")$min)
                 /5) + 1

#     Replace NA values with the average of the daily mean and interval mean
    imputedActivity_di$steps[i] = round((totalStepsPerDay$steps[day]/nrow(meanStepsPerInterval) + meanStepsPerInterval$steps[interval]) / 2)
#     Replace NA values with interval mean only
    imputedActivity_i$steps[i] = meanStepsPerInterval$steps[interval]
#     Replace NA values with daily mean only
    imputedActivity_d$steps[i] = totalStepsPerDay$steps[day]/nrow(meanStepsPerInterval)
    
#    print(totalStepsPerDay$steps[day])
#    print(meanStepsPerInterval$steps[interval])
#    print(paste("i = ", i, "   day = ", day, "    interval = ", interval, "   steps = ", imputedActivity_di$steps[i], sep=""))
    checkcount <- checkcount + 1
  }
}

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total
#   number of steps taken per day. Do these values differ from the estimates from the first part of the assignment?
#   What is the impact of imputing missing data on the estimates of the total daily number of steps?

imputed_di_totalStepsPerDay <- aggregate(imputedActivity_di$steps, by = list(imputedActivity_di$date), FUN=sum, na.rm=TRUE)
names(imputed_di_totalStepsPerDay) <- c("date", "steps")

png(filename = paste("./", plotFolderName, "/", "Imputed_DI_StepsPerDay.png", sep=""), width = 480, height = 480, units = "px")
hist(imputed_di_totalStepsPerDay$steps, main="Total Imputed DI Steps Per Day", xlab="Total Number of Steps",
     ylab="Frequency in Days", ylim=c(0,40), breaks=5)
dev.off()


imputed_d_totalStepsPerDay <- aggregate(imputedActivity_d$steps, by = list(imputedActivity_d$date), FUN=sum, na.rm=TRUE)
names(imputed_d_totalStepsPerDay) <- c("date", "steps")

png(filename = paste("./", plotFolderName, "/", "Imputed_D_StepsPerDay.png", sep=""), width = 480, height = 480, units = "px")
hist(imputed_d_totalStepsPerDay$steps, main="Total  Imputed D Steps Per Day", xlab="Total Number of Steps",
     ylab="Frequency in Days", ylim=c(0,40), breaks=5)
dev.off()


imputed_i_totalStepsPerDay <- aggregate(imputedActivity_i$steps, by = list(imputedActivity_i$date), FUN=sum, na.rm=TRUE)
names(imputed_i_totalStepsPerDay) <- c("date", "steps")

png(filename = paste("./", plotFolderName, "/", "Imputed_I_StepsPerDay.png", sep=""), width = 480, height = 480, units = "px")
hist(imputed_i_totalStepsPerDay$steps, main="Total Imputed I Steps Per Day", xlab="Total Number of Steps",
     ylab="Frequency in Days", ylim=c(0,40), breaks=5)
dev.off()

#
# Yes, the values differ from the estimates from the first part of the assignment.
#
# The impact of imputing missing data on the estimates of the total daily number of steps is that the steps increase.
#   Depending on the model chosen there are different impacts.  In this case, the time of day(the interval) seems to be
#   a more rational (consistent?) method to impute these missing values.
#
# Therefore, imputedActivity_i (imputing using just the interval) will be used for the final part of the project.
#

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date
#   is a weekday or weekend day.
#
# $wday returns numeric weekday (0-6 starting on Sunday). Therefore, 0 or 6 for weekend

imputedActivity_i$daytype <- factor(ifelse(imputedActivity_i$datetime$wday==0 | imputedActivity_i$datetime$wday==6,
                             "weekend", "weekday"))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average
#   number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub
#   repository to see an example of what this plot should look like using simulated data.

weekend <- subset(imputedActivity_i, imputedActivity_i$daytype == "weekend")
weekday <- subset(imputedActivity_i, imputedActivity_i$daytype == "weekday")

weekendInterval <- aggregate(weekend$steps, by = list(weekend$interval), FUN=mean, na.rm=TRUE)
names(weekendInterval) <- c("interval", "steps")

weekdayInterval <- aggregate(weekday$steps, by = list(weekday$interval), FUN=mean, na.rm=TRUE)
names(weekdayInterval) <- c("interval", "steps")

png(filename = paste("./", plotFolderName, "/", "WeekendStepsPerInterval_Imputed.png", sep=""), width = 480, height = 480, units = "px")
plot((strptime(formatC(weekendInterval$interval, width=4, flag="0"), "%H%M")$hour * 60
      +strptime(formatC(weekendInterval$interval, width=4, flag="0"), "%H%M")$min),
     weekendInterval$steps, type="l",
     main="Weekend Imputed Mean Steps Per Interval", xlab="Interval in Minutes From Midnight, NOT 24-hour Time", ylab="Imputed Mean Steps",
     ylim=c(0, max(maxMeanSteps, weekendInterval$steps, na.rm=TRUE)), col = "blue")
abline(h = mean(weekendInterval$steps), col = "red")
legend("topright", c("Imputed Steps Per Interval", "Daily Average Steps"),lty=c(1,1),col=c("blue","red"))
dev.off()

png(filename = paste("./", plotFolderName, "/", "WeekdayStepsPerInterval_Imputed.png", sep=""), width = 480, height = 480, units = "px")
plot((strptime(formatC(weekdayInterval$interval, width=4, flag="0"), "%H%M")$hour * 60
      +strptime(formatC(weekdayInterval$interval, width=4, flag="0"), "%H%M")$min),
     weekdayInterval$steps, type="l",
     main="Weekday Imputed Mean Steps Per Interval", xlab="Interval in Minutes From Midnight, NOT 24-hour Time", ylab="Imputed Mean Steps",
     ylim=c(0, max(maxMeanSteps, weekdayInterval$steps, na.rm=TRUE)), col = "blue")
abline(h = mean(weekdayInterval$steps), col = "red")
legend("topright", c("Imputed Steps Per Interval", "Daily Average Steps"),lty=c(1,1),col=c("blue","red"))
dev.off()

png(filename = paste("./", plotFolderName, "/", "Weekday-weekendStepsPerInterval_Imputed.png", sep=""), width = 480, height = 480, units = "px")
par(mfrow=c(2,1))
par(mar=c(0, 4, 4, 2) + 0.1)
par(mgp = c(2, 0.6, 0))

plot((strptime(formatC(weekendInterval$interval, width=4, flag="0"), "%H%M")$hour * 60
      +strptime(formatC(weekendInterval$interval, width=4, flag="0"), "%H%M")$min),
     weekendInterval$steps, type="l",
     main="Imputed Mean Steps Per Interval", ylab="Weekend", xaxt="n", xlab="",
     ylim=c(0, max(maxMeanSteps, weekendInterval$steps, na.rm=TRUE)), col = "blue")
abline(h = mean(weekendInterval$steps), col = "red")

par(mar=c(5, 4, 0, 2) + 0.1)
plot((strptime(formatC(weekdayInterval$interval, width=4, flag="0"), "%H%M")$hour * 60
      +strptime(formatC(weekdayInterval$interval, width=4, flag="0"), "%H%M")$min),
     weekdayInterval$steps, type="l",
     xlab="Interval in Minutes From Midnight, NOT 24-hour Time", ylab="Weekday",
     ylim=c(0, max(maxMeanSteps, weekdayInterval$steps, na.rm=TRUE)), col = "blue")
abline(h = mean(weekdayInterval$steps), col = "red")
dev.off()

