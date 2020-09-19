# Load CSV  File and look at the data
# 

filAct <- "activity.csv"
if (!file.exists(filAct)) {
        errorCondition("Ensure Activity.csv exists in the working
                       directory")
        stop("Activity File is Missing")
        }
        
dfAct <- read.csv(filAct)

# What is mean total number of steps taken per day?
# 
 
dfDays <- dfAct %>% group_by(date) %>% 
        summarize(SumSteps = sum(steps,na.rm = TRUE))

# Make a histogram of the total number of steps taken each day
ggplot(dfDays, aes(x = as.Date(date), y = SumSteps)) + geom_col() + 
        scale_x_date(labels = date_format("%m/%d") , breaks = "1 week") + 
        labs(title = "Total Steps Each Day", x = "Week Of...", y = "Total Steps")

#Calculate and report the mean and median total number of steps taken per day
rpt <- summarise(dfDays, "Mean per Day" = mean(SumSteps, na.rm = TRUE),
                 "Median per Day" = median(SumSteps, na.rm = TRUE))

# What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
dfIntervals <- dfAct %>% group_by(interval) %>% 
        summarise(MeanSteps = mean(steps, na.rm = TRUE))

g <- ggplot(dfIntervals, aes(x = interval, y = MeanSteps)) + 
        geom_line() + 
        labs(title = "Average Steps in Interval", 
        x = "5-Minute Interval",
        y = "Mean Steps")
g + geom_point(data = dfMaxPoint, aes(x = interval, y = MeanSteps,
        color = "red"), size = 3) + 
        geom_text(x = dfMaxPoint$interval, 
        y = dfMaxPoint$MeanSteps, 
        hjust=-.6, label = paste("Max = ",dfMaxPoint$MeanSteps))

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
sum(is.na(dfAct$steps))
colsums(is.na(dfAct))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use 
# the mean/median for that day, or the mean for that 5-minute interval, etc.
dfImpute <- dfAct
dfImpute$steps <- ifelse(is.na(dfImpute$steps) == TRUE, 
                       dfIntervals$MeanSteps[dfIntervals$interval 
                        %in% dfImpute$interval], dfImpute$steps)

# Make a histogram of the total number of steps taken each day and Calculate 
# and report the mean and median total number of steps taken per day. Do these 
# values differ from the estimates from the first part of the assignment? What 
# is the impact of imputing missing data on the estimates of the total daily 
# number of steps?
# 
dfImputeDays <- dfImpute %>% group_by(date) %>% 
        summarize(SumSteps = sum(steps))

dfImputeInterval <- dfImpute %>% group_by(interval) %>% 
        summarise(MeanSteps = mean(steps))

dfMerge <- merge(dfIntervals, dfImputeInterval, by = "interval")

ggplot(dfMerge, aes(x = interval)) + 
        geom_line(aes(y = MeanSteps.x, color = "red"), 
                  size = 1, alpha = 0.7) + 
        geom_line(aes(y = MeanSteps.y, color = "Black"), 
                  alpha = 0.5, size = 4) + 
        labs(title = "Average Steps in Interval", x = "5-Minute Interval", 
             y = "Mean Steps")

# Create a new factor variable in the dataset with two levels -- "weekday" and 
# "weekend" indicating whether a given date is a weekday or weekend day.    
# 
dfDayOfWk <- dfImpute %>% mutate(WkDay = weekdays(as.Date(dfImpute$date)))
dfDayOfWk$WkDay <- ifelse(dfDayOfWk$WkDay %in% c("Saturday", "Sunday"), 
                          "weekend", "weekday")
dfDayOfWk$WkDay <- factor(dfDayOfWk$WkDay)
dfIntWkDay <- dfDayOfWk %>% group_by(interval,WkDay) %>% 
        summarise(MeanSteps = mean(steps))
g <- ggplot(dfIntWkDay)
g + geom_line(aes(x=interval, y = MeanSteps, group = WkDay)) + 
        facet_grid(WkDay~.)
