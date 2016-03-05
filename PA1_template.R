#=====================================
# Assignment 1 - Reproducable Research
#=====================================

####install the usual Packages####
if(!require(ggplot2)) install.packages("ggplot2")
require(ggplot2)

if(!require(rmarkdown)) install.packages("rmarkdown")
require(rmarkdown)

if(!require(knitr)) install.packages("knitr")
require(knitr)

if(!require(data.table)) install.packages("data.table")
require(data.table)

if(!require(gridExtra)) install.packages("gridExtra")
require(gridExtra)

#--------------------------------------------------------------------------
# Dowload File and extract file ####
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filen <- "repdata-data-activity"
download.file(fileurl, paste0(filen,".zip"), mode = "wb")
unzip(paste0(filen, ".zip"),"activity.csv")

# load file into data table ####
actfn <- "activity.csv"
activity <- fread(actfn, sep = "auto", colClasses = c("numeric", "charachter", "numeric"))
#convert to date
activity$date <- as.Date(activity$date)

#--------------------------------------------------------------------------
## What is mean total number of steps taken per day? ####
# 1.Total Number of Steps taken per day
stepsperday <- activity[,.(StepsPerDay=sum(steps, na.rm = TRUE)), by = date]

# 2. Histogram of StepsPerDay frequency with bin width of 1000
ggplot(stepsperday, aes(StepsPerDay)) + geom_histogram(binwidth = 5000, color = "black", fill = "green") + 
  xlab("Steps Per Day") +
  ylab("Count")

# 3. Mean and Median of the total number of steps taken each day
stepsperday.stats <- stepsperday[,.(StepsPerDay_Mean = mean(StepsPerDay, na.rm = TRUE), StepsPerDay_Median = median(StepsPerDay))]
stepsperday.stats$StepsPerDay_Mean
stepsperday.stats$StepsPerDay_Median

## What is the average daily activity pattern? ####===============================================================
# 1. Time series plot of average number of steps taken per day
StepsPerInt <- activity[,.(AveragePerInterval = mean(steps, na.rm = TRUE)), by = interval]

ggplot(StepsPerInt, aes(interval, AveragePerInterval)) + geom_line(color = "blue", size = 1) + 
  xlab("Interval") + 
  ylab("Average Daily Steps")


# 2. The 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
StepsPerInt[AveragePerInterval == max(AveragePerInterval), interval]

#--------------------------------------------------------------------------
## Imputing missing values ##
# 1. total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity))

# 2. Imputing missing values using the average value for the interval
#       Strategy: Use average value per interval to replace NAs
str(activity)
str(StepsPerInt)

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity.imp <- as.data.table(merge(activity, StepsPerInt, by = "interval"))[is.na(steps), 
                                      steps := AveragePerInterval][,AveragePerInterval:=NULL]

#4. Make a histogram of the total number of steps taken each day and 
#     Calculate and report the mean and median total number of steps taken per day. 
stepsperday.imp <- activity.imp[,.(StepsPerDay=sum(steps, na.rm = TRUE)), by = date]

ggplot(stepsperday.imp, aes(StepsPerDay)) + geom_histogram(binwidth = 5000, color = "black", fill = "green") + 
  xlab("Steps Per Day") +
  ylab("Count")

stepsperday.stats.imp <- stepsperday.imp[,.(StepsPerDay_Mean = mean(StepsPerDay, na.rm = TRUE), StepsPerDay_Median = median(StepsPerDay))]

statsperday.all <- rbind(stepsperday.stats[,DataSource:="Original"], stepsperday.stats.imp[,DataSource:="Imputed"])
statsperday.all

# Shows that the both the mean and the median for the imputed values are higher
ggplot(statsperday.all, aes(x=StepsPerDay_Mean, y=StepsPerDay_Median, label = DataSource)) + 
  geom_text(hjust = 0.5, vjust= -1, nudge_x = 0) + geom_point(size=2) +
  expand_limits(x=c(9000, 11000), y=c(10000, 11000)) + xlab("Average Steps Per Day") + ylab("Median Steps Per Day")

#--------------------------------------------------------------------------
## Are there differences in activity patterns between weekdays and weekends?
# split up the data sets for plotting

activity.daytype <- activity.imp[,TypeOfWday:=as.factor(ifelse(weekdays(date)=="Saturday"|weekdays(date)=="Sunday", 
                                                               "Weekend", "Weekday"))]

wkend <- activity.daytype[TypeOfWday=="Weekend",.(StepsPerInt=mean(steps)), by = interval]
wkday <- activity.daytype[TypeOfWday=="Weekday",.(StepsPerInt=mean(steps)), by = interval]

#weekend plot
plot1 <- ggplot(wkend, aes(x=interval, y = StepsPerInt)) + 
  geom_line(color = "blue") + 
  xlab("") + ylab("") + 
  theme(axis.text.x = element_blank()) +
  ylim(0,200)+
  ggtitle("Weekend")

#weekday plot
plot2 <- ggplot(wkday, aes(x=interval, y = StepsPerInt)) + 
  geom_line(color = "red") + 
  xlab("") + ylab("") +
  ylim(0,200) +
  ggtitle("Weekday")

#combine the plots above
grid.arrange(plot1, plot2, nrow=2,left = "Number of steps", bottom = "Interval")

#--------------------------------------------------------------------------
#Cleanup Memory
closeAllConnections()
rm(list=ls())
gc()
