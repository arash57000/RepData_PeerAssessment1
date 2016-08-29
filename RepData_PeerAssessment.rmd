#1. Code for reading in the dataset and/or processing the data
#This  statement is used to load the data by read.csv().
daily_exercise <- read.csv("Coursera/Reproducible Research/RepData_PeerAssessment1-master/activity.csv",
               header = TRUE, sep = ",")
#Lubridate provides tools that make it easier to parse and manipulate dates.

library(lubridate)

#Convert the date into date data type and interva field to Factor class

daily_exercise$date <- as.Date(daily_exercise$date, format = "%Y-%m-%d")


#What is mean total number of steps taken per day?


library(dplyr)

 Perday <- daily_exercise %>% group_by(date) %>%
 
  summarize(total_steps = sum(steps, na.rm = T),
           mean_steps = mean(steps, na.rm = T))
 

 #2: Make a histogram of the total number of steps taken each day
  

 library(ggplot2)
  g<- ggplot(Perday, aes(x=total_steps))
 g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),  
     axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")                    
 

 
  #3. Mean and median number of steps taken each day

  #Calculate and report the mean and median of the total number of steps taken per day
 
 summary(Perday$total_steps)
 
 summary(Perday$mean_steps)
 
 #What is the average daily activity pattern?
 
 Interval_5_minutes<- daily_exercise %>% group_by(interval) %>%
     summarize(mean_steps = mean(steps, na.rm = T))
 

  g <- ggplot(Interval_5_minutes, aes(x = interval, y = mean_steps))
 g + geom_line() + theme(axis.text = element_text(size = 12),  
                                             axis.title = element_text(size = 14))  +
   labs(y = "mean number of steps") + labs(x = "Interval")
 
 
 #Imputing missing values
 
 #The following lines will calculate the percentage of missing data as 
 #well as the number of rows that contain an NA.
 mean(is.na(daily_exercise$steps))
 sum(is.na(daily_exercise$steps))
 
 #There are several alternatives we can use to fill the NAs
 #Using the average steps per interval will be the approach to fill in the NAs
 
 
 # There are no missing values in this variable so I will use it to fill in for NAs.
 sum(is.na(Interval_5_minutes$mean_steps))
 
 NewDb <- daily_exercise   # This is another copy of original data 
 
 # before filling the NAs in our data, check to see which rows has NAs in steps column
 
 for (i in 1:nrow(NewDb)) {
      if (is.na(NewDb$steps[i])) {
          k <- NewDb$interval[i]
          v <- subset(Interval_5_minutes, interval == k)
          NewDb$steps[i] <- v$mean_steps
      }
 }
 
 # In this section I caclualte the total daily steps
 
 newPerday <- NewDb %>% group_by(date) %>%
   
   summarize(total_steps = sum(steps, na.rm = T),
             mean_steps = mean(steps, na.rm = T))
 
 # New Histogram with the new Data without NAs
 
 g<- ggplot(newPerday, aes(x=total_steps))
 g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),  
                                             axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")                    
 
 # summary and standard deviation of the new data 
 
 summary(newPerday$total_steps)
 sd(newPerday$total_steps, na.rm = T)
 
 # comparing the summary and standard deviaiton of new data to original data
 
 summary(Perday$total_steps)
 sd(Perday$total_steps, na.rm = T)
 
 # smaller standard deviaiton of new data without NAs shows that repalcing NAs with mean
 # values for the time intervals decrased the spread
 
 #8. Panel plot comparing the average number of steps taken per 5-minute interval 
 #across weekdays and weekends
 
 # creating a new column containing weekend and weekdays values in Newdb
 
 NewDb$day <- ifelse(weekdays(NewDb$date) %in% c("Saturday","Sunday"), "weekend","weekday")
 
 # crating two subsets , one for weekdays data and one for weekend data
  weekday <- filter(NewDb, day == "weekday")
  weekend <- filter(NewDb, day == "weekend")
  
  # Again I group these two data by interval and I calculate the mean number
  # of steps for each interval
  
  weekday <- weekday %>%
             group_by(interval) %>%
              summarize(mean_steps = mean(steps))
              weekday$day <- "weekday"
  
  weekend <- weekend %>%
              group_by(interval) %>%
              summarize(mean_steps = mean(steps))
              weekend$day <- "weekend"
              
    # now I merge these two data into one data set
    
    NewInterval_5_minutes <- rbind(weekday, weekend)
    NewInterval_5_minutes$day <- as.factor(NewInterval_5_minutes$day)
    NewInterval_5_minutes$day <- relevel(NewInterval_5_minutes$day, "weekend")
    
    
    
    for (j in 1:nrow(NewInterval_5_minutes)) {
      if (is.na(NewInterval_5_minutes$mean_steps[j])) {
        kk <- NewInterval_5_minutes$interval[j]
        vv <- subset(Interval_5_minutes, interval == kk)
        NewInterval_5_minutes$mean_steps[j] <- vv$mean_steps
      }
  
    }
    
    # I plot weekdays and weekend  side by side by using day column as a factor to 
    # differentiate between weekdays and weekend
    
     gg<- ggplot(NewInterval_5_minutes, aes(interval, mean_steps))
     gg + geom_line() +facet_grid(day~.) +theme(axis.text = element_text(12), axis.title = element_text(14)) +
      labs(y = "Number of Steps") +labs(x = "Interval")
    
     # activity during weekdays and weekend is very different.
     # during weekdays activity remains below 100 steps and peaks during morning
     # in wekend there is no show of high level activity