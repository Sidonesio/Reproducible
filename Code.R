

################################### GET DATA ###################################

# file URL
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 

# download data
download.file(URL, destfile = "./data.zip") 

# read/load data into R environment
data <- read.table(unz("data.zip", "activity.csv"), sep = ",", header = TRUE)

################################## QUESTION 1 ##################################

## WHAT IS THE MEAN TOTAL NUMBER OF STEPS TAKEN PER DAY?

# total number of steps taken per day
question1 <- data %>%
  group_by(date) %>%
  summarise(steps = sum(steps))

# mean and median of the total number of steps taken per day
mean1 <- mean(question1$steps, na.rm = TRUE)
median1 <- median(question1$steps, na.rm = TRUE)
vertical1 <- c(mean1, median1)

# histogram of the total number of steps taken per day
windows()
library(ggplot2)
g <- ggplot(data = question1, mapping = aes(steps))
g + 
  geom_density(color="darkblue", 
               fill="lightblue", size=1) + 
  geom_histogram(aes(y = ..density..), 
                 bins = 9, fill="cornsilk2", 
                 alpha=.5, color="black", 
                 size=1) + 
  labs(title = "Total Steps per Day (without imputation)",
       x = "Total Steps",
       y = "Density") + 
  theme(plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 13.5),
        axis.title.x = element_text(size = 13.5),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_blank())

# mean and median of the total number of steps taken per day
rbind(c("Mean", "Median"), 
      c(round(mean(question1$steps, na.rm = TRUE), 2), 
        round(median(question1$steps, na.rm = TRUE), 2)))

################################## QUESTION 2 ##################################

## WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?

# 5-minute interval and average number of steps, averaged across all days
library(dplyr)
question2 <- data %>% 
  group_by(interval) %>%
  summarise(mean(steps, na.rm = TRUE))
colnames(question2)[2] <- "stepsmean"
g <- ggplot(data = question2, mapping = aes(interval, stepsmean))
g + 
  geom_line(size=1, color="darkblue") + 
  labs(title = "Average Steps per Interval",
       x = "Interval",
       y = "Average Steps") + 
  theme(plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 8))

# 5-minute interval, across all days, with the maximum average steps
as.vector(t(question2[which.max(question2$stepsmean),1]))

################################## QUESTION 3 ##################################

## IMPUTE MISSING VALUES

# total number of rows with NAs
sum(apply(data, 1, anyNA))

# substitute NAs by mean for that 5-minute interval
question3 <- data %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

# total number of steps taken per day
total <- question3 %>%
  group_by(date) %>%
  summarise(steps = sum(steps))

# mean and median of the total number of steps taken per day
mean2 <- mean(total$steps)
median2 <- median(total$steps)
vertical2 <- c(mean2, median2)

# mean and median of the total number of steps taken per day
rbind(c("Mean", "Median"), 
      c(round(mean(total$steps), 2), 
        round(median(total$steps), 2)))

# histogram of total number of steps taken each day
windows()
library(ggplot2)
g <- ggplot(data = total, mapping = aes(steps))
g + 
  geom_density(color="darkblue", 
               fill="lightblue", size=1) + 
  geom_histogram(aes(y = ..density..), 
                 bins = 9, fill="cornsilk2", 
                 alpha=.5, color="black", 
                 size=1) + 
  labs(title = "Total Steps per Day (with imputation)",
       x = "Total Steps",
       y = "Density") + 
  theme(plot.title = element_text(size = 16),
        axis.title.y = element_text(size = 13.5),
        axis.title.x = element_text(size = 13.5),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_blank())

################################## QUESTION 4 ##################################

# are there differences in activity patterns between weekdays and weekends?

# create new factor variable with levels "weekday" and "weekend"
question3$category <- question3$date
question3$category <- as.Date(question3$category)
question3$category <- weekdays(question3$category)
question3$category <- ifelse(question3$category=="sábado" | 
                               question3$category=="domingo", "weekend", 
                             "weekday")
question3$category <- as.factor(question3$category)

# plot average steps per interval, by weekday/weekend
question4 <- question3 %>%
  group_by(interval, category) %>%
  summarise(stepsmean = mean(steps))
g <- ggplot(data=question4, aes(x=interval, y=stepsmean))
g + 
  geom_line(size=1, aes(colour=category)) +
  facet_grid(category~.) + 
  labs(title = "Average Steps by Intervals, in Weekdays and Weekends",
       x = "Interval",
       y = "Average Steps") + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

