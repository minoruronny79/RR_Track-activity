#########################################################################
################PROJECT ACTIVITY MONITORING##############################
#########################################################################

remove(list = ls())
library(knitr)
library(dplyr)
library(ggplot2)

activity<-read.csv("F:/Edu_Universidad/4_Online/1_Coursera/Econometrics/JHopkins_Datascience/5_Reproducible_Research/RR_Week2/CourseProject_RR_W2/activity.csv", header = TRUE, sep = ",")


#Transforming the variable date in date
activity$date<-as.Date(activity$date)
class(activity$date)
head(activity)

#Creating a new dataset without NA's

activity2<-subset(activity, !is.na(activity$steps)) #Eliminating NAs in data set      

activity2<-filter(activity, steps!="NA") #Eliminating with dplyr


######################################################################
#######1) What is mean total number of steps taken per day?###########

#Obtaining the daily total steps of the individual using group_by and summarise
daily<-group_by(activity2, date)
dailytotal<-summarise(daily, sum(steps))

#Generating a histogram
hist(dailytotal$`sum(steps)`, main="Daily steps", col="red", xlab = "steps", breaks = 20)
abline(v=mean(dailytotal$`sum(steps)`), col="blue", lwd=4)

#Mean and median
mean(dailytotal$`sum(steps)`)
median(dailytotal$`sum(steps)`)


#########################################################################
#############2) What is the average interval pattern?####################

#Mean of steps separate by intervals using aggregate
stepsdata<-aggregate(steps~interval, data = activity2, mean)
plot(stepsdata$interval, stepsdata$steps, type="l", main="Average Steps per Five Minute Interval", xlab="Interval No.", ylab="steps")

#Mean of steps separate by intervals using group_by
intervalgroup<-group_by(activity2, interval)
intervalmean<-summarise(intervalgroup, mean(steps))
plot(intervalmean$interval, intervalmean$`mean(steps)`, type = "l")


#########################################################################
###################3) Imputing missing values###########################

sum(is.na(activity$steps))
sum(is.na(activity$date))
sum(is.na(activity$interval))
summary(activity)

#Creating a new variable steps2=step and obtaining the mean for the mission
activity$steps2<-activity$steps
activity$steps2[is.na(activity$steps2)]<-mean(activity$steps, na.rm = TRUE)

#Removing a column in a dataset
#activity$steps2<-NULL

#Creating a new dataset replacing the missing values of the mean
activity3<-activity
head(activity3)
activity3$steps[is.na(activity3$steps)]<-mean(activity$steps, na.rm = TRUE)

daily3<-group_by(activity3, date)
dailyactivity3<-summarise(daily3, sum(steps))

hist(dailyactivity3$`sum(steps)`, breaks = 20, col="red", xlab = "daily steps")

mean(dailyactivity3$`sum(steps)`)
median(dailyactivity3$`sum(steps)`)



#########################################################################
################4) Differences in activity patterns###########################

#Finding the week day
activity3$day<-weekdays(activity3$date)

activity3$weekend<-ifelse((activity3$day=="sábado" | activity3$day=="domingo"),1,0)

weekend_weekday<-group_by(activity3, weekend, interval)
zz<-summarise(weekend_weekday, sum(steps))
zz1<-filter(zz, weekend==1)
zz2<-filter(zz,weekend==0)

par(mfrow=c(2,1), mar=c(1,1,1,1))
plot(zz1$interval, zz1$`sum(steps)`,  type = "l", main = "Weekend" )
plot(zz2$interval, zz2$`sum(steps)`, type="l", main = "weekdays")

?plot

