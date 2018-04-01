install.packages("ggplot2")
install.packages("e1071")
install.packages("cowplot")
install.packages("ggmap")
install.packages("randomForest")
install.packages("dplyr")
install.packages("lubridate")
install.packages("alr4")
install.packages("caTools")

library(caTools)
library(ggplot2)
library(e1071) 
library(dplyr)
library(cowplot)
library(ggmap)
library(randomForest)
library(lubridate)
library(alr4)

#1.1
url<-"https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv"
data<-read.csv(url)
head(data)

getwd()
data<-read.csv("C:/Users/Ujval/Downloads/Capitalone/green_tripdata_2015-09.csv")
head(data)


#1.2
#The number of columns in the dataset
ncol(data)
#The number of rows in the dataset
nrow(data)
#str function Provides both the number of rows and columns
str(data)
cat('There are', nrow(data), 'rows and ',ncol(data),'columns in the data')

#2.1
#plotting the histogram of trip distances
hist(data$Trip_distance,main="Histogram of Trip Distances", xlab="Trip Distance",
     ylab="Number of Occurences(Count)",xlim = c(0,150))


qplot(data$Trip_distance,geom="histogram",binwidth = 0.2,  main = "Histogram of Trip Distances", 
      xlab = "Trip Distance", ylab="Number of Occurences(Count)", 
      fill=I("blue"), col=I("red"), alpha=I(0.2), xlim=c(0,25))

#2.2
Mean_data=mean(data$Trip_distance,na.rm = TRUE)
Median_data=median(data$Trip_distance,na.rm = TRUE)
Skew=skewness(data$Trip_distance)
Kurtosis=kurtosis(data$Trip_distance)

#3.1 Mean and Median Trip trip_Distance

data$hours<-format(strptime(data$lpep_pickup_datetime,"%Y-%m-%d %H:%M:%S"),"%H")
Hourlytrip_distance <- data.frame(data%>%
                                          group_by(hours)%>%
                                          summarize(meantripdistance = mean(Trip_distance, na.rm = TRUE),
                                                    mediantripdistance = median(Trip_distance, na.rm = TRUE)))
Hourlytrip_distance
p1 <- ggplot(Hourlytrip_distance, aes(x=hours, y=meantripdistance)) + geom_bar(stat = "identity")
p2 <- ggplot(Hourlytrip_distance, aes(x=hours, y=mediantripdistance)) + geom_bar(stat = "identity") 
plot(p1)
plot(p2)

#3.2

#Identifying trips that originate or terminate at one of the NYC area airports. 

#The data suggests that location can be used as a measure to estimate the Airport. 

#Having calculated the Hourly Trip distance, I am Interested to look at the total number of dropoff and 
#pickup at JFk in reference to the hour of the day
jfk<-data[(data$Dropoff_latitude < 40.660 & data$Dropoff_latitude > 40.635 & 
             data$Dropoff_longitude < -73.775 & data$Dropoff_longitude > -73.815)|
                 (data$Pickup_latitude < 40.660 & data$Pickup_latitude > 40.635 & 
                    data$Pickup_longitude < -73.775 & data$Pickup_longitude > -73.815),]

hourly=hour(jfk$lpep_pickup_datetime)
trips = aggregate(jfk$lpep_pickup_datetime,list(hourly),length)
names(trips)<- c("Hour of the day","Number of trips")
plot(trips,main="Hourly number of Pickups and Dropoffs at JFK")

#Calculating the average Fare amount during the dropoffs
data$jfk_dropoff <- ifelse(data$Dropoff_latitude < 40.660 & data$Dropoff_latitude > 40.635 
                                  & data$Dropoff_longitude < -73.775 & data$Dropoff_longitude > -73.815,
                                  1,0)

data$time_trip <- as.numeric(difftime(strptime(data$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"), 
                                            strptime(data$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"), 
                                            units = "mins"))
data$weekday <- weekdays(as.Date(data$lpep_pickup_datetime))

data.frame(data%>% 
             group_by(jfk_dropoff)%>%
             summarise(avg_Fare = mean(Fare_amount)))


#Original dataframe data copied to data1 for prediction purposes
data1<-data.frame(data)
str(data1)
#Q4.1 The derived variable tippercent is expressed as the percent of the total amount and also when total amount is 0 
#or tip amunt is 0, It will be considered as zero to avoid infinity values(NaN).

data1$tippercent<-ifelse(data1$Tip_amount==0.00 | data1$Total_amount==0.00 ,
                          0.00,round((data1$Tip_amount/data1$Total_amount)*100,2))
data1$tippercent

#Q4.2
#Predictive model for tip as a percentage of total fare using Linear Regression

#Conversion of characters or categorical variables to factors.
data1$Payment_type <- factor(data1$Payment_type)
data1$VendorID <- factor(data1$VendorID)
data1$Trip_type <- factor(data1$Trip_type)


#Before using the data1 for prediction,Filtering of missing data1 is essential
data1 <- data1[!complete.cases(data1),]
data1 <- data1[!is.na(data1$Trip_type),]
data1 <- data1[!data1$RateCodeID==99,]

#Outlier Trip Distances
data1 <- data1[data1$time_travelled < 350,]

#tip amount, toll amount, total_amount being less than 0
data1 <- data1[!data1$Tip_amount < 0,]
data1 <- data1[!data1$Tolls_amount < 0,]
data1 <- data1[!data1$Total_amount < 0,]

#Splitting the data1 into training (75%) and test (25%) for using Regression

samplesize <- floor(0.75* nrow(data1))
set.seed(100)
index <- sample(seq_len(nrow(data1)), size = samplesize)
training_set<- data1[index, ]
test_set <- data1[-index, ]


#Linear regression Model
regression_model<- lm(tippercent~VendorID+Passenger_count+
                        Extra+Tolls_amount+Total_amount+Payment_type+Trip_type,data =training_set)
summary(regression_model)
prediction<-predict(regression_model,test_set)
plot(prediction)

#The root mean square error is calculated using the below code for estimating the prediction
se<-(prediction-test_set$tippercent)^2
mean_se<-mean(se,na.rm=T)
mean_se
(rmse<-sqrt(mean_se))


#Q5.1
#The derived variable representing the average speed  over the course of the trip
data1$time_travelled <- as.numeric(difftime(strptime(data1$Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"), 
                                      strptime(data1$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"), units = "mins"))
data1$speed <- ifelse(data1$Trip_distance==0.00 | data1$time_travelled==0.00 ,0.00,(data1$Trip_distance/data1$time_travelled)*60)

data1$day <- as.numeric(format(strptime(data1$lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"),"%d"))
data1$week_num <- ifelse(data1$day <=7,1,ifelse(data1$day <= 14,2,ifelse(data1$day <=21,3,4)))
average_speed<-aggregate(speed~week_num,data1 = data1, mean)

#The Average speeds when grouped by weeks are 
average_speed


#Q5.2
#Hypothesis testing of the means

df2<-data1.frame(speed,data1$week_num)

fit<-lm(formula = speed~data1$week_num,data1=df2)
#t test also provides the F statistic
summary(fit)
#A one way ANOVA is performed to check if the NH of equal means and the AH of different means.
anova(fit)


#Q5.3

data1$hour <- hour(data1$lpep_pickup_datetime) + minute(data1$lpep_pickup_datetime)/60
sample = sample.split(data1$lpep_pickup_datetime, SplitRatio = .75)
data1$days<-format(strptime(data1$lpep_pickup_datetime,"%Y-%m-%d %H:%M:%S"),"%d")
t1 <- filter(data1,days<2)
head(t1,10)
training_set = subset(t1, sample == TRUE)
test_set = subset(t1, sample == FALSE)
lr <- lm(training_set$speed~training_set$hour)
summary(lr)
lr_pol <- lm(training_set$speed~training_set$hour + I(training_set$hour^2))
summary(lr_pol)
plot(training_set$speed~training_set$hour)

