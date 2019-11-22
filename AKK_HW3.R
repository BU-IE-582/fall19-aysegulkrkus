library(data.table)
require(arules)
library(glmnet)
library(lubridate)
library(ggplot2)
library(penalized)
library(ie2misc)
library(dplyr) 
library(tidyverse)


#Reading the data between [01.01.2016-16.11.2019]
all_data<-fread("RealTimeConsumption.csv", stringsAsFactors = FALSE, data.table = FALSE)
all_data=all_data[!is.na(all_data$`Consumption (MWh)`), ]
str(all_data)


#Introducing the data format and adding the weekdays to the specific dates
all_data$Date<-as.Date(all_data$Date, format = "%d.%m.%Y")
all_data$Days<-weekdays(all_data$Date)

#usage of consumption values of 2 days before(lag 48) and 7 days before(lag 168) for regression
all_data[["lag48"]] = c(rep(0.0, 48), all_data$`Consumption (MWh)`[1:(length(all_data$`Consumption (MWh)`)-48)])
all_data[["lag168"]] = c(rep(0.0, 168), all_data$`Consumption (MWh)`[1:(length(all_data$`Consumption (MWh)`)-168)])

#Dividing the data set into training and testing data by filtering through dates
training_data<-filter(all_data, Date<="2019-10-31")
testing_data<-filter(all_data, Date>"2019-10-31")
#removing zeros for lag 48 and lag 168
training_data<-filter(training_data, Date>"2016-01-07")

##################TASK A#############################
#using directly the consumption values of lag 48 and lag 168 without any model

testingmape_48<-mape(testing_data$lag48, testing_data$`Consumption (MWh)`)
testingmape_168<-mape(testing_data$lag168, testing_data$`Consumption (MWh)`)

##################TASK B#############################
#Using linear regression as y=beta0+beta1*lag48+beta2*lag168

train_data<-cbind(training_data$lag48, training_data$lag168)
train_data<-data.frame(train_data)
train_results<-training_data$`Consumption (MWh)`
linear_regression=lm(train_results~., data=train_data)

summary(linear_regression)
#The formula is found as consumption=0.005592+0.3862xlag48+0.04458

testing_df<-data.frame(testing_data[,1:6])
names(testing_df)[5] <- "X1"
names(testing_df)[6] <- "X2"
testing_df$Estimate <- predict(linear_regression,testing_df)
mape_allhour<-mape(testing_df$Estimate,testing_df$Consumption..MWh.)

plot(testing_df$Estimate, testing_df$Consumption..MWh. )

##################TASK C#############################
#modelling each hour by performing 24 separate models
date_train<-unique(training_data$Date)
hour_days<-unique(training_data$Hour)
linear_regression<-c()
hourly_coeff<-c()
hourly_mape<-c()

for(i in 1:24){
  trainhour_data<-filter(training_data, Hour==hour_days[i])
  trainhour_data_x<-cbind(trainhour_data[,5], trainhour_data[,6])
  trainhour_data_x<-data.frame(trainhour_data_x)
  train_results<-trainhour_data[,3]
  linear_regression<-lm(train_results~.,data=trainhour_data_x)
  
  hourly_coeff[[i]]<-as.matrix(linear_regression$coefficients)
  
  testinghour_data<-filter(testing_data, Hour==hour_days[i])
  testing_df<-data.frame(testing_data[,1:6])
  names(testing_df)[5] <- "X1"
  names(testing_df)[6] <- "X2"
  testing_df$Estimate <- predict(linear_regression,testing_df)
  hourly_mape[i]<-mape(testing_df$Estimate,testing_df$Consumption..MWh.)
}

hourly_coeff_df <- data.frame(matrix(unlist(hourly_coeff), nrow=length(hourly_coeff), byrow=T))
hourly_mape<-data.frame(hourly_mape)

#######TASK D
#################################
#To be sure that the training and the testing data are original

training_data<-filter(all_data, Date<="2019-10-31")
testing_data<-filter(all_data, Date>"2019-10-31")
#removing zeros for lag 48 and lag 168
training_data<-filter(training_data, Date>"2016-01-07")

#Transforming to wide format for train data
w<-dcast(training_data,Date~ Hour, value.var="lag48")
w2<-dcast(training_data,Date~ Hour, value.var="lag168")
wide_training_data<-cbind(w, w2[,2:25])
names(wide_training_data)[-1] <-  paste0('x', 1:(ncol(wide_training_data)))

#Transforming to wide format for test data
w_test<-dcast(testing_data,Date~ Hour, value.var="lag48")
w2_test<-dcast(testing_data,Date~ Hour, value.var="lag168")
wide_testing_data<-cbind(w_test, w2_test[,2:25])
names(wide_testing_data)[-1] <-  paste0('x', 1:(ncol(wide_testing_data)))

#####Creating hourly Lasso regression
date_train<-unique(training_data$Date)
hour_days<-unique(training_data$Hour)
hourly_coeff_lasso<-c()
hourly_mape_lasso<-c()
bestlam<-c()

for(i in 1:24){

  wide_train_y<-filter(training_data, Hour == hour_days[i])
  wide_test_y<-filter(testing_data, Hour == hour_days[i])
  ###################################################
  wide_train_y<-wide_train_y$`Consumption (MWh)`
  figure_2<-cbind(wide_training_data,wide_train_y)
  
  train_data<-as.matrix(wide_training_data[,2:49])
  train_results<-as.matrix(wide_train_y)
  testing_m<-data.matrix(wide_testing_data[,1:49])
  
  ##########lasso regression
  cv_output<-cv.glmnet(train_data, train_results, family = "gaussian", alpha = 1)
  
  bestlam[i]=cv_output$lambda.min
  lasso_best<-glmnet(train_data, train_results, family = "gaussian", alpha = 1, lambda = bestlam)
  wide_testing_data$Lasso_pred<-predict(lasso_best, newx =  as(testing_m[,2:49], "dgCMatrix"), type = "response")
  
  hourly_mape_lasso[i]<-mape(wide_testing_data$Lasso_pred[,1],wide_test_y$`Consumption (MWh)`)
  
}
hourly_mape_lasso<-data.frame(hourly_mape_lasso)

par(mfrow=c(1,2))
boxplot(hourly_mape_lasso, col = "orange", main="Lasso")
boxplot(hourly_mape, col = "blue", main="Linear Reg.")


