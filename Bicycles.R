library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(car)

setwd("C:\\Users\\Chamis\\Desktop\\Projects\\Bicycles")

train = read.csv("train_bicycles.csv")
test = read.csv("test_bicycles.csv")


#Inspecting train and test data sets

table(is.na(train)) # Find no NA values  
summary(train) # Find that humidity and wind have 0 values, so will inspect it further
dim(train)
str(train)

table(is.na(test)) # Find no NA values  
summary(test) 
dim(test)
str(test)

which(train$humidity==0) #Finding index. Inspecting that few consecutive values are missing 
train$humidity[1092:1113]=mean(train$humidity[1091],train$humidity[1114]) #Finding mean of the closest values 
train$humidity[1092:1113] # Filling new values into missing range


#Joining tables together

test$registered=0
test$casual=0
test$count=0

all_data=rbind(train,test)


#Plotting histograms for visual inspection
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(train$season)
hist(train$holiday)
hist(train$workingday)
hist(train$weather)
hist(train$temp) #Temperature seems positively correlated with bike demand
hist(train$atemp)
hist(train$humidity)
hist(train$windspeed)


#Would be good to know how different times of the day and different years affect bike demand

train$year=as.integer(substring(train$datetime,1,4)) #Extracting year number
test$year=as.integer(substring(test$datetime,1,4))

train$hour=as.integer(substring(train$datetime,12,13)) #Extracting hour number 
test$hour=as.integer(substring(test$datetime,12,13))

par(mfrow=c(1,1))
boxplot(train$count~train$year,xlab="year", ylab="bikes rented")
#Observing there was a growth in bike rents from 2011 to 2012

boxplot(train$count~train$hour,xlab="hour", ylab="bikes rented")
#Observing increase in bike rents in the mornings and evenings, when people go to 
#or from work. Will investigate by plotting for registered and casual users separately

par(mfrow=c(2,1))
boxplot(train$registered~train$hour,main="Registered Users",xlab="hour", ylab="bikes rented")
boxplot(train$casual~train$hour,main="Casual Users",xlab="hour", ylab="bikes rented")
#Observing that registered users rent bikes a lot at the start and end of working hours 
#While casual user bike demand has a normal distribution througout daylight and low demand at night




# Linear Regression 

#Will perform correlation matrix to inspect continous variables 

continous_variables = data.frame(train$temp, train$atemp, train$humidity,
       train$windspeed, train$casual,train$registered, train$count)
cor(continous_variables) 
#As expected atemp and temp are highly correlated (0.98) so will not include atemp to avoid colliniarity
#temp is positively correlated to bicycle rents (0.39) and more so for casual rents (0.47)
#humidity is negatively correlated to bicycle rents (-0.33)
#windspeed correlation with bicycles rented is very small at (0.1)



# Next, we will create time bins for registered and casual users separately

train$timebin_reg[train$hour %in% 7:9 | train$hour %in% 17:19]="peak time"
train$timebin_reg[train$hour %in% 10:16| train$hour %in% 20:22]="moderate time"
train$timebin_reg[train$hour %in% 0:6 | train$hour == 23] = "off-peak time"

test$timebin_reg[test$hour %in% 7:9 | test$hour %in% 17:19]="peak time"
test$timebin_reg[test$hour %in% 10:16| test$hour %in% 20:22]="moderate time"
test$timebin_reg[test$hour %in% 0:6 | test$hour == 23] = "off-peak time"

train$timebin_cas[train$hour %in% 11:18] ="peak time"
train$timebin_cas[train$hour %in% 8:10 | train$hour %in% 19:21]="moderate time"
train$timebin_cas[train$hour %in% 22:23 | train$hour %in% 0:8] = "off-peak time"                          
                            
test$timebin_cas[test$hour %in% 11:18] ="peak time"
test$timebin_cas[test$hour %in% 8:10 | test$hour %in% 19:21]="moderate time"
test$timebin_cas[test$hour %in% 22:23 | test$hour %in% 0:8] = "off-peak time"


                            
# Running regression for registered and casual users separately 

#converting to factors
train$season=as.factor(train$season)
train$holiday=as.factor(train$holiday)
train$workingday=as.factor(train$workingday)
train$weather=as.factor(train$weather)
train$year=as.factor(train$year)
train$timebin_reg=as.factor(train$timebin_reg)
train$timebin_cas=as.factor(train$timebin_cas)

test$season=as.factor(test$season)
test$holiday=as.factor(test$holiday)
test$workingday=as.factor(test$workingday)
test$weather=as.factor(test$weather)
test$year=as.factor(test$year)
test$timebin_reg=as.factor(test$timebin_reg)
test$timebin_cas=as.factor(test$timebin_cas)

fit_reg = lm(registered~ temp+humidity+timebin_reg+
    year+workingday+weather+season+holiday, data=train)
summary(fit_reg) 
#Will remove holiday variable since it is insignificant 
fit_reg = lm(registered~ temp+humidity+timebin_reg+
    year+workingday+weather+season, data=train)
summary(fit_reg) 
#Although model fit is decent at R squared adjusted = 0.60, 
#but the residual standard error is quite large at 94.98


fit_cas = lm(casual~ temp+humidity+timebin_cas
    +year+workingday+weather+season+holiday, data=train)
summary(fit_cas)
#Model fit is decent at R squared adjusted = 0.57 and 
#residual standard error is relatively lower than registered rents at 32.77
 

#Will plot componet+residual plots to see the relationship between predcitors and explanatory variables
crPlots(fit_reg)
crPlots(fit_cas)
#Since there are natural outliers, so 
#we will predict log of the dependent variable instead 


#Running log-linear regressions for registered and casual 

train$casual_log = log(train$casual+1) #Adding 1 to cope with zero rent values 
train$registered_log = log(train$registered+1)

fit_reg_log = lm(registered_log~ temp+humidity+timebin_reg+year+workingday+weather+season, data=train)
summary(fit_reg_log)

fit_cas_log = lm(casual_log~ temp+humidity+timebin_cas+year+workingday+weather+season+holiday, data=train)
summary(fit_cas_log)



#Making predictions

##Registered 
test$reg_log_prediction = predict(fit_reg_log,test)

##Casual
test$cas_log_prediction = predict(fit_cas_log,test)

#Retransforming logs 
test$registered=exp(test$reg_log_prediction)-1
test$casual=exp(test$cas_log_prediction)-1
test$count =test$registered + test$casual

#Exporting to excel and sending for submission to Kaggle
write.csv(data.frame(datetime=test$datetime,count=test$count),file="submit.csv",row.names=FALSE)

#Received Root Mean Squared Logarithmic Error from Kaggle 
RMSLE_regression=0.76714 #which is top 80% of the competition



#Random Forest Model

#Feature Engineering 

#Joining Data

# Combining both data sets to create bins 
train=train[,-c(17:18)]
test=test[,-c(17:18)]
data_all = rbind(train,test)

#Creating time(hour) bins based on decision trees
hour_reg=rpart(registered~hour,train)
fancyRpartPlot(hour_reg)

hour_cas=rpart(casual~hour,train)
fancyRpartPlot(hour_cas)

data_all$hour_reg = 0 
data_all$hour_reg[data_all$hour<7]=1
data_all$hour_reg[data_all$hour>=20]=2
data_all$hour_reg[data_all$hour>8 & data_all$hour <16]=3
data_all$hour_reg[data_all$hour == 7]=4
data_all$hour_reg[data_all$hour== 8]=5
data_all$hour_reg[data_all$hour== 18|data_all$hour==19]=6
data_all$hour_reg[data_all$hour== 16|data_all$hour==17]=7

data_all$hour_cas = 0
data_all$hour_cas[data_all$hour <=7]=1
data_all$hour_cas[data_all$hour == 8 | data_all$hour ==9]=2
data_all$hour_cas[data_all$hour >=20]=3
data_all$hour_cas[data_all$hour >=10 & data_all$hour <20]=4


#Creating temperature bins based on decision trees
temperature_reg=rpart(registered~temp,data=train)
fancyRpartPlot(temperature_reg)

temperature_cas=rpart(casual~temp,train)
fancyRpartPlot(temperature_cas)

data_all$temp_reg[data_all$temp<13]=1
data_all$temp_reg[data_all$temp>=13 & data_all$temp<23]=2
data_all$temp_reg[data_all$temp>=23 & data_all$temp<30]=3
data_all$temp_reg[data_all$temp>=30]=4

data_all$temp_cas[data_all$temp<15]=1
data_all$temp_cas[data_all$temp>=15 & data_all$temp<23]=2
data_all$temp_cas[data_all$temp>=23 & data_all$temp<30]=3
data_all$temp_cas[data_all$temp>=30]=4


#Creating humidity bins based on decision trees
humidity_reg=rpart(registered~humidity,data=train)
fancyRpartPlot(humidity_reg)

humidity_cas=rpart(casual~humidity,data=train)
fancyRpartPlot(humidity_cas)

data_all$humidity_reg[data_all$humidity>=66]=1
data_all$humidity_reg[data_all$humidity>=0 & data_all$humidity<=44]=2
data_all$humidity_reg[data_all$humidity>44 & data_all$humidity<66]=3

data_all$humidity_cas[data_all$humidity>=74]=1
data_all$humidity_cas[data_all$humidity>=56 & data_all$humidity<74]=2
data_all$humidity_cas[data_all$humidity<56 & data_all$humidity>=40]=3
data_all$humidity_cas[data_all$humidity<40]=4


#Creating Month/Year bins for regular and casual rents
data_all$time_bin[data_all$year=='2011']=1
data_all$time_bin[data_all$year=='2011' & data_all$month>3]=2
data_all$time_bin[data_all$year=='2011' & data_all$month>6]=3
data_all$time_bin[data_all$year=='2011' & data_all$month>9]=4
data_all$time_bin[data_all$year=='2012']=5
data_all$time_bin[data_all$year=='2012' & data_all$month>3]=6
data_all$time_bin[data_all$year=='2012' & data_all$month>6]=7
data_all$time_bin[data_all$year=='2012' & data_all$month>9]=8


#Creating "type of the day" variable 
data_all$type_day = ""
data_all$type_day[data_all$holiday ==0 & data_all$workingday==0]="weekend"
data_all$type_day[data_all$holiday==1]="holiday"
data_all$type_day[data_all$holiday==0 & data_all$workingday==1]="workday"
data_all$type_day=as.factor(data_all$type_day)


#Separating data back to train and test sets 
train=data_all[1:10886,]
test=data_all[10887:17379,]


#Making predictions 

#Transofrming dependent variable again 
train$registered_log = log(train$registered+1) #Adding 1 to cope with zero rent values 
train$casual_log = log(train$casual+1) 

#Registered
set.seed(100)
fit_reg_forest = randomForest(registered_log~hour+year+atemp+windspeed+holiday+workingday+weather+season+time_bin+
  temp_reg+hour_reg+type_day+humidity_reg,data=train,importance=TRUE,ntree=250)
test$log_reg_forest=predict(fit_reg_forest,test)

#Casual
set.seed(100)
fit_cas_forest = randomForest(casual_log~hour+year+atemp+windspeed+holiday+workingday+weather+season+time_bin+
  temp_cas+hour_cas+type_day+humidity_cas,data=train,importance=TRUE,ntree=250)
test$log_cas_forest=predict(fit_cas_forest,test)

#Retransforming logs
test$registered_forest=exp(test$log_reg_forest)-1
test$casual_forest=exp(test$log_cas_forest)-1
test$count_forest=test$casual_forest+test$registered_forest

#Exporting to excel and sending for submission to Kaggle
write.csv(data.frame(datetime=test$datetime,count=test$count_forest),file="submit.csv",row.names=FALSE)

#Received Root Mean Squared Logarithmic Error from Kaggle 
RMSLE_forest=0.40809 #which is top 10% of the competition 


#Conclusions

#Random forest outperformed linear regression with a big margin,
#and precition was in the top 10%(RMSLE_regression=0.76714) vs top %80(RMSLE_forest=0.40947) for regression 

#Further improvements to the model could be made by introducing conditional inferece forest or 
#by hyperparameter tuning in random forest model 

