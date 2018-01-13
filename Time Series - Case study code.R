
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(tseries)
install.packages("forecast")
#set working directory
setwd('D:/Work/SA Training/Time Series/Case Study Rossman')
train <- read.csv("train.csv")



#Step 1: Define scope
#Aggregate at date level to find out no. of stores operating every day
by_Date2 <- aggregate(train$Store, function(x) length(unique(x)), 
                      by = list(Group.date = train$Date))
plot(by_Date2, type="l")

#Find out the stores with missing data
unique(by_Date2$x)
by_Date2[by_Date2$x==1114,]

all_stores <- unique(train$Store)
stores_reporting <- train$Store[train$Date == "2013-01-01"]
missing_stores <- all_stores[!(all_stores %in% stores_reporting)]

#remove the stores from the data
train <- train[!(train$Store==988),]

#repeat again
by_Date2[by_Date2$x==935,]
stores_reporting <- train$Store[train$Date == "2014-07-01"]
missing_stores <- all_stores[!(all_stores %in% stores_reporting)]


# for (date in seq(as.Date("2014-7-2"),as.Date("2014-12-31"),by="day")) {
#  stores_reporting <- train$Store[train$Date == date]
#  missing_on_date <- all_stores[!(all_stores %in% stores_reporting)]
#  if (length(setdiff(missing_on_date,missing_stores)) > 0) {
#    cat("Date:",date," Difference in missing stores",setdiff(missing_on_date,missing_stores))
#  }
# }

#filter out missing stores
train2<-train[!(train$Store %in% missing_stores),]

#convert factor to date
str(train2)
train2$Date <- as.Date(train2$Date)

test2 <- train2[train2$Date>='2015-06-01',]
train2 <- train2[!(train2$Date>='2015-06-01'),]

#check again
by_Date2 <- aggregate(train2$Store, function(x) length(unique(x)), by = list(Group.date = train2$Date))
 #ggplot(by_Date2, aes(Group.date,x)) + geom_line()
plot(by_Date2, type="l")


#train <- train2
#test <- test2


#step 2: Data exploration
summary(train2)

# Are all the test stores are also in the train data
sum(unique(test2$Store) %in% unique(train2$Store)) 

#test vs train data
table(train2$Open) / nrow(train2) # Percent Open Train
table(test2$Open) / nrow(test2) # Percent Open Test 
table(train2$Promo) / nrow(train2) # Percent of the time promo in train
table(test2$Promo) / nrow(test2) # Percent of the time promo in test
table(train2$StateHoliday) / nrow(train2) # Percent of the time holiday in train
table(test2$StateHoliday) / nrow(test2) # Percent of the time holiday in test
table(train2$SchoolHoliday) / nrow(train2) # Percent of the time school holiday in train
table(test2$SchoolHoliday) / nrow(test2) # Percent of the time school holiday in test


#Let's look at the distribution of the variables and average values:
hist(train2$Sales, 100)
hist(aggregate(train2[train2$Sales != 0,]$Sales,
               by = list(train2[train2$Sales != 0,]$Store),mean)$x, 100, 
     main = "Mean sales per store when store was not closed")


hist(train2$Customers, 100)
 hist(aggregate(train2[train2$Sales != 0,]$Customers, 
               by = list(train2[train2$Sales != 0,]$Store), mean)$x, 100,
     main = "Mean customers per store when store was not closed")

#Let's plot the variables
ggplot(train2[train2$Sales != 0,], aes(x = factor(SchoolHoliday), y = Sales)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

ggplot(train2[train2$Sales != 0 & train2$Customers != 0,],
       aes(x = Customers, y = Sales)) + 
  geom_point(alpha = 0.2) + geom_smooth()

ggplot(train2[train2$Sales != 0 & train2$Customers != 0,],
       aes(x = factor(Promo), y = Sales)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

ggplot(train2[train2$Sales != 0 & train2$Customers != 0,],
       aes(x = factor(Promo), y = Customers)) + 
  geom_jitter(alpha = 0.1) +
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

#I chose to not plot that data including days with 0 sales 
#or customers because that would have biased the boxplots.

#Sales is as expected strongly correlated with the number of customers.
#It looks like the Boxplots of customers overlap a little more than the boxplots
#of sales. This would mean that the promos are not mainly attracting more customers
#but make customers spend more

#What is the effect of promotions
with(train2[train2$Sales != 0 & train2$Promo == 0,], mean(Sales / Customers))
with(train2[train2$Sales != 0 & train2$Promo == 1,], mean(Sales / Customers))
#The mean amount spent per customer is about one Euro higher


table(ifelse(train2$Sales != 0, "Sales > 0", "Sales = 0"),
      ifelse(train2$Promo, "Promo", "No promo"))
#There are sometimes promos while the respective store is closed and there are promos
#38% of the time:

table(ifelse(train2$Open == 1, "Opened", "Closed"),
      ifelse(train2$Sales > 0, "Sales > 0", "Sales = 0"))
#At least there are no sales when the stores are closed but there are some stores
#that, according to the data, made no sales although they were opened even if 
#they had some customers. These observations *may* be errors in the data / outliers. What to do with this?

train2$Open <- ifelse(train2$Open==1 & train2$Sales==0,0,train2$Open)
sum(train2$Open)


ggplot(train[train$Sales != 0,],
       aes(x = factor(DayOfWeek), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)
#The variability of sales on sundays is quite high while the median is not:



#step3: First we will try to predict the sales only for one store
#train2$logSales <- log1p(train2$Sales)
strn <- train2[train2$Store == 11,]
strn <- strn[order(strn$Date),]
plot(strn$Sales, type="l")
Sales <- ts(strn$Sales,start=2013,freq=365)
plot(Sales)

seasonplot(Sales, col=c("red", "green", "yellow"))

#step4: lag analysis
lag.plot(strn$Sales, lags=9, do.lines=FALSE)
acf(strn$Sales)

#step5 build baseline

testdata<- test2[test2$Store == 11,]
forecast1 <- meanf(Sales,61) #weighted mean
forecast2 <- naive(Sales,61)
# forecast3 <- rwf(strn$Sales,61)
# summary(forecast1)
accuracy(forecast1,testdata[order(testdata$Date),4])
accuracy(forecast2,testdata[order(testdata$Date),4])
# accuracy(forecast3,testdata[order(testdata$Date),4])


#step6 see if any transformation of ts variable is required to simplify the pattern. 
#box cox transformation uses both power and log transformations. If value of lambda is
#0 then it is log transformation else it is (sales to the power of lambda -1)/lambda
# The BoxCox.lambda() function will help choose a value of lambda.
plot(strn$Sales,type="l")
plot(log(strn$Sales),type="l")
lambda <- BoxCox.lambda(strn$Sales)
plot(BoxCox(strn$Sales,lambda),type="l")
Sales <- ts(BoxCox(strn$Sales,lambda),start=2013,freq=365)
#step7 use decomposition to studying time series data, and exploring the historical changes over time
#STL means Seasonal Trend Decomposition using Loess,The two main parameters to be chosen when using STL are the trend window (t.window) and seasonal window (s.window). 
#These control how rapidly the trend and seasonal components can change. Small values allow more rapid change.
decom<-stl(Sales, t.window = 365, s.window="periodic")
plot(decom)

######################################################################################
######################################################################################
#################   Using Auto ARIMA for modelling   #################################
######################################################################################
######################################################################################

#step 7: now run the auto arima to get a sense of orders (for univariate)
fit <- auto.arima(strn$Sales,seasonal=TRUE)
summary(fit)
accuracy(forecast(fit))
plot(forecast(fit,h=61))
#This model doesn't seem to be doing well

#Step 8: Use regressors and build the model again (for multivariate)
reg_vars <- cbind(DayOfWeek = strn$DayOfWeek , 
              Open = strn$Open,
              Promo = strn$Promo,
              StateHoliday = strn$StateHoliday,
              SchoolHoliday = strn$SchoolHoliday
              
)

fit <- auto.arima(strn$Sales,xreg=reg_vars)
summary(fit)
accuracy(forecast(fit))

#When you forecast, then the regressors should come from test data
newxreg <- cbind(DayOfWeek = testdata$DayOfWeek , 
              Open = testdata$Open,
              Promo = testdata$Promo,
              StateHoliday = testdata$StateHoliday,
              SchoolHoliday = testdata$SchoolHoliday
              
)

plot(forecast(fit,h=61,xreg=newxreg))

#step 9: check the residuals
acf(residuals(fit))

#step 10: Calculate MAPE
#DO IT YOURSELF :)

######################################################################################
######################################################################################
#################   Build your own model using ARIMA   ###############################
######################################################################################
######################################################################################


#step 11: check the stationarity of the series
# The stationarity /non-stationarity of the data can be known by applying Unit Root Tests 
# - augmented Dickey-Fuller test (ADF), Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test.
# The null-hypothesis for an ADF test is that the data are non-stationary.
# So large p-values are indicative of non-stationarity, and small p-values suggest 
# stationarity. Using the usual 5% threshold, differencing is required if the p-value 
# is greater than 0.05.
adf = adf.test(Sales)
adf

# Another popular unit root test is the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) 
# test. This reverses the hypotheses, so the null-hypothesis is that the data are stationary. 
# In this case, small p-values (e.g., less than 0.05) suggest that differencing is required.
kpss = kpss.test(Sales)
kpss

#step 12: If the series is non stationary, find differencing order and do differencing
ndiffs(Sales) #find the order
diff(Sales) #DO the differencing

#step 13: run acf and pacf to get idea of orders
acf(strn$Sales)
pacf(strn$Sales)


#step 14: Build your own model
fit <- arima(strn$Sales,order= c(3,0,4), xreg=reg_vars)
summary(fit)
plot(forecast(fit,h=61,xreg=newxreg))

#step 15: check the residuals
acf(residuals(fit))

#step 16: Check AIC and BIC to get the best values of p,d,q and finalize best 5 models
#DO IT YOURSELF

#step 17: Calculate MAPE to find out the best model for your data
#DO IT YOURSELF


#Step 18: Build the model for all the stores and save the output in a dataframe

