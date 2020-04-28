#setting working directory
getwd()
setwd("D:/Folders/R/Time Series")

#installing and importing libraries
install.packages("tseries")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("rlang")
install.packages("forecast")
install.packages("zoo")
install.packages("urca")
install.packages("xts")
install.packages("rlang")

library(tseries)
library(dplyr)
library(ggplot2)
library(corrplot)
library(forecast)
library(zoo)
library(roll)
library(urca)
library(xts)
library(rlang)

#DATA IMPORT AND CLEANING
#importing data - taken from https://www.kaggle.com/andreazzini/international-airline-passengers/data
#fileencoding allows to read the characters as correctly as they would appear on the raw dataset
#na.strings allows to replace blanks with NAs
IntlAirPassengers4 <- read.csv("D:/Folders/R/Time Series/international-airline-passengers.csv",fileEncoding="UTF-8-BOM",na.strings = '..', stringsAsFactors =FALSE)

#data cleaning
IntlAirPassengers3 <- IntlAirPassengers4 %>%  rename(monthly_passengers = International.airline.passengers..monthly.totals.in.thousands..Jan.49...Dec.60,
                                                     month_old = Month) 

IntlAirPassengers2 <- IntlAirPassengers3[-c(145),]

#converting to datetime
IntlAirPassengers2$Month <- as.Date(paste(IntlAirPassengers2$month_old,"-01",sep=""))
#removing unwanted rows
IntlAirPassengers <- IntlAirPassengers2[,-c(1)]
IntlAirPassengers

# CHECKING THE DATA - MEASURE OF CENTRAL TENDENCY AND DATA TYPES
### list objects in the working environment
ls(IntlAirPassengers)

### list the structure of IntlAirPassengers
str(IntlAirPassengers)

### dimensions of an object
dim(IntlAirPassengers)

### class of an object (numeric, matrix, data frame, etc)
class(IntlAirPassengers)

### print first 10 rows of mydata
head(IntlAirPassengers, n=10)
summary(IntlAirPassengers)

#printing max and min time period
strtdt <- min(IntlAirPassengers$Month)
enddt <- max(IntlAirPassengers$Month)
cat("Time period start: ", as.character(strtdt))
cat("Time period end: ", as.character(enddt))

#DATA VISUALIZATION FOR INSPECTION
#in case there are multiple columns, we may need to combine them to see a boxplot
#df2 = df[,c(1,3,4,6,7,9,10)] %>%   # select relevant columns 
# pivot_longer(c(2,3,4,5,6,7),names_to = 'Score')
#view(df2)

#barplot
ggplot(data = IntlAirPassengers, mapping = aes(x=IntlAirPassengers$monthly_passenger)) + 
  geom_histogram(aes(y=..density..),fill="blue",color="white",alpha=0.7,binwidth=50) + 
  geom_density() +
  labs(x='# passengers') +
  theme_minimal()

#boxplot by years
#Median values across years confirms an upwards trend
#Steady increase in the spread, or middle 50% of the data (boxes) over time
#A model considering seasonality might work well
# Boxplot across the months will give us a sense of the seasonal effects
year <- as.numeric(format(IntlAirPassengers$Month,'%Y'))
boxplot(IntlAirPassengers$monthly_passengers~year, col=year)

monthly <- as.numeric(format(IntlAirPassengers$Month,'%m'))
boxplot(IntlAirPassengers$monthly_passengers~monthly, col=monthly)

# checking normal distribution. data is right skewed, not perfectly Gaussian (normal), transformations might be helpful
x <- IntlAirPassengers$monthly_passengers
y <- dnorm(IntlAirPassengers$monthly_passengers, mean = mean(IntlAirPassengers$monthly_passengers), sd = sd(IntlAirPassengers$monthly_passengers))
plot(x,y)


#CHECKING THE DATA FREQUENCY
frequency(IntlAirPassengers$monthly_passengers) #The cycle of this time series is 1

#DECOMPOSING THE DATA TO CHECK THE TREND, SEASONALITY AND RANDOM NOISE
# Seasonal: Patterns that repeat with a fixed period of time. For example, a website might receive more visits during weekends; this would produce data with a seasonality of 7 days.
# Trend: The underlying trend of the metrics. A website increasing in popularity should show a general trend that goes up.
# Random: Also call "noise", "irregular" or "remainder," this is the residuals of the original time series after the seasonal and trend series are removed.
# Additive decomposition = Seasonal + Trend + Random
# Multiplicative decomposition = Seasonal * Trend * Random

# TO DETECT TREND

ts_air <- ts(IntlAirPassengers$monthly_passengers, frequency = 12)
decompose_air <- decompose(ts_air, "multiplicative")
par(mfrow=c(1,1))
plot(as.ts(decompose_air$seasonal), main = "Seasonality", type="l", col="blue")
plot(as.ts(decompose_air$trend), main = "Trend", type="l", col="blue")
plot(as.ts(decompose_air$random), main = "Randomness", type="l", col="blue")

# Line graph
plot(IntlAirPassengers$Month,IntlAirPassengers$monthly_passengers, type="l", col="blue", xlab = "time", ylab = "# passengers", main = "Airline passengers over time")
par(mfrow=c(1,1))
#Stationarity
#A Time Series is said to be stationary if its statistical properties such as mean, variance remain constant over time.
#Most of the Time Series models work on the assumption that the TS is stationary. Major reason for this is that there are many ways in which a series can be non-stationary, but only one way for stationarity.
#Intuitively, we can say that if a Time Series has a particular behaviour over time, there is a very high probability that it will follow the same in the future.
#Also, the theories related to stationary series are more mature and easier to implement as compared to non-stationary series.

#We can check stationarity using the following:
  
#ACF and PACF plots: If the time series is stationary, the ACF/PACF plots will show a quick drop-off in correlation after a small amount of lag between points.
#Plotting Rolling Statistics: We can plot the moving average or moving variance and see if it varies with time. Moving average/variance is for any instant 't', the average/variance of the last year, i.e. last 12 months.
#Augmented Dickey-Fuller Test: This is one of the statistical tests for checking stationarity. Here the null hypothesis is that the TS is non-stationary. The test results comprise of a Test Statistic and some Critical Values for difference confidence levels. 
#If the 'Test Statistic' is less than the 'Critical Value', we can reject the null hypothesis and say that the series is stationary. Refer this article for details.



#ACF and PACF plots
#Let's review the Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF) plots
#If the time series is stationary, the ACF/PACF plots will show a quick drop-off in correlation after a small amount of lag between points.
#This data is non-stationary as a high number of previous observations are correlated with future values.
#Confidence intervals are drawn as a cone.
#By default, this is set to a 95% confidence interval, suggesting that correlation values outside of this code are very likely a correlation and not a statistical fluke.
#The partial autocorrelation at lag k is the correlation that results after removing the effect of any correlations due to the terms at shorter lags.

# ACF and PACF Plots
acf(IntlAirPassengers$monthly_passengers)
pacf(IntlAirPassengers$monthly_passengers)


#Clearly, the decay of ACF chart is very slow, which means that the population is not stationary.


#Plotting Rolling Statistics
#We observe that the rolling mean and Standard deviation are not constant with respect to time (increasing trend)
#The time series is hence not stationary

rolling_mean <- rollmean(IntlAirPassengers$monthly_passengers, k = 12, fill = NA)
rolling_sd <- rollapply(IntlAirPassengers$monthly_passengers, width = 12, FUN = sd, fill = NA)

plot(IntlAirPassengers$Month,IntlAirPassengers$monthly_passengers, type="l", col="blue", xlab = "time", ylab = "# passengers", main = "Rolling Statistics",ylim=c(0,600)) 
lines(IntlAirPassengers$Month,rolling_mean, type="l", col="green") 
lines(IntlAirPassengers$Month,rolling_sd, type="l", col="red") 

#Augmented Dickey-Fuller Test
#The intuition behind the test is that if the series is integrated then the lagged level of the series y(t-1) will provide no relevant information in predicting the change in y(t).
#Null hypothesis: The time series is not stationary
#Rejecting the null hypothesis (i.e. a very low p-value) will indicate staionarity


adf.test(IntlAirPassengers$monthly_passengers, alternative = c("stationary", "explosive"))


# Inferences from above graphs
# The year on year trend clearly shows that the #passengers have been increasing without fail.
# The variance and the mean value in July and August is much higher than rest of the months.
# Even though the mean value of each month is quite different their variance is small. Hence, we have strong seasonal effect with a cycle of 12 months or less.


#Making Time Series Stationary
#There are 2 major reasons behind non-stationaruty of a TS:
  
#Trend - varying mean over time. For eg, in this case we saw that on average, the number of passengers was growing over time.
#Seasonality - variations at specific time-frames. eg people might have a tendency to buy cars in a particular month because of pay increment or festivals.

#FUNCTION FOR ERROR CALCULATIONS

errorcalc <- function(z.chart, z.calc) {
  RMSE = sqrt(mean((z.chart - z.calc)^2))
  MPE  = sum((z.calc - z.chart) / z.chart) * 100 / n()
  MAPE = sum(abs((z.calc - z.chart) / z.chart)) * 100 / n()
  MSE  = sum((z.calc - z.chart)^2) / n()
  RSS  = sum((z.calc - z.chart)^2)
  MAE  = sum(abs(z.calc - z.chart)) / n()
  
  print("The Mean Squared Error is: ", MSE)
  print("The Root Mean Squared Error is: ", RMSE)
  print("The Mean Percentage error is: ", MPE)
  print("The Mean Absolute Percentage Error is: ", MAPE)
  print("The Root Mean Squared Error is: ", RSS)
  print("The Mean Absolute Error is: ", MAE)
  
}

#Transformations
#We can apply transformation which penalize higher values more than smaller values. These can be taking a log, square root, cube root, etc. Lets take a log transform here for simplicity:

tslog <- IntlAirPassengers
tslog$monthly_passengers <- log(IntlAirPassengers$monthly_passengers)
ts_log <- xts(tslog$monthly_passengers, order.by = tslog$Month)

plot(as.xts(ts_log)) 


#Other possible transformations:
#  Exponential tranformation
#Box Cox transformation
#Square root transformation

#Techniques to remove Trend - Smoothing
#Smoothing is taking rolling averages over windows of time

#Moving Average
#We take average of 'k' consecutive values depending on the frequency of time series.
#Here we can take the average over the past 1 year, i.e. last 12 values.
#A drawback in this particular approach is that the time-period has to be strictly defined.

moving_avg <- rollmean(ts_log, k = 12,fill = NA)
plot(as.xts(ts_log), type="l", col="blue", xlab = "time", ylab = "# passengers(log transform)") 
lines(as.xts(moving_avg), type="l", col="red")


ts_log_moving_avg_diff<- na.omit(ts_log-moving_avg)

rolling_mean_test <- rollmean(ts_log_moving_avg_diff, k = 12,fill = NA)
rolling_sd_test <- rollapply(ts_log_moving_avg_diff, width = 12, FUN = sd,fill = NA)

plot(as.xts(ts_log_moving_avg_diff), type="l", col="blue", xlab = "time", ylab = "# passengers", main = "Rolling Statistics") 
lines(as.xts(rolling_mean_test), type="l", col="green") 
lines(as.xts(rolling_sd_test), type="l", col="red") 

adf.test(ts_log_moving_avg_diff, alternative = c("stationary", "explosive"))

# ACF and PACF Plots
par(mfrow=c(2,1))
acf(ts_log_moving_avg_diff)
pacf(ts_log_moving_avg_diff)
par(mfrow=c(1,1))


#Further Techniques to remove Seasonality and Trend
#The simple trend reduction techniques discussed before don't work in all cases, particularly the ones with high seasonality.
#Differencing
#In this technique, we take the difference of the observation at a particular instant with that at the previous instant.

#newdf2 is the xts version of the initial data
ts_log_diff <- diff(ts_log)

plot(ts_log_diff)

rolling_mean_test2 <- rollmean(ts_log_diff, k = 12, fill = NA)
rolling_sd_test2 <- rollapply(ts_log_diff, width = 12, FUN = sd, fill = NA)

plot(ts_log_diff, type="l", col="blue", xlab = "time", ylab = "# passengers", main = "Rolling Statistics") 
lines(rolling_mean_test2, type="l", col="green") 
lines(rolling_sd_test2, type="l", col="red") 

ts_log_diff_na <- na.omit(ts_log_diff)
par(mfrow=c(2,1))
acf(ts_log_diff_na)
pacf(ts_log_diff_na)
par(mfrow=c(1,1))

adf.test(ts_log_diff_na, alternative = c("stationary", "explosive"), k=1) #p-value <0.05, hence data is stationary


#Autoregression (AR)
#The autoregression (AR) method models the next step in the sequence as a linear function of the observations at prior time steps.
#Number of AR (Auto-Regressive) terms (p): p is the parameter associated with the auto-regressive aspect of the model, which incorporates 
#past values i.e lags of dependent variable. For instance if p is 5, the predictors for x(t) will be x(t-1)..x(t-5).

model_ar <- ar(ts_log,aic = TRUE)
plot(model_ar$resid)
ts.plot(2.78^ts_log)
AR_fit <- (2.78^ts_log) - residuals(model_ar)
points(AR_fit, type = "l", col = 2, lty = 2)

plot(newdf$Month,newdf$monthly_passengers)
plot(2.718^newdf2)
plot(2.718^pred$pred)

model_ar <- ar.ols(x = ts_log_diff2, order.max = 1, demean = F, intercept = T)
(fit2 <- arima(ts_log_diff, c(1, 0, 0),seasonal = list(order = c(1, 0, 0), period = 12)))


# ACF Plots

acf(log(IntlAirPassengers$monthly_passengers))

#Clearly, the decay of ACF chart is very slow, which means that the population is not stationary.

(fit <- arima(ts_log_diff2, c(1, 0, 0),seasonal = list(order = c(1, 0, 0), period = 12)))
(fit <- arima(ts_log_diff2, c(1, 1, 0),seasonal = list(order = c(1,1,0), period = 12)))
(fit <- arima(ts_log_diff2, c(1, 1, 1),seasonal = list(order = c(1,1,1), period = 12)))
(fit <- arima(ts_log_diff2, c(1, 0, 1),seasonal = list(order = c(1,0,1), period = 12)))
(fit <- arima(ts_log_diff2, c(2, 1, 0),seasonal = list(order = c(2,1,0), period = 12)))

ts.plot(2.78^ts_log)
AR_fit <- (2.78^ts_log) - residuals(fit)
points(AR_fit, type = "l", col = 2, lty = 2)

pred <- predict(fit, n.ahead = 10*12)

ts.plot(2.718^ts_log_diff2,2.718^pred$pred, log = "y", lty = c(1,3))

