# Libraries
library(forecast)
library(zoo)
library(TTR)



# Load the dataset from a CSV file containing truck sales data
sales.data <- read.csv("Truck Sales.csv")

# Display the first 6 rows of the dataset to understand its structure
head(sales.data)

# Time Series Data
# Convert the "Trucks.Sold" column to a time series object
# - 'start' defines the starting year and month (January 2003)
# - 'end' defines the ending year and month (December 2014)
# - 'freq' indicates monthly frequency (12 months per year)
sales.ts <- ts(sales.data$Trucks.Sold, start = c(2003, 1), end = c(2014, 12), freq = 12)

# Print the time series data to confirm the conversion
sales.ts


# Fit a linear trend model to the sales time series using tslm
sales.lin <- tslm(sales.ts ~ trend)

# Plot the original sales data (time series) with custom axis labels and titles
plot(sales.ts, 
     xlab = "Time", ylab = "Number of Trucks Sold", ylim = c(0, 1200), xaxt = 'n', main = "Sales of Trucks")

# Add the fitted linear trend line to the plot
# 'sales.lin$fitted' contains the fitted values from the linear model
lines(sales.lin$fitted, lwd = 2, col = "blue")
axis(1, at = seq(2003, 2014, 1), labels = format(seq(2003, 2014, 1)))

#Time Series decomposed components (seasonality, trend, and remainder)
#This function decomposes the time series (sales.ts) into -
# - three components: seasonal, trend, and remainder
salesdata.stl <- stl(sales.ts, s.window = "periodic")
autoplot(salesdata.stl, main = "decomposed components (seasonality, trend, and remainder) for Truck Sales")

#This function computes and plots the - 
# - autocorrelation for the given time series data
# 'lag.max = 12' specifies that the autocorrelation -
# - should be calculated for up to 12 lags (one for each month of the year)
autocorrelation <- Acf(sales.ts, lag.max = 12, main = "Autocorrelation for Sales of Trucks")


#AR(1) model (Autoregressive model of order 1) to the truck sales time series data
#This AR(1) model helps analyze the relationship between past sales and current sales
slaes.ar1<- Arima(sales.ts, order = c(1,0,0))
summary(slaes.ar1)

#Z-test to test the null hypothesis 
#that beta coefficient of AR(1) is equal to 1(i.e data is random walk ot not )

ar1 <- 0.9494
s.e. <- 0.0258
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

#Compute the first difference of the truck sales time series data
diff.sales <- diff(sales.ts, lag = 1)
#Acf() function to identify autocorrealtion for first differenced
Acf(diff.sales, lag.max = 12, main = "Autocorrelation for Differenced Truck Sales")

# Define partition sizes
nValid <- 36 # 36 periods for validation
nTrain <- length(sales.ts) - nValid # Remaining for training

# Create training and validation partitions
train.ts <- window(sales.ts, start = c(2003, 1), end = c(2003, nTrain))
valid.ts <- window(sales.ts, start = c(2003, nTrain + 1), end = c(2003, nTrain + nValid))
train.ts
valid.ts



# Forecasting sales in validation partition
# Model 1: Two-level forecast with linear trend and seasonality and trailing MA forecast
# # Use tslm() function to create linear trend and seasonal model
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred

# Regression residuals in the training period
trend.seas.res <- trend.seas$residuals
trend.seas.res

#Trailing MA for training residuals
ma.trail.res <- rollmean(trend.seas.res, k = 12, align = "right")
ma.trail.res

# Trailing MA forecast validation residuals 
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

# Two-level forecast for the validation period 
#(regression and trailing MA forecast for residuals)
fst.two.level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.two.level

# Data table with validation data, regression forecast, trailing MA forecast for residuals, and two-level forecast in validation period)
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, ma.trail.res.pred$mean, fst.two.level), 3)
names(valid.df) <- c("Sales", "Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
valid.df

round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.two.level, valid.ts), 3)

# Original data and regression forecast for training and validation partitions
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1200), bty = "l", xlim = c(2002, 2017), xaxt = "n", main = "Regression Forecast in Training and Validation Partitions") 
axis(1, at = seq(2002, 2017, 1), labels = format(seq(2002, 2017, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,84000, legend = c("Sales Data", "Regression Forecast, Training Partition", "Regression Forecast, Validation Partition"), col = c("black", "blue", "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")



# Model 2: Holt Winter's Model

# Holt-Winter’s (HW) model with automated selection of error, trend, and seasonality options, and automated selection of smoothing parameters for the training partition
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ
summary(hw.ZZZ)

# Forecast monthly sales for the validation period 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

# Original data and regression forecast for training and validation partitions
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1200), bty = "l", xlim = c(2003, 2015.25), xaxt = "n", main = "Holts winter model for Regression Forecast in Training \n and Validation Partitions") 
axis(1, at = seq(2003, 2015.25, 1), labels = format(seq(2003, 2015.25, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2, lty = 1)
lines(hw.ZZZ.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(1992,84000, legend = c("Sales Data", "Regression Forecast, Training Partition", "Regression Forecast, Validation Partition"), col = c("black", "blue", "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Model 3: Quadratic trend and seasonality model

# Regression model with quadratic trend and seasonality 
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)
summary(train.quad.season)

# Forecast
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

# Original data and regression forecast for training and validation partitions.
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1200), bty = "l", xlim = c(2003, 2015.25), xaxt = "n", main = "Regression model with quadratic trend and seasonality \n to Forecast in Training and Validation Partitions") 
axis(1, at = seq(2003, 2015.25, 1), labels = format(seq(2003, 2015.25, 1)))
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2, lty = 1)
lines(train.quad.season.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(2003,84000, legend = c("Sales Data", "Regression Forecast, Training Partition", "Regression Forecast, Validation Partition"), col = c("black", "blue", "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Model 4: Auto Arima model

train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
# Forecast for validation period  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Original data and regression forecast for training and validation partitions
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1200), bty = "l", xlim = c(2003, 2015.25), xaxt = "n", main = "Auto ARIMA model for Regression Forecast \n in Training and Validation Partitions") 
axis(1, at = seq(2003, 2015.25, 1), labels = format(seq(2003, 2015.25, 1)))
lines(train.auto.arima.pred$fitted, col = "blue", lwd = 2, lty = 1)
lines(train.auto.arima.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(2003,84000, legend = c("Sales Data", "Regression Forecast, Training Partition", "Regression Forecast, Validation Partition"), col = c("black", "blue", "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Accuracy measures for all models in the validation period
round(accuracy(fst.two.level, valid.ts), 3)
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)


# For entire dataset 

# Model 1: Two-level forecast

# Regression model with linear trend and seasonality for entire data set 
tot.trend.seas <- tslm(sales.ts ~ trend  + season)
summary(tot.trend.seas)
# Forecast
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 24, level = 0)
tot.trend.seas.pred

# Regression residuals for entire data set
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 12, align = "right")
tot.ma.trail.res

# Forecast for trailing MA residuals for future 24 periods
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 24, level = 0)
tot.ma.trail.res.pred

# Two-level forecast for future 24 periods (regression and trailing MA for residuals for future 24 periods)
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Data table with regression forecast, trailing MA for residuals, and total forecast for future 24 periods
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# Plot original and forecasted Sales time series data for 2 level forecast model
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1200), bty = "l", xlim = c(2003, 2017.25), lwd =1, xaxt = "n", main = "Sales Data and Regression with Trend and Seasonality") 
axis(1, at = seq(2003, 2017.25, 1), labels = format(seq(2003, 2017.25, 1)))
lines(tot.trend.seas.pred$fitted, col = "blue", lwd = 2)
lines(tot.trend.seas.pred$mean, col = "blue", lty =5, lwd = 2)
legend(2003,94000, legend = c("Sales", "Regression", "Regression Forecast for Future Periods"), col = c("black", "blue" , "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Model 2: HW model

# HW model for the model with the automated selection of error, trend, and seasonality options, and automated selection of smoothing parameters
HW.ZZZ <- ets(sales.ts, model = "ZZZ")
HW.ZZZ 
summary(HW.ZZZ)

# Forecast monthly sales in the 12 months of 2015,2016
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 24 , level = 0)
HW.ZZZ.pred

# Original and forecasted sales time series data for HW model
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1300), bty = "l", xlim = c(2003, 2017.25), lwd =1, xaxt = "n", main = "Sales Forecast for future 24months \n using Holt-Winter's Model") 
axis(1, at = seq(2003, 2017.25, 1), labels = format(seq(2003, 2017.25, 1)))
lines(HW.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(HW.ZZZ.pred$mean, col = "blue", lty =5, lwd = 2)
legend(2003,94000, legend = c("Sales", "Regression", "Regression Forecast for Future Periods"), col = c("black", "blue" , "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")


# Model 3: Quadratic trend and seasonality

# Regression model with quadratic trend and seasonality
quad.season <- tslm(sales.ts ~ trend + I(trend^2) + season)
summary(quad.season)
# Forecast
quad.season.pred <- forecast(quad.season, h = 24, level = 0)
quad.season.pred

# Original and forecasted sales time series data for quadratic trend and seasonality model
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1300), bty = "l", xlim = c(2003, 2017.25), lwd =1, xaxt = "n", main = "Sales Forecast for future 24months \n using Quadratic Trend and Seasonality Model") 
axis(1, at = seq(2003, 2017.25, 1), labels = format(seq(2003, 2017.25, 1)))
lines(quad.season.pred$fitted, col = "blue", lwd = 2)
lines(quad.season.pred$mean, col = "blue", lty =5, lwd = 2)
legend(2003,94000, legend = c("Sales", "Regression", "Regression Forecast for Future Periods"), col = c("black", "blue" , "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")




# Model 4: Auto Arima model

# Auto ARIMA for the entire data set
auto.arima <- auto.arima(sales.ts)
summary(auto.arima)

# Forecast
auto.arima.pred <- forecast(auto.arima, h = 24, level = 0)
auto.arima.pred

# Original and forecasted sales time series data for Auto ARIMA model
plot(sales.ts, xlab = "Time", ylab = "Sales", ylim = c(0, 1200), bty = "l", xlim = c(2003, 2017.25), lwd =1, xaxt = "n", main = "Sales Data and Regression with Auto ARIMA Model") 
axis(1, at = seq(2003, 2017.25, 1), labels = format(seq(2003, 2017.25, 1)))
lines(auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "blue", lty =5, lwd = 2)
legend(2003,94000, legend = c("Sales", "Regression", "Regression Forecast for Future Periods"), col = c("black", "blue" , "blue"), lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")



# Accuracy measures for models for the entire data set
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, sales.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, sales.ts), 3)
round(accuracy(quad.season.pred$fitted, sales.ts),3)
round(accuracy(auto.arima.pred$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)