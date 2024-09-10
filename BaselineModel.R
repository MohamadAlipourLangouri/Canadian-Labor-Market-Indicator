
library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(rio)
library(zoo)



util_ds <- subset(Completed_data_filled_forecast ,
                  Completed_data_filled_forecast$NAICS == "Utilities [22]")
util_ds$Date <- as.Date(util_ds$Date)

util_ds$Year <- year(util_ds$Date)
util_ds$Month <- month(util_ds$Date, label = TRUE)

util_ds$Index2 <- paste(util_ds$Year , util_ds$Month)
str(util_ds)


# create a time series object based on Job Vacancy Value
val_TSobject <- ts(util_ds[,c('Job_vacancies_Value')])

#tsclean function to ID and replace outliers and input missing values if any
util_ds$clean_val = tsclean(val_TSobject)


#plot the cleaned data
ggplot()+
  geom_line(data = util_ds , aes(x = Date , y = clean_val)) + 
  ylab('Total Job vacancy Values')


# get a monthly and weekly moving averages to compare with cleaned data with monthly Value

util_ds$val_ma30 = ma(util_ds$Job_vacancies_Value, order = 30) # 30 month?
util_ds$val_ma7 = ma(util_ds$Job_vacancies_Value , order = 7) # 7 month?
util_ds$val_ma3 = ma(util_ds$Job_vacancies_Value , order = 3) # seasonal I guess

summary(util_ds) # note the NA values

#using na.aggregate function
util_ds2 <- replace(util_ds , TRUE,lapply(util_ds , na.aggregate))
summary(util_ds2)

#plot 4 different plots of moving averages and original values

#plot the uncleaned data
ggplot()+
  geom_line(data = util_ds , aes(x = Date , y = Job_vacancies_Value, colour = "Total Values")) +
  geom_line(data = util_ds , aes(x = Date , y = val_ma30 , colour = "Monthly Move")) +
  geom_line(data = util_ds , aes(x = Date , y = val_ma7 , colour = "weekly Move")) +
  geom_line(data = util_ds , aes(x = Date , y = val_ma3 , colour = "3 month or day Move")) +
  ylab('Total Job vacancy Values')

#plot the cleaned data
ggplot()+
  geom_line(data = util_ds2 , aes(x = Date , y = clean_val, colour = "Total Values")) +
  geom_line(data = util_ds2 , aes(x = Date , y = val_ma30 , colour = "Monthly Move")) +
  geom_line(data = util_ds2 , aes(x = Date , y = val_ma7 , colour = "weekly Move")) +
  geom_line(data = util_ds2 , aes(x = Date , y = val_ma3 , colour = "3 month or day Move")) +
  ylab('Total Job vacancy Values')



#Decomposition of the data - take Seasonality, trend, cycle into account
value_ma = ts(na.omit(util_ds2$Job_vacancies_Value) , frequency = 12)


#seasonal adjustment the time series
decomp = stl(value_ma ,  "periodic")
deseasonal_val <- seasadj(decomp)
plot(decomp) #clearly we have seasonality inside our time series data


# test for stationary
#Augmented dickey-Fuller test
adf.test(value_ma , alternative = "stationary")
value_ma <- diff(value_ma , difference = 1)

adf.test(value_ma , alternative = "stationary")

par(mfrow = c(1,1))
#ACF dispays correlation between a series and its lag
Acf(value_ma, main = '')

#PACF displays correlation between a series and its lag that explained by its previous lag
Pacf(value_ma , main = '')


#difference of 1 is sufficient
value_d1 = diff(deseasonal_val , difference = 1)
plot(value_d1)
adf.test(value_d1 , alternative = "stationary")

#looking for spikes at specific lag points in differenced series
#ACF dispays correlation between a series and its lag
Acf(value_d1, main = '')

#PACF displays correlation between a series and its lag that explained by its previous lag
Pacf(value_d1 , main = '')



#fit the auto arima model
auto.arima(deseasonal_val , seasonal = FALSE)


fit <- auto.arima(deseasonal_val , seasonal = FALSE)
tsdisplay(residuals(fit) , lag.max = 40 , main = '(1,1,2) Model Residuals')

#take into account the lag point in 24
fit2 <- arima(deseasonal_val , order = c(1,1,24))
tsdisplay(residuals(fit2) ,lag.max = 40, main ='(1,1,24) Model Residuals')



#add deseasonal component to the actual data set2
util_ds2$deseasonal_val = deseasonal_val


# Test a model with a HOLDOUT set without seasonality

hold <- window(ts(deseasonal_val) , start=70)
fit_no_holdout = arima(ts(deseasonal_val[-c(70:98)]) , order = c(1,1,24))
fcast_no_holdout <- forecast(fit_no_holdout , h=28)
plot(fcast_no_holdout , main='(1,1,24) Model')

lines(ts(util_ds2$Job_vacancies_Value))
lines(ts(deseasonal_val))
lines(ts(util_ds2$val_ma3))



#Bringing back the seasonality
fit_w_seasonality = auto.arima(ts(deseasonal_val[-c(70:98)]) , seasonal = TRUE)
seas_fcast <- forecast(fit_w_seasonality , h=28)
plot(seas_fcast)

lines(ts(deseasonal_val))
lines(ts(util_ds2$val_ma3))



#further testing and analyzing our model
tsdisplay(residuals(fit_w_seasonality) , lag.max = 40 , main = 'Seasonal Model Residuals')



fit4 = auto.arima(deseasonal_val , seasonal = FALSE)
tsdisplay(residuals(fit4) , lag.max = 27 , main = 'Auto Arima (1,1,2)')


fit5 = arima(deseasonal_val , order = c(1,1,24))
tsdisplay(residuals(fit5) , lag.max = 27 , main = 'Custom Arima (1,1,24) Residuals')

fit6 = arima(deseasonal_val , order = c(1,1,1))
tsdisplay(residuals(fit6) , lag.max = 27 , main = 'Custom Arima (1,1,1) Residuals')

#ETS of original Job Vacancy Value
fit6 = ets(util_ds2$Job_vacancies_Value)
plot(fit6)
tsdisplay(residuals(fit6) , lag.max = 40 , main = 'ets of original Value')

###############################################################################
#plotting all the models
par(mfrow = c(2,3))

#Auto arima with seasonality (0,1,2)
fit_w_seasonality = auto.arima(ts(deseasonal_val[-c(70:98)]) , seasonal = TRUE)
seas_fcast <- forecast(fit_w_seasonality , h=28)
plot(seas_fcast)

lines(ts(deseasonal_val), col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#auto arima without seasonality (0,1,2)
fit4 = auto.arima(ts(deseasonal_val[-c(70:98)]) , seasonal = FALSE)
fcast4 <- forecast(fit4 , h=28)
plot(fcast4)

lines(ts(deseasonal_val), col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#custom Arima model (1,1,25)
fit5 = arima(ts(deseasonal_val[-c(70:98)]) , order = c(1,1,24))
fcast5 <- forecast(fit5 , h=28)
plot(fcast5)

lines(ts(deseasonal_val), col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#custom Arima model (1,1,1)
fit6 =arima(ts(deseasonal_val[-c(70:98)]) , order = c(1,1,1))
fcast6 <- forecast(fit6 , h=28)
plot(fcast6)

lines(ts(deseasonal_val), col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#ETS model
fit7 = ets(util_ds2$Job_vacancies_Value[-c(70:98)])
fcast7 <- forecast(fit7 , h=28)
plot(fcast7)

lines(ts(deseasonal_val), col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)




# numeric Accuracy measures
accuracy(fit_w_seasonality)# with seasonality (0,1,2)
accuracy(fit4)# without seasonality (0,1,2)
accuracy(fit5)# custom Arima (1,1,25)
accuracy(fit6)# default Arima (1,1,1)



write.csv(util_ds2, file = "C:\\Users\\moham\\OneDrive\\Desktop\\forecast\\utilities_des.csv")




































