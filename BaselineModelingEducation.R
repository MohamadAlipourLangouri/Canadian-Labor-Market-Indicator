library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(rio)
library(zoo)


ed_ds <- subset(Completed_data_filled_forecast , 
                Completed_data_filled_forecast$NAICS == "Educational services [61]")
ed_ds$Date <- as.Date(ed_ds$Date)

# create a time series object based on Job Vacancy Value
val_TSobject <- ts(ed_ds[,c('Job_vacancies_Value')])

#tsclean function to ID and replace outliers and input missing values if any
ed_ds$clean_val = tsclean(val_TSobject)


#plot the cleaned data
ggplot()+
  geom_line(data = ed_ds , aes(x = Date , y = clean_val)) + 
  ylab('Total Job vacancy Values')

# get a yearly and quartarly moving average
ed_ds$val_ma12 = ma(ed_ds$Job_vacancies_Value, order = 12) # Yearly Moving average
ed_ds$val_ma3 = ma(ed_ds$Job_vacancies_Value , order = 3) # seasonal Moving average

summary(ed_ds)
#using na.aggregate function
ed_ds <- replace(ed_ds , TRUE,lapply(ed_ds , na.aggregate))
summary(ed_ds)

#plot 2 moving averages


#plot the uncleaned data
ggplot()+
  geom_line(data = ed_ds , aes(x = Date , y = Job_vacancies_Value, colour = "Total Values")) +
  geom_line(data = ed_ds , aes(x = Date , y = val_ma3 , colour = "seasonal Move")) +
  geom_line(data = ed_ds , aes(x = Date , y = val_ma12 , colour = "yearly Move")) +
  ylab('Total Job vacancy Values')


#Decomposition of the data - take Seasonality, trend, cycle into account
value_ed_ma = ts(na.omit(ed_ds$Job_vacancies_Value) , frequency = 12)


#seasonal adjustment the time series
decomp = stl(value_ed_ma ,  "periodic")
deseasonal_val_ed <- seasadj(decomp)
plot(decomp) #clearly we have seasonality inside our time series data


# test for stationary
#Augmented dickey-Fuller test
adf.test(value_ed_ma , alternative = "stationary")

#ACF dispays correlation between a series and its lag
Acf(value_ed_ma, main = '')

#PACF displays correlation between a series and its lag that explained by its previous lag
Pacf(value_ed_ma , main = '')


#difference of 1 is sufficient
value_d1_ed = diff(deseasonal_val_ed , difference = 1)
plot(value_d1_ed)
adf.test(value_d1_ed , alternative = "stationary")

#looking for spikes at specific lag points in differenced series
#ACF dispays correlation between a series and its lag
Acf(value_d1_ed, main = '')

#PACF displays correlation between a series and its lag that explained by its previous lag
Pacf(value_d1_ed , main = '')


#fit the auto arima model
auto.arima(deseasonal_val_ed , seasonal = FALSE)


fit_ed <- auto.arima(deseasonal_val_ed , seasonal = FALSE)
tsdisplay(residuals(fit_ed) , lag.max = 40 , main = '(3,1,0) Model Residuals')

#take into account the lag point in 24
fit_ed2 <- arima(deseasonal_val_ed , order = c(3,1,24))
tsdisplay(residuals(fit_ed2) ,lag.max = 40, main ='(3,1,24) Model Residuals')


#adding deseasonal value to the data set
ed_ds$deseasonal_val_ed <- deseasonal_val_ed


hold <- window(ts(deseasonal_val_ed) , start=76)
fit_no_holdout_ed = arima(ts(deseasonal_val_ed[-c(76:106)]) , order = c(3,1,24))
fcast_no_holdout_ed <- forecast(fit_no_holdout_ed , h=50)
plot(fcast_no_holdout_ed , main='(3,1,24) Model')


lines(ts(ed_ds$Job_vacancies_Value), col= "red")
lines(ts(deseasonal_val_ed))
#lines(ts(ed_ds$val_ma3))
legend("topleft", 
       legend = c("Job Vacancies", "Deseasonalized Values"), 
       col = c("red", "black"), 
       lty = c(1, 1))


#Bringing back the seasonality
fit_w_seasonality_ed = auto.arima(ts(deseasonal_val_ed[-c(76:106)]) , seasonal = TRUE)
seas_fcast_ed <- forecast(fit_w_seasonality_ed , h=30)
plot(seas_fcast_ed)

lines(ts(deseasonal_val_ed))
lines(ts(ed_ds$Job_vacancies_Value) , col ="red")
legend("topleft", 
       legend = c("Job Vacancies", "Deseasonalized Values"), 
       col = c("red", "black"), 
       lty = c(1, 1))


#further testing and analyzing our model
tsdisplay(residuals(fit_w_seasonality_ed) , lag.max = 40 , main = 'Seasonal Model Residuals')



fit_ed = auto.arima(deseasonal_val_ed , seasonal = FALSE)
tsdisplay(residuals(fit_ed) , lag.max = 27 , main = 'Auto Arima (3,1,0)')


fit_ed2 = arima(deseasonal_val_ed , order = c(3,1,24))
tsdisplay(residuals(fit_ed2) , lag.max = 27 , main = 'Custom Arima (3,1,24) Residuals')

fit_ed3 = arima(deseasonal_val_ed , order = c(1,1,1))
tsdisplay(residuals(fit_ed3) , lag.max = 27 , main = 'Custom Arima (1,1,1) Residuals')

#ETS of original Job Vacancy Value
fit_ed4 = ets(ed_ds$Job_vacancies_Value)
plot(fit_ed4)
tsdisplay(residuals(fit_ed4) , lag.max = 27 , main = 'ets of original Value')




######
#plotting all the models
par(mfrow = c(2,2))

#fit of seasonal model
seas_fcast_ed <- forecast(fit_w_seasonality_ed , h=28)
plot(seas_fcast_ed)

lines(ts(deseasonal_val_ed), col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#unseasonal Model
fit_ed = auto.arima(ts(deseasonal_val_ed[-c(76:106)]) , seasonal = FALSE)
fcast_ed <- forecast(fit_ed , h=35)
plot(fcast_ed)

lines(ts(deseasonal_val_ed), col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#Custom ARIMA (3,1,24)
fit_ed2 = arima(ts(deseasonal_val_ed[-c(76:106)]) , order = c(3,1,24))
fcast_ed2 <- forecast(fit_ed2 , h=35)
plot(fcast_ed2)

lines(ts(deseasonal_val_ed), col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#Original ARIMA model
fit_ed3 = arima(ts(deseasonal_val_ed[-c(76:106)]) , order = c(1,1,1))
fcast_ed3 <- forecast(fit_ed3 , h =35)
plot(fcast_ed3)

lines(ts(deseasonal_val_ed), col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)



accuracy(fit_w_seasonality)
accuracy(fit_ed)
accuracy(fit_ed2)
accuracy(fit_ed3)



###### adding external variables
#using external deseasonalized variables by adding them to the data set

#Stock market monthly Volume
value_meanStock_ed = ts(na.omit(ed_ds$Mean_Volume.y) , frequency = 12)

#seasonal adjustment the time series
decompX1_ed = stl(value_meanStock_ed ,  "periodic")
deseasonal_X1_ed <- seasadj(decompX1_ed)
plot(decompX1_ed) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_meanStock_ed , alternative = "stationary")

ed_ds$deseasonal_X1 <- deseasonal_X1_ed




#Treasury bills 1 Year
value_mean1YTB_ed = ts(na.omit(ed_ds$mean_TB.CDN.1Y.MID) , frequency = 12)

#seasonal adjustment the time series
decompX2_ed = stl(value_mean1YTB_ed ,  "periodic")
deseasonal_X2_ed <- seasadj(decompX2_ed)
plot(decompX2_ed) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_mean1YTB_ed , alternative = "stationary")
value_mean1YTB_ed = diff(value_mean1YTB_ed , difference = 2)
adf.test(value_mean1YTB_ed , alternative = "stationary")

ed_ds$deseasonal_X2 <- deseasonal_X2_ed


#trasury bills 6 month
value_mean180DTB_ed = ts(na.omit(ed_ds$mean_TB.CDN.180D.MID) , frequency = 12)

#seasonal adjustment the time series
decompX3_ed = stl(value_mean180DTB_ed ,  "periodic")
deseasonal_X3_ed <- seasadj(decompX3_ed)
plot(decompX3_ed) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_mean180DTB_ed , alternative = "stationary")
value_mean180DTB_ed = diff(value_mean180DTB_ed , difference = 2)
adf.test(value_mean180DTB_ed , alternative = "stationary")

ed_ds$deseasonal_X3 <- deseasonal_X3_ed




#fitting the models
par(mfrow=c(1,2))
# Baseline model
fit_ed2 = arima(ts(deseasonal_val_ed[-c(76:106)]) , order = c(3,1,24))
fcast_ed2 <- forecast(fit_ed2 , h=35)
plot(fcast_ed2, main = "ARIMA Baseline model")

lines(ts(deseasonal_val_ed), col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.6)

#with stock market
fitXreg_ed = forecast::Arima(ts(deseasonal_val_ed[-c(76:106)]) , order = c(3,1,24) , xreg = ts(deseasonal_X1_ed[-c(76:106)]))
fcastXreg_ed <- forecast(fitXreg_ed ,xreg = ts(deseasonal_X1_ed[-c(1:76)]), h=35)
plot(fcastXreg_ed, main = "ARIMA model with Monthly Stock Market Volume as ExVariable")

lines(ts(deseasonal_val_ed), col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.7)


#with 1 year TB added
fitXreg2_ed = forecast::Arima(ts(deseasonal_val_ed[-c(76:106)]) , order = c(3,1,24) , xreg = cbind(ts(deseasonal_X1_ed[-c(76:106)]) , 
                                                                                            ts(deseasonal_X2_ed[-c(76:106)])))
fcastXreg2_ed <- forecast(fitXreg2_ed ,xreg =cbind( ts(deseasonal_X1_ed[-c(1:76)]) ,
                                              ts(deseasonal_X2_ed[-c(1:76)])), h=35)
plot(fcastXreg2_ed, main = "ARIMA with monthly Stock Market + 1 Year TB")

lines(ts(deseasonal_val_ed)  , col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#with 6 month TB added
#with 1 year TB
fitXreg3_ed = forecast::Arima(ts(deseasonal_val_ed[-c(76:106)]) , order = c(3,1,24) , xreg = cbind(ts(deseasonal_X2_ed[-c(76:106)])))
fcastXreg3_ed <- forecast(fitXreg3_ed ,xreg =cbind(ts(deseasonal_X2_ed[-c(1:76)])), h=35)
plot(fcastXreg3_ed, main = "ARIMA with 1 Year TB as external variable")

lines(ts(deseasonal_val_ed)  , col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "skyblue" ,"darkgray" ), 
       lty = c(1, 1),
       cex = 0.7)














































