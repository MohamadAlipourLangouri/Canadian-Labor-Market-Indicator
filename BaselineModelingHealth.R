library(ggplot2)
library(forecast)
library(tseries)
library(tidyverse)
library(rio)
library(zoo)


he_ds <- subset(Completed_data_filled_forecast , 
                Completed_data_filled_forecast$NAICS == "Health care and social assistance [62]")
he_ds$Date <- as.Date(he_ds$Date)

# create a time series object based on Job Vacancy Value
val_TSobject <- ts(he_ds[,c('Job_vacancies_Value')])

#tsclean function to ID and replace outliers and input missing values if any
he_ds$clean_val = tsclean(val_TSobject)


# get a yearly and quartarly moving average
he_ds$val_ma12 = ma(he_ds$Job_vacancies_Value, order = 12) # Yearly Moving average
he_ds$val_ma3 = ma(he_ds$Job_vacancies_Value , order = 3) # seasonal Moving average

summary(he_ds)

#Decomposition of the data - take Seasonality, trend, cycle into account
value_he_ma = ts(na.omit(he_ds$Job_vacancies_Value) , frequency = 12)

#seasonal adjustment the time series
decomp = stl(value_he_ma ,  "periodic")
deseasonal_val_he <- seasadj(decomp)
plot(decomp) #clearly we have seasonality inside our time series data


# test for stationary
#Augmented dickey-Fuller test
adf.test(value_he_ma , alternative = "stationary")
value_he_ma <- diff(value_he_ma , differences = 2)
adf.test(value_he_ma , alternative = "stationary")

#ACF dispays correlation between a series and its lag
Acf(value_he_ma, main = '')

#PACF displays correlation between a series and its lag that explained by its previous lag
Pacf(value_he_ma , main = '')

#difference of 1 is sufficient
value_d1_he = diff(deseasonal_val_he , difference = 1)
plot(value_d1_he)
adf.test(value_d1_he , alternative = "stationary")


#looking for spikes at specific lag points in differenced series
#ACF dispays correlation between a series and its lag
Acf(value_d1_he, main = '')

#PACF displays correlation between a series and its lag that explained by its previous lag
Pacf(value_d1_he , main = '')


#fit the auto arima model
auto.arima(deseasonal_val_he , seasonal = FALSE)


fit_he <- auto.arima(deseasonal_val_he , seasonal = FALSE)
tsdisplay(residuals(fit_he) , lag.max = 40 , main = '(2,1,3) Model Residuals')

#take into account the lag point in 24
fit_he2 <- arima(deseasonal_val_he , order = c(2,1,24))
tsdisplay(residuals(fit_he2) ,lag.max = 40, main ='(2,1,24) Model Residuals')


#adding deseasonal value to the data set
he_ds$deseasonal_val_he <- deseasonal_val_he


hold <- window(ts(deseasonal_val_he) , start=76)
fit_no_holdout_he = arima(ts(deseasonal_val_he[-c(76:106)]) , order = c(2,1,24))
fcast_no_holdout_he <- forecast(fit_no_holdout_he , h=50)
plot(fcast_no_holdout_he , main='(2,1,24) Model')

lines(ts(he_ds$Job_vacancies_Value), col= "red")
lines(ts(deseasonal_val_he))
#lines(ts(ed_ds$val_ma3))
legend("topleft", 
       legend = c("Job Vacancies", "Deseasonalized Values"), 
       col = c("red", "black"), 
       lty = c(1, 1))

#Bringing back the seasonality
fit_w_seasonality_he = auto.arima(ts(deseasonal_val_he[-c(76:106)]) , seasonal = TRUE)
seas_fcast_he <- forecast(fit_w_seasonality_he , h=45)
plot(seas_fcast_he)

lines(ts(deseasonal_val_he))
lines(ts(he_ds$Job_vacancies_Value) , col ="red")
legend("topleft", 
       legend = c("Job Vacancies", "Deseasonalized Values"), 
       col = c("red", "black"), 
       lty = c(1, 1))


#further testing and analyzing our model
tsdisplay(residuals(fit_w_seasonality_he) , lag.max = 40 , main = 'Seasonal Model Residuals')


#fitting without seasonality
fit_he = auto.arima(ts(deseasonal_val_he[-c(76:106)]) , seasonal = FALSE)
unses_fcast_he <- forecast(fit_he , h = 45)
plot(unses_fcast_he)
lines(ts(deseasonal_val_he))
lines(ts(he_ds$Job_vacancies_Value) , col ="red")
legend("topleft", 
       legend = c("Job Vacancies", "Deseasonalized Values"), 
       col = c("red", "black"), 
       lty = c(1, 1))
tsdisplay(residuals(fit_he) , lag.max = 27 , main = 'Auto Arima (2,1,3)')



fit_he2 = arima(deseasonal_val_he , order = c(2,1,24))
tsdisplay(residuals(fit_he2) , lag.max = 27 , main = 'Custom Arima (2,1,24) Residuals')

fit_he3 = arima(deseasonal_val_he , order = c(1,1,1))
tsdisplay(residuals(fit_he3) , lag.max = 27 , main = 'Custom Arima (1,1,1) Residuals')

#ETS of original Job Vacancy Value
fit_he4 = ets(he_ds$Job_vacancies_Value)
plot(fit_he4)
tsdisplay(residuals(fit_he4) , lag.max = 27 , main = 'ets of original Value')

######
#plotting all the models
par(mfrow = c(2,2))

#fit of seasonal model
seas_fcast_he <- forecast(fit_w_seasonality_he , h=30)
plot(seas_fcast_he)

lines(ts(deseasonal_val_he), col = "red")
lines(ts(he_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue","darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#unseasonal Model
fit_he = auto.arima(ts(deseasonal_val_he[-c(86:106)]) , seasonal = FALSE)
fcast_he <- forecast(fit_he , h=20)
plot(fcast_ed)

lines(ts(deseasonal_val_ed), col = "red")
lines(ts(ed_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#Custom ARIMA (3,1,24)
fit_he2 = arima(ts(deseasonal_val_he[-c(86:106)]) , order = c(2,1,24))
fcast_he2 <- forecast(fit_he2 , h=25)
plot(fcast_he2)

lines(ts(deseasonal_val_he), col = "red")
lines(ts(he_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#Original ARIMA model
fit_he3 = arima(ts(deseasonal_val_he[-c(86:106)]) , order = c(1,1,1))
fcast_he3 <- forecast(fit_he3 , h =25)
plot(fcast_he3)

lines(ts(deseasonal_val_he), col = "red")
lines(ts(he_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)



accuracy(fit_w_seasonality_he)
accuracy(fit_he)
accuracy(fit_he2) #baseline model selected
accuracy(fit_he3)



###### adding external variables
#using external deseasonalized variables by adding them to the data set

#Stock market monthly Volume
value_meanStock_he = ts(na.omit(he_ds$Mean_Volume.y) , frequency = 12)

#seasonal adjustment the time series
decompX1_he = stl(value_meanStock_he ,  "periodic")
deseasonal_X1_he <- seasadj(decompX1_he)
plot(decompX1_he) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_meanStock_he , alternative = "stationary")

he_ds$deseasonal_X1 <- deseasonal_X1_he




#Treasury bills 1 Year
value_mean1YTB_he = ts(na.omit(he_ds$mean_TB.CDN.1Y.MID) , frequency = 12)

#seasonal adjustment the time series
decompX2_he = stl(value_mean1YTB_he ,  "periodic")
deseasonal_X2_he <- seasadj(decompX2_he)
plot(decompX2_he) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_mean1YTB_he , alternative = "stationary")
value_mean1YTB_he = diff(value_mean1YTB_he , difference = 2)
adf.test(value_mean1YTB_he , alternative = "stationary")

he_ds$deseasonal_X2 <- deseasonal_X2_he


#trasury bills 6 month
value_mean180DTB_he = ts(na.omit(he_ds$mean_TB.CDN.180D.MID) , frequency = 12)

#seasonal adjustment the time series
decompX3_he = stl(value_mean180DTB_he ,  "periodic")
deseasonal_X3_he <- seasadj(decompX3_he)
plot(decompX3_he) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_mean180DTB_he , alternative = "stationary")
value_mean180DTB_he = diff(value_mean180DTB_he , difference = 2)
adf.test(value_mean180DTB_he , alternative = "stationary")

he_ds$deseasonal_X3 <- deseasonal_X3_he




#fitting the models
par(mfrow=c(1,2))
# Baseline model
fit_he2 = arima(ts(deseasonal_val_he[-c(86:106)]) , order = c(2,1,24))
fcast_he2 <- forecast(fit_he2 , h=25)
plot(fcast_he2, main = "ARIMA Baseline model")

lines(ts(deseasonal_val_he), col = "red")
lines(ts(he_ds$Job_vacancies_Value))
#lines(ts(util_ds2$val_ma3))
#lines(ts(value_ma))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)

#with stock market
fitXreg_he = forecast::Arima(ts(deseasonal_val_he[-c(86:106)]) , order = c(2,1,24) , xreg = ts(deseasonal_X1_he[-c(86:106)]))
fcastXreg_he <- forecast(fitXreg_he ,xreg = ts(deseasonal_X1_he[-c(1:86)]), h=25)
plot(fcastXreg_he, main = "ARIMA model with Monthly Stock Market Volume as ExVariable")

lines(ts(deseasonal_val_he), col = "red")
lines(ts(he_ds$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.7)


#with 1 year TB added
fitXreg2_he = forecast::Arima(ts(deseasonal_val_he[-c(86:106)]) , order = c(2,1,24) , xreg = cbind(ts(deseasonal_X1_he[-c(86:106)]) , 
                                                                                                   ts(deseasonal_X2_he[-c(86:106)])))
fcastXreg2_he <- forecast(fitXreg2_he ,xreg =cbind( ts(deseasonal_X1_he[-c(1:86)]) ,
                                                    ts(deseasonal_X2_he[-c(1:86)])), h=25)
plot(fcastXreg2_he, main = "ARIMA with monthly Stock Market + 1 Year TB")

lines(ts(deseasonal_val_he)  , col = "red")
lines(ts(he_ds$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)


#with 6 month TB added
#with 1 year TB
fitXreg3_he = forecast::Arima(ts(deseasonal_val_he[-c(86:106)]) , order = c(2,1,24) , xreg = cbind(ts(deseasonal_X2_he[-c(86:106)])))
fcastXreg3_he <- forecast(fitXreg3_he ,xreg =cbind(ts(deseasonal_X2_he[-c(1:86)])), h=30)
plot(fcastXreg3_he, main = "ARIMA with 1 Year TB as external variable")

lines(ts(deseasonal_val_he)  , col = "red")
lines(ts(he_ds$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.7)

accuracy(fitXreg3_he)
accuracy(fitXreg_he)
accuracy(fit_he2)











































































