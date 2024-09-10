


fit5 = arima(ts(deseasonal_val[-c(70:98)]) , order = c(1,1,24))
fcast5 <- forecast(fit5 , h=28)
plot(fcast5)

lines(ts(deseasonal_val), col="red")
lines(ts(util_ds2$Job_vacancies_Value))


#adding first (Stock market) external variables to the model
#Decomposition of the data - take Seasonality, trend, cycle into account
value_meanStock = ts(na.omit(util_ds2$Mean_Volume.y) , frequency = 12)

#seasonal adjustment the time series
decompX1 = stl(value_meanStock ,  "periodic")
deseasonal_X1 <- seasadj(decompX1)
plot(decompX1) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_meanStock , alternative = "stationary")

util_ds2$deseasonal_X1 <- deseasonal_X1


fitXreg = forecast::Arima(ts(deseasonal_val[-c(70:98)]) , order = c(1,1,24) , xreg = ts(deseasonal_X1[-c(70:98)]))
fcastXreg <- forecast(fitXreg ,xreg = ts(deseasonal_X1[-c(1:70)]), h=28)
plot(fcastXreg)

lines(ts(deseasonal_val), col = "red")
lines(ts(util_ds2$Job_vacancies_Value))


#adding the second (1Y TB) external variable to the model

#Decomposition of the data - take Seasonality, trend, cycle into account
value_mean1YTB = ts(na.omit(util_ds2$mean_TB.CDN.1Y.MID) , frequency = 12)

#seasonal adjustment the time series
decompX2 = stl(value_mean1YTB ,  "periodic")
deseasonal_X2 <- seasadj(decompX2)
plot(decompX2) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_mean1YTB , alternative = "stationary")
value_mean1YTB = diff(value_mean1YTB , difference = 2)
adf.test(value_mean1YTB , alternative = "stationary")

util_ds2$deseasonal_X2 <- deseasonal_X2

#fitting the model

fitXreg2 = forecast::Arima(ts(deseasonal_val[-c(70:98)]) , order = c(1,1,24) , xreg = cbind(ts(deseasonal_X1[-c(70:98)]) , 
                                                                                        ts(deseasonal_X2[-c(70:98)])))
fcastXreg2 <- forecast(fitXreg2 ,xreg =cbind( ts(deseasonal_X1[-c(1:70)]) ,
                                         ts(deseasonal_X2[-c(1:70)])), h=28)
plot(fcastXreg2)

lines(ts(deseasonal_val)  , col = "red")
lines(ts(util_ds2$Job_vacancies_Value))



#adding the second (180D TB) external variable to the model

#Decomposition of the data - take Seasonality, trend, cycle into account
value_mean180DTB = ts(na.omit(util_ds2$mean_TB.CDN.180D.MID) , frequency = 12)

#seasonal adjustment the time series
decompX3 = stl(value_mean180DTB ,  "periodic")
deseasonal_X3 <- seasadj(decompX3)
plot(decompX3) #clearly we have seasonality inside our time series data

#Augmented dickey-Fuller test
adf.test(value_mean180DTB , alternative = "stationary")
value_mean180DTB = diff(value_mean180DTB , difference = 2)
adf.test(value_mean180DTB , alternative = "stationary")

util_ds2$deseasonal_X3 <- deseasonal_X3
#fitting the model

fitXreg3 = forecast::Arima(ts(deseasonal_val[-c(70:98)]) , order = c(1,1,24) , xreg = cbind(ts(deseasonal_X1[-c(70:98)]) , 
                                                                                            ts(deseasonal_X2[-c(70:98)]),
                                                                                            ts(deseasonal_X3[-c(70:98)])))
fcastXreg3 <- forecast(fitXreg3 ,xreg =cbind( ts(deseasonal_X1[-c(1:70)]) ,
                                              ts(deseasonal_X2[-c(1:70)]) ,
                                              ts(deseasonal_X3[-c(1:70)])), h=28)
plot(fcastXreg3)

lines(ts(deseasonal_val)  , col = "red")
lines(ts(util_ds2$Job_vacancies_Value))



######
#plotting the final models

par(mfrow = c(2,2))
plot(fcastXreg, main = "with mean of Monthly Stock Market Volume as ExVariables")

lines(ts(deseasonal_val), col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)


plot(fcastXreg2 , main = "with Stock Market + 1Year treasury bills as ExVariables")

lines(ts(deseasonal_val)  , col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)


plot(fcastXreg3 , main = "with Stock Market + 1Year +180 Days treasury bills as ExVariables ")

lines(ts(deseasonal_val)  , col = "red")
lines(ts(util_ds2$Job_vacancies_Value))
legend("topleft", 
       legend = c("actual value", "Deseasonalized Values" , "prediction" , "80% CInt" , "95% CInt"), 
       col = c("black", "red" , "blue", "darkgray" ,"lightgray" ), 
       lty = c(1, 1),
       cex = 0.6)











accuracy(fit5) #baseline model
accuracy(fitXreg) #with Stock Market
accuracy(fitXreg2) #with stock market + 1Y Treasury bills
accuracy(fitXreg3) # with stock market +1Y +180 D Treasury bills
















