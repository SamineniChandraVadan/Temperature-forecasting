library(forecast)
library(readxl)
library(data.table)
library(lubridate)
#some_date <- c("01/02/1979", "03/04/1980")
#month(as.POSIXlt(some_date, format="%d/%m/%Y"))



Temp <- read_excel("C:/Users/scvch/Downloads/TempData.xlsx")



#Temp <- as.data.table(Temp)

#Temp$month =  month(as.POSIXlt(Temp$Date,format = '%Y-%m-%d'))
#Temp$Year = year(as.POSIXlt(Temp$Date,format = '%Y-%m-%d'))




#Temp <- Temp[,mean(Temperature),.(month,Year)]

#names(Temp) <- c('month','Year','Temperature')

Temp.ts <- ts(Temp$Temperature, start = c(1981, 1), end = c(1990, 12), freq = 365)



#min(Temp.ts)
#max(Temp.ts)
plot(Temp.ts, main='Temp recorded',xlab = 'year', ylab = 'sales')


Temp_dt <- ma(Temp.ts,order=12,centre = T)
plot(Temp.ts,main = "Temp data at daily level")
lines(Temp_dt)


par(mfrow=c(1,1))

par(mar = c(5,5,3,1))
#plot(Comte.ts, main='Comte sales')

#par(mfrow=c(2,2))
acf(Temp.ts,lag.max = 300,main='Acf plot of Temp data')
pacf(Temp.ts,lag.max = 300,main='Pacf plot of Temp data')



################################################ A ##################################


nvalid <- 60
#ntrain <- length(Comte.ts) - nvalid -> the problem is focus on Brie not Comte
ntrain <- length(Temp.ts) - nvalid

train.ts <- window(Temp.ts, start = c(1981, 1), end = c(1981, ntrain))  #partion data
valid.ts <- window(Temp.ts, start = c(1981, ntrain + 1), end = c(1981, ntrain + nvalid))  #partion data






################################################  B & C ##################################



##a). Decomposition Model



### Only trend


train.qm <- tslm(train.ts ~ trend )     # produce and save model on train.ts
summary(train.qm)                                          #view model summary


valid.qpred<-forecast(train.qm, h = nvalid, level=0)         # derive forecast accuracies on holdout
accuracy(valid.qpred,valid.ts)

plot(valid.qpred, main='Temp data Only Trend') #correct to Brie Sales               # plot train data = holdout forecast
lines(train.qm$fitted,col=2)                              # plot train fit
lines(valid.ts)    



### Trend and Seasonality


train.qm <- tslm(train.ts ~  season)     # produce and save model on train.ts
summary(train.qm)                                          #view model summary


valid.qpred<-forecast(train.qm, h = nvalid, level=0)         # derive forecast accuracies on holdout
accuracy(valid.qpred,valid.ts)

plot(valid.qpred, main='Temp data with only seasonality') #correct to Brie Sales               # plot train data = holdout forecast
lines(train.qm$fitted,col=2)                              # plot train fit
lines(valid.ts)                                           # plot holdout data

###### Quadratric Trend and Seasonality ##########

train.qm <- tslm(train.ts ~ trend + I(trend^2) + season)     # produce and save model on train.ts
summary(train.qm)                                          #view model summary


valid.qpred<-forecast(train.qm, h = nvalid, level=0)         # derive forecast accuracies on holdout
accuracy(valid.qpred,valid.ts)

plot(valid.qpred, main='Temp data') #correct to Brie Sales               # plot train data = holdout forecast
lines(train.qm$fitted,col=2)                              # plot train fit
lines(valid.ts)    


#Comte.qm <- tslm(Comte.ts ~ trend +I(trend^2)+ season) 
par(mfrow=c(1,1))
acf(train.qm$residuals,main='Acf plot for linear decomposition model')
pacf(train.qm$residuals,main='PAcf plot for linear decomposition model')









## b). Arima Model

par(mfrow=c(1,1))

train.ts  %>% plot(main='Brie sales Data')
train.ts %>% diff() %>% plot(main='lag-1 Differencing')
train.ts %>% diff(lag=12) %>% plot(main='lag-12 Differencing')
train.ts %>% diff() %>% diff(lag=12) %>%  plot(main='lag1 and lag-12 Differencing')

train.ts %>% diff() %>% diff(lag=12) %>% acf(lag.max=48, main='d=1 D=1 ACF')

train.ts %>% diff() %>% diff(lag=12) %>% pacf(lag.max=48,main='d=1, D=1 PACF')


# 3. significant lag-1 PACF implies q=1;
# start to fit ARIMA models with chosen d=1, D=1, and p=1. 
# check the residuals ACF and PACF
fit1<- Arima(train.ts, order=c(1,1,0), seasonal=c(0,1,0))
acf(fit1$residuals, lag.max=24,main='ARIMA(0,1,1)(0,1,0)[12] ACF')
pacf(fit1$residuals,lag.max=24, main='ARIMA(0,1,1)(0,1,0)[12] PACF')


# 4. significant lag-1[12] PACF implies Q=1.
# fit ARIMA(1,1,0)(1,1,0)[12] and check the residuals ACF and PACF
fit2 <- Arima(train.ts, order=c(1,1,2), seasonal=c(0,1,0))

par(mfrow=c(1,1))
acf(fit2$residuals, lag.max=60,main='ARIMA(0,1,1)(0,1,1)[12] ACF')
pacf(fit2$residuals,lag.max=24, main='ARIMA(0,1,1)(0,1,1)[12] PACF')


summary(fit2)


fit3 <- Arima(train.ts, order=c(0,1,2), seasonal=c(1,1,0))

par(mfrow=c(1,1))
acf(fit3$residuals, lag.max=24,main='ARIMA(0,1,1)(0,1,1)[12] ACF')
pacf(fit3$residuals,lag.max=24, main='ARIMA(0,1,1)(0,1,1)[12] PACF')

valid.arima_pred <- forecast(fit3, h = nvalid, level=0)         # derive forecast accuracies on holdout
accuracy(valid.arima_pred,valid.ts)
plot(fit3$residuals,main = 'Residuals')



# Both residual ACF and PACF are nearly insignificant, indicating 
# random walk/white noise residuals. stop further modeling. 
#fit6 <- Arima(train.ts, order=c(8,1,1), seasonal=c(2,2,1),method="ML")
#acf(fit6$residuals, lag.max=24,main='ARIMA(1,1,1)(1,1,1)[12] ACF')
#pacf(fit6$residuals,lag.max=24, main='ARIMA(1,1,1)(1,1,1)[12] PACF')



### c). Auto Arima Model

# automatic ARIMA fit and compare w/ manual fit
auto.fit <- auto.arima(train.ts)
#fit4
auto.fit
acf(auto.fit$residuals,lag.max = 24)
pacf(auto.fit$residuals,lag.max = 24)

valid.auto_pred<-forecast(auto.fit, h= nvalid, level=0)         # derive forecast accuracies on holdout
accuracy(valid.auto_pred,valid.ts)
plot(auto.fit$residuals,main = 'Residuals')


par(mfrow=c(1,1))
acf(auto.fit$residuals,lag.max = 24,main='Auto ARIMA ACF')
pacf(auto.fit$residuals,lag.max = 24, main='Auto ARIMA PACF')



### d). Holt-Winter's exponential smoothing

## Holt-Winters Validation
# use ets() with option model = "AAA" to fit Holt-Winter's smoothing 
# with additive error, trend, seasonality, and default paramenters 
hwin <- ets(train.ts, model = "AAA")
hwin
# create predictions
hwin.pred <- forecast(hwin, h = nvalid, level = 0)
accuracy(hwin.pred,valid.ts)
# plot the series
plot(hwin.pred,  main = "Brie sales AES Vlidation") #change from Amtrak to Brie sales
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#valid.qpred<-forecast(auto.fit, h=nValid, level=0)         # derive forecast accuracies on holdout

par(mfrow = c(1,1))
acf(hwin$residuals,lag.max = 24,main = 'Residual Acf of Holt-Winters smoothening on train data')
pacf(hwin$residuals,lag.max = 24,main = 'Residual Pacf of Holt-Winters smoothening on train data')


################################################  D ##################################

hwin <- ets(train.ts, model = "MMM")
hwin
# create predictions
hwin.pred <- forecast(hwin, h = nvalid, level = 0)
accuracy(hwin.pred,valid.ts)
# plot the series
plot(hwin.pred,  main = "Brie sales AES Vlidation") #change from Amtrak to Brie sales
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#valid.qpred<-forecast(auto.fit, h=nValid, level=0)         # derive forecast accuracies on holdout

par(mfrow = c(1,1))
acf(hwin$residuals,lag.max = 24,main = 'Residual Acf of Holt-Winters smoothening on train data')
pacf(hwin$residuals,lag.max = 24,main = 'Residual Pacf of Holt-Winters smoothening on train data')







########################################################

hwin <- ets(train.ts, model = "ANN")
hwin
# create predictions
hwin.pred <- forecast(hwin, h = nvalid, level = 0)
accuracy(hwin.pred,valid.ts)
# plot the series
plot(hwin.pred,  main = "Brie sales AES Vlidation") #change from Amtrak to Brie sales
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#valid.qpred<-forecast(auto.fit, h=nValid, level=0)         # derive forecast accuracies on holdout

par(mfrow = c(1,1))
acf(hwin$residuals,lag.max = 24,main = 'Residual Acf of Holt-Winters smoothening on train data')
pacf(hwin$residuals,lag.max = 24,main = 'Residual Pacf of Holt-Winters smoothening on train data')


############################################################

hwin <- ets(train.ts, model = "ANA")
hwin
# create predictions
hwin.pred <- forecast(hwin, h = nvalid, level = 0)
accuracy(hwin.pred,valid.ts)
# plot the series
plot(hwin.pred,  main = "Brie sales AES Vlidation") #change from Amtrak to Brie sales
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#valid.qpred<-forecast(auto.fit, h=nValid, level=0)         # derive forecast accuracies on holdout

par(mfrow = c(1,1))
acf(hwin$residuals,lag.max = 24,main = 'Residual Acf of Holt-Winters smoothening on train data')
pacf(hwin$residuals,lag.max = 24,main = 'Residual Pacf of Holt-Winters smoothening on train data')




############################################################

hwin <- ets(train.ts, model = "MNA")
hwin
# create predictions
hwin.pred <- forecast(hwin, h = nvalid, level = 0)
accuracy(hwin.pred,valid.ts)
# plot the series
plot(hwin.pred,  main = "Brie sales AES Vlidation") #change from Amtrak to Brie sales
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#valid.qpred<-forecast(auto.fit, h=nValid, level=0)         # derive forecast accuracies on holdout

par(mfrow = c(1,1))
acf(hwin$residuals,lag.max = 24,main = 'Residual Acf of Holt-Winters smoothening on train data')
pacf(hwin$residuals,lag.max = 24,main = 'Residual Pacf of Holt-Winters smoothening on train data')







############################################################

hwin <- ets(train.ts, model = "MNM")
hwin
# create predictions
hwin.pred <- forecast(hwin, h = nvalid, level = 0)
accuracy(hwin.pred,valid.ts)
# plot the series
plot(hwin.pred,  main = "Brie sales AES Vlidation") #change from Amtrak to Brie sales
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#valid.qpred<-forecast(auto.fit, h=nValid, level=0)         # derive forecast accuracies on holdout

par(mfrow = c(1,1))
acf(hwin$residuals,lag.max = 24,main = 'Residual Acf of Holt-Winters smoothening on train data')
pacf(hwin$residuals,lag.max = 24,main = 'Residual Pacf of Holt-Winters smoothening on train data')




############################################################

hwin <- ets(train.ts, model = "MAM")
hwin
# create predictions
hwin.pred <- forecast(hwin, h = nvalid, level = 0)
accuracy(hwin.pred,valid.ts)
# plot the series
plot(hwin.pred,  main = "Brie sales AES Vlidation") #change from Amtrak to Brie sales
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts)

#valid.qpred<-forecast(auto.fit, h=nValid, level=0)         # derive forecast accuracies on holdout

par(mfrow = c(1,1))
acf(hwin$residuals,lag.max = 24,main = 'Residual Acf of Holt-Winters smoothening on train data')
pacf(hwin$residuals,lag.max = 24,main = 'Residual Pacf of Holt-Winters smoothening on train data')






accuracy(valid.qpred,valid.ts)

accuracy(valid.arima_pred,valid.ts)

accuracy(valid.auto_pred,valid.ts)

accuracy(hwin.pred,valid.ts)




acf(train.qm$residuals, lag.max = 24,main="Decomposition with Linear Trend ACF")



################################################  E ##################################
### Using Fit 5 for modelling the answer
#use all data to update the model

final_fit <- auto.arima(Temp.ts)
final_fit.pred <- forecast(final_fit, h = 6, level = 0)
final_fit.pred

#acf(final_fit$residuals, lag.max=24,main='ARIMA(1,1,1)(1,1,1)[12] ACF')
#pacf(final_fit$residuals,lag.max=24, main='ARIMA(1,1,1)(1,1,1)[12] PACF')



################################################  F ##################################

plot(final_fit.pred,main = 'Arima model forecasting',xlab = 'time')
lines(final_fit.pred$fitted,col =2)

plot(final_fit$residuals)



par(mfrow = c(1,1))
plot(Temp.ts, main='Brie Sales Forecast', ylab='Sales in Kilograms',xlim = c(1981,1992))
lines(final_fit$fitted, col='Red')
lines(final_fit.pred$mean, col='Blue', lwd=3)
#legend(x='topright', legend=c('True Sales','Fitted Sales','Predicted Sales'), col=c('Black','Red','Blue'), lty=1, lwd=c(1,1,3))




