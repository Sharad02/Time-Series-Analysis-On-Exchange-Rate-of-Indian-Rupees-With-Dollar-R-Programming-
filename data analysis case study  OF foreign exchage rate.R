
library(forecast)
library(smooth)
library(graphics)
library(datasets)
library(tseries)
library(ggplot2)
library(fpp2)
setwd("C:\\Users\\Sharad Deshmukh\\Desktop\\MSC=3")
data <- read.csv("Reference Exchange Rate.csv")
print(data)
Exchange_rate=data$Price
Exchange_rate_timeseries=ts(Exchange_rate, frequency=12, start=c(2010,1))
ts.plot(Exchange_rate_timeseries)
abline(lm(Exchange_rate_timeseries~time(Exchange_rate_timeseries)))
ER_decompose=decompose(Exchange_rate_timeseries)
autoplot(ER_decompose,main="Additive Decomposition")
ER_decomposemulti=decompose(Exchange_rate_timeseries,type="multiplicative")
autoplot(ER_decomposemulti,main="multiplicative mode Decomposition")


##Now  we also glance at Cyclical Pattern we have get boxplot
#boxplot(Exchange_rate_timeseries~cycle(Exchange_rate_timeseries))



###Apply Single Exponential Smooting
ER_SES=ses(Exchange_rate_timeseries,h=12)
autoplot(ER_SES)
ER_SES$model

##Double exponential Smoothing
ER_holt=holt(Exchange_rate_timeseries,h=12)
autoplot(ER_holt)
ER_holt$model
summary(ER_holt)
diff(Exchange_rate_timeseries)




##apply Triple  Exponetial Smoothing

ER_HW=hw(Exchange_rate_timeseries,h=12,seasonal="a")
plot(ER_HW)
autoplot(ER_HW)
ER_HW$model
summary(ER_HW) 

#####ARIMA MODEL

##Visualizing Stationary 
hist(Exchange_rate_timeseries) ##we can see his is not normally distributed so as we can see data is not normally Distributed.
shapiro.test(Exchange_rate_timeseries)
## If the value of p is equal to or less than 0.05, then the hypothesis of normality will be rejected by the Shapiro test.
library(nortest)
ad.test(Exchange_rate_timeseries)
##lags
gglagplot(Exchange_rate_timeseries)
###ADF statistical Test Stationarity
adf.test(Exchange_rate_timeseries)
##Null Hyph: time series is not Stationary
#Alternative :Time Series is  Stationary
#In general, a p-value of less than 5% means you can reject the null hypothesis
## So we can say that it is no stationary


##Non stationary to Stationary
ER_DIFF=diff(Exchange_rate_timeseries)
autoplot(ER_DIFF,main="first ORDER Differencing")
abline(lm((ER_DIFF)~time(ER_DIFF)))
##
##lets check Data stastically whaeather data is Stationary Or not
adf.test(ER_DIFF)
 ##So as i can say the time series is stationary


###perform to check Stationary data and autocorrelation

acf(ER_DIFF)
pacf(ER_DIFF)
		

###Training Data And Testing data

T_train1=window(Exchange_rate_timeseries,end=c(2020,12),frequency=12) ##Train Data
T_train=ts(T_train1, frequency=12, start=c(2010,1))

T_test1=window(Exchange_rate_timeseries,start=c(2021,1),frequency=12) ##Test Data
T_test=ts(T_test1,,start=c(2021,1),frequency=12)

#########
##Visualizing Stationary 
hist(T_train) ##we can see his is not normally distributed so as we can see data is not normally Distributed.
shapiro.test(T_train)
## If the value of p is equal to or less than 0.05, then the hypothesis of normality will be rejected by the Shapiro test.
library(nortest)
ad.test(T_train)
##lags
gglagplot(T_train)
###ADF statistical Test Stationarity
adf.test(T_train)
##Null Hyph: time series is not Stationary
#Alternative :Time Series is  Stationary
#In general, a p-value of less than 5% means you can reject the null hypothesis
## So we can say that it is no stationary


##Non stationary to Stationary
ER_DIFF=diff(T_train)
autoplot(ER_DIFF,main="first ORDER Differencing")
abline(lm((ER_DIFF)~time(ER_DIFF)))
##
##lets check Data stastically whaeather data is Stationary Or not
adf.test(ER_DIFF)
 ##So as i can say the time series is stationary


###perform to check Stationary data and autocorrelation

acf(ER_DIFF)
pacf(ER_DIFF)
		



##Auto Arima Model for Exchange_rate_timeseries
auto.arima(T_train, trace=TRUE) 
Arima_fit_Train=auto.arima(T_train,seasonal=TRUE,ic="bic")
Arima_fit_Train
predict(Arima_fit_Train,n.ahead=12)->pre_time;pre_time$pred

autoplot(pre_time$pred)
summary(Arima_fit_Train)
ts.plot(T_train,pre_time$pred,log="y",col=c("blue","red"),main="forecasted Value Arima (0,1,0)") ##plotting predicted Values
##Compare Forecasted With Actual Values of Exchange_rate_timeseries
projected=as.data.frame(pre_time$pred)
act_for=cbind(T_test,projected)
act_for
ts.plot(act_for,col=c("blue","red"),main="actual VS Projected")
accuracy(
################
###Accuracy of the model
y=pre_time$pred
data.frame(y,T_test)
accuracy(y,T_test)

###check residual
checkresiduals(Arima_fit_Train)
##

#############
###Apply Single Exponential Smooting
ER_SES=ses(T_train,h=12)
autoplot(ER_SES)
ER_SES$model
SES_Projected=as.data.frame(ER_SES)
SES_Forecasted=cbind(T_test,SES_Projected[,1])
SES_Forecasted
ts.plot(SES_Forecasted,col=c("blue","red"),main="actual VS Projected")
accuracy(ER_SES,T_test)

#######
##apply Triple  Exponetial Smoothing

ER_HW=hw(T_train,h=12,seasonal="a")
plot(ER_HW)
autoplot(ER_HW)
ER_HW$model
summary(ER_HW) 
act_for=cbind(T_test,as.data.frame(ER_HW)[,1])
act_for
ts.plot(act_for,col=c("blue","red"),main="actual VS Projected")
accuracy(ER_HW,T_test)
###
ER_holt=holt(T_train,h=12)
autoplot(ER_holt)
act_for=cbind(T_test,as.data.frame(ER_holt)[,1])
act_for
ts.plot(act_for,col=c("blue","red"),main="actual VS Projected")
summary(ER_holt)
accuracy(ER_holt,T_test)
ER_holt$model
######
##holtwinter Multiplicative

hw <- HoltWinters(T_train,seasonal="multiplicative")
plot(hw,main="Holt-Winter Multiplicative Model")
forecast <- predict(hw, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw, forecast,main="Holt-Winter Multiplicative Model Forecasting")
hw_forecast=cbind(T_test,forecast[,1])
ts.plot(hw_forecast,col=c("blue","red"),main="actual VS Projected")

####holtwinter Multiplicative And Additive Plot
hw=hw(T_train,seasonal="multiplicative")
aust <- window(austourists,start=2005)
fit1 <-hw(T_train,seasonal="additive")

fit2 <- hw(T_train,seasonal="multiplicative")

autoplot(T_train) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
    PI=FALSE) +
  xlab("Year") +
  ylab("Rupees") +
  ggtitle("India Rupees Exchange Rate With USD Doller") +
  guides(colour=guide_legend(title="Forecast"))
accuracy(fit1,T_test)
accuracy(fit2,T_test)
checkresiduals(hw)


####Conclusion
accuracy(y,T_test)         ##arima
accuracy(ER_SES,T_test)   ##Single
accuracy(ER_holt,T_test)  ##dou
accuracy(fit2,T_test)    ##Multipl
accuracy(ER_HW,T_test)  ##Triple
###
