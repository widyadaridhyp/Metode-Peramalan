install.packages(c("timeSeries","EnvStats","tseries","FitAR"))
install.packages(c("TSA","forecast","normtest"))
library(timeSeries)
library(EnvStats)
library(tseries)
library(TSA)
library(forecast)
library(normtest)

str(Data_Transaksi_Barber)
barber <- as.ts(Data_Transaksi_Barber)
str(barber)
plot.ts(barber)

#Uji Stasioner Mean Data
adf.test(barber)

#Menentukan order AR, MA, atau ARMA
acf(barber)
pacf(barber)
eacf(barber)

#Pemodelan 
fit1 <- Arima(barber, order = c(1,0,2), include.constant = TRUE)
fit2 <- Arima(barber, order = c(2,0,2), include.constant = TRUE)
fit3 <- Arima(barber, order = c(3,0,3), include.constant = TRUE)
fit4 <- Arima(barber, order = c(7,0,5), include.constant = TRUE)

cbind(fit1, fit2, fit3, fit4) #AIC terkecil fit4
model <- Arima(barber, order = c(7,0,5), include.constant = TRUE)
#Uji Signifikansi Parameter
install.packages("lmtest")
library(lmtest)
coeftest(model)

#Diagnosa Model
#Uji stasioneritas residual
adf.test(model$residuals)

#ljung box ho: residual independen
checkresiduals(model)

#uji normalitas
qqnorm(model$residuals)
qqline(model$residuals, col = 'red', lwd=3)

shapiro.test(model$residuals)
ks.test(model$residuals,'pnorm',0,sd(fit1$residuals))

jb.norm.test(model$residuals) 

res <- model$residuals
res
Box.test(res, lag=3)

#Overfitting
#ARIMA(7,0,5)
#Overfit1: (8,0,5)
#Overfit2: (7,0,6)
overfit1 <-Arima(barber,order=c(8,0,5),include.constant = TRUE)
overfit1
overfit2 <-Arima(barber,order=c(7,0,6),include.constant = TRUE)
overfit2
cbind(model,overfit1,overfit2) #lebih baik ARIMA(7,0,6)


model2 <-Arima(barber,order=c(7,0,6),include.constant = TRUE)
#################gapake
#H0: phi(parameter)=0
#uji-t
#Overfit1 : AR2
stat_uji <- overfit1$coef[['ar2']]/0.0666
stat_uji
df <- length(barber)-1
df

#t-tabel
daerah_kritis <- qt(0.025,df)
daerah_kritis; stat_uji

#p-value
2*(pt(stat_uji,df))

#####################
#model terbaik: ARIMA(7,0,6)

#FORECASTING
#cross-validation
train <- window(barber, end=194)
test <- window(barber, start=195)
train
test

train_fit <- Arima(train,order=c(7,0,6),include.constant = TRUE)
forecast_train <- forecast(train_fit, 30)
cbind(test,forecast_train)

plot(forecast_train,
     fcol="blue",lwd=2,
     main = 'Perbandingan data test vs hasil prediksi ARIMA(7,0,6)',
     xlab = 'periode',
     ylab = 'customer')
lines(seq(195,224),
      test[1:30],
      col = 'red',
      lwd = 2)
legend('bottomright',
       col=c('blue','red'),
       legend = c('nilai prediksi','nilai aktual'),
       lwd=2,
       bty='n')

mean(abs(test-forecast_train$mean))

#Forecast final (data baru)
model2 #pake semua data
forecast_final <- forecast(model2,h=30)
forecast_final

plot(forecast_final,
     fcol="blue",lwd=2,
     main = 'Hasil prediksi ARIMA(7,0,6) untuk 30 Hari',
     xlab = 'periode',
     ylab = 'customer')
legend('bottomright',
       col=c('blue'),
       legend = c('nilai prediksi'),
       lwd=2,
       bty='n')
