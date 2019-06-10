### this is homework 6
library(imputeTS)
library(forecast)
###
## 42
#a
data = read.csv("B23.csv")
train = data[1:698,]
test = data[699:874,]
# Seasonal Adjustment then Mean
train_ts = ts(train[[2]])
train_season = na.seasplit(train_ts, algorithm = "mean")
model = auto.arima(train_season)
plot(forecast(model, h = 176))

#56 yt = 25 + 0.8 * yt-1 + et
##a
xt <- arima.sim(list(order = c(1,0,0), ar = 0.8), n = 100)
yt <- xt + 25   
plot.ts(yt)
model = auto.arima(yt)
##b
x = rnorm(100, mean = 0, sd = 1)
yt = yt + x
plot.ts(yt)
##c
yt.ts = ts(yt)
acf(yt.ts)
pacf(yt.ts)


###60
##a theoretical acf and pacf
acf_t = ARMAacf(ar=c(0.6, 0.25), lag.max=50)
pacf_t = ARMAacf(ar=c(0.6, 0.25), lag.max=50, pacf = TRUE)
write.csv(acf_t, file = "acf_t.csv")
write.csv(pacf_t, file = "pacf_t.csv")
##b AR(1)
acf_s = ARMAacf(ar=c(0.6), lag.max=50)
pacf_s = ARMAacf(ar=c(0.6), lag.max=50, pacf = TRUE)
write.csv(acf_s, file = "acf_s.csv")
write.csv(pacf_s, file = "pacf_s.csv")
