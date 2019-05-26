### chapter 5
library(ggplot2)
library(forecast)
library(tseries)
## 2
#a
e5.1 = read.csv("E5.1.csv")
ggplot(e5.1, aes(period, yt)) + geom_line() +xlab("Period") + ylab("y")

#b
e5.ts = ts(e5.1$yt)
e5.acf = acf(e5.ts)
write.csv(e5.acf$acf, file = "5.2b.csv")
e5.pacf = pacf(e5.ts)

#c
model = auto.arima(e5.ts, seasonal = FALSE)
model
forecast10 = forecast(model, h = 10)
write.csv(forecast10$residuals, file = "5.2c.csv")
plot(forecast(model, h = 10))

## 11
#a
b.2 = read.csv("b.2.csv")
b.2.ts = b.2$Sales..in.Thousands[1:110]
model = auto.arima(b.2.ts)
model

#b
forecast(model, h = 10)
plot(forecast(model, h = 10))
write.csv(forecast(model, h = 10), file = "5.11b.csv")

##26
B11 = read.csv("B.11.csv")
#a
b11.ts = ts(B11$Sales..in.Thousands.of.Bottles[1:84])
model = auto.arima(b11.ts)
model
#b
forecast12 = forecast(model, h = 12)
plot(forecast12)
#c
write.csv(forecast12$residuals, file = "5.26b.csv")
