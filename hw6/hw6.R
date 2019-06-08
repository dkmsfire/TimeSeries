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

#56
