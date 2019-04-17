library(orcutt)
data = read.csv("13.csv")
attach(data)
model = lm(y~x)
model.fit = cochrane.orcutt(model)
model.fit

x.diff = diff(data$x)
y.diff = diff(data$y)
data.diff = cbind(x.diff, y.diff)
data.diff = data.frame(data.diff)
attach(data.diff)
model.diff = lm(y.diff ~ x.diff)
model.diff.fit = cochrane.orcutt(model.diff)
model.diff.fit

#q18
#a
data = read.csv("ex2.csv")
model1 = lm(y ~ x1 + x2 + x3 +x4, data = data)
model2 = lm(y.1 ~ x1 + x2 + x3 +x4, data = data)
model3 = lm(y.2 ~ x1 + x2 + x3 +x4, data = data)
summary(model1)
summary(model2)
summary(model3)
par(mfrow = c(1,1))
plot(model1$residuals)

#b
for(i in 1:8){
  sample = c(data[i,5], data[i,6], data[i,7])
  data[i,8] = var(sample)
}
model.wls = lm(y ~ x1 + x2 + x3 + x4, data = data, weights = data$V8)
summary(model.wls)
plot(model.wls$residuals)

#c
model.variance = lm(V8 ~ x1 + x2 + x3 + x4, data = data)
summary(model.variance)
plot(model.variance$residuals)

model.variance.wls = lm(V8 ~ x1 + x2 + x3 + x4, data = data, weights = data$V8)

#32
library(forecast)
library(tseries)
data = read.csv("3.5.csv")
attach(data)
model = arima(y, xreg = x, order = c(1,0,0))
summary(model)
model = lm(y ~ x, data = data)
model.fit = cochrane.orcutt(model)
model.fit

#33
