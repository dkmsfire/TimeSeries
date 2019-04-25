#variogram
variogram = function(x,y){
  ##alarm
  if(y > length(x)-2) return("error : lag is too big!");
  ##main
  n=length(x);
  d1=x[2:n]-x[1:(n-1)];
  var1= var(d1);
  variogram=rep(0,y);
  lag=1:y;
  
  for(i in 1:y){
    d=x[(1+i):n] - x[1:(n-i)];
    variogram[i]= var(d)/var1;
  }
  plot(lag,variogram,type="o",col="#FFA042",pch=19);
  return(variogram);
}


lazy= function(x,y){
  par(mfrow=c(1,2));
  x_acf = acf(x,lag=25);
  x_variogram = variogram(x,25);
  xx= cbind(lag=1:25,ACF=x_acf$acf[2:26],x_variogram);
  
  filename=paste(y,".csv",sep="");
  write.table(xx,file=filename,row.names = F,col.names = c("Lag","ACF","Variogram"),sep = ",");
  par(mfrow=c(1,1));
  return (xx);
  
}
lazy(diff(b6[,2]),"Air")

#2
b4 = read.csv("B.4.csv")
b4.ts = ts(b4[,2])
plot(b4.ts, ylab = "Production")
b4.ts.acf = acf(b4.ts)
b4.acf = acf(b4[,2])
lazy1 = lazy(b4.ts, "Production")
write.csv(lazy1, file = "number2.csv")
lazy2 = lazy(diff(b4.ts), "Production")
write.csv(lazy2, file = "set2.csv")

#3
library(forecast)
b3 = read.csv("b3.csv")
#a
plot.ts(b3[[2]], type = "l", ylab = "Reading", xlab = "Time", main = "Time Series Plot")
#b
simple  = ses(b3[[2]], h = 10, alpha = 0.1, initial = "simple")
autoplot(simple, ylab = "Reading", main = "Forecasting from Simple Exponential Smoothing lambda = 0.1")
#c
predict = simple$fitted[1:10]
residuals = simple$residuals[1:10]
output = cbind(predict, residuals)
write.csv(output, file = "number4.csv")
#d
simple = ses(b3[[2]], h = 10, initial = "simple")

##5
simple = ses(b4[[2]], h = 10, initial = "simple")
predict = simple$fitted[1:10]
residuals = simple$residuals[1:10]
output = cbind(predict, residuals)
write.csv(output, file = "number5.csv")
autoplot(simple, ylab = "Production", main = "Forecasting from Simple Exponential Smoothing")

#6
b10 = read.csv("b10.csv")
plot.ts(b10[[2]], ylab = "Miles", main = "Time Series Plot")
b10.ts = ts(b10[[2]])
mod = auto.arima(b10.ts, seasonal = TRUE, test = 'adf', ic = 'aic')
mod
plot(forecast(mod, h = 12))
mod1 = auto.arima(b10.ts, seasonal = TRUE)
mod1

#8
data = read.csv("e37.csv")
##a
model = lm(y ~ x1 + x2 + x3 + x4, data = data)
summary(model)
plot(model$residuals)
##b
for(i in 1:8){
  sample = c(data[i,5], data[i,6], data[i,7])
  data[i,8] = var(sample)
}
model.wls = lm(y ~ x1 + x2 + x3 + x4, data = data, weights = data$V8)
summary(model.wls)
plot(model.wls$residuals)
##c
model.variance = lm(V8 ~ x1 + x2 + x3 + x4, data = data)
summary(model.variance)
plot(model.variance$residuals)

model.variance.wls = lm(V8 ~ x1 + x2 + x3 + x4, data = data, weights = data$V8)
summary(model.variance.wls)

##9
##a
data = read.csv("b25.csv")
attach(data)
null = lm(Fatalities ~ 1, data = data)
full = lm(Fatalities ~ ., data = data)
forward.lm = step(null, scope = list(lower = null, upper = full), direction = "forward")

model = lm(Fatalities ~ Registered.Motor.Vehicles..Thousands. + Vehicle.Miles.Traveled..Billions., data = data)
summary(model)
plot(model$residuals)
library(lmtest)
model.dwt = dwtest(Fatalities ~ Registered.Motor.Vehicles..Thousands. + Vehicle.Miles.Traveled..Billions., data = data)
model.dwt

##b


##10
#a
library(forecast)
library(tseries)
library(orcutt)
data = read.csv("e36.csv")
colnames(data) = c("time", "manufacturer", "industry")
attach(data)
model = lm(industry ~ manufacturer)
summary(model)
plot(model$residuals, xlab = "time")
plot(industry ~ manufacturer)
##b
model.dwt = dwtest(industry ~ manufacturer)
model.dwt
##c
model.fit = cochrane.orcutt(model)
model.fit
##d

##e
xt = diff(data[[2]])
yt = diff(data[[3]])
diff = data.frame(cbind(xt, yt))
model.diff = lm(yt ~ xt, data = diff)
summary(model.diff)

model.diff.dwt = dwtest(model.diff)
model.diff.dwt
