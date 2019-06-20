library(openxlsx)
library(tseries)
library(forecast)
### Q1
Q1 = read.xlsx("2019Spring_Time_Series_Analysis_final_exam_0620.xlsx", sheet = 1)
data = ts(Q1$Tobacco.Production)
model = auto.arima(data, seasonal = FALSE, test = "adf", ic = "aic")
summary(model)
predict_10 = forecast(model, h = 10)
plot(predict_10)
forecast_result = data.frame(Year = c(1986:1995), predict_10$mean, predict_10$lower, predict_10$upper)
forecast_result = forecast_result[,c(1,2,4,6)]
write.csv(forecast_result, file = "Q1.csv")

### Q2
Q2 = read.xlsx("2019Spring_Time_Series_Analysis_final_exam_0620.xlsx", sheet = 2)
data = ts(Q2)
model = auto.arima(data, seasonal = TRUE, test = "adf", ic = "aic")
summary(model)
forecast_result = forecast(model, h = 12, level = 99)
plot(forecast_result)
forecast = data.frame(Index = c(1:12), forecast_result$mean, forecast_result$lower, forecast_result$upper)
write.csv(forecast, file = "Q2.csv")

### Q3
Q3 = read.xlsx("2019Spring_Time_Series_Analysis_final_exam_0620.xlsx", sheet = 3)
library(TSA)
ts.xt<-ts(Q3$Ad)
lag3.x<-lag(ts.xt,-3)
ts.yt<-ts(Q3$sales)
dat3<-cbind(ts.xt,lag3.x,ts.yt)
dimnames(dat3)[[2]]<-c("xt","lag3x","yt")
data2<-na.omit(as.data.frame(dat3))

visc.tf <- arimax(data2$yt, order=c(1,0,0), xtransf=data.frame(data2$lag3x),
                transfer=list(c(2,0)), include.mean = FALSE)
visc.tf

model = as.vector(fitted(visc.tf))
forecast_result = forecast(model, h = 12, level = 95)
plot(forecast_result)
forecast = data.frame(Index = c(1:12), forecast_result$lower, forecast_result$mean, forecast_result$upper)
write.csv(forecast, file = "Q3.csv")

### Q4
Q4 = read.xlsx("2019Spring_Time_Series_Analysis_final_exam_0620.xlsx", sheet = 4)
prevention = Q4[1:129,]
pre_data = ts(prevention)
model1 = auto.arima(pre_data, seasonal = FALSE, test = "adf", ic = "aic")
summary(model1)

T <- nrow(Q4)
St <- c(rep(0,129),rep(1,(T-129)))
sales.tf<-arimax(diff(ts(Q4)), order=c(0,0,1), xtransf= St[2:T],
                 transfer=list(c(0,0)), include.mean = FALSE)
sales.tf
model = as.vector(fitted(sales.tf))
forecast_result = forecast(model, h = 12, level = 95)
plot(forecast_result)
forecast = data.frame(Index = c(1:12), forecast_result$lower, forecast_result$mean, forecast_result$upper)
write.csv(forecast, file = "Q4.csv")
