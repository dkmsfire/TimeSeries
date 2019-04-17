# 45
library(ctmm)
b6 = read.csv("b.6.csv")
b6_ts_Ano = ts(b6[,2])

b6_ts_CO2 = ts(b6[,3])
b6_ts_Ano

plot.ts(b6_ts_Ano)
plot.ts(b6_ts_CO2)

acf_air = acf(b6_ts_Ano)
acf_co2 = acf(b6_ts_CO2)

acf_air
acf_co2

air = data.frame(lag = acf_air[[4]] , acf = acf_air[[1]])
co2 = data.frame(lag = acf_co2[[4]] , acf = acf_air[[1]])

write.csv(air , file = "air.csv")
write.csv(co2 , file = "co2.csv")

plot(diff(b6_ts_Ano))
plot(diff(b6_ts_CO2))

air_diff = acf(diff(b6_ts_Ano))
co2_diff = acf(diff(b6_ts_CO2))

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
lazy(diff(b6[,3]),"CO2")

# 26 27 28 
b22 = read.csv("b22Clean.csv")
b22$NewEnergy = as.integer(b22[,2])
lazy(b22[,3],"Energy")
lazy(diff(b22[,3]) , "Diff(Energy)")

library(zoo)
am6 = rollmean(diff(b22[,3]) , k=6)
plot.ts(diff(b22[,3]) , col = "red")
line(am6)
plot.zoo(cbind(diff(b22[,3]) , am6 ), plot.type = "single" , col = c("red" , "blue") , ylab ="" , xlab = "Time")
