library(rjd3highfreq)
library(rjd3modelling)
traffic<-read.csv("./Data/traffic.csv")
y<-log(traffic[-(1:5844),2])

jhol<-calendar.new()
calendar.holiday(jhol, "NEWYEAR")
calendar.fixedday(jhol, month=3, day=21)
calendar.holiday(jhol, "NEWYEAR", offset = 1)
calendar.holiday(jhol, "GOODFRIDAY")
calendar.holiday(jhol, "EASTERMONDAY")
calendar.fixedday(jhol, month=4, day=27)
calendar.holiday(jhol, "MAYDAY")
calendar.fixedday(jhol, month=6, day=16)
calendar.fixedday(jhol, month=8, day=9)
calendar.fixedday(jhol, month=9, day=24)
calendar.fixedday(jhol, month=12, day=16)
calendar.holiday(jhol, "CHRISTMAS")
calendar.holiday(jhol, "CHRISTMAS", offset=1)


vars<-rjd3modelling::holidays(jhol, "2010-01-01", length = length(y), type = "Skip")

# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)

rslt<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7))

# some output (will be improved in future releases)
print(rslt$estimation$parameters)
print(rslt$model$variables)
print(rslt$model$b)
print(rslt$model$b/sqrt(diag(rslt$model$bcov)))

# linearized series (y-Xb)      
lin<-rslt$model$linearized

c<-rjd3highfreq::fractionalAirlineDecomposition(lin, period=7)
c1<-rjd3highfreq::fractionalAirlineDecomposition(c$decomposition$sa, period=365.25)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effects))
w<-c$decomposition$s
t<-c1$decomposition$t
sa<-c1$decomposition$sa
s<-c1$decomposition$s
i<-c1$decomposition$i
seatsdecomp<-cbind(lin,t,sa,w,s,i)

x11c<-rjd3highfreq::x11(lin, period=7, mul=F, trend.horizon = 15, seas.s1 = 'S3X15')
x11c1<-rjd3highfreq::x11(x11c$decomposition$sa, mul=F, period=365.25, trend.horizon = 367)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effects))
x11w<-x11c$decomposition$s
x11t<-x11c1$decomposition$t
x11sa<-x11c1$decomposition$sa
x11s<-x11c1$decomposition$s
x11i<-x11c1$decomposition$i
x11decomp<-cbind(lin,x11t,x11sa,x11w,x11s,x11i)


# Some charts. 
n<-length(y)
plot(seatsdecomp[(n-400):n, "w"], type="l")
lines(seatsdecomp[(n-400):n, "s"], col="red")
plot(y[(n-400):n], type="l", col="gray")
lines(seatsdecomp[(n-400):n, "sa"], col="blue")
lines(seatsdecomp[(n-400):n, "t"], col="red")

plot(x11decomp[(n-400):n, "x11w"], type="l")
lines(x11decomp[(n-400):n, "x11s"], col="red")


elin<-lin[(n-228):n]
ec<-rjd3highfreq::fractionalAirlineDecomposition(elin, period=7, TRUE)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effets))
ew<-ec$decomposition$s
et<-ec$decomposition$t
esa<-ec$decomposition$sa

# Some charts. 
plot(seatsdecomp[(n-228):n, "w"], type="l")
lines(ew, col="red")
plot(y[(n-228):n], type="l", col="gray")
lines(esa, col="blue")
lines(et, col="red")
lines(seatsdecomp[(n-228):n, "sa"], col="green")

rslt_out<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7), outliers = c('ao', 'ls'), criticalValue = 10)
