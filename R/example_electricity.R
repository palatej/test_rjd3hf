library(rjd3highfreq)
library(rjd3toolkit)
#edf<-read.table("./Data/edf.txt")

edf2<-read.csv("./Data/edf.csv")

y<-log(edf2$y)

# Complete "two-steps" seasonal adjustment.
# Step 1. RegArima pre-processing
# Step 2. Decomposition of the linearized series (in this example, canonical decomposition is used. 
# It could be replaced by X11 or STL (or others). see testall.R for examples
# The final result (combining step 1 and step 2) is not automated, but is trivial (regressors and coefficients
# are provided in the output). 

# Create holidays. See also testholidays.R for other examples and for weekly variables.
MyCalendar <- national_calendar(list(
  fixed_day(5,8),
  fixed_day(7, 21),
  special_day('NEWYEAR'),
  special_day('CHRISTMAS'),
  special_day('CHRISTMAS', 1),
  special_day('MAYDAY'),
  easter_day(1), # Easter Monday
  special_day('ASCENSION'),
  special_day('WHITMONDAY'),
  special_day('ASSUMPTION'),
  special_day('ALLSAINTSDAY'),
  special_day('ARMISTICE')))


hol<-holidays(MyCalendar, "1996-01-01", length = length(y), type = "Skip")

# adding some user-defined variables. Dummies for end of months (first obs at 1/1/leapyear)
months<-c(31,28,31,30,31,30,31,31,30,31,30,31)
lpmonths<-c(31,29,31,30,31,30,31,31,30,31,30,31)
y4<-c(lpmonths,months,months,months)
cy40<-cumsum(c(y4, y4, y4, y4, y4,y4, y4, y4, y4, y4))
idx<-cy40[1:(Position(function(x){x>length(y)}, cy40)-1)]

endofmonth1<-array(0,dim=length(y))
endofmonth2<-array(0,dim=length(y))

endofmonth1[idx]<-1
endofmonth2[idx-1]<-1

vars<-cbind(hol, endofmonth1, endofmonth2)


# RegArima (fractional airline), using the pre-specified regression variables (X), any periodicities (all are considered together)
# and automatic outlier detection. Possible outliers are additive outliers (ao = ...0 0 1 0 0 ...), 
# level shifts (ls = ...0 0 1 1 1...) and "shift" outliers (wo = 0 0 1 -1 0 ...)

rslt0<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7), outliers=c("ao", "ls"), criticalValue = 6)

# some output (will be improved in future releases)
print(rslt0$estimation$parameters)
print(rslt0$model$variables)
print(rslt0$model$b)
print(rslt0$model$b/sqrt(diag(rslt0$model$bcov)))

rslt<-rjd3highfreq::fractionalAirlineEstimation(y, x=vars, periods=c(7, 365.25), ndiff=3, outliers=c("ao", "ls"), criticalValue = 6)

# some output (will be improved in future releases)
print(rslt$estimation$parameters)
print(rslt$model$variables)
print(rslt$model$b)
print(rslt$model$b/sqrt(diag(rslt$model$bcov)))

# linearized series (y-Xb)      
lin<-rslt$model$linearized

c<-rjd3highfreq::fractionalAirlineDecomposition(lin, period=7)
c1<-rjd3highfreq::multiAirlineDecomposition(c$decomposition$sa, periods=365, ndiff=2)
# final decomposition (w=weekly component, s=annual component. Final seasonal component is w+s (+calendar effets))
w<-c$decomposition$s
t<-c1$decomposition$t
sa<-c1$decomposition$sa
s<-c1$decomposition$s
i<-c1$decomposition$i

seatsdecomp<-cbind(lin,t,sa,w,s,i)

c2<-rjd3highfreq::multiAirlineDecomposition(lin, periods=c(7, 365.25), ndiff=2)
w<-c2$decomposition$s_7
t<-c2$decomposition$t
s<-c2$decomposition$s_365.25
i<-c2$decomposition$i
sa<-c2$decomposition$sa
seatsdecomp2<-cbind(lin,t,sa,w,s,i)

m3<-fractionalAirlineEstimation(lin, periods=c(7, 365), ndiff=3)


c3<-rjd3highfreq::multiAirlineDecomposition(lin, periods=c(7, 365.25), ndiff=3)
w<-c3$decomposition$s_7
t<-c3$decomposition$t
s<-c3$decomposition$s_365.25
i<-c3$decomposition$i
sa<-c3$decomposition$sa
seatsdecomp3<-cbind(lin,t,w,s)


# Some charts. In this example, we can see that weekly component and annual component are not independent.
plot(exp(seatsdecomp[1:1097, "s"]), type="l")
lines(exp(seatsdecomp[1:1097, "w"]), col="red")

plot(exp(seatsdecomp2[1:1097, "s"]), type="l")
lines(exp(seatsdecomp2[1:1097, "w"]), col="red")

plot(exp(seatsdecomp[1:140, "w"]), type='l')
lines(exp(seatsdecomp2[1:140, "w"]), col="red")

plot(exp(seatsdecomp[-(1:6000), "lin"]), type='l')
lines(exp(seatsdecomp[-(1:6000), "sa"]), col='blue')
lines(exp(seatsdecomp[-(1:6000), "t"]), col='red')
plot(exp(seatsdecomp2[-(1:6000), "lin"]), type='l')
lines(exp(seatsdecomp2[-(1:6000), "sa"]), col='blue')
lines(exp(seatsdecomp2[-(1:6000), "t"]), col='red')

