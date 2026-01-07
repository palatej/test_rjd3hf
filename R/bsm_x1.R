suppressPackageStartupMessages(library(rjd3highfreq))
source("./R/bias.R")
usclaims<-read.table("./Data/usclaims.txt")
y=usclaims[,1]

rslt<-rjd3highfreq::fractionalAirlineEstimation(log(y), periods=c(365.25/7), outliers=c("ao"), criticalValue = 6)
z<-rslt$model$linearized

bsm_hr<-function(z, har=1:11){
  # create the model
  sm<-rjd3sts::model()
  eq<-rjd3sts::equation("eq")
  # create the components and add them to the model
  rjd3sts::add(sm, rjd3sts::noise("n"))
  rjd3sts::add(eq, "n")
  rjd3sts::add(sm, rjd3sts::locallineartrend("ll"))
  rjd3sts::add(eq, "ll")
  rjd3sts::add(sm, rjd3sts::periodic("s", 365.25/7, har))
  rjd3sts::add(eq, "s")
  rjd3sts::add(sm, eq)
  #estimate the model
  rslt<-rjd3sts::estimate(sm, z, marginal=F, initialization="Augmented_Robust", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-10)
  return(rslt)
}

bsm_w<-function(z, model="Trigonometric"){
  # create the model
  sm<-rjd3sts::model()
  eq<-rjd3sts::equation("eq")
  # create the components and add them to the model
  rjd3sts::add(sm, rjd3sts::noise("n"))
  rjd3sts::add(eq, "n")
  rjd3sts::add(sm, rjd3sts::locallineartrend("ll"))
  rjd3sts::add(eq, "ll")
  rjd3sts::add(sm, rjd3sts::seasonal("s", 52, model))
  rjd3sts::add(eq, "s")
  rjd3sts::add(sm, eq)
  #estimate the model
  rslt<-rjd3sts::estimate(sm, z, marginal=F, initialization="Augmented_Robust", optimizer="LevenbergMarquardt", concentrated=TRUE, precision = 1e-10)
  return(rslt)
}

q1<-bsm_hr(rslt$model$linearized)
q2<-bsm_w(rslt$model$linearized)

ss1<-result(q1, "ssf.smoothing.states")

t1<-ss1[,2]
i1<-ss1[,1]
s1<-z-t1-i1


t2<-result(q2, "ssf.smoothing.cmp(1)")
i2<-result(q2, "ssf.smoothing.cmp(0)")
s2<-z-t2-i2


plot(s1, type='l', col='red')
lines(s2, col='blue')

plot(z, type='l', col='gray')
lines(t1, col='red')
lines(t2, col='blue')


plot(z, type='l', col='gray')
lines(z-s1, col='red')
lines(z-s2, col='blue')

plot(s1, type='l', col='red')
lines(s2, col='blue')


plot(s1[1:103], type='l', col='red')
lines(s2[1:103], col='blue')
