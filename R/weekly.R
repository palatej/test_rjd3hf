usclaims<-read.table("./Data/usclaims.txt")
y=usclaims[,1]
rslt<-rjd3highfreq::fractionalAirlineEstimation(log(y), periods=c(365.25/7), outliers=c("ao"), criticalValue = 6)

z<-rslt$model$linearized

m1<-rjd3highfreq::fractionalAirlineDecomposition(z, 365.25/7)
m2<-rjd3highfreq::fractionalAirlineDecomposition(z, 52)
n<-length(y)
idx<-(n-52*3):n
plot(idx, m1$decomposition$s[idx], "l")
lines(idx, m2$decomposition$s[idx], col="red")

period<-365.25/7

model<-rjd3sts::model()

ll<-rjd3sts::locallevel('l')
seas<-rjd3sts::splines_regular('s', period, nnodes=25)
n<-rjd3sts::noise('n')
rjd3sts::add(model, ll)
rjd3sts::add(model, seas)
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, z)
sdecomp<-rjd3sts::smoothed_components(rslt)
lines(idx, sdecomp[,2][idx], col="blue")

