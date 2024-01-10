suppressPackageStartupMessages(library(rjd3highfreq))
usclaims<-read.table("./data/usclaims.txt")
edf<-read.csv("./data/edf.csv")
y1=log(usclaims[,1])
y2<-log(edf$y)

rslt_w1<-rjd3highfreq::fractionalAirlineEstimation(y1, periods=c(365.25/7), outliers=c("ao"), criticalValue = 6)
rslt_w2<-rjd3highfreq::fractionalAirlineEstimation(y1, periods=c(52), outliers=c("ao"), criticalValue = 6)

library(rjd3toolkit)

FR_Calendar <- national_calendar(list(
  fixed_day(5,8),
  fixed_day(7, 14),
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


hol<-holidays(FR_Calendar, "1996-01-01", length = length(y2), type = "Skip")


rslt_d1<-rjd3highfreq::fractionalAirlineEstimation(y2, periods=c(7), x=hol, outliers=c("ao"), criticalValue = 8)
rslt_d2<-rjd3highfreq::fractionalAirlineEstimation(y2, periods=c(7, 365.25), x=hol, outliers=c("ao"), criticalValue = 8)

