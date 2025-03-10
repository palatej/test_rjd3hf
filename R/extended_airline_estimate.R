library(rjd3highfreq)
library(rjd3toolkit)

edf<-read.csv("./Data/edf.csv")

MyCalendar <- national_calendar(list(
  special_day('MAYDAY'),
  special_day('NEWYEAR'),
  special_day('CHRISTMAS'),
  special_day('CHRISTMAS', 1),
  easter_day(1), # Easter Monday
  special_day('ASCENSION'),
  special_day('WHITMONDAY'),
  special_day('ASSUMPTION'),
  special_day('ALLSAINTSDAY'),
  fixed_day(5,8),
  fixed_day(7, 14),
  special_day('ARMISTICE')))

y<-edf$y

hol<-holidays(MyCalendar, "1996-01-01", length = length(y), type = "Skip", single = FALSE)

spec1<-.extendedairline_spec(7)

regarima1<-.extendedairline_regarima(y, spec1)

rslt<-.extendedairline_estimation(regarima1, spec1)

print(result(rslt, "likelihood.ll"))

spec2<-.extendedairline_spec(c(7, 365.25))

regarima2<-.extendedairline_regarima(y, spec2)

rslt<-.extendedairline_estimation(regarima2, spec2)

print(result(rslt, "likelihood.ll"))

regarima1<-.extendedairline_regarima(log(y), spec1)

rslt<-.extendedairline_estimation(regarima1, spec1)

print(result(rslt, "likelihood.ll"))

regarima2<-.extendedairline_regarima(log(y), spec1, X=hol)

rslt<-.extendedairline_estimation(regarima2, spec1)

print(result(rslt, "likelihood.aicc"))

regarima3<-.extendedairline_regarima(log(y), spec2, X=hol)

rslt<-.extendedairline_estimation(regarima3, spec2)

print(result(rslt, "likelihood.aicc"))
