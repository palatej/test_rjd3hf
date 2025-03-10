library(rjd3highfreq)
library(rjd3toolkit)

edf<-read.csv("./Data/edf.csv")

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

y<-edf$y

hol<-holidays(MyCalendar, "1996-01-01", length = length(y), type = "Skip", single = FALSE)

spec1<-.extendedairline_spec(7)

regarima1<-.extendedairline_regarima(y, spec1)

print(.extendedairline_loglevel(regarima1, spec1))

spec2<-.extendedairline_spec(c(7, 365.25))

regarima2<-.extendedairline_regarima(y, spec2)

print(.extendedairline_loglevel(regarima2, spec2))

regarima3<-.extendedairline_regarima(y, spec1, X= hol)

print(.extendedairline_loglevel(regarima3, spec1))

regarima4<-.extendedairline_regarima(y, spec2, X=hol)

print(.extendedairline_loglevel(regarima4, spec2))
