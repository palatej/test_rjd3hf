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

spec1<-.extended_airline_spec(7)

regarima1<-.extended_airline_regarima(y, spec1)

print(.extended_airline_loglevel(regarima1, spec1))

spec2<-.extended_airline_spec(c(7, 365.25))

regarima2<-.extended_airline_regarima(y, spec2)

print(.extended_airline_loglevel(regarima2, spec2))

regarima3<-.extended_airline_regarima(y, spec1, X= hol)

print(.extended_airline_loglevel(regarima3, spec1))

regarima4<-.extended_airline_regarima(y, spec2, X=hol)

print(.extended_airline_loglevel(regarima4, spec2))
