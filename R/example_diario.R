suppressPackageStartupMessages(library(rjd3highfreq))
library(xlsx)
#edf<-read.table("./Data/edf.txt")

edf2<-read.csv("./Data/edf.csv")

y<-log(edf2$y)

