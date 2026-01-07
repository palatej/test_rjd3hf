edf<-read.csv("./Data/edf.csv")
y<-edf$Electricity

# Create holidays. See also testholidays.R for other examples and for weekly variables.
FR <- rjd3toolkit::national_calendar(list(
  rjd3toolkit::fixed_day(7,14),
  rjd3toolkit::fixed_day(5,8),
  rjd3toolkit::special_day('NEWYEAR'),
  rjd3toolkit::special_day('CHRISTMAS'),
  rjd3toolkit::special_day('MAYDAY'),
  rjd3toolkit::special_day('EASTERMONDAY'),
  rjd3toolkit::special_day('ASCENSION'),
  rjd3toolkit::special_day('WHITMONDAY'),
  rjd3toolkit::special_day('ASSUMPTION'),
  rjd3toolkit::special_day('ALLSAINTSDAY'),
  rjd3toolkit::special_day('ARMISTICE')
))

# raw adjustment (removal of the trend)
sa<-rjd3stl::mstl(y, c(7, 365), multiplicative = FALSE)
z<-sa$decomposition[,'y']/sa$decomposition[,'t']
n<-length(z)

x<-0:364
x<-c(x,x,x,x,x)
start<-rjd3sax::.periodic_adaptive_splines(x=x, y=z[1:(365*5)], period=365.25, nknots=350)

knots<-rjd3toolkit::result(start, "selectedKnots")


hol<-rjd3toolkit::holidays(FR, "1996-01-01", 
              length = length(y), type = "Skip", nonworking = c(6,7), single = FALSE)



model<-rjd3sts::model()
ll<-rjd3sts::locallevel('l', variance = 1, fixed=FALSE)
seas1<-rjd3sts::seasonal('w', 7, "HarrisonStevens", variance = 0.1, fixed=FALSE )
#seas2<-rjd3sts::splines_regular('y', 365.25, nnodes=45)
seas2<-rjd3sts::splines_daily('y', startYear=1996, knots=knots, 
                              variance = 0.01, fixed = FALSE)
n<-rjd3sts::noise('n', variance = 1, fixed=FALSE)
rjd3sts::add(model, ll)
rjd3sts::add(model, seas1)
rjd3sts::add(model, seas2)
rjd3sts::add(model, rjd3sts::reg("cal", hol, 0.1))
rjd3sts::add(model, n)

rslt<-rjd3sts::estimate(model, y)
m1<-rjd3sts::smoothed_components(rslt)

n<-length(y)
np<-365*3+1
idx0<-1:np
matplot(idx0, m1[idx0, c(3,4,2)], "l", col=c("blue", "magenta", "gray"))

idx<-(n/2-np/2)+1:np
matplot(idx, m1[idx, c(3,4,2)], "l", col=c("blue", "magenta", "gray"))

idx1<-(n-np):n
matplot(idx1, m1[idx1, c(3,4,2)], "l", col=c("blue", "magenta", "gray"))

matplot(1:n, m1[,c(2,3,4)], "l", col=c("gray","blue", "magenta"))

source("./R/butterworth.R")

tc<-butterworth(m1[,1],d=1, var=100000)
matplot(1:n, cbind(m1[,1],tc[,1]), "l", col=c("gray","red"))
