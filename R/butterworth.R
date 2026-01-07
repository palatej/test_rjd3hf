
butterworth<-function(x, d=2, var=1000){
  if (d == 1)
    delta=c(1,-1)
  else if (d == 2)
    delta=c(1,-2,1)
  else stop("not implemented yet")
  I<-rjd3toolkit::arima_model(name="signal", delta = delta )
  N<-rjd3toolkit::arima_model(name="noise", variance = var)
  
  ucm<-rjd3toolkit::ucarima_model(components=list(I, N))
  
  return (rjd3toolkit::ucarima_estimate(x, ucm, stdev=FALSE))
}

