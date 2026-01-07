# In order to run the test change the file names to test_LS.R and test_LS.csv, 
# and provide below the path to the .csv file.

# With this particular input the pre-processing step finds a level shift that 
# does not exist. The pre-processed and the SA are corrupted as a result.

library(rjd3highfreq)
library(rjd3toolkit)

one_geo <- read.csv("./Data/test_LS.csv")
one_geo$date <- as.Date(one_geo$date)

ts.plot(ts(one_geo$VALUE, start=c(2010, 1), frequency=52)
        , gpars = list(main=("Published data(raw)")))

pre_pro <- fractionalAirlineEstimation(y=one_geo$VALUE, 
                                       y_time = one_geo$date,
                                       periods=52.1785, 
                                       outliers = c("ao", "ls", "wo"),
                                       criticalValue = 5, #just to get more ls
                                       log = T
)

# level-shift effects 
ils<-startsWith(pre_pro$model$variables, 'LS')
if (any(ils)){
  ls<-pre_pro$model$xreg[,ils]%*%pre_pro$model$b[ils]
  tstat_ls<-pre_pro$model$b[ils]/sqrt(diag(pre_pro$model$bcov)[ils])
}else{
  ls<-NULL
  tstat_ls<-NULL
}
  
# same for AO
iao<-startsWith(pre_pro$model$variables, 'AO')
if (any(iao)){
  ao<-pre_pro$model$xreg[,ils]%*%pre_pro$model$b[iao]
  tstat_ao<-pre_pro$model$b[iao]/sqrt(diag(pre_pro$model$bcov)[iao])
}else{
  ao<-NULL
  tstat_ao<-NULL
}
#... To be completed for other variables

plot(pre_pro)
# Pre-processing finds this level shift, which is also visible on 
# output graphs but not in the raw data 
pre_pro$model$variables

amb.woy <- fractionalAirlineDecomposition(
  y = pre_pro$model$linearized, 
  y_time = one_geo$date,
  period=52.1785 
)

plot(amb.woy, type_chart = "y-sa-trend")
# The same level shift appears in the SA without existing in the raw

ts.plot(window(ts(one_geo$VALUE, start=c(2010, 1), frequency=52),
               start = c(2019,1), end=c(2022,52))
        , gpars = list(main=("? no level shift visible in April 2020, with the raw data"))
        )

sa_final<-amb.woy$decomposition$sa
t_final<-amb.woy$decomposition$t
i_final<-amb.woy$decomposition$i

if (! is.null(ls)) {sa_final<-sa_final+ls; t_final<-t_final+ls}
if (! is.null(ao)) {sa_final<-sa_final+ls; i_final<-i_final+ls}

ts.plot(ts(one_geo$VALUE, start=c(2010, 1), frequency=52),
               exp(sa_final)
        , gpars = list(main=("Raw(orange) vs SA(green) output. SA cannot be at a different level in any year."), 
                  col = c("orange", "green"))
)



