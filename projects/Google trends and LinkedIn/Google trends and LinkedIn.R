
# installing the package
# install.packages("gtrendsR")

## loading the library 
library(gtrendsR)

# setting the time period
time_period<-"2016-01-01 2018-07-18"

# creating some exploratory charts
res_trend <- gtrends(c("נהג אוטובוס"),geo=c("IL"), time=time_period)
plot(res_trend)

res_trend <- gtrends(c("נהג מונית"),geo=c("IL"), time=time_period)
plot(res_trend)

res_trend <- gtrends(c("נהג הסעות"),geo=c("IL"), time=time_period)
plot(res_trend)

res_trend <- gtrends(c("לוח דרושים"),geo=c("IL"), time=time_period)
plot(res_trend)

res_trend <- gtrends(c("דרושים עובדים"),geo=c("IL"), time=time_period)
plot(res_trend)

res_trend <- gtrends(c("הכשרה מקצועית"),geo=c("IL"), time=time_period)
plot(res_trend)

