###################################################################################
############  Plotting text from Google Trends and LinkedIn over time  ############ 
###################################################################################

# installing the packages
# install.packages("gtrendsR")
#install.packages("Rlinkedin")

## loading the library 
library(gtrendsR)
library(Rlinkedin)

# setting the time period
time_period<-"2016-01-01 2018-07-18"

# creating some exploratory plots
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

