###################################################################################
############  Plotting text from Google Trends and LinkedIn over time  ############ 
###################################################################################

# installing the packages
# install.packages("gtrendsR")
# install.packages("httr")
# install.packages("devtools")
# install.packages("Rlinkedin")
# install_github("mpiccirilli/Rlinkedin")

## loading the libraries 
library(Rlinkedin)
library(gtrendsR)
library(devtools)
library(httr)


# Google trends
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


# LinkedIn

in.auth <- inOAuth()
app_name <- "xxx"
consumer_key <- "xxx"
consumer_secret <- "xxx"

in.auth <- inOAuth(app_name, consumer_key, consumer_secret)


endpoints <- oauth_endpoints("linkedin")
endpoints

myapp <- oauth_app("linkedin",
                   key = "xxx",
                   secret = "XXX"
)

token <- oauth2.0_token(endpoints, myapp, scope = "r_liteprofile")

req <- GET("https://api.linkedin.com/v2/me", config(token = token))
stop_for_status(req)
content(req)

# https://github.com/r-lib/httr/blob/master/demo/oauth2-linkedin.r


search.results <- searchJobs(token = in.auth, keywords = "נהג אוטובוס", country="IL")
colnames(search.results)
nrow(search.results)

search.results <- searchJobs(token = in.auth, keywords = "נהג מונית", country="IL")
colnames(search.results)
nrow(search.results)

search.results <- searchJobs(token = in.auth, keywords = "נהג הסעות", country="IL")
colnames(search.results)
nrow(search.results)

search.results <- searchJobs(token = in.auth, keywords = "Designer", country="IL")
colnames(search.results)
nrow(search.results)

search.results <- searchJobs(token = in.auth, keywords = "UX Designer", country="IL")
colnames(search.results)
nrow(search.results)
