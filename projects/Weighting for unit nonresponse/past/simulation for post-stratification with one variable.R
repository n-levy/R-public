################################################
###### simulation of post-stratification #######
################################################

# simulating a dataset 

# Three predictor variables - 'device_type', 'country', 'version'
# One outcome variable - 'outcome'
# 100,000 interviewees

# creating the dataset
# device type
device_type_values<-c('LE','MR','HE')
device_type_values<-factor(device_type_values, levels=c('LE','MR','HE'), ordered=TRUE)
levels(device_type_values)
device_type <- sample(device_type_values, size=10^5, replace=TRUE, prob=c(0.5,0.3,0.2))
head(device_type)
table(device_type)  

# country
country_values<-c('Brazil', 'Indonesia', 'India', 'Mexico' , 'Philippines', 'Thailand')
country_values<-factor(country_values, 
                       levels=c('Brazil', 'Indonesia', 'India', 'Mexico' , 'Philippines', 'Thailand')
                           , ordered=TRUE)
levels(country_values)
country <- sample(country_values, size=10^5, replace=TRUE, prob=c(0.2,0.2,0.3,0.1,0.1,0.1))
head(country)
table(country)  

# version
version_values<-c('Old','New')
version_values<-factor(version_values, levels=c('Old','New'), ordered=TRUE)
levels(version_values)
version <- sample(version_values, size=10^5, replace=TRUE, prob=c(0.2,0.8))
head(version)
table(version)  

# creating the dataset with the predicted variables only
population_data<-as.data.frame(cbind(device_type,country,version))
class(population_data)
head(population_data)
# view(population_data[1:10,])

#######################################################################################
########   simulation #1 - unit non-response associated with only one variable   ######
#######################################################################################

# outcome
population_data$outcome[population_data$device==1]<-1
population_data$outcome[population_data$device==2]<-2
population_data$outcome[population_data$device==3]<-3

table(population_data$outcome)

# creating sample data
library(dplyr)
dat<-sample_n(population_data,10^3)

class(dat)
dim(dat)
names(dat)
head(dat)
# view(dat[1:10,])

# checking number of respondents from each device type
table (dat$device_type)

# post-stratifying by device type
# caluclating the mean of the outcome in the population data
mean(population_data$outcome)

# calculating the mean of the outcome in the sample, before post-stratification
mean(dat$outcome)

# adding an fpc column to the data (finite population correction factor)
dat$fpc <- 10^5
head(dat)

# summarizing the variable of interst
summary(dat$outcome)

# marking the dataset as 'survey' data in R
# this assumes no weight and simple random sampling
# library(survey)
preliminary.design <- 
  svydesign( 
    id = ~1 , 
    data = dat ,
    fpc = ~fpc
  )

# creating a variable that contains 
# the frequencies of the stratification variable (device type)
# in the population.
table(device_type)
freq_device_type<-as.numeric(table(device_type))
freq_device_type

table(dat$device_type)

# Create a secondary data.frame object with two columns 
# in order to perform the actual post-stratification: 
# 1) The variable used to post-stratify by (in this example - type of device). 
# This column is data set-specific. 
# 2) The post-stratification target frequencies (Freq). 
# This column should always be called Freq 
# because the R survey package searches for that column name.

ps.weights <-
  data.frame(
    device_type = c( 1 , 2, 3) ,
    Freq = freq_device_type 
  )

ps.weights$Freq

# creating a weighted dataset, in which 
# the weights are obtained by post-stratification
dat_weighted <- 
  postStratify(
    preliminary.design ,
    strata = ~device_type ,
    population = ps.weights
  )

# exploring the data
class(dat_weighted)

# View the weighted total population of this survey design, 
# by referring to the Freq column from the post-stratification 
# specification data.frame:
sum(ps.weights$Freq)

# View the number of unique poststrata in this survey design, 
# by referring to the type column from the post-stratification 
# target weights data.frame:
length(unique(ps.weights$device_type))

# View the degrees of freedom for this survey design object:
degf(dat_weighted)

# Count the number of unweighted observations 
# where the variable 'outcome' is not missing:
unwtd.count(~outcome, dat_weighted)

# Print the mean and standard error of this variable:
svymean(~outcome, dat_weighted)

# Print the mean and standard error of the 'outcome' variable, 
# broken out by the 'device_type' variable:
svyby(~outcome, ~device_type, dat_weighted, svymean)

# Alternatively, the result of a function call like svymean, svytotal, or svyby 
# can be stored into a secondary R object.

mysvyby <- svyby(~outcome, ~device_type, dat_weighted, svytotal)

# Once created, this svyby can be queried independently from the mydesign object. 
# For example, the coef and SE functions can directly extract those attributes:
# the estimate is the sum of the coefficients divided by the number of people
# in the population (which we described in the fpc)
coef(mysvyby)

SE(mysvyby)

# We can use the confint function 
# to obtain confidence intervals for the coefficient estimates.
confint(mysvyby)

# Also note that the number of decimal places shown can be adjusted 
# by modifying the digits parameter within the options function at any time.
options(digits = 10)
confint(mysvyby, df = degf(dat_weighted))

# examining the weights
svyby(~weights(dat_weighted), ~device_type, dat_weighted, svymean)

