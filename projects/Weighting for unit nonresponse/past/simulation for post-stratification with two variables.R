#######################################################################################
########   simulation #2 - unit non-response associated with two variables   ##########
#######################################################################################

# loading libaries
library(dplyr)
library (dslabs)
library (survey)

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
version_values<-c('old','new')
version_values<-factor(version_values, levels=c('old','new'), ordered=TRUE)
levels(version_values)
version <- sample(version_values, size=10^5, replace=TRUE, prob=c(0.2,0.8))
head(version)
table(version)  

# creating the dataset with the predicted variables only
population_data<-as.data.frame(cbind(as.data.frame(device_type), 
                                     as.data.frame(country),
                                     as.data.frame(version)))
class(population_data)
head(population_data)
table(population_data$device_type, population_data$version)

# creating a variable with a unique value for each strata
population_data$strata<-999
population_data$strata[population_data$device_type=='LE' &
                         population_data$version=='old']<-'LE_old'
population_data$strata[population_data$device_type=='LE' &
                         population_data$version=='new']<-'LE_new'
population_data$strata[population_data$device_type=='MR' &
                         population_data$version=='old']<-'MR_old'
population_data$strata[population_data$device_type=='MR' &
                         population_data$version=='new']<-'MR_new'
population_data$strata[population_data$device_type=='HE' &
                         population_data$version=='old']<-'HE_old'
population_data$strata[population_data$device_type=='HE' &
                         population_data$version=='new']<-'HE_new'

# making sure that the variable was created properly
table(population_data$strata)
table (population_data$device_type, population_data$version)

# creating an outcome variable called 'constraint'
# It can assume either 0 or 1, depending on whether the person
# perceives that he has this constraint.
# The values depend on the type of device the person is using
# (variable 'device_type') and the vresion (variable 'version').
population_data$constraint<-99
table (population_data$constraint)

population_data$constraint[device_type=='LE' & version=='old']<-0.8
population_data$constraint[device_type=='LE' & version=='new']<-0.7
population_data$constraint[device_type=='MR' & version=='old']<-0.6
population_data$constraint[device_type=='MR' & version=='new']<-0.5
population_data$constraint[device_type=='HE' & version=='old']<-0.4
population_data$constraint[device_type=='HE' & version=='new']<-0.3

table (population_data$constraint)
class (population_data$constraint)

# making sure that the constraint variable was created properly
# library(dplyr)
population_data %>%
  group_by(device_type, version) %>%
  summarize(mean(constraint))

table(population_data$device_type, population_data$version)

# sampling
# Low-end devices with old versions
population_data$LE_old<-with(population_data,
                             ifelse((device_type=='LE' & version=='old'),1,0))
population_data$responded_LE_old<-with(population_data,
                                       ifelse(LE_old==1,sample(x=c(0,1),
                                                               size=sum(LE_old),
                                                               replace=TRUE, 
                                                               prob=c(1-0.007,0.007)),0))
# making sure the variable was created properly
table(population_data$responded_LE_old, population_data$LE_old)

# Low-end devices with new versions
population_data$LE_new<-with(population_data,
                             ifelse((device_type=='LE' & version=='new'),1,0))
population_data$responded_LE_new<-with(population_data,
                                       ifelse(LE_new==1,sample(x=c(0,1),
                                                               size=sum(LE_new),
                                                               replace=TRUE, 
                                                               prob=c(1-0.008,0.008)),0))
# making sure the variable was created properly
table(population_data$responded_LE_new, population_data$LE_new)

# Mid-range devices with old versions
population_data$MR_old<-with(population_data,
                             ifelse((device_type=='MR' & version=='old'),1,0))
population_data$responded_MR_old<-with(population_data,
                                       ifelse(MR_old==1,sample(x=c(0,1),
                                                               size=sum(MR_old),
                                                               replace=TRUE, 
                                                               prob=c(1-0.009,0.009)),0))
# making sure the variable was created properly
table(population_data$responded_MR_old, population_data$MR_old)

# Mid-range devices with new versions
population_data$MR_new<-with(population_data,
                             ifelse((device_type=='MR' & version=='new'),1,0))
population_data$responded_MR_new<-with(population_data,
                                       ifelse(MR_new==1,sample(x=c(0,1),
                                                               size=sum(MR_new),
                                                               replace=TRUE, 
                                                               prob=c(1-0.01,0.01)),0))
# making sure the variable was created properly
table(population_data$responded_MR_new, population_data$MR_new)

# High-end devices with old versions
population_data$HE_old<-with(population_data,
                             ifelse((device_type=='HE' & version=='old'),1,0))
population_data$responded_HE_old<-with(population_data,
                                       ifelse(HE_old==1,sample(x=c(0,1),
                                                               size=sum(HE_old),
                                                               replace=TRUE, 
                                                               prob=c(1-0.011,0.011)),0))
# making sure the variable was created properly
table(population_data$responded_HE_old, population_data$HE_old)

# High-end devices with new versions
population_data$HE_new<-with(population_data,
                             ifelse((device_type=='HE' & version=='new'),1,0))
population_data$responded_HE_new<-with(population_data,
                                       ifelse(HE_new==1,sample(x=c(0,1),
                                                               size=sum(HE_new),
                                                               replace=TRUE, 
                                                               prob=c(1-0.012,0.012)),0))
# making sure the variable was created properly
table(population_data$responded_HE_new, population_data$HE_new)

# creating the sample data
sample_dat<-population_data %>%
  filter(responded_LE_old+responded_LE_new+responded_MR_old+
           responded_MR_new+responded_HE_old+responded_HE_new==1)
dim(sample_dat)

# checking number of respondents from each device type
table (sample_dat$device_type)

# checking number of respondents from each version
table (sample_dat$version)

# checking number of respondents from each device type and version
table (sample_dat$device_type, sample_dat$version)

# creating a variable with a unique value for each strata
sample_dat$strata<-999
sample_dat$strata[sample_dat$device_type=='LE' &
                    sample_dat$version=='old']<-'LE_old'
sample_dat$strata[sample_dat$device_type=='LE' &
                    sample_dat$version=='new']<-'LE_new'
sample_dat$strata[sample_dat$device_type=='MR' &
                    sample_dat$version=='old']<-'MR_old'
sample_dat$strata[sample_dat$device_type=='MR' &
                    sample_dat$version=='new']<-'MR_new'
sample_dat$strata[sample_dat$device_type=='HE' &
                    sample_dat$version=='old']<-'HE_old'
sample_dat$strata[sample_dat$device_type=='HE' &
                    sample_dat$version=='new']<-'HE_new'

# making sure that the variable was created properly
table(sample_dat$strata)
table (sample_dat$device_type, sample_dat$version)

# creating a variable with the number of people 
# in the population in each strata (Nh)
strata_pop<-as.numeric(table(population_data$strata))

# making sure that the variable was created properly
table(population_data$strata)
strata_pop

# creating a variable with the unique values of that strata
strata_unique<-names(table(population_data$strata))

# making sure that the variables for the population count in each cell
# and the names of the cells are in the same order
strata_pop
strata_unique
table(population_data$strata)

# adding an fpc column to the data (finite population correction factor)
sample_dat$fpc <- 10^5

# turning the sample data into a survey object
# library(survey)
preliminary.design <- 
  svydesign( 
    id = ~1 , 
    data = sample_dat ,
    fpc = ~fpc
  )

# Create a secondary data.frame object with two columns: 
# 1) The strata variable. 
# 2) The post-stratification target frequencies (Freq). 

ps.weights <-
  data.frame(
    strata = strata_unique,
    Freq = strata_pop)

# making sure that the dataframe was created properly
ps.weights
table(population_data$strata)

# Viewing the weighted total population of this survey design, 
# by referring to the Freq column from the post-stratification data.frame
sum(ps.weights$Freq)

# Viewing the number of unique poststrata in this survey design, 
# by referring to the type column from the post-stratification 
# target weights data.frame:
length(unique(ps.weights$strata))

# creating the weighted dataset
dat_weighted <- 
  postStratify(
    preliminary.design ,
    strata = ~strata ,
    population = ps.weights
  )

# exploring the data
class(dat_weighted)
names(dat_weighted)
head(dat_weighted$variables) # these are the variables
table(dat_weighted$strata) # it looks like this denotes whether the row belongs in a strata
# (in this example they all do)
unique(dat_weighted$cluster)  # this is probably for cluster sampling
unique(dat_weighted$has.strata) # i don't know what this is
unique(dat_weighted$strata) # i don't know what this is
unique(dat_weighted$prob) # this is the number of respondents (nh)
head(dat_weighted$prob)   # divided by the number of people in the population (Nh)
dat_weighted$prob         # in each strata (nh / Nh)
table(dat_weighted$prob)  
table(dat_weighted$prob, dat_weighted$variables$strata)
unique(dat_weighted$allprob) # this is the total number of respondents (nh)
head(dat_weighted$allprob)   # divided by the total number of people in the population (Nh)
class(dat_weighted$call) # i don't know what this is
class(dat_weighted$pps) # i don't know what this is
head(dat_weighted$pps) # i don't know what this is
class(dat_weighted$postStrata) # these are the weights (Nh / nh) in each strata
head(dat_weighted$postStrata)
table(dat_weighted$postStrata)

# another way to examine the weights
svyby(~weights(dat_weighted), ~strata, dat_weighted, svymean)

# Viewing the degrees of freedom for this survey design object:
nrow(dat_weighted)
degf(dat_weighted)

# Printing the mean and standard error of the outcome variable:
svymean(~constraint, dat_weighted)

# Printing the mean and standard error of the outcome variable, 
# by the 'device_type' variable:
svyby(~constraint, ~strata, dat_weighted, svymean)

# Alternatively, the result of a function call like svymean, svytotal, or svyby 
# can be stored into a secondary R object.

# example with the total:
mysvyby <- svyby(~constraint, ~strata, dat_weighted, svytotal)

# Once created, this svyby can be queried independently from the mydesign object. 
# For example, the coef and SE functions can directly extract those attributes:
# the estimate is the sum of the coefficients divided by the number of people
# in the population (which we described in the fpc)
coef(mysvyby) # this is the estimated sum of the outcome variable 
# in each strata in the population  (Nh / nh * mean(yh))
# where mean(yh) is the mean of the outcome variable in each strata
SE(mysvyby)   # this is the standard error of this sum

# We can use the confint function 
# to obtain confidence intervals for the coefficient estimates.
confint(mysvyby)

# example with the mean:
mysvyby <- svyby(~constraint, ~strata, dat_weighted, svymean)

# Once created, this svyby can be queried independently from the mydesign object. 
# For example, the coef and SE functions can directly extract those attributes:
# the estimate is the sum of the coefficients divided by the number of people
# in the population (which we described in the fpc)
coef(mysvyby) # this is the estimated mean of the outcome variable 
# in each strata in the population  (mean(yh))
# where mean(yh) is the mean of the outcome variable in each strata
SE(mysvyby)   # this is the standard error of this mean

# We can use the confint function 
# to obtain confidence intervals for the coefficient estimates.
confint(mysvyby)


# Also note that the number of decimal places shown can be adjusted 
# by modifying the digits parameter within the options function at any time.
options(digits = 10)
confint(mysvyby, df = degf(dat_weighted))

#############################################################################
######## Using logistic regression to calculate weights   ###################
#############################################################################
# creating a variable denoting whether the person responded to the survey
population_data$responded<- with(population_data,
                                 ifelse(responded_LE_old+responded_LE_new+responded_MR_old+
                                          responded_MR_new+responded_HE_old+responded_HE_new==1,1,0)) 

# making sure that the variable was created properly
table(population_data$responded)
with(population_data, table(responded, responded_LE_old))
with(population_data, table(responded, responded_LE_new))
with(population_data, table(responded, responded_MR_old))
with(population_data, table(responded, responded_MR_new))
with(population_data, table(responded, responded_HE_old))
with(population_data, table(responded, responded_HE_new))

# decomposing 'device_type' to binary variables
device_LE<-with(population_data,
                ifelse(device_type=="LE",1,0))
device_MR<-with(population_data,
                ifelse(device_type=="MR",1,0))
device_HE<-with(population_data,
                ifelse(device_type=="HE",1,0))

# verifying
table(device_LE,device_type)
table(device_MR,device_type)
table(device_HE,device_type)

# decomposing 'version' to binary variables
version_old<-with(population_data,
                  ifelse(version=="old",1,0))
version_new<-with(population_data,
                  ifelse(version=="new",1,0))

# verifying
table(version_old,version)
table(version_new,version)

# decomposing 'country to binary variables
Brazil<-with(population_data,
             ifelse(country=="Brazil",1,0))
Indonesia<-with(population_data,
                ifelse(country=="Indonesia",1,0))
India<-with(population_data,
            ifelse(country=="India",1,0))
Mexico<-with(population_data,
             ifelse(country=="Mexico",1,0))
Philippines<-with(population_data,
                  ifelse(country=="Philippines",1,0))
Thailand<-with(population_data,
               ifelse(country=="Thailand",1,0))

# verifying
table(Brazil,country)
table(Indonesia,country)
table(India,country)
table(Mexico,country)
table(Philippines,country)
table(Thailand,country)

# creating the formula for the logistic regression
response_equation<- with(population_data, 
                         "responded ~ 
                         device_MR +
                         device_HE +
                         version_new +
                         Indonesia +
                         India +
                         Mexico +
                         Philippines +
                         Thailand")

# running the logistic regression
responded_logit <- glm(response_equation, data=population_data, family = "binomial")
summary(responded_logit)


