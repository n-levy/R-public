# loading the data
library(foreign)
mydata <- 
  read.dta( 
    "https://stats.idre.ucla.edu/stat/data/dogcats.dta" , 
    convert.factors = FALSE 
  )

# exploring the data
class(mydata)
head(mydata)
tail(mydata)
ncol(mydata)
dim(mydata)

# adding an fpc column to the data
mydata$fpc <- 1300
head(mydata)

# summarizing the variable of interest
summary(mydata$totexp)

# marking the dataset as 'survey' data in R
# this assumes no weight and simple random sampling
preliminary.design <- 
  svydesign( 
    id = ~1 , 
    data = mydata ,
    fpc = ~fpc
  )

# Create a secondary data.frame object with two columns 
# in order to perform the actual post-stratification: 
# 1) The variable used to post-stratify by (type). 
# This column is data set-specific. 
# 2) The post-stratification target frequencies (Freq). 
# This column should always be called Freq 
# because the R survey package searches for that column name.

ps.weights <-
  data.frame(
    type = c( 1 , 2 ) ,
    Freq = c( 850 , 450 ) 
  )

# creating a weighted dataset, in which 
# the weights are obtained by post-stratification
mydesign <- 
  postStratify(
    preliminary.design ,
    strata = ~type ,
    population = ps.weights
  )

# exploring the data
class(mydesign)

# View the weighted total population of this survey design, 
# by referring to the Freq column from the post-stratification 
# specification data.frame:
sum(ps.weights$Freq)

# View the number of unique poststrata in this survey design, 
# by referring to the type column from the post-stratification 
# target weights data.frame:
length(unique(ps.weights$type))

# View the degrees of freedom for this survey design object:
degf(mydesign)

# Count the number of unweighted observations where the variable totexp is not missing:
unwtd.count(~totexp, mydesign)

# Print the mean and standard error of this variable:
svymean(~totexp, mydesign)

# Print the mean and standard error of the totexp variable, 
# broken out by the type variable:
svyby(~totexp, ~type, mydesign, svymean)

# Alternatively, the result of a function call like svymean, svytotal, or svyby 
# can be stored into a secondary R object.

mysvyby <- svyby(~totexp, ~type, mydesign, svytotal)


# Once created, this svyby can be queried independently from the mydesign object. 
# For example, the coef and SE functions can directly extract those attributes:

coef(mysvyby)
SE(mysvyby)

# We can use the confint function 
# to obtain confidence intervals for the coefficient estimates.
confint(mysvyby)

# Also note that the number of decimal places shown can be adjusted 
# by modifying the digits parameter within the options function at any time.
options(digits = 10)
confint(mysvyby, df = degf(mydesign))
