#################################
######### Matching ##############
#################################

####### Sample code provided by Coursera #########


###################
#RHC Example

#install packages
install.packages("tableone")
install.packages("Matching")

#load packages
library(tableone)
library(Matching)

#read in data
load(url("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav"))
#view data
View(rhc)

#treatment variables is swang1
#x variables that we will use
#cat1: primary disease category
#age
#sex
#meanbp1: mean blood pressure

#create a data set with just these variables, for simplicity
ARF<-as.numeric(rhc$cat1=='ARF')
CHF<-as.numeric(rhc$cat1=='CHF')
Cirr<-as.numeric(rhc$cat1=='Cirrhosis')
colcan<-as.numeric(rhc$cat1=='Colon Cancer')
Coma<-as.numeric(rhc$cat1=='Coma')
COPD<-as.numeric(rhc$cat1=='COPD')
lungcan<-as.numeric(rhc$cat1=='Lung Cancer')
MOSF<-as.numeric(rhc$cat1=='MOSF w/Malignancy')
sepsis<-as.numeric(rhc$cat1=='MOSF w/Sepsis')
female<-as.numeric(rhc$sex=='Female')
died<-as.numeric(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1

#new dataset
mydata<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,
              age,female,meanbp1,treatment,died)
mydata<-data.frame(mydata)

#covariates we will use (shorter list than you would use in practice)
xvars<-c("ARF","CHF","Cirr","colcan","Coma","lungcan","MOSF","sepsis",
         "age","female","meanbp1")

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treatment", data=mydata, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)


############################################
#do greedy matching on Mahalanobis distance
############################################

greedymatch<-Match(Tr=treatment,M=1,X=mydata[xvars],replace=FALSE)
matched<-mydata[unlist(greedymatch[c("index.treated","index.control")]), ]

#get table 1 for matched data with standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treatment", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#outcome analysis
y_trt<-matched$died[matched$treatment==1]
y_con<-matched$died[matched$treatment==0]

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
t.test(diffy)

#McNemar test
table(y_trt,y_con)

mcnemar.test(matrix(c(973,513,395,303),2,2))



##########################
#propensity score matching
#########################

#fit a propensity score model. logistic regression

psmodel<-glm(treatment~ARF+CHF+Cirr+colcan+Coma+lungcan+MOSF+
               sepsis+age+female+meanbp1+aps,
             family=binomial(),data=mydata)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values


#do greedy matching on logit(PS) using Match with a caliper

logit <- function(p) {log(p)-log(1-p)}
psmatch<-Match(Tr=mydata$treatment,M=1,X=logit(pscore),replace=FALSE,caliper=.2)
matched<-mydata[unlist(psmatch[c("index.treated","index.control")]), ]
xvars<-c("ARF","CHF","Cirr","colcan","Coma","lungcan","MOSF","sepsis",
         "age","female","meanbp1")

#get standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treatment", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#outcome analysis
y_trt<-matched$died[matched$treatment==1]
y_con<-matched$died[matched$treatment==0]

#pairwise difference
diffy<-y_trt-y_con

#paired t-test
t.test(diffy)


### Quiz ###

### Data analysis project - analyze data in R using propensity score matching ###

# Installing packages and loading libraries

install.packages("tableone")

install.packages("Matching")

install.packages("MatchIt")

library(tableone)

library(Matching)

# Loading the data

library(MatchIt)

data(lalonde)
names(lalonde)

# Find the standardized differences for all of the confounding variables (pre-matching).

# define xvars
class(lalonde$race)
lalonde$black<-0
lalonde$black[lalonde$race=="black"]<-1
table(lalonde$black, lalonde$race)

lalonde$hispan<-0
lalonde$hispan[lalonde$race=="hispan"]<-1
table(lalonde$hispan, lalonde$race)


xvars<-c("age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75", "re78")

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treat", data=lalonde, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)

# fit a propensity score model. logistic regression

psmodel<-glm(treat~age+educ+race+married+nodegree+re74+re75,
             family=binomial(),data=lalonde)

#show coefficients etc
summary(psmodel)
#create propensity score
pscore<-psmodel$fitted.values

min(pscore)
max(pscore)

# carrying out propensity score matching
set.seed(931139)
psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]
xvars<-c("age", "educ", "black", "hispan", "married", "nodegree", "re74", "re75") # removing re78 from xvars

#get standardized differences
matchedtab1<-CreateTableOne(vars=xvars, strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

# Re-do the matching, but use a caliper this time. Set the caliper=0.1 in the options in the Match function.
set.seed(931139)

psmatch<-Match(Tr=lalonde$treat,M=1,X=pscore,replace=FALSE, caliper=0.1)
matched<-lalonde[unlist(psmatch[c("index.treated","index.control")]), ]

#get standardized differences
matchedtab1<-CreateTableOne(vars=c(xvars, "re78"), strata ="treat", 
                            data=matched, test = FALSE)
print(matchedtab1, smd = TRUE)

#outcome analysis
y_trt<-matched$re78[matched$treat==1]
y_con<-matched$re78[matched$treat==0]

#pairwise difference
diffy<-y_trt-y_con
diff

#paired t-test
t.test(diffy)

# Carry out a paired t-test for the effect of treatment on earnings. What are the values of the 95% confidence interval?

