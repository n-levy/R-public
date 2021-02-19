###################
#RHC Example

#install packages (if needed)
install.packages("rlang")
install.packages("tableone")
install.packages("ipw")
install.packages("sandwich")
install.packages("survey")

#load packages
library(tableone)
library(ipw)
library(sandwich) #for robust variance estimation
library(survey)

expit <- function(x) {1/(1+exp(-x)) }
logit <- function(p) {log(p)-log(1-p)}

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
died<-as.integer(rhc$death=='Yes')
age<-rhc$age
treatment<-as.numeric(rhc$swang1=='RHC')
meanbp1<-rhc$meanbp1

#new dataset
mydata<-cbind(ARF,CHF,Cirr,colcan,Coma,lungcan,MOSF,sepsis,
              age,female,meanbp1,treatment,died)
mydata<-data.frame(mydata)

#covariates we will use (shorter list than you would use in practice)
xvars<-c("age","female","meanbp1","ARF","CHF","Cirr","colcan",
         "Coma","lungcan","MOSF","sepsis")

#look at a table 1
table1<- CreateTableOne(vars=xvars,strata="treatment", data=mydata, test=FALSE)
## include standardized mean difference (SMD)
print(table1,smd=TRUE)

#propensity score model
psmodel <- glm(treatment ~ age + female + meanbp1+ARF+CHF+Cirr+colcan+
                 Coma+lungcan+MOSF+sepsis,
               family  = binomial(link ="logit"))

## value of propensity score for each subject
ps <-predict(psmodel, type = "response")

#create weights
weight<-ifelse(treatment==1,1/(ps),1/(1-ps))

#apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =mydata, weights = ~ weight)

#weighted table 1
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treatment", 
                                  data = weighteddata, test = FALSE)
## Show table with SMD
print(weightedtable, smd = TRUE)

#to get a weighted mean for a single covariate directly:
mean(weight[treatment==1]*age[treatment==1])/(mean(weight[treatment==1]))

#get causal risk difference
glm.obj<-glm(died~treatment,weights=weight,family=quasibinomial(link="identity"))
#summary(glm.obj)
betaiptw<-coef(glm.obj)
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))

causalrd<-(betaiptw[2])
lcl<-(betaiptw[2]-1.96*SE[2])
ucl<-(betaiptw[2]+1.96*SE[2])
c(lcl,causalrd,ucl)

#get causal relative risk. Weighted GLM
glm.obj<-glm(died~treatment,weights=weight,family=quasibinomial(link=log))
#summary(glm.obj)
betaiptw<-coef(glm.obj)
#to properly account for weighting, use asymptotic (sandwich) variance
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))

#get point estimate and CI for relative risk (need to exponentiate)
causalrr<-exp(betaiptw[2])
lcl<-exp(betaiptw[2]-1.96*SE[2])
ucl<-exp(betaiptw[2]+1.96*SE[2])
c(lcl,causalrr,ucl)

#truncate weights at 10

truncweight<-replace(weight,weight>10,10)
#get causal risk difference
glm.obj<-glm(died~treatment,weights=truncweight,family=quasibinomial(link="identity"))
#summary(glm.obj)
betaiptw<-coef(glm.obj)
SE<-sqrt(diag(vcovHC(glm.obj, type="HC0")))

causalrd<-(betaiptw[2])
lcl<-(betaiptw[2]-1.96*SE[2])
ucl<-(betaiptw[2]+1.96*SE[2])
c(lcl,causalrd,ucl)

#############################
#alternative: use ipw package
#############################

#first fit propensity score model to get weights
weightmodel<-ipwpoint(exposure= treatment, family = "binomial", link ="logit",
                      denominator= ~ age + female + meanbp1+ARF+CHF+Cirr+colcan+
                        Coma+lungcan+MOSF+sepsis, data=mydata)
#numeric summary of weights
summary(weightmodel$ipw.weights)
#plot of weights
ipwplot(weights = weightmodel$ipw.weights, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
mydata$wt<-weightmodel$ipw.weights

#fit a marginal structural model (risk difference)
msm <- (svyglm(died ~ treatment, design = svydesign(~ 1, weights = ~wt,
                                                    data =mydata)))
coef(msm)
confint(msm)


# fit propensity score model to get weights, but truncated
weightmodel<-ipwpoint(exposure= treatment, family = "binomial", link ="logit",
                      denominator= ~ age + female + meanbp1+ARF+CHF+Cirr+colcan+
                        Coma+lungcan+MOSF+sepsis, data=mydata,trunc=.01)

#numeric summary of weights
summary(weightmodel$weights.trun)
#plot of weights
ipwplot(weights = weightmodel$weights.trun, logscale = FALSE,
        main = "weights", xlim = c(0, 22))
mydata$wt<-weightmodel$weights.trun
#fit a marginal structural model (risk difference)
msm <- (svyglm(died ~ treatment, design = svydesign(~ 1, weights = ~wt,
                                                    data =mydata)))
coef(msm)
confint(msm)

# Assignment
# For this assignment we will use data from Lalonde (1986),
# that aimed to evaluate the impact of  National
# Supported Work (NSW) Demonstration, which is a labor training program, on post-intervention
# income levels. Interest is in estimating the causal effect of this training
# program on income.

# First load the packages TableOne, Matching, ipw, and survey:

install.packages("tableone")

install.packages("Matching")

install.packages("ipw")

install.packages("survey")

install.packages("MatchIt")

library(tableone)

library(Matching)

library(ipw) 

library(survey)

# Now load the lalonde data (which is in the MatchIt package):
  
library(MatchIt)

data(lalonde)

dim(lalonde)

names(lalonde)

unique(lalonde$race)

lalonde$black<-as.numeric(lalonde$race=="black")

lalonde$hispan<-as.numeric(lalonde$race=="hispan")

xvars<-c("age", "educ", "black", "hispan", "married", "nodegree", 
         "re74", "re75")

#propensity score model
psmodel <- glm(treat ~ age + educ + black + hispan + married + 
                 nodegree + re74 + re75, data = lalonde,
               family  = binomial(link ="logit"))

## value of propensity score for each subject
lalonde$ps <-predict(psmodel, type = "response")

#create weights
# library(dplyr)
weight<- ifelse(lalonde$treat==1,1/(lalonde$ps),1/(1-lalonde$ps))
max(weight)
min(weight)

#apply weights to data
weighteddata<-svydesign(ids = ~ 1, data =lalonde, weights = ~ weight)

#weighted table 1
weightedtable <-svyCreateTableOne(vars = xvars, strata = "treat", 
                                  data = weighteddata, test = FALSE)
## Show table with SMD
print(weightedtable, smd = TRUE)

