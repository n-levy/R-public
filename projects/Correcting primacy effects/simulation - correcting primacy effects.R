#######################################################
########## Simulation of primacy correction  ##########
#######################################################

# removing all objects from the workspace
rm(list = ls())

###### setting the parameters for all of the resplications ####
N<-10^6 # the population size
n<-10^3 # the number of respondents

# setting the number of replications
n_replications<-5

# preparing the variables of interest
absolute_bias_before_correction<-c(rep(NA,n_replications))
absoloute_error_after_correction<-c(rep(NA,n_replications))
absoloute_error_after_correction<-mean_absolute_bias_before_correction<-c(rep(NA,n_replications))
mean_absoloute_error_after_correction<-c(rep(NA,n_replications))
mean_se_error_before_correction<-c(rep(NA,n_replications))
mean_se_error_after_correction<-c(rep(NA,n_replications))

# running the simulation
for(k in 1:n_replications){

###### setting the parameters that change in each replication ####
# the number of randomized questions (q) 
min_q<-3
max_q<-9
q <- sample(seq(min_q,max_q,1), size=1, p=rep(1/(max_q-min_q+1),(max_q-min_q+1)), replace=TRUE) 

# setting the proportion for each response option 
pi_underlying <- numeric(); 
pi_underlying[1] <- runif(1,0,0.7); # the underlying proportion of the first characteristic
for( i in 2:q ){ pi_underlying[i] <- runif(1,0,1-sum(pi_underlying[1:i-1]))}; # the underlying proportion of the rest of the characteristics that we ask about in randomized responses
for(i in q+1){pi_underlying[i]<-1-sum(pi_underlying[1:i-1])} # the underlying proportion of a characteristic that we ask about in a non-randomized response

# setting the tendency to choose the randomized response option that appears first (t1) or last (tq) 
t1<-runif(1,min=0,max=0.4)
tq<-runif(1,min=0,max=0.4)

# setting the characteristic of interest in the population
population_characteristics<-sample(seq(1,q+1,1), size=N, p=pi_underlying, replace=TRUE)
pi_population<-as.data.frame(prop.table(table(population_characteristics))) # the proportion of people in the population with each characteristic

# sampling the population
survey_respondent_id<-sample(seq(1,N,1), size=n, p=rep(1/N,N), replace=FALSE)

# creating the order of response options for each respondent
ordered_responses<-t(replicate(n,sample(seq(1:q),size=q,prob=rep(1/q,q), replace=FALSE)))
ordered_responses<-cbind(ordered_responses,q+1)

# determining the response that each respondent provides
survey_responses<-numeric(); 
survey_responses[1]<-q+1 # setting at least one respondent who chose 'other' in order to enable creation of a data frame comparing estimates
for( i in 2:n ){
  survey_responses[i] <- sample(c(ordered_responses[i,1],population_characteristics[survey_respondent_id[i]],ordered_responses[i,q]),
                                             prob=c(t1,1-(t1+tq),tq),size=1)}

# calculating the share of respondents who provided each response (ci)
ci<-as.data.frame(prop.table(table(survey_responses)))

# determining the position of each chosen response (appeared first in the question, appeared second, etc.) 
response_position<-numeric(); 
for(i in 1:n){ response_position[i] <- which(ordered_responses[i,]==survey_responses[i])}

# calculating the share of people who provided the response in each position (si) 
si<-as.data.frame(prop.table(table(response_position)))
si
# estimating the tendency to choose the first response (t1) and the last randomized response (tq) 
t1_hat<-si$Freq[1]-mean(si$Freq[2:(q-1)])
tq_hat<-si$Freq[q]-mean(si$Freq[2:(q-1)])

# estimating the underlying probabilities (pi) 
pi_hat <- numeric(); 
for( i in 1:q ){pi_hat<-(ci$Freq-(t1_hat+tq_hat)/q)/(1-(t1_hat+tq_hat))}; 
for(i in q+1){pi_hat[i]<-1-sum(pi_hat[1:q])}

# comparing the estimates 
pi_estimates<-cbind.data.frame(pi_underlying,ci$Freq,pi_hat)
pi_estimates

# calculating the differences between the estimates and the underlying p
absolute_bias_before_correction<-abs(pi_underlying-ci$Freq)
absoloute_error_after_correction<-abs(pi_underlying-pi_hat)
mean_absolute_bias_before_correction[k]<-mean(absolute_bias_before_correction[k])
mean_absoloute_error_after_correction[k]<-mean(absoloute_error_after_correction[k])

# calculating the standard deviation and standard errors
sd_population<-as.data.frame(sqrt((pi_underlying*(1-pi_underlying))/n)) # the standard deviation in the population
se_before_correction<-as.data.frame(sqrt((ci$Freq*(1-ci$Freq))/n)) # the standard errors before correction
se_after_correction<-as.data.frame(sqrt((pi_hat*(1-pi_hat))/n)) # the standard errors before correction

# comparing the estimates
# sd_se<-cbind.data.frame(sd_population,se_before_correction,se_after_correction)
# names(sd_se)<-c('sd','se_before_correction','se_after_correction')
# sd_se
se_error_before_correction<-sd_population-se_before_correction
se_error_after_correction<-sd_population-se_after_correction

mean_se_error_before_correction[k]<-mean(se_error_before_correction[,1][k])
mean_se_error_after_correction[k]<-mean(se_error_after_correction[,1][k])
}
 
# displaying the simulation results
# comparing the difference in estimates of the shares of people who chose each response
# before and after the correction
mean(mean_absolute_bias_before_correction)
sd(mean_absolute_bias_before_correction)

mean(mean_absoloute_error_after_correction)
sd(mean_absoloute_error_after_correction)

# compring the difference in estimates standard errors
# before and after the correction
mean(mean_se_error_before_correction)
sd(mean_se_error_before_correction)

mean(mean_se_error_after_correction)
sd(mean_se_error_after_correction)

# looks good

# the end