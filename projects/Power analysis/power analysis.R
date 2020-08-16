###############################
##### power analysis in r #####
###############################

# Installing the package 
# install.packages("pwr")
# install.packages("epiR")
# install.packages("Epi")
# install.packages("binom")

# loading the library
# library(binom)

# creating the function
minimum_detectable_difference_func<-function(number_of_questions, n_q1, p_1, dropout_rate, alpha, power) {
  for (i in 1:number_of_questions){
  ifelse(i==1, n_group[i]<-n_q1, n_group[i]<-n_group[i-1]*(1-dropout_rate)) # the number of people in each group
  power_stats<-power.prop.test(n = n_group[i], p1 = p_1, p2 = NULL, sig.level = alpha, power = power, alternative = "two.sided") # the power analysis
  delta<-power_stats$p2-power_stats$p1 # the difference between the proportions in each group
  min_det_diff[i]<-delta # the minimum detectable difference
  n_group<<-n_group # copying the n_group variable outside the function
  min_det_diff<<-min_det_diff # copying the minimum detectable difference variable outside the function 
}
}

# calculating the minimum detecatable difference between two groups, each one of them containing n_group people  
qnum<-11 # the number of questions 
dropout_rate<-0.2 # the percentage of respondents who don't continue to the next question
p_1<-0.5 # the proportion that we expect to find in group 1

# preparing variables for the function
n_group<-NA # the number of people in each of the two groups we compare (n_group=2000 means we compare two groups and there are 2000 respondents in each of them)
power_stats<-NA # the data frame containing the results of the power analysis
delta<-NA # the difference between the proportions in each group
min_det_diff<-NA # the minimum detectable difference
n_q1<-7000 # the number of people who respond to the first question

minimum_detectable_difference_func(number_of_questions = qnum, n_q1 = n_q1, p_1 = p_1, 
                    dropout_rate = dropout_rate, alpha = 0.05, power = 0.8)
min_det_diff
# preparing the dataframe with the results
qnum_for_data_frame<-paste("Q",1:qnum,sep="") # the question number
power_stats<-cbind.data.frame(qnum_for_data_frame,round(n_group,0),min_det_diff) # creating the data frame
power_stats

# turning the data frame to an Excel spreadsheet on my desktop
write.csv(power_stats, "C:/Users/nirlevy/Desktop/power_stats.csv") 

# The End #


# alternative method
# library(pwr)
method2<-pwr.2p.test(h = NULL, n = 2000, sig.level = 0.05, power = 0.8, 
            alternative = c("two.sided"))

# comparing the two methods
method2$h/2 # in the second method we need to divide by two, see this webpage for example: https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
power_stats

# both functions give very similar results. see discussion of the slight differences here:
# https://stats.stackexchange.com/questions/83700/why-do-these-power-functions-for-difference-in-proportions-give-different-answer
