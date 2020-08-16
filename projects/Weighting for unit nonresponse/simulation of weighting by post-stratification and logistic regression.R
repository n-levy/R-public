
###############################################################
#######   Preparing the code for the Lite4iOS analysis  #######
###############################################################

# loading libaries
library(dplyr)
library (dslabs)
library (survey)
library(ggplot2)
library(purrr)
library(tidyr)

# removing all objects from the workspace
rm(list = ls())


######################################################
#######   Creating the population data   #############
######################################################

# 100,000 users in the population
# Creating the independent variables:
# 'gender', 'age', 'lness', 'device_type', 'country', 'version'

# gender
gender_values<-c('female','male')
gender_values<-factor(gender_values, levels=c('female','male'), ordered=TRUE)
levels(gender_values)
gender <- sample(gender_values, size=10^5, replace=TRUE, prob=c(0.5,0.5))
head(gender)
table(gender)  

# age
age_values<-c('20-29','30-39','40-49','50-59','60-69','70-79','80plus')
age_20_29_share<-0.3
age_30_39_share<-0.3
age_40_49_share<-0.2
age_50_59_share<-0.15
age_60_69_share<-0.03
age_70_79_share<-0.015
age_80plus_share<-0.005
age_20_29_share+age_30_39_share+age_40_49_share+age_50_59_share+age_60_69_share+age_70_79_share+age_80plus_share
age_share<-c(age_20_29_share,age_30_39_share,age_40_49_share,
             age_50_59_share,age_60_69_share,age_70_79_share,age_80plus_share)

age <- sample(age_values, size=10^5, replace=TRUE, prob=age_share)
head(age)
table_age<-table(age)  
prop.table(table_age)
age_df<-as.data.frame(age)
head(age_df)
#ggplot(age_df, aes(age)) + geom_bar(fill = "#0073C2FF")


# lness
lness_values<-c('1-27','28-30')
lness_1_27_share<-0.2
lness_28_30_share<-0.8

lness_share<-c(lness_1_27_share,lness_28_30_share)
lness <- sample(lness_values, size=10^5, replace=TRUE, prob=lness_share)
head(lness)
table_lness<-table(lness)  
prop.table(table_lness)
lness_df<-as.data.frame(lness)
head(lness_df)
# ggplot(lness_df, aes(lness)) +  geom_bar(fill = "#0073C2FF")

# device type
device_type_values<-c('LE','MR','HE')
device_type_values<-factor(device_type_values, levels=c('LE','MR','HE'), ordered=TRUE)
levels(device_type_values)
device_type <- sample(device_type_values, size=10^5, replace=TRUE, prob=c(0.5,0.3,0.2))
head(device_type)
table(device_type)  

# decomposing 'device_type' to binary variables
device_LE<-ifelse(device_type=="LE",1,0)
device_MR<-ifelse(device_type=="MR",1,0)
device_HE<-ifelse(device_type=="HE",1,0)

# verifying
table(device_LE,device_type)
table(device_MR,device_type)
table(device_HE,device_type)

# country
country_values<-c('Brazil', 'Indonesia', 'India', 'Mexico' , 'Philippines', 'Thailand')
country_values<-factor(country_values, 
                       levels=c('Brazil', 'Indonesia', 'India', 'Mexico' , 'Philippines', 'Thailand')
                       , ordered=TRUE)
levels(country_values)
country <- sample(country_values, size=10^5, replace=TRUE, prob=c(0.2,0.2,0.3,0.1,0.1,0.1))
head(country)
table(country)  

# decomposing 'country to binary variables
Brazil<-ifelse(country=="Brazil",1,0)
Indonesia<-ifelse(country=="Indonesia",1,0)
India<-ifelse(country=="India",1,0)
Mexico<-ifelse(country=="Mexico",1,0)
Philippines<-ifelse(country=="Philippines",1,0)
Thailand<-ifelse(country=="Thailand",1,0)

# verifying
table(Brazil,country)
table(Indonesia,country)
table(India,country)
table(Mexico,country)
table(Philippines,country)
table(Thailand,country)

# version
version_values<-c('old','new')
version_values<-factor(version_values, levels=c('old','new'), ordered=TRUE)
levels(version_values)
version <- sample(version_values, size=10^5, replace=TRUE, prob=c(0.2,0.8))
head(version)
table(version)  

# decomposing 'version' to binary variables
version_old<-ifelse(version=="old",1,0)
version_new<-ifelse(version=="new",1,0)

# verifying
table(version_old,version)
table(version_new,version)


# Dependent variables 

# Q1 Why are you using Facebook Lite on this phone? Please select all that apply.
# It doesn't use a lot of mobile data
# It works fast on this phone
# I like to try new apps
# It doesn't freeze or crash often
# It's simple to use
# It takes up little storage space on the phone
# It saves battery
# I like the way it looks and feels
# I'm not sure
# Other (please describe)
Q1_why_using_facebook_data_values<-c(0,1)
Q1_why_using_facebook_fast_values<-c(0,1)
Q1_why_using_facebook_try_new_values<-c(0,1)
Q1_why_using_facebook_freeze_crash_values<-c(0,1)
Q1_why_using_facebook_simple_values<-c(0,1)
Q1_why_using_facebook_storage_values<-c(0,1)
Q1_why_using_facebook_battery_values<-c(0,1)
Q1_why_using_facebook_looks_feels_values<-c(0,1)
Q1_why_using_facebook_not_sure_values<-c(0,1)
Q1_why_using_facebook_other_values<-c(0,1)

Q1_why_using_facebook_data<-sample(Q1_why_using_facebook_data_values,size=10^5, replace=TRUE, 
                                   prob=c(0.5,0.5))
head(Q1_why_using_facebook_data)
table(Q1_why_using_facebook_data)

Q1_why_using_facebook_fast<-sample(Q1_why_using_facebook_fast_values,size=10^5, replace=TRUE, 
                              prob=c(0.5,0.5))
head(Q1_why_using_facebook_fast)
table(Q1_why_using_facebook_fast)

Q1_why_using_facebook_try_new<-sample(Q1_why_using_facebook_try_new_values,size=10^5, replace=TRUE, 
                                   prob=c(0.9,0.1))
head(Q1_why_using_facebook_try_new)
table(Q1_why_using_facebook_try_new)
prop.table(table(Q1_why_using_facebook_try_new))


Q1_why_using_facebook_freeze_crash<-sample(Q1_why_using_facebook_freeze_crash_values,size=10^5, replace=TRUE, 
                                      prob=c(0.7,0.3))
head(Q1_why_using_facebook_freeze_crash)
table(Q1_why_using_facebook_freeze_crash)
prop.table(table(Q1_why_using_facebook_freeze_crash))


Q1_why_using_facebook_simple<-sample(Q1_why_using_facebook_simple_values,size=10^5, replace=TRUE, 
                                           prob=c(0.5,0.5))
head(Q1_why_using_facebook_simple)
table(Q1_why_using_facebook_simple)
prop.table(table(Q1_why_using_facebook_simple))
#simple

Q1_why_using_facebook_storage<-sample(Q1_why_using_facebook_storage_values,size=10^5, replace=TRUE, 
                                     prob=c(0.6,0.4))
head(Q1_why_using_facebook_storage)
table(Q1_why_using_facebook_storage)
prop.table(table(Q1_why_using_facebook_storage))
#storage

Q1_why_using_facebook_battery<-sample(Q1_why_using_facebook_battery_values,size=10^5, replace=TRUE, 
                                      prob=c(0.6,0.4))
head(Q1_why_using_facebook_battery)
table(Q1_why_using_facebook_battery)
prop.table(table(Q1_why_using_facebook_battery))
#battery

Q1_why_using_facebook_looks_feels<-sample(Q1_why_using_facebook_looks_feels_values,size=10^5, replace=TRUE, 
                                      prob=c(0.95,0.05))
head(Q1_why_using_facebook_looks_feels)
table(Q1_why_using_facebook_looks_feels)
prop.table(table(Q1_why_using_facebook_looks_feels))
#looks_feels end

Q1_why_using_facebook_not_sure<-sample(Q1_why_using_facebook_not_sure_values,size=10^5, replace=TRUE, 
                                          prob=c(0.999,0.001))
head(Q1_why_using_facebook_not_sure)
table(Q1_why_using_facebook_not_sure)
prop.table(table(Q1_why_using_facebook_not_sure))
#not_sure end

Q1_why_using_facebook_other<-ifelse(Q1_why_using_facebook_data+
                                    Q1_why_using_facebook_fast+
                                    Q1_why_using_facebook_try_new+
                                    Q1_why_using_facebook_freeze_crash+
                                    Q1_why_using_facebook_simple+
                                    Q1_why_using_facebook_storage+
                                    Q1_why_using_facebook_battery+
                                    Q1_why_using_facebook_looks_feels+
                                    Q1_why_using_facebook_not_sure==0,1,0)

head(Q1_why_using_facebook_other)
table(Q1_why_using_facebook_other)
prop.table(table(Q1_why_using_facebook_other))
#other end

# creating composite variable 
Q1_why_using_facebook<-as.data.frame(cbind(Q1_why_using_facebook_data,
                                           Q1_why_using_facebook_fast,
                                           Q1_why_using_facebook_try_new,
                                           Q1_why_using_facebook_freeze_crash,
                                           Q1_why_using_facebook_simple,
                                           Q1_why_using_facebook_storage,
                                           Q1_why_using_facebook_battery,
                                           Q1_why_using_facebook_looks_feels,
                                           Q1_why_using_facebook_not_sure,
                                           Q1_why_using_facebook_other))

dim(Q1_why_using_facebook)

# shortening the names of the values in the composite variable
Q1_short_names <- sub("Q1_why_using_facebook_", "Q1_", names(Q1_why_using_facebook))
names(Q1_why_using_facebook)<-Q1_short_names
Q1_plot_names<-c('Data', 'Fast', 'Try new', 'Freeze / Crash',
                                'Simple', 'Storage', 'Battery', 'Looks / Feels',
                                'Not sure', 'Other')

# create dataframe of proportions of all Q1 responses
proportion<-colMeans(Q1_why_using_facebook)
Q1_why_using_facebook_proportions<-as.data.frame(proportion)
Q1_why_using_facebook_proportions

# plot the Q1 proportions
# png("C:/Users/nirlevy/Google Drive/sync/data analytics/R plots/Q1_why_using_facebook_plot.png", width = 800, height = 500)
# Q1_why_using_facebook_proportions %>%
#   ggplot(aes(x = reorder(Q1_plot_names, -proportion), y=proportion)) + # setting x and y
#   geom_bar(stat = "identity", fill="#054C70", position="identity") + # creating a bar plot
#   xlab("") + # removing x label
#   ylab("") + # removing y label
#   labs(title = "Why are you using Facebook Lite on this phone?") + # chart title
#   scale_y_continuous(limits=c(0,1), expand=c(0,0),
#                      labels=scales::percent_format(accuracy=1)) + # scaling the y axis
#   geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=0.5, size=3.5) + # creating labels above bars with their value
#   # geom_label(aes(y=0.4, label=country_n_label), fill = "snow2",
#   #           position=position_stack(vjust=0.15),
#   #           label.padding = unit(0.3, "lines"), alpha=0.9)+ # creating boxes with number of observations
#   # geom_errorbar(aes(x=names,ymin=at_least_one-1.96*proportion_se,ymax=at_least_one+1.96*proportion_se),
#   #              width=0.07, colour="black", alpha=0.5, size=0.8)+ # creating error bars
#   theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=10), # creating x axis lables
#         axis.text.y = element_text(), # removing y axis labels
#         axis.ticks.x=element_blank(), # removing x axis ticks
#         axis.ticks.y=element_blank(), # removing y axis ticks
#         panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
#         plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
#         plot.title = element_text(hjust = 0.5, vjust=-5, size=15), # designing plot title
#         axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
#         axis.line.y = element_line(color="gray69", size = 0.5), # designing y axis
#         legend.position = "none") # removing legend
# dev.off()

# Q2 Why does Facebook Lite work fast on this phone?
# The phone isn't powerful enough (e.g., older device, low memory)
# Network conditions are not good (e.g., 2G or disrupted connection)
# I'm not sure
# Other (please describe)
# Q2 Why does Facebook Lite work fast on this phone?
# The phone isn't powerful enough (e.g., older device, low memory)
# Network conditions are not good (e.g., 2G or disrupted connection)
# I'm not sure
# Other (please describe)

Q2_why_works_fast_not_powerful_values<-c(0,1)
Q2_why_works_fast_network_values<-c(0,1)
Q2_why_works_fast_not_sure_values<-c(0,1)
Q2_why_works_fast_other_values<-c(0,1)

Q2_why_works_fast_not_powerful<-sample(Q2_why_works_fast_not_powerful_values,size=10^5, replace=TRUE, 
                                       prob=c(0.1,0.9))
head(Q2_why_works_fast_not_powerful)
table(Q2_why_works_fast_not_powerful)

Q2_why_works_fast_network<-sample(Q2_why_works_fast_network_values,size=10^5, replace=TRUE, 
                                  prob=c(0.5,0.5))
head(Q2_why_works_fast_network)
table(Q2_why_works_fast_network)

Q2_why_works_fast_not_sure<-sample(Q2_why_works_fast_not_sure_values,size=10^5, replace=TRUE, 
                                   prob=c(0.999,0.001))
head(Q2_why_works_fast_not_sure)
table(Q2_why_works_fast_not_sure)
prop.table(table(Q2_why_works_fast_not_sure))


Q2_why_works_fast_other<-ifelse(Q2_why_works_fast_not_powerful+
                                  Q2_why_works_fast_network+
                                  Q2_why_works_fast_not_sure==0,1,0)

head(Q2_why_works_fast_other)
table(Q2_why_works_fast_other)
prop.table(table(Q2_why_works_fast_other))

# creating composite variable 
Q2_why_works_fast<-as.data.frame(cbind(Q2_why_works_fast_not_powerful,
                                       Q2_why_works_fast_network,
                                       Q2_why_works_fast_not_sure,
                                       Q2_why_works_fast_other))
dim(Q2_why_works_fast)

# shortening the names of the values in the composite variable
Q2_short_names <- sub("Q2_why_works_fast_", "Q2_", names(Q2_why_works_fast))
names(Q2_why_works_fast)<-Q2_short_names
Q2_plot_names<-c('Not powerful', 'Network', 'Not sure', 'Other')

# creating dataframe of proportions of all Q2 responses
proportion<-colMeans(Q2_why_works_fast)
Q2_why_works_fast_proportions<-as.data.frame(proportion)
Q2_why_works_fast_proportions

# # plotting the Q2 proportions
# png("C:/Users/nirlevy/Google Drive/sync/data analytics/R plots/Q2_why_works_fast_plot.png", width = 800, height = 500)
# Q2_why_works_fast_proportions %>%
#   ggplot(aes(x = reorder(Q2_plot_names, -proportion), y=proportion)) + # setting x and y
#   geom_bar(stat = "identity", fill="#054C70", position="identity") + # creating a bar plot
#   xlab("") + # removing x label
#   ylab("") + # removing y label
#   labs(title = "Why does Facebook Lite work fast on this phone?") + # chart title
#   scale_y_continuous(limits=c(0,1), expand=c(0,0), 
#                      labels=scales::percent_format(accuracy=1)) + # scaling the y axis
#   geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=0.5, size=3.5) + # creating labels above bars with their value
#   # geom_label(aes(y=0.4, label=country_n_label), fill = "snow2", 
#   #           position=position_stack(vjust=0.15), 
#   #           label.padding = unit(0.3, "lines"), alpha=0.9)+ # creating boxes with number of observations
#   # geom_errorbar(aes(x=names,ymin=at_least_one-1.96*proportion_se,ymax=at_least_one+1.96*proportion_se),
#   #              width=0.07, colour="black", alpha=0.5, size=0.8)+ # creating error bars
#   theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=10), # creating x axis lables
#         axis.text.y = element_text(), # removing y axis labels
#         axis.ticks.x=element_blank(), # removing x axis ticks
#         axis.ticks.y=element_blank(), # removing y axis ticks
#         panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
#         plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
#         plot.title = element_text(hjust = 0.5, vjust=-5, size=15), # designing plot title
#         axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
#         axis.line.y = element_line(color="gray69", size = 0.5), # designing y axis
#         legend.position = "none") # removing legend
# dev.off()

# Q3 In the past week, have you used other ways to access Facebook on this phone, 
# in addition to Facebook Lite (i.e., the Facebook app or the Facebook website)?
# Yes
# No
# I don't remember

library(ggplot2)

# Q3 In the past week, have you used other ways to access Facebook on this phone, 
# in addition to Facebook Lite (i.e., the Facebook app or the Facebook website)?
# Yes
# No
# I don't remember

Q3_used_other_ways_values<-c('Yes', 'No', 'Do not remember')

Q3_used_other_ways_values<-factor(Q3_used_other_ways_values, 
                                  levels=c('Yes', 'No', 'Do not remember'), ordered=TRUE)
levels(Q3_used_other_ways_values)
Q3_used_other_ways<-sample(Q3_used_other_ways_values,size=10^5, replace=TRUE, 
                           prob=c(0.3,0.6,0.1))
head(Q3_used_other_ways)
table(Q3_used_other_ways)

# creating a data frame with the counts and proportions 
Q3_used_other_ways_proportions<-as.data.frame(table(Q3_used_other_ways))
names(Q3_used_other_ways_proportions) <- c('used_other_ways', 'count')
dim(Q3_used_other_ways_proportions)
head(Q3_used_other_ways_proportions)
Q3_used_other_ways_proportions$proportion<-
  Q3_used_other_ways_proportions$count/sum(Q3_used_other_ways_proportions$count)
head(Q3_used_other_ways_proportions)
sum(Q3_used_other_ways_proportions$proportion)
class(Q3_used_other_ways_proportions$proportion)

# renaming the columns
names(Q3_used_other_ways_proportions)<-c('response','count','proportion')

# # plotting the Q3 proportions
# png("C:/Users/nirlevy/Google Drive/sync/data analytics/R plots/Q3_used_other_ways_plot.png", width = 800, height = 500)
# Q3_used_other_ways_proportions %>%
#   ggplot(aes(x = reorder(response, -proportion), y=proportion)) + # setting x and y
#   geom_bar(stat = "identity", fill="#054C70", position="identity") + # creating a bar plot
#   xlab("") + # removing x label
#   ylab("") + # removing y label
#   labs(title = "Have you used other ways to access Facebook on this phone?") + # chart title
#   scale_y_continuous(limits=c(0,1), expand=c(0,0), 
#                      labels=scales::percent_format(accuracy=1)) + # scaling the y axis
#   geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=0.5, size=3.5) + # creating labels above bars with their value
#   # geom_label(aes(y=0.4, label=country_n_label), fill = "snow2", 
#   #           position=position_stack(vjust=0.15), 
#   #           label.padding = unit(0.3, "lines"), alpha=0.9)+ # creating boxes with number of observations
#   # geom_errorbar(aes(x=names,ymin=at_least_one-1.96*proportion_se,ymax=at_least_one+1.96*proportion_se),
#   #              width=0.07, colour="black", alpha=0.5, size=0.8)+ # creating error bars
#   theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=10), # creating x axis lables
#         axis.text.y = element_text(), # removing y axis labels
#         axis.ticks.x=element_blank(), # removing x axis ticks
#         axis.ticks.y=element_blank(), # removing y axis ticks
#         panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
#         plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
#         plot.title = element_text(hjust = 0.5, vjust=-5, size=15), # designing plot title
#         axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
#         axis.line.y = element_line(color="gray69", size = 0.5), # designing y axis
#         legend.position = "none") # removing legend
# dev.off()


# Q4 Why do you use other ways to access Facebook on this phone, 
# in addition to Facebook Lite? 
# Please select all that apply.

# They look and feel better
# They have less crashes and bugs
# They allow me to do more things
# They're easier to use
# I'm not sure
# Other (please specify):

# creating the values
Q4_why_used_other_ways_look_feel_values<-c(0,1)
Q4_why_used_other_ways_less_crashes_bugs_values<-c(0,1)
Q4_why_used_other_ways_do_more_things_values<-c(0,1)
Q4_why_used_other_ways_easier_to_use_values<-c(0,1)
Q4_why_used_other_ways_not_sure_values<-c(0,1)
Q4_why_used_other_ways_other_values<-c(0,1)

# creating the variables
# look_feel
Q4_why_used_other_ways_look_feel<-sample(Q4_why_used_other_ways_look_feel_values,size=10^5, replace=TRUE, 
                                         prob=c(0.9,0.1))
head(Q4_why_used_other_ways_look_feel)
table(Q4_why_used_other_ways_look_feel)

# less_crashes_bugs
Q4_why_used_other_ways_less_crashes_bugs<-sample(Q4_why_used_other_ways_less_crashes_bugs_values,size=10^5, replace=TRUE, 
                                                 prob=c(0.6,0.4))
head(Q4_why_used_other_ways_less_crashes_bugs)
table(Q4_why_used_other_ways_less_crashes_bugs)

#do_more_things
Q4_why_used_other_ways_do_more_things<-sample(Q4_why_used_other_ways_do_more_things_values,size=10^5, replace=TRUE, 
                                              prob=c(0.9,0.1))
head(Q4_why_used_other_ways_do_more_things)
table(Q4_why_used_other_ways_do_more_things)

# easier_to_use
Q4_why_used_other_ways_easier_to_use<-sample(Q4_why_used_other_ways_easier_to_use_values,size=10^5, replace=TRUE, 
                                             prob=c(0.9,0.1))
head(Q4_why_used_other_ways_easier_to_use)
table(Q4_why_used_other_ways_easier_to_use)

# not_sure
Q4_why_used_other_ways_not_sure<-sample(Q4_why_used_other_ways_not_sure_values,size=10^5, replace=TRUE, 
                                        prob=c(0.9,0.1))
head(Q4_why_used_other_ways_not_sure)
table(Q4_why_used_other_ways_not_sure)

# other
Q4_why_used_other_ways_other<-ifelse(Q4_why_used_other_ways_look_feel+
                                       Q4_why_used_other_ways_less_crashes_bugs+
                                       Q4_why_used_other_ways_do_more_things+
                                       Q4_why_used_other_ways_easier_to_use+
                                       Q4_why_used_other_ways_not_sure==0,1,0)

head(Q4_why_used_other_ways_other)
table(Q4_why_used_other_ways_other)
prop.table(table(Q4_why_used_other_ways_other))

# creating composite variable 
Q4_why_used_other_ways<-as.data.frame(cbind(Q4_why_used_other_ways_look_feel,
                                            Q4_why_used_other_ways_less_crashes_bugs,
                                            Q4_why_used_other_ways_do_more_things,
                                            Q4_why_used_other_ways_easier_to_use,
                                            Q4_why_used_other_ways_not_sure,
                                            Q4_why_used_other_ways_other))

dim(Q4_why_used_other_ways)
head(Q4_why_used_other_ways)

# shortening the names of the values in the composite variable
Q4_short_names <- sub("Q4_why_used_other_ways_", "Q4_", names(Q4_why_used_other_ways))
names(Q4_why_used_other_ways)<-Q4_short_names
Q4_plot_names<-c('Look & Feel', 'Less Crashes & Bugs', 'Do more things', 'Easier to use',
                                 'Not sure', 'Other')

# create dataframe of proportions of all Q4 responses
proportion<-colMeans(Q4_why_used_other_ways)
Q4_why_used_other_ways_proportions<-as.data.frame(proportion)
Q4_why_used_other_ways_proportions

# # plotting the Q4 proportions
# png("C:/Users/nirlevy/Google Drive/sync/data analytics/R plots/Q4_why_used_other_ways_plot.png", width = 800, height = 500)
# Q4_why_used_other_ways_proportions %>%
#   ggplot(aes(x = reorder(Q4_plot_names, -proportion), y=proportion)) + # setting x and y
#   geom_bar(stat = "identity", fill="#054C70", position="identity") + # creating a bar plot
#   xlab("") + # removing x label
#   ylab("") + # removing y label
#   labs(title = "Reasons for using other ways to access Facebook") + # chart title
#   scale_y_continuous(limits=c(0,1), expand=c(0,0), 
#                      labels=scales::percent_format(accuracy=1)) + # scaling the y axis
#   geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=0.5, size=3.5) + # creating labels above bars with their value
#   # geom_label(aes(y=0.4, label=country_n_label), fill = "snow2", 
#   #           position=position_stack(vjust=0.15), 
#   #           label.padding = unit(0.3, "lines"), alpha=0.9)+ # creating boxes with number of observations
#   # geom_errorbar(aes(x=names,ymin=at_least_one-1.96*proportion_se,ymax=at_least_one+1.96*proportion_se),
#   #              width=0.07, colour="black", alpha=0.5, size=0.8)+ # creating error bars
#   theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=10), # creating x axis lables
#         axis.text.y = element_text(), # removing y axis labels
#         axis.ticks.x=element_blank(), # removing x axis ticks
#         axis.ticks.y=element_blank(), # removing y axis ticks
#         panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
#         plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
#         plot.title = element_text(hjust = 0.5, vjust=-5, size=15), # designing plot title
#         axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
#         axis.line.y = element_line(color="gray69", size = 0.5), # designing y axis
#         legend.position = "none") # removing legend
# dev.off()


# Q5 What things do you prefer doing when accessing Facebook on a phone 
# in other ways instead of Facebook Lite? Please select all that apply.
# Add posts
# Add stories
# View stories
# Watch videos
# Use Marketplace
# Search for things on Facebook
# Find and respond to events
# Find and participate in relevant groups
# Other (please specify):

# Q5 What things do you prefer doing when accessing Facebook on a phone 
# in other ways instead of Facebook Lite? Please select all that apply.
# Add posts
# Add stories
# View stories
# Watch videos
# Use Marketplace
# Search for things on Facebook
# Find and respond to events
# Find and participate in relevant groups
# Other (please specify):

# creating the values
Q5_activities_not_on_Lite_add_posts_values<-c(0,1)
Q5_activities_not_on_Lite_add_stories_values<-c(0,1)
Q5_activities_not_on_Lite_view_stories_values<-c(0,1)
Q5_activities_not_on_Lite_watch_videos_values<-c(0,1)
Q5_activities_not_on_Lite_use_marketplace_values<-c(0,1)
Q5_activities_not_on_Lite_search_facebook_values<-c(0,1)
Q5_activities_not_on_Lite_find_respond_events_values<-c(0,1)
Q5_activities_not_on_Lite_find_participate_groups_values<-c(0,1)

# creating the variables
# 1. add_posts
Q5_activities_not_on_Lite_add_posts<-sample(Q5_activities_not_on_Lite_add_posts_values,size=10^5, replace=TRUE, 
                                            prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_add_posts)
table(Q5_activities_not_on_Lite_add_posts)

# 2. add_stories
Q5_activities_not_on_Lite_add_stories<-sample(Q5_activities_not_on_Lite_add_stories_values,size=10^5, replace=TRUE, 
                                              prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_add_stories)
table(Q5_activities_not_on_Lite_add_stories)

# 3. view_stories
Q5_activities_not_on_Lite_view_stories<-sample(Q5_activities_not_on_Lite_view_stories_values,size=10^5, replace=TRUE, 
                                               prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_view_stories)
table(Q5_activities_not_on_Lite_view_stories)

# 4. watch_videos
Q5_activities_not_on_Lite_watch_videos<-sample(Q5_activities_not_on_Lite_watch_videos_values,size=10^5, replace=TRUE, 
                                               prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_watch_videos)
table(Q5_activities_not_on_Lite_watch_videos)

# 5. use_marketplace
Q5_activities_not_on_Lite_use_marketplace<-sample(Q5_activities_not_on_Lite_use_marketplace_values,size=10^5, replace=TRUE, 
                                                  prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_use_marketplace)
table(Q5_activities_not_on_Lite_use_marketplace)

# 6. search_facebook
Q5_activities_not_on_Lite_search_facebook<-sample(Q5_activities_not_on_Lite_search_facebook_values,size=10^5, replace=TRUE, 
                                                  prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_search_facebook)
table(Q5_activities_not_on_Lite_search_facebook)

# 7. find_respond_events
Q5_activities_not_on_Lite_find_respond_events<-sample(Q5_activities_not_on_Lite_find_respond_events_values,size=10^5, replace=TRUE, 
                                                      prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_find_respond_events)
table(Q5_activities_not_on_Lite_find_respond_events)

# 8. find_participate_groups
Q5_activities_not_on_Lite_find_participate_groups<-sample(Q5_activities_not_on_Lite_find_participate_groups_values,size=10^5, replace=TRUE, 
                                                          prob=c(0.8,0.2))
head(Q5_activities_not_on_Lite_find_participate_groups)
table(Q5_activities_not_on_Lite_find_participate_groups)

# 9. other
Q5_activities_not_on_Lite_other<-ifelse(Q5_activities_not_on_Lite_add_posts+
                                          Q5_activities_not_on_Lite_add_stories+
                                          Q5_activities_not_on_Lite_view_stories+
                                          Q5_activities_not_on_Lite_watch_videos+
                                          Q5_activities_not_on_Lite_use_marketplace+
                                          Q5_activities_not_on_Lite_search_facebook+
                                          Q5_activities_not_on_Lite_find_respond_events+
                                          Q5_activities_not_on_Lite_find_participate_groups==0,1,0)

head(Q5_activities_not_on_Lite_other)
table(Q5_activities_not_on_Lite_other)
prop.table(table(Q5_activities_not_on_Lite_other))

# creating composite variable 
Q5_why_used_other_ways<-as.data.frame(cbind(Q5_activities_not_on_Lite_add_posts,
                                            Q5_activities_not_on_Lite_add_stories,
                                            Q5_activities_not_on_Lite_view_stories,
                                            Q5_activities_not_on_Lite_watch_videos,
                                            Q5_activities_not_on_Lite_use_marketplace,
                                            Q5_activities_not_on_Lite_search_facebook,
                                            Q5_activities_not_on_Lite_find_respond_events,
                                            Q5_activities_not_on_Lite_find_participate_groups,
                                            Q5_activities_not_on_Lite_other))

dim(Q5_why_used_other_ways)
head(Q5_why_used_other_ways)

# shortening the names of the values in the composite variable
Q5_short_names <- sub("Q5_activities_not_on_Lite_", "Q5_", names(Q5_why_used_other_ways))
names(Q5_why_used_other_ways)<-Q5_short_names
Q5_plot_names<-c('Add posts', 'Add stories', 'View stories',
                                 'Watch videos', 'Use marketplace', 'Search Facebook',
                                 'Find-respond-events', 'find-participate-groups', 'other')

# create dataframe of proportions of all Q5 responses
proportion<-colMeans(Q5_why_used_other_ways)
Q5_why_used_other_ways_proportions<-as.data.frame(proportion)
Q5_why_used_other_ways_proportions

# # plotting the Q5 proportions
# png("C:/Users/nirlevy/Google Drive/sync/data analytics/R plots/Q5_why_used_other_ways_proportions_plot.png", width = 800, height = 500)
# Q5_why_used_other_ways_proportions %>%
#   ggplot(aes(x = reorder(Q5_plot_names, -proportion), y=proportion)) + # setting x and y
#   geom_bar(stat = "identity", fill="#054C70", position="identity") + # creating a bar plot
#   xlab("") + # removing x label
#   ylab("") + # removing y label
#   labs(title = "Activities that the respondent prefers to do
#        on other platforms (not Lite)") + # chart title
#   scale_y_continuous(limits=c(0,1), expand=c(0,0), 
#                      labels=scales::percent_format(accuracy=1)) + # scaling the y axis
#   geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=0.5, size=3.5) + # creating labels above bars with their value
#   # geom_label(aes(y=0.4, label=country_n_label), fill = "snow2", 
#   #           position=position_stack(vjust=0.15), 
#   #           label.padding = unit(0.3, "lines"), alpha=0.9)+ # creating boxes with number of observations
#   # geom_errorbar(aes(x=names,ymin=at_least_one-1.96*proportion_se,ymax=at_least_one+1.96*proportion_se),
#   #              width=0.07, colour="black", alpha=0.5, size=0.8)+ # creating error bars
#   theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=10), # creating x axis lables
#         axis.text.y = element_text(), # removing y axis labels
#         axis.ticks.x=element_blank(), # removing x axis ticks
#         axis.ticks.y=element_blank(), # removing y axis ticks
#         panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
#         plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
#         plot.title = element_text(hjust = 0.5, vjust=-5, size=15), # designing plot title
#         axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
#         axis.line.y = element_line(color="gray69", size = 0.5), # designing y axis
#         legend.position = "none") # removing legend
# dev.off()

# Q6 How can we improve Facebook Lite?
Q6_how_can_we_improve_values<-c('a','b')
Q6_how_can_we_improve<-sample(Q6_how_can_we_improve_values, size = 10^5,
                              replace=TRUE, prob=c(0.5,0.5))
table(Q6_how_can_we_improve)


# creating the population dataset
population_data<-cbind(gender,
                       age,
                       lness,
                       device_type,
                       device_LE,
                       device_MR,
                       device_HE,
                       country,
                       Philippines,
                       Brazil,
                       Indonesia,
                       India,
                       Mexico,
                       Thailand,
                       version,
                       version_old,
                       version_new,
                       Q1_why_using_facebook,
                       Q2_why_works_fast,
                       Q3_used_other_ways,
                       Q4_why_used_other_ways,
                       Q5_why_used_other_ways,
                       Q6_how_can_we_improve)
#View(population_data[1:10,])

# Q7 Simulated outcome for checking the weighting
# This outcome will be identical for each strata
# step 1 - creating a variable denoting the strata in the population data

population_data$strata<-999
population_data$strata[population_data$country=='Brazil' &
                         population_data$device_type=='LE']<-'Brazil_LE'
population_data$strata[population_data$country=='Brazil' &
                         population_data$device_type=='MR']<-'Brazil_MR'
population_data$strata[population_data$country=='Brazil' &
                         population_data$device_type=='HE']<-'Brazil_HE'
population_data$strata[population_data$country=='Indonesia' &
                         population_data$device_type=='LE']<-'Indonesia_LE'
population_data$strata[population_data$country=='Indonesia' &
                         population_data$device_type=='MR']<-'Indonesia_MR'
population_data$strata[population_data$country=='Indonesia' &
                         population_data$device_type=='HE']<-'Indonesia_HE'
population_data$strata[population_data$country=='India' &
                         population_data$device_type=='LE']<-'India_LE'
population_data$strata[population_data$country=='India' &
                         population_data$device_type=='MR']<-'India_MR'
population_data$strata[population_data$country=='India' &
                         population_data$device_type=='HE']<-'India_HE'
population_data$strata[population_data$country=='Mexico' &
                         population_data$device_type=='LE']<-'Mexico_LE'
population_data$strata[population_data$country=='Mexico' &
                         population_data$device_type=='MR']<-'Mexico_MR'
population_data$strata[population_data$country=='Mexico' &
                         population_data$device_type=='HE']<-'Mexico_HE'
population_data$strata[population_data$country=='Philippines' &
                         population_data$device_type=='LE']<-'Philippines_LE'
population_data$strata[population_data$country=='Philippines' &
                         population_data$device_type=='MR']<-'Philippines_MR'
population_data$strata[population_data$country=='Philippines' &
                         population_data$device_type=='HE']<-'Philippines_HE'
population_data$strata[population_data$country=='Thailand' &
                         population_data$device_type=='LE']<-'Thailand_LE'
population_data$strata[population_data$country=='Thailand' &
                         population_data$device_type=='MR']<-'Thailand_MR'
population_data$strata[population_data$country=='Thailand' &
                         population_data$device_type=='HE']<-'Thailand_HE'

# step 2 - creating the unique outcome variable in each strata
population_data$Q7_outcome_for_weight_test<-999
population_data$Q7_outcome_for_weight_test[population_data$strata=='Brazil_LE']<-1
population_data$Q7_outcome_for_weight_test[population_data$strata=='Brazil_MR']<-2
population_data$Q7_outcome_for_weight_test[population_data$strata=='Brazil_HE']<-3
population_data$Q7_outcome_for_weight_test[population_data$strata=='Indonesia_LE']<-4
population_data$Q7_outcome_for_weight_test[population_data$strata=='Indonesia_MR']<-5
population_data$Q7_outcome_for_weight_test[population_data$strata=='Indonesia_HE']<-6
population_data$Q7_outcome_for_weight_test[population_data$strata=='India_LE']<-7
population_data$Q7_outcome_for_weight_test[population_data$strata=='India_MR']<-8
population_data$Q7_outcome_for_weight_test[population_data$strata=='India_HE']<-9
population_data$Q7_outcome_for_weight_test[population_data$strata=='Mexico_LE']<-10
population_data$Q7_outcome_for_weight_test[population_data$strata=='Mexico_MR']<-11
population_data$Q7_outcome_for_weight_test[population_data$strata=='Mexico_HE']<-12
population_data$Q7_outcome_for_weight_test[population_data$strata=='Philippines_LE']<-13
population_data$Q7_outcome_for_weight_test[population_data$strata=='Philippines_MR']<-14
population_data$Q7_outcome_for_weight_test[population_data$strata=='Philippines_HE']<-15
population_data$Q7_outcome_for_weight_test[population_data$strata=='Thailand_LE']<-16
population_data$Q7_outcome_for_weight_test[population_data$strata=='Thailand_MR']<-17
population_data$Q7_outcome_for_weight_test[population_data$strata=='Thailand_HE']<-18

table(population_data$Q7_outcome_for_weight_test)


# creating a variable for the propensity to respond
population_data$propensity_to_respond<-
  0.05*device_LE+
  0.04*device_MR+
  0.06*device_HE+
  0.075*Philippines+
  0.025*Brazil+
  0.06*Indonesia+
  0.04*India+
  0.035*Mexico+
  0.065*Thailand
mean(population_data$propensity_to_respond)
length(population_data$propensity_to_respond)
class(population_data$propensity_to_respond)
summary(population_data$propensity_to_respond)

# creating the variable denting whether the person responded to the survey
response_function<-function(p){
  r<-sample(c(0,1),size=1,prob=c(1-p,p))}

population_data$responded<-sapply(population_data$propensity_to_respond,response_function)
summary(population_data$responded)


############################################################
########## creating the dataset of the responses ###########
############################################################

data_unweighted<-population_data %>% filter(responded==1) %>% select(-responded)
dim(data_unweighted)
#View(data_unweighted[1:10,])

###########################################################
###### turning the sample data into a survey object  ######
###########################################################

# adding an fpc column to the data (finite population correction factor)
data_unweighted$fpc <- 10^5

# library(survey)
preliminary.design <- 
  svydesign( 
    id = ~1 , 
    data = data_unweighted ,
    fpc = ~fpc
  )

#####################################################################################################
########  weighting  by post-stratification - method 1 - using the postStratify command   ###########
#####################################################################################################

# calculating 'real' weight
data_unweighted$real_weight<-1/data_unweighted$propensity_to_respond
summary(data_unweighted$real_weight)

# weighting according to country and device type

# checking number of respondents from each device type
table (data_unweighted$device_type)

# checking number of respondents from each country
table (data_unweighted$country)

# checking number of respondents from each device type and version
table (data_unweighted$device_type, data_unweighted$country)

# creating a variable with a unique value for each strata
data_unweighted$strata<-999
data_unweighted$strata[data_unweighted$country=='Brazil' &
                         data_unweighted$device_type=='LE']<-'Brazil_LE'
data_unweighted$strata[data_unweighted$country=='Brazil' &
                         data_unweighted$device_type=='MR']<-'Brazil_MR'
data_unweighted$strata[data_unweighted$country=='Brazil' &
                         data_unweighted$device_type=='HE']<-'Brazil_HE'
data_unweighted$strata[data_unweighted$country=='Indonesia' &
                         data_unweighted$device_type=='LE']<-'Indonesia_LE'
data_unweighted$strata[data_unweighted$country=='Indonesia' &
                         data_unweighted$device_type=='MR']<-'Indonesia_MR'
data_unweighted$strata[data_unweighted$country=='Indonesia' &
                         data_unweighted$device_type=='HE']<-'Indonesia_HE'
data_unweighted$strata[data_unweighted$country=='India' &
                         data_unweighted$device_type=='LE']<-'India_LE'
data_unweighted$strata[data_unweighted$country=='India' &
                         data_unweighted$device_type=='MR']<-'India_MR'
data_unweighted$strata[data_unweighted$country=='India' &
                         data_unweighted$device_type=='HE']<-'India_HE'
data_unweighted$strata[data_unweighted$country=='Mexico' &
                         data_unweighted$device_type=='LE']<-'Mexico_LE'
data_unweighted$strata[data_unweighted$country=='Mexico' &
                         data_unweighted$device_type=='MR']<-'Mexico_MR'
data_unweighted$strata[data_unweighted$country=='Mexico' &
                         data_unweighted$device_type=='HE']<-'Mexico_HE'
data_unweighted$strata[data_unweighted$country=='Philippines' &
                         data_unweighted$device_type=='LE']<-'Philippines_LE'
data_unweighted$strata[data_unweighted$country=='Philippines' &
                         data_unweighted$device_type=='MR']<-'Philippines_MR'
data_unweighted$strata[data_unweighted$country=='Philippines' &
                         data_unweighted$device_type=='HE']<-'Philippines_HE'
data_unweighted$strata[data_unweighted$country=='Thailand' &
                         data_unweighted$device_type=='LE']<-'Thailand_LE'
data_unweighted$strata[data_unweighted$country=='Thailand' &
                         data_unweighted$device_type=='MR']<-'Thailand_MR'
data_unweighted$strata[data_unweighted$country=='Thailand' &
                         data_unweighted$device_type=='HE']<-'Thailand_HE'

# making sure that the variable was created properly
table(data_unweighted$strata)
table (data_unweighted$device_type, data_unweighted$country)

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

# Creating a secondary data.frame object with two columns: 
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

# creating the weighted dataset with the 'postStratify' command
data_weighted <- 
  postStratify(
    preliminary.design ,
    strata = ~strata ,
    population = ps.weights
  )


# exploring the data
class(data_weighted)
names(data_weighted)
head(data_weighted$variables) # these are the variables
table(data_weighted$strata) # it looks like this denotes whether the row belongs in a strata (in this example they all do)
unique(data_weighted$cluster)  # this is probably for cluster sampling
unique(data_weighted$has.strata) # i don't know what this is
unique(data_weighted$strata) # i don't know what this is
unique(data_weighted$prob) # this is (nh / Nh) - the number of respondents in each strata 
head(data_weighted$prob)   # divided by the number of people in the population 
data_weighted$prob         # in that strata 
table(data_weighted$prob)  
table(data_weighted$prob, data_weighted$variables$strata)
unique(data_weighted$allprob) # this is the total (nh/Nh) - the total number of respondents 
head(data_weighted$allprob)   # divided by the total number of people in the population 
class(data_weighted$call) # i don't know what this is
class(data_weighted$pps) # i don't know what this is
head(data_weighted$pps) # i don't know what this is
class(data_weighted$postStrata) # these are the number of people (nh) in each strata
table(data_weighted$postStrata) # in the response dataset
table(data_unweighted$strata)

# comparing the calculated probability to respond
# with the defined propesity to respond
difference<-data_weighted$prob-data_weighted$variables$propensity_to_respond
summary(difference)

# another way to examine the weights
svyby(~weights(data_weighted), ~strata, data_weighted, svymean)

# Viewing the degrees of freedom for this survey design object:
nrow(data_weighted)
degf(data_weighted)

# Printing the mean and standard error of an outcome variable:
svymean(~Q7_outcome_for_weight_test, data_weighted)

# Printing the mean and standard error of the outcome variable, 
# by strata:
svyby(~Q7_outcome_for_weight_test, ~strata, data_weighted, svymean)

# Alternatively, the result of a function call like svymean, svytotal, or svyby 
# can be stored into a secondary R object.

# example with the mean:
mysvyby <- svyby(~Q7_outcome_for_weight_test, ~strata, data_weighted, svymean)

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

# Also note that the number of decimal places shown can be adjusted 
# by modifying the digits parameter within the options function at any time.
options(digits = 3)
confint(mysvyby, df = degf(data_weighted))

# comparing the mean in the survey data to the mean
# in the population data, to see if the weighting was good
mean(population_data$Q7_outcome_for_weight_test)
mean(data_unweighted$Q7_outcome_for_weight_test)
svymean(~Q7_outcome_for_weight_test, data_weighted)

#########################################################################################################
########  weighting  by post-stratification - method 2 - not using the postStratify command   ###########
#########################################################################################################

# making sure that the variable was created properly
table(data_unweighted$strata)
table (data_unweighted$device_type, data_unweighted$country)

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

# creating a variable denoting the number of people in each strata in the population
population_data <- population_data %>%
  group_by(strata) %>%
  mutate(Nh = n())

class(population_data)

# verifying
table(population_data$Nh)
table(population_data$Nh,population_data$strata)

# creating a variable denoting the number of people in each strata in the respondent dataset
data_unweighted <- data_unweighted %>%
  group_by(strata) %>%
  mutate(nh = n())

# verifying
table(data_unweighted$nh)
table(data_unweighted$nh,data_unweighted$strata)

# Adding the variable with the number of people in each strata in the population to the data
lookup<-as.data.frame(cbind(strata_unique,colSums(table(population_data$Nh,population_data$strata))))
dim(lookup)
names(lookup)<-c('strata','Nh')
lookup

unique(data_unweighted$strata)
lookup$strata

newdat <- merge(data_unweighted, lookup, by = 'strata')
names(newdat)
head(newdat$Nh)
newdat$Nh<-as.numeric(levels(newdat$Nh)[newdat$Nh])

weights_manual<-newdat$Nh/newdat$nh
weights_manual

# creating the weighted dataset 
data_weighted_2 <- 
  svydesign( 
    id = ~1, 
    data = newdat,
    fpc = ~fpc,
    weights = weights_manual
  )


# exploring the data
class(data_weighted_2)
names(data_weighted_2)
head(data_weighted_2$variables) # these are the variables
table(data_weighted_2$strata) # it looks like this denotes whether the row belongs in a strata (in this example they all do)
unique(data_weighted_2$cluster)  # this is probably for cluster sampling
unique(data_weighted_2$has.strata) # i don't know what this is
unique(data_weighted_2$strata) # i don't know what this is
unique(data_weighted_2$prob) # this is (nh / Nh) - the number of respondents in each strata 
head(data_weighted_2$prob)   # divided by the number of people in the population 
data_weighted_2$prob         # in that strata 
table(data_weighted_2$prob)  
table(data_weighted_2$prob, data_weighted_2$variables$strata)
unique(data_weighted_2$allprob) # this is the total (nh/Nh) - the total number of respondents 
head(data_weighted_2$allprob)   # divided by the total number of people in the population 
class(data_weighted_2$call) # i don't know what this is
class(data_weighted_2$pps) # i don't know what this is
head(data_weighted_2$pps) # i don't know what this is
class(data_weighted_2$postStrata) # these are the number of people (nh) in each strata
table(data_weighted_2$postStrata) # in the response dataset
table(data_unweighted$strata)

# comparing the calculated probability to respond
# with the defined propesity to respond
difference<-data_weighted_2$prob-data_weighted_2$variables$propensity_to_respond
summary(difference)

# Viewing the degrees of freedom for this survey design object:
nrow(data_weighted_2)
degf(data_weighted_2)

# Printing the mean and standard error of an outcome variable:
svymean(~Q7_outcome_for_weight_test, data_weighted_2)

# Printing the mean and standard error of the outcome variable, 
# by strata:
svyby(~Q7_outcome_for_weight_test, ~strata, data_weighted_2, svymean)

# Alternatively, the result of a function call like svymean, svytotal, or svyby 
# can be stored into a secondary R object.

# example with the mean:
mysvyby <- svyby(~Q7_outcome_for_weight_test, ~strata, data_weighted_2, svymean)

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

# Also note that the number of decimal places shown can be adjusted 
# by modifying the digits parameter within the options function at any time.
options(digits = 3)
confint(mysvyby, df = degf(data_weighted_2))

# comparing the mean in the survey data to the mean
# in the population data, to see if the weighting was good
mean(population_data$Q7_outcome_for_weight_test)
mean(data_unweighted$Q7_outcome_for_weight_test)
svymean(~Q7_outcome_for_weight_test, data_weighted_2)


#############################################################################
######## weighting  by logistic regression with no bins   ###################
#############################################################################
# creating a variable denoting whether the person responded to the survey
table(population_data$responded)

# creating the formula for the logistic regression
response_equation<- with(population_data, 
                         "responded ~ 
                         gender +
                         age +
                         lness +
                         version +
                         device_MR +
                         device_HE +
                         Indonesia +
                         India +
                         Mexico +
                         Philippines +
                         Thailand")

# running the logistic regression
responded_logit <- glm(response_equation, data=population_data, family = "binomial"(link="logit"))
summary(responded_logit)

# obtaining the predicted variables for each respondent
population_data$predicted_propensity_to_respond<-predict(responded_logit, type="response")
length(population_data$predicted_propensity_to_respond)

# comparing the predicted propensity to respond in the logit model 
# with the one calculated by post-stratification:
preliminary_data_weighted_logistic<-population_data %>% filter(responded==1)
dim(preliminary_data_weighted_logistic)
summary(preliminary_data_weighted_logistic$predicted_propensity_to_respond)

# creating the finite population correction variable
preliminary_data_weighted_logistic$fpc<-10^5

# creating the weights variable
preliminary_data_weighted_logistic$weights_no_bins<-1/(preliminary_data_weighted_logistic$predicted_propensity_to_respond)
length(preliminary_data_weighted_logistic$weights_no_bins)
summary(preliminary_data_weighted_logistic$weights_no_bins) 
head(preliminary_data_weighted_logistic)

# plotting the weights
png("C:/Users/nirlevy/Google Drive/sync/data analytics/R plots/preliminary_data_weighted_logistic_no_bins.png", width = 800, height = 500)
ggplot(preliminary_data_weighted_logistic, aes(x = weights_no_bins)) + # setting dataset and x 
geom_density() + # defining the type of plot
geom_vline(aes(xintercept = mean(weights_no_bins)), 
             linetype = "dashed", size = 0.6)+ # defining a vertical line showing the mean of x
  # xlab("") + # removing x label
  # ylab("") + # removing y label
  # labs(title = "Have you used other ways to access Facebook on this phone?") + # chart title
  # scale_y_continuous(limits=c(0,1), expand=c(0,0),
  #                   labels=scales::percent_format(accuracy=1)) + # scaling the y axis
  # geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=0.5, size=3.5) + # creating labels above bars with their value
  # geom_label(aes(y=0.4, label=country_n_label), fill = "snow2",
  #           position=position_stack(vjust=0.15),
  #           label.padding = unit(0.3, "lines"), alpha=0.9)+ # creating boxes with number of observations
  # geom_errorbar(aes(x=names,ymin=at_least_one-1.96*proportion_se,ymax=at_least_one+1.96*proportion_se),
  #              width=0.07, colour="black", alpha=0.5, size=0.8)+ # creating error bars
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=10), # creating x axis lables
        axis.text.y = element_text(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-5, size=15), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
        axis.line.y = element_line(color="gray69", size = 0.5), # designing y axis
        legend.position = "none") # removing legend
dev.off()

# if there would have been extreme weights - trim them or use bins.

# creating the survey data with the weights variable based on the logistic regression
survey_data_with_logit_weights <- 
  svydesign( 
    id = ~1, 
    data = preliminary_data_weighted_logistic,
    fpc = ~fpc,
    weights = preliminary_data_weighted_logistic$weights_no_bins
  )
 
# exploring the data
class(survey_data_with_logit_weights)
names(survey_data_with_logit_weights)
dim(survey_data_with_logit_weights$variables) # these are the variables
# unique(survey_data_with_logit_weights$prob) # this is (nh / Nh) - the number of respondents in each strata 
# table(survey_data_with_logit_weights$prob)  
head(survey_data_with_logit_weights$allprob)   # divided by the total number of people in the population 

# Viewing the degrees of freedom for this survey design object:
nrow(survey_data_with_logit_weights)
degf(survey_data_with_logit_weights)

# Printing the mean and standard error of an outcome variable:
svymean(~Q7_outcome_for_weight_test, survey_data_with_logit_weights)

# Printing the mean and standard error of the outcome variable, 
# by strata:
svyby(~Q7_outcome_for_weight_test, ~strata, survey_data_with_logit_weights, svymean)

# Alternatively, the result of a function call like svymean, svytotal, or svyby 
# can be stored into a secondary R object.
# example with the mean:
mysvyby <- svyby(~Q7_outcome_for_weight_test, ~strata, survey_data_with_logit_weights, svymean)

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

# Also note that the number of decimal places shown can be adjusted 
# by modifying the digits parameter within the options function at any time.
options(digits = 3)
confint(mysvyby, df = degf(survey_data_with_logit_weights))

###########################################################################
######## weighting  by logistic regression with  bins   ###################
###########################################################################

# dividing the weights variable into bins
nbins<-10 # setting the number of bins
preliminary_data_weighted_logistic$quantile_for_weighting<-ntile(preliminary_data_weighted_logistic$weights_no_bins, nbins)
table(preliminary_data_weighted_logistic$quantile_for_weighting)

preliminary_data_weighted_logistic <- preliminary_data_weighted_logistic %>%
  group_by(quantile_for_weighting) %>%
  mutate(weight_in_quantile= mean(weights_no_bins))

table(preliminary_data_weighted_logistic$weight_in_quantile)

# plotting the weights
png("C:/Users/nirlevy/Google Drive/sync/data analytics/R plots/preliminary_data_weighted_logistic_bins.png", width = 800, height = 500)
ggplot(preliminary_data_weighted_logistic, aes(x = weight_in_quantile)) + # setting dataset and x 
  geom_bar() + # defining the type of plot
  geom_vline(aes(xintercept = mean(weights_no_bins)), 
             linetype = "dashed", size = 0.6)+ # defining a vertical line showing the mean of x
  # xlab("") + # removing x label
  # ylab("") + # removing y label
  # labs(title = "Have you used other ways to access Facebook on this phone?") + # chart title
  # scale_y_continuous(limits=c(0,1), expand=c(0,0),
  #                   labels=scales::percent_format(accuracy=1)) + # scaling the y axis
  # geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=0.5, size=3.5) + # creating labels above bars with their value
  # geom_label(aes(y=0.4, label=country_n_label), fill = "snow2",
  #           position=position_stack(vjust=0.15),
  #           label.padding = unit(0.3, "lines"), alpha=0.9)+ # creating boxes with number of observations
  # geom_errorbar(aes(x=names,ymin=at_least_one-1.96*proportion_se,ymax=at_least_one+1.96*proportion_se),
  #              width=0.07, colour="black", alpha=0.5, size=0.8)+ # creating error bars
theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=10), # creating x axis lables
      axis.text.y = element_text(), # removing y axis labels
      axis.ticks.x=element_blank(), # removing x axis ticks
      axis.ticks.y=element_blank(), # removing y axis ticks
      panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
      plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
      plot.title = element_text(hjust = 0.5, vjust=-5, size=15), # designing plot title
      axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
      axis.line.y = element_line(color="gray69", size = 0.5), # designing y axis
      legend.position = "none") # removing legend
dev.off()

# creating the survey data with the weights variable based on the logistic regression
survey_data_with_logit_weights_bins <- 
  svydesign( 
    id = ~1, 
    data = preliminary_data_weighted_logistic,
    fpc = ~fpc,
    weights = preliminary_data_weighted_logistic$weight_in_quantile
  )

# exploring the data
class(survey_data_with_logit_weights_bins)
names(survey_data_with_logit_weights_bins)
dim(survey_data_with_logit_weights_bins$variables) # these are the variables
# unique(survey_data_with_logit_weights_bins$prob) # this is (nh / Nh) - the number of respondents in each strata 
# table(survey_data_with_logit_weights_bins$prob)  
head(survey_data_with_logit_weights_bins$allprob)   # divided by the total number of people in the population 

# Viewing the degrees of freedom for this survey design object:
nrow(survey_data_with_logit_weights_bins)
degf(survey_data_with_logit_weights_bins)

# Printing the mean and standard error of an outcome variable:
svymean(~Q7_outcome_for_weight_test, survey_data_with_logit_weights_bins)

# Printing the mean and standard error of the outcome variable, 
# by strata:
svyby(~Q7_outcome_for_weight_test, ~strata, survey_data_with_logit_weights_bins, svymean)

# Alternatively, the result of a function call like svymean, svytotal, or svyby 
# can be stored into a secondary R object.
# example with the mean:
mysvyby <- svyby(~Q7_outcome_for_weight_test, ~strata, survey_data_with_logit_weights_bins, svymean)

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

# Also note that the number of decimal places shown can be adjusted 
# by modifying the digits parameter within the options function at any time.
options(digits = 3)
confint(mysvyby, df = degf(survey_data_with_logit_weights_bins))

###########################################################################
######### comparing the means in the different weighting methods ##########
###########################################################################
# in the population data, to see if the weighting was good
mean(population_data$Q7_outcome_for_weight_test)

# no weighting
mean(data_unweighted$Q7_outcome_for_weight_test)

# weighting method 1 - post-stratification - calculated with postStratify command
svymean(~Q7_outcome_for_weight_test, data_weighted)

# weighting method 2 - post-stratification - calculated manually
svymean(~Q7_outcome_for_weight_test, data_weighted_2)

# weighting method 3 - logistic regression, no bins
svymean(~Q7_outcome_for_weight_test, survey_data_with_logit_weights)

# weighting method 4 - logistic regression with bins
svymean(~Q7_outcome_for_weight_test, survey_data_with_logit_weights_bins)

