# Analyzing teen data

### Code from Andy ###

# Installing packages
fbr::with_proxy(install.packages("srvyr"))
fbr::with_proxy(install.packages("prediction"))
fbr::with_proxy(install.packages("stringi"))
fbr::with_proxy(install.packages("ggplot2"))
fbr::with_proxy(install.packages("ggthemes"))
fbr::with_proxy(install.packages("corrplot"))

# Loading libraries
library(fbr)
library(dplyr)
library(glue)
library(ggplot2)
library(ggthemes)
library(scales)
library(survey)
library(srvyr)
library(tidyverse)
library(prediction)

# Preparing variables

# Newest version
# response_cols and groupby_cols should all be factors
# Think about adding .drop=FALSE parameter
# two_levels checks to make sure the variable has at least two levels (R throws an error otherwise)
# filter(!is.na(i_temp) | keep_na): determines if we could missing in the proportion or not
# The unweighted function throws a warning, but it can be ignored 
survey_by_prop <- function(surv_obj, response_cols, groupby_cols = list(), keep_na=FALSE) {
  response_prop=data.frame(matrix(ncol=0,nrow=0))
  for (i in response_cols){
    two_levels <- surv_obj %>% 
      summarise(n_distinct(as.factor(!!sym(i)), na.rm = TRUE) >= 2) 
    if(two_levels[1,1]==TRUE) {
      temp <- surv_obj %>%
        mutate(i_temp = as.factor(!!sym(i))) %>% 
        filter(!is.na(i_temp) | keep_na) %>% 
        group_by(.dots=groupby_cols, .drop=FALSE) %>% 
        group_by(i_temp, add = TRUE, .drop=FALSE) %>%
        summarize(proportion = survey_mean(na.rm = TRUE, vartype = c("se", "ci")),
                  weighted_n = survey_total(na.rm = TRUE, vartype =NULL), 
                  n_responses = unweighted(n()))
      temp$question <- i
      temp <- temp %>%
        rename(value = i_temp)
      response_prop <- bind_rows(response_prop, temp)
    }
    else{
      print(glue('Dropping {i} because it had less than two levels'))
    }
  }
  return(response_prop)
}

## This is a function that probably won't have much use outside of this data set 
# The way the function works is pretty specific to the naming conventions for this data
# It will also only work when you want all the summary stats for a set of columns ending in '_{app}'
# response_col_stem is the stem of those set of columns (do not add the '_' to the stem - the function does that)
prop_by_app <- function(surv_obj, app_list, response_col_stem, groupby_cols = list(), keep_na=FALSE) {
  prop_by_app=data.frame(matrix(ncol=0,nrow=0))
  for(app in app_list){
    col_app <- colnames(surv_obj$variables[grep(glue("{response_col_stem}_{app}"), names(surv_obj$variables))])
    temp <- survey_by_prop(surv_obj, col_app, groupby_cols) 
    if(nrow(temp) != 0) {
      temp$app <- app
      prop_by_app <- bind_rows(prop_by_app, temp)
    }
    else{
      print(glue('Dropping {app} because it had no responses'))
    }
  }
  return(prop_by_app)
}

# Doesn't handle any groupby
props_by_question_app <- function(surv_obj, app_list, response_col_stem_list, groupby_cols = list()) {
  props_by_question_app=data.frame(matrix(ncol=0,nrow=0))
  for(col in response_col_stem_list){
    temp <- prop_by_app(surv_obj, app_list, col, groupby_cols)
    props_by_question_app <- bind_rows(props_by_question_app, temp)
  }
  return(props_by_question_app)
}

# Trim the app name off the end of a question and then return the unique stems
trim_apps <- function(list) {
  list <- list %>%  
    str_replace("_facebook", "") %>% 
    str_replace("_instagram", "") %>% 
    str_replace("_twitter", "") %>% 
    str_replace("_snapchat", "") %>% 
    str_replace("_messenger", "") %>% 
    str_replace("_whatsapp", "") %>% 
    str_replace("_imessage", "") %>% 
    str_replace("_youtube", "") %>%
    str_replace("_linkedin", "") %>% 
    str_replace("_tiktok", "") %>% 
    str_replace("_pinterest", "") %>% 
    str_replace("_none", "")
  
  return(unique(list))
  
}

# Get columns with stem, activity/adj, and app, and drop ordered
get_complex_cols <- function(svy, col_stem) {
  list <- colnames(svy$variables[grep(col_stem, names(svy$variables))])
  list <- grep("ordered", list, value=TRUE, invert=TRUE)
  return(trim_apps(list))
}

# Takes a df and list of columns and relevels them all based on levels_list
# This is useful - clean up and save
relevel_cols <- function(df, cols_to_relevel, levels_list) {
  for (i in cols_to_relevel){
    df <- df %>% 
      mutate(!!i :=  factor(!!sym(i), levels = levels_list))
  }
  return(df)
}


###########################
### Get the survey data ###
###########################
svy_query <- "SELECT * FROM off_platform_teens_data"

svy_df <- presto(svy_query, namespace = "growth")
nrow(svy_df)
ncol(svy_df)

svy_df[svy_df==""]  <- NA 

##############################
### Turn into srvyr object ###
##############################

svy_unweighted <- svy_df %>% as_survey_design(ids = 1, weights=NULL) 


########################
### Get Demographics ###
########################
app_list <- list('facebook',
                 'instagram',
                 'twitter',
                 'snapchat',
                 'messenger',
                 'whatsapp',
                 'imessage',
                 'youtube',
                 'linkedin',
                 'tiktok',
                 'pinterest')

## Questions by app
app_question_list <- list("main_reason", "app_favorability", "app_favorability_vs_three_years_ago", "app_usage_l4w",
                          "app_usage_vs_three_years_ago", "meaningful", "meaningful_vs_three_years_ago", "posts_l4w", 
                          "posts_vs_three_years_ago", "deleted_account", "how_relevant_l4w", "negative_positive", 
                          "how_often_see_from_care_about", "info_shared_worth_it", "value_from", "care_about_users", 
                          "share_sns_time", "share_sns_time_three_years_ago", "load_time", "num_notifications", "main_usage_method",
                          "ever_used_app", "changed_overall", "changed_value", "changed_connected_closeness", "changed_relevant", 
                          "changed_negative_interactions", "changed_more_difficult", "changed_motivated_to_share", "builds_community",
                          "community_connected_impossible_offline", "community_facilitate_offline", "community_unable", 
                          "community_prefer_not_connection", "useful_for_many_things", "connect_w_many_types_of_people", 
                          "waste_of_time", "easily_live_without", "core_value_activator_type", "how_often_unfriend", "political_bias", 'add_new_friend_on')
how_easy_list <- get_complex_cols(svy_unweighted, "how_easy_")
saw_amount_list <- get_complex_cols(svy_unweighted, "saw_amount_")
associate_adj_list <- get_complex_cols(svy_unweighted, "associate_")
neg_consequence_list <- get_complex_cols(svy_unweighted, "neg_consequence_")
neg_exp_list  <- get_complex_cols(svy_unweighted, "neg_exp_")
connected_to_list  <- get_complex_cols(svy_unweighted, "connected_to_")
app_question_list_complex <- c(app_question_list, how_easy_list, saw_amount_list, associate_adj_list, neg_consequence_list, neg_exp_list, connected_to_list)
props_by_question_by_app <- props_by_question_app(svy_unweighted, app_list, app_question_list_complex)
write_csv(props_by_question_by_app, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/props_by_question_by_app.csv")

props_by_cohort_facebook <- props_by_question_app(svy_unweighted, list("facebook"), app_question_list_complex, list('cohort_facebook'))
#props_by_cohort_facebook
write_csv(props_by_cohort_facebook, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/props_by_cohort_facebook.csv")

## Questions not connected to apps
demo_list = list('gender', 
                 'age_bucket',
                 'marital_status',
                 'children_in_hh',
                 'cohort_facebook', 
                 'country', 
                 'urban_suburban_rural', 
                 'education_level', 
                 'political_views', 
                 'favorite_app',
                 'consent', 
                 'share_about_live_event', 
                 'get_info_live_event')
#demo_props <- survey_by_prop(svy_unweighted, demos)
#demo_props

#adj_activity_cols = c(how_important, how_often_activity, how_often_v3yrs_activity, app_best_for_list)
#adj_activity_props <- survey_by_prop(svy_unweighted, response_cols= adj_activity_cols, groupby_cols = list(), keep_na=FALSE)

how_important_adj_list <- get_complex_cols(svy_unweighted, "how_important_")
how_often_activity_list <- get_complex_cols(svy_unweighted, "how_often_online_")
how_often_v3yrs_activity_list <- get_complex_cols(svy_unweighted, "how_often_vs_three_years_ago_")
app_best_for_list <- get_complex_cols(svy_unweighted, "app_best_for_")
offline_event_list <- list("offline_started_relationship",
                           "offline_ended_relationship",
                           "offline_got_married",
                           "offline_had_child",
                           "offline_employment_change",
                           "offline_moved_same_city",
                           "offline_moved_new_city",
                           "offline_lost_someone",
                           "offline_finished_school",   
                           "offline_prefer_not")
# Something is off with ethnicity right now
#ethnicity_list <- get_complex_cols(svy_unweighted, "ethnicity_")
privacy_list <- get_complex_cols(svy_unweighted, "privacy_")
non_app_questions <- c(demo_list, 
                       how_important_adj_list, 
                       how_often_activity_list, 
                       how_often_v3yrs_activity_list, 
                       app_best_for_list, 
                       offline_event_list,
                       #ethnicity_list, 
                       privacy_list)
props_by_non_app_qs <- survey_by_prop(svy_unweighted, non_app_questions)
write_csv(props_by_non_app_qs, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/props_by_non_app_qs.csv")

props_by_non_app_fb_cohort <- survey_by_prop(svy_unweighted, non_app_questions, list('cohort_facebook'))
write_csv(props_by_non_app_fb_cohort, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/props_by_non_app_fb_cohort.csv")

privacy_by_country <- survey_by_prop(svy_unweighted, privacy_list, list('country'))
write_csv(privacy_by_country, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/privacy_by_country.csv")

#svy_unweighted$variables %>% 
#  select(new_friend) %>% 
#  summarise_each(funs(sum(is.na(.)))) 
#svy_unweighted$variables %>% 
#  select(new_friend) %>%
#  filter(!is.na(add_new_friend_on_none)) %>% 
#  summarise_each(funs(sum)) 

###################################
## Where would you add a friend? ##
###################################
new_friend <- colnames(svy_unweighted$variables[grep("add", names(svy_unweighted$variables))])
asked_new_friend <- svy_unweighted %>% 
  filter(!is.na(add_new_friend_on_none)) 
add_new_friend_by_app <- survey_by_prop(asked_new_friend, new_friend, list('country')) %>% 
  filter(value==1)
write_csv(add_new_friend_by_app, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/add_new_friend_by_app.csv")
# Where add friend by age bucket (US only)
asked_new_friend_age_us <- svy_unweighted %>% 
  filter(!is.na(add_new_friend_on_none) & (country=='US')) 
add_new_friend_by_app_age_us <- survey_by_prop(asked_new_friend_age_us, new_friend, list('age_bucket')) %>% 
  filter(value==1)
write_csv(add_new_friend_by_app_age_us, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/add_new_friend_by_app_age_us.csv")
# Where add friend by reg cohort (US only)
asked_new_friend_cohort_us <- svy_unweighted %>% 
  filter(!is.na(add_new_friend_on_none) & (country=='US')) 
add_new_friend_by_app_cohort_us <- survey_by_prop(asked_new_friend_cohort_us , new_friend, list('cohort_facebook')) %>% 
  filter(value==1)
write_csv(add_new_friend_by_app_cohort_us, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/add_new_friend_by_app_cohort_us.csv")
# By country, churned only
asked_new_friend_churned <- svy_unweighted %>% 
  filter(!is.na(add_new_friend_on_none) & (churned_facebook == 1)) 
add_new_friend_by_app_churned <- survey_by_prop(asked_new_friend_churned, new_friend, list('country')) %>% 
  filter(value==1)
write_csv(add_new_friend_by_app_churned, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/add_new_friend_by_app_churned.csv")

####################################
## Main Reason for Using Facebook ##
####################################
main_reason_facebook_list <- colnames(svy_unweighted$variables[grep("main_reason_facebook", names(svy_unweighted$variables))])
main_reason_facebook_list
main_reason_facebook <- svy_unweighted %>% 
  filter(!is.na(main_reason_facebook_sharing_about_daily_life)) 
main_reason_facebook_by_country <- survey_by_prop(main_reason_facebook, main_reason_facebook_list, list('country')) %>% 
  filter(value==1)
write_csv(main_reason_facebook_by_country, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/main_reason_facebook_by_country.csv")

# Main reason by reg cohort (US only)
main_reason_facebook_us <- svy_unweighted %>% 
  filter(!is.na(main_reason_facebook_sharing_about_daily_life) & (country=='US')) 
main_reason_cohort_us <- survey_by_prop(main_reason_facebook_us, main_reason_facebook_list, list('cohort_facebook')) %>% 
  filter(value==1)
write_csv(main_reason_cohort_us, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/main_reason_cohort_us.csv")


# time_on_sns_vs_three_years_ago split by churned
time_vs_3_churned <- survey_by_prop(svy_unweighted, list('time_on_sns_vs_three_years_ago'), list('churned_facebook','country')) 
write_csv(time_vs_3_churned, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/time_vs_3_churned.csv")

# What apps are churned vs non-churned users using?
app_used_by_churned <- props_by_question_app(churned_fb, app_list, list('app_usage_l4w'),  list('country'))
app_used_by_churned
non_churned_fb <- svy_unweighted %>% 
  filter(churned_facebook == 0) 
app_used_by_non_churned <- props_by_question_app(non_churned_fb, app_list, list('app_usage_l4w'),  list('country'))
app_used_by_non_churned
app_used_by_churned$churned <- 1
app_used_by_non_churned$churned <- 0
app_usage_by_fb_status <- rbind(app_used_by_churned,app_used_by_non_churned)
write_csv(app_usage_by_fb_status, 
          "/home/nirlevy/projects/teen_off_platform/csv_files/app_usage_by_fb_status.csv")

# End of code from Andy

# Nir - code #

# Saving backup file of the data
save.image("~/projects/teen_off_platform/backup_workspaces/workspace_28.10.2019.RData")
save.image("~/projects/teen_off_platform/workspace_28.10.2019.RData")

# Exploring the data
dim(svy_unweighted)
ls()
class(svy_unweighted)
head(svy_unweighted, n=1)
names(svy_unweighted)

# ggplot code from Andy
ggplot(aes(x = reg_year_bucket, y = proportion, fill=value)) +
  geom_bar(stat = "identity", position=position_dodge(1)) +
  xlab("Reg Year") +
  ylab("% by Answer Choice") +
  labs(title = i) +
  scale_fill_tableau(palette ="Color Blind") +
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), labels=percent) + 
  theme_classic()

# exploring Andy's tables
# exploring Andy's tables
# exploring add_new_friend_by_app
# exploring the whole dataset
class (add_new_friend_by_app)
dim (add_new_friend_by_app)
head(add_new_friend_by_app)

# exploring the country variable within the add_new_friend_by_app dataset 
class(add_new_friend_by_app$country)
levels(add_new_friend_by_app$country)
length(levels(add_new_friend_by_app$country))
head(add_new_friend_by_app$country)

# exploring the proportion variable within the add_new_friend_by_app dataset 
class(add_new_friend_by_app$proportion)
head(add_new_friend_by_app$proportion)
range(add_new_friend_by_app$proportion)

# exploring the question variable within the add_new_friend_by_app dataset 
class(add_new_friend_by_app$question)
levels(add_new_friend_by_app$question)
length(levels(add_new_friend_by_app$question))
head(add_new_friend_by_app$question)

# Question F6
# F6: If you met a new friend in real life that you wanted to connect with online, 
# which social network sites would you be most likely to connect on?

# creating a variable for the number of observations
add_new_friend_by_app <- add_new_friend_by_app %>%
  mutate(n_total=round(n_responses/proportion))

# making sure that the variable was created correctly
add_new_friend_by_app$n_total

# creating a label for the number of observations
add_new_friend_by_app <- add_new_friend_by_app %>%
  mutate(n_label=paste("n=",n_total, sep=""))

# making sure that the variable was created correctly
add_new_friend_by_app$n_label

##################################################################################################
######## Section 1 - Connecting with new friends, by country #####################################
##################################################################################################

# creating a variable for the number of observations

# Creating a variable with the number of observations per country
add_new_friend_by_app <- add_new_friend_by_app %>% 
  mutate(n_country=round((n_responses/proportion),0)) 

# Creating a variable with the number of teens who would connect with a new friend on at least one social network
add_new_friend_by_app$at_least_one[add_new_friend_by_app$question=="add_new_friend_on_none"]<-round(1-add_new_friend_by_app$proportion[add_new_friend_by_app$question=="add_new_friend_on_none"],2)

# Creating a label variable
unique(add_new_friend_by_app$question)
add_new_friend_by_app$question_label<-factor(add_new_friend_by_app$question,
                                                       levels=unique(add_new_friend_by_app$question),
                                                       labels=c("Facebook",
                                                                "Instagram",
                                                                "Twitter",
                                                                "Snapchat",
                                                                "Messenger",
                                                                "Whatsapp",
                                                                "iMessage",
                                                                "Youtube",
                                                                "Linkedin",
                                                                "Tiktok",
                                                                "Pinterest",
                                                                "Other",
                                                                "None"))

# view(add_new_friend_by_app)


# Plotting the percentage who would reported they would not connect with a new friend on any social network, by country
https://stackoverflow.com/questions/22305023/how-to-get-a-barplot-with-several-variables-side-by-side-grouped-by-a-factor

# Plotting the top 5 apps in the US
png("F6_add_new_friend_by_app_US.png", width = 600, height = 500)
add_new_friend_by_app %>%
  filter(country=="US") %>%
  mutate(rank_prop=rank(-proportion)) %>%
  filter(rank_prop<=5) %>%
  ggplot(aes(x = reorder(question_label , -proportion), y = proportion)) + # setting x and y
  geom_bar(stat = "identity", fill="#054C70", position=position_dodge(1)) + # creating a bar plot
  xlab("") + #removing x label 
  ylab("") + #removing y label
  labs(title = "Top 5 apps for connecting with new friends in the US")+ # chart title
  scale_y_continuous(limits=c(0,1.1), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=1.2, size=5)+ # creating labels above bars with each bar's value
  geom_errorbar(aes(x=question_label,ymin=proportion_low,ymax=proportion_upp),
                width=0.07, colour="black", alpha=0.5, size=0.5)+ # creating error bars
  annotate("text", x = 3, y = 0.75, label = paste("n=", max(add_new_friend_by_app$n_country*(add_new_friend_by_app$country=="US")), sep=""), size=6)+
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1 
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-30, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
        legend.position = "none") # removing legend
dev.off()

# Plotting the top 5 apps in Canada

# view(add_new_friend_by_app)
png("F6_add_new_friend_by_app_Canada.png", width = 600, height = 500)
add_new_friend_by_app %>%
  filter(country=="Canada") %>%
  mutate(rank_prop=rank(-proportion)) %>%
  filter(rank_prop<=5) %>%
  ggplot(aes(x = reorder(question_label , -proportion), y = proportion)) + # setting x and y
  geom_bar(stat = "identity", fill="#054C70", position=position_dodge(1)) + # creating a bar plot
  xlab("") + #removing x label 
  ylab("") + #removing y label
  labs(title = "Top 5 apps for connecting with new friends in Canada")+ # chart title
  scale_y_continuous(limits=c(0,1.1), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=1.2, size=5)+ # creating labels above bars with each bar's value
  geom_errorbar(aes(x=question_label,ymin=proportion_low,ymax=proportion_upp),
                width=0.07, colour="black", alpha=0.5, size=0.5)+ # creating error bars
  annotate("text", x = 3, y = 0.65, label = paste("n=", max(add_new_friend_by_app$n_country*(add_new_friend_by_app$country=="Canada")), sep=""), size=6)+
    theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1 
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-40, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
        legend.position = "none") # removing legend
dev.off()

# Plotting the top 5 apps in Germany

# view(add_new_friend_by_app)
png("F6_add_new_friend_by_app_Germany.png", width = 600, height = 500)
add_new_friend_by_app %>%
  filter(country=="Germany") %>%
  mutate(rank_prop=rank(-proportion)) %>%
  filter(rank_prop<=5) %>%
  ggplot(aes(x = reorder(question_label , -proportion), y = proportion)) + # setting x and y
  geom_bar(stat = "identity", fill="#054C70", position=position_dodge(1)) + # creating a bar plot
  xlab("") + #removing x label 
  ylab("") + #removing y label
  labs(title = "Top 5 apps for connecting with new friends in Germany")+ # chart title
  scale_y_continuous(limits=c(0,1.1), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=1.2, size=5)+ # creating labels above bars with each bar's value
  geom_errorbar(aes(x=question_label,ymin=proportion_low,ymax=proportion_upp),
                width=0.07, colour="black", alpha=0.5, size=0.5)+ # creating error bars
  annotate("text", x = 3, y = 1, label = paste("n=", max(add_new_friend_by_app$n_country*(add_new_friend_by_app$country=="Germany")), sep=""), size=6)+
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1 
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-6, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
        legend.position = "none") # removing legend
dev.off()

# Plotting the top 5 apps in Spain

# view(add_new_friend_by_app)
png("F6_add_new_friend_by_app_Spain.png", width = 600, height = 500)
add_new_friend_by_app %>%
  filter(country=="Spain") %>%
  mutate(rank_prop=rank(-proportion)) %>%
  filter(rank_prop<=5) %>%
  ggplot(aes(x = reorder(question_label , -proportion), y = proportion)) + # setting x and y
  geom_bar(stat = "identity", fill="#054C70", position=position_dodge(1)) + # creating a bar plot
  xlab("") + #removing x label 
  ylab("") + #removing y label
  labs(title = "Top 5 apps for connecting with new friends in Spain")+ # chart title
  scale_y_continuous(limits=c(0,1.1), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=1.2, size=5)+ # creating labels above bars with each bar's value
  geom_errorbar(aes(x=question_label,ymin=proportion_low,ymax=proportion_upp),
                width=0.07, colour="black", alpha=0.5, size=0.5)+ # creating error bars
  annotate("text", x = 3, y = 0.85, label = paste("n=", max(add_new_friend_by_app$n_country*(add_new_friend_by_app$country=="Spain")), sep=""), size=6)+
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1 
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-20, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing x axis
        legend.position = "none") # removing legend
dev.off()




##############################################################################################
######## Section 2 – Reasons for using Facebook  #############################################
##############################################################################################

# Creating a variable with the number of observations per country
main_reason_facebook_by_country <- main_reason_facebook_by_country %>% 
  mutate(n_country=round((n_responses/proportion),0)) %>%
  mutate(n_country_canada=(country=="Canada")*n_country) %>%
  mutate(n_country_germany=(country=="Germany")*n_country) %>%
  mutate(n_country_spain=(country=="Spain")*n_country) %>%
  mutate(n_country_us=(country=="US")*n_country)

# view(main_reason_facebook_by_country)

# Creating a label variable
unique(main_reason_facebook_by_country$question)
main_reason_facebook_by_country$question_label<-factor(main_reason_facebook_by_country$question,
                                                             levels=unique(main_reason_facebook_by_country$question),
                                                             labels=c("Sharing about daily life",
                                                                      "Sharing about major life events",
                                                                      "Sharing photos",
                                                                      "Sharing videos",
                                                                      "Sending private messages",
                                                                      "Connecting with Family and close friends",
                                                                      "Connecting with more distant friends and acquaintances",
                                                                      "Looking and photos",
                                                                      "Watching videos",
                                                                      "Meeting new people",
                                                                      "Connecting to your passions and interests",
                                                                      "Getting news",
                                                                      "Selling goods",
                                                                      "Promoting / running / coordinating a business",
                                                                      "Making professional connections",
                                                                      "Planning / coordinating events",
                                                                      "Playing games",
                                                                      "Passing time",
                                                                      "Debating",
                                                                      "Promoting a cause / Fundraising",
                                                                      "Viewing memes / humor"))

# Creating a variable for the order of the question labels in the plot
main_reason_facebook_by_country$question_label_order<-NA
main_reason_facebook_by_country$question_label_order[main_reason_facebook_by_country$question_label=="Connecting with Family and close friends"]<-1
main_reason_facebook_by_country$question_label_order[main_reason_facebook_by_country$question_label=="Passing time"]<-2
main_reason_facebook_by_country$question_label_order[main_reason_facebook_by_country$question_label=="Looking and photos"]<-3
main_reason_facebook_by_country$question_label_order[main_reason_facebook_by_country$question_label=="Sharing photos"]<-4
main_reason_facebook_by_country$question_label_order[main_reason_facebook_by_country$question_label=="Connecting with more distant friends and acquaintances"]<-5

# Making sure that the variable was created properly
#main_reason_facebook_by_country$question_label
#view(main_reason_facebook_by_country)
unique(main_reason_facebook_by_country$n_country)

# Plotting the main reasons for connecting to Facebook in the US
png("E2_main_reason_US.png", width = 900, height = 500)
main_reason_facebook_by_country %>%
  filter(country=="US") %>%
  filter(rank(-proportion)<=5) %>%
  ggplot(aes(x = reorder(question_label , question_label_order), y = proportion, fill=reorder(question_label, question_label_order))) +
  geom_bar(position="dodge", stat = "identity") + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Main reasons for using Facebook in the US")+ # chart title
  labs(fill = "")+ # legend title
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), 
            vjust=-0.5, hjust=1.2, size=5, position=position_dodge(0.75))+ # creating labels above bars with their value
  # geom_label(aes(y=.2, label=n_label), fill = "snow2", 
  #                 label.padding = unit(0.4, "lines"), vjust=5.4, alpha=0.9, position=position_dodge(0.92))+ # creating boxes with numbers of observations
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp), 
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.75)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") +
  annotate("text", x = 3, y = 0.75, 
           label = paste("n=", max(main_reason_facebook_by_country$n_country_us), sep=""), size=6) + # creating a text box with the number of observations
  theme(axis.text.x = element_blank(), # removing x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-10, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()


# Plotting the main reasons for connecting to Facebook in Canada
png("E2_main_reason_Canada.png", width = 900, height = 500)
main_reason_facebook_by_country %>%
  filter(country=="Canada") %>%
  filter(rank(-proportion)<=5) %>%
  ggplot(aes(x = reorder(question_label , question_label_order), y = proportion, fill=reorder(question_label, question_label_order))) +
  geom_bar(position="dodge", stat = "identity") + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Main reasons for using Facebook in Canada")+ # chart title
  labs(fill = "")+ # legend title
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=1.2, size=5, position=position_dodge(0.75))+ # creating labels above bars with their value
  # geom_label(aes(y=.2, label=n_label), fill = "snow2", label.padding = unit(0.4, "lines"), vjust=5.4, alpha=0.9, position=position_dodge(0.92))+ # creating boxes with numbers of observations
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp), 
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.75)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") +
  annotate("text", x = 3, y = 0.75, label = paste("n=", max(main_reason_facebook_by_country$n_country_canada), sep=""), size=6) + # creating a text box with the number of observations
  theme(axis.text.x = element_blank(), # removing x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-10, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()


# Plotting the main reasons for connecting to Facebook in Germany
png("E2_main_reason_Germany.png", width = 900, height = 500)
main_reason_facebook_by_country %>%
  filter(country=="Germany") %>%
  filter(rank(-proportion)<=5) %>%
  ggplot(aes(x = reorder(question_label , question_label_order), y = proportion, fill=reorder(question_label, question_label_order))) +
  geom_bar(position="dodge", stat = "identity") + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Main reasons for using Facebook in Germany")+ # chart title
  labs(fill = "")+ # legend title
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=1.2, size=5, position=position_dodge(0.75))+ # creating labels above bars with their value
  # geom_label(aes(y=.2, label=n_label), fill = "snow2", label.padding = unit(0.4, "lines"), vjust=5.4, alpha=0.9, position=position_dodge(0.92))+ # creating boxes with numbers of observations
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp), 
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.75)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") +
  annotate("text", x = 3, y = 0.75, label = paste("n=", max(main_reason_facebook_by_country$n_country_germany), sep=""), size=6) + # creating a text box with the number of observations
  theme(axis.text.x = element_blank(), # removing x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-10, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()

# Plotting the main reasons for connecting to Facebook in Spain
png("E2_main_reason_Spain.png", width = 900, height = 500)
main_reason_facebook_by_country %>%
  filter(country=="Spain") %>%
  filter(rank(-proportion)<=5) %>%
  ggplot(aes(x = reorder(question_label , question_label_order), y = proportion, fill=reorder(question_label, question_label_order))) +
  geom_bar(position="dodge", stat = "identity") + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Main reasons for using Facebook in Spain")+ # chart title
  labs(fill = "")+ # legend title
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-0.5, hjust=1.2, size=5, position=position_dodge(0.75))+ # creating labels above bars with their value
  # geom_label(aes(y=.2, label=n_label), fill = "snow2", label.padding = unit(0.4, "lines"), vjust=5.4, alpha=0.9, position=position_dodge(0.92))+ # creating boxes with numbers of observations
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp), 
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.75)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") +
  annotate("text", x = 3, y = 0.75, label = paste("n=", max(main_reason_facebook_by_country$n_country_spain), sep=""), size=6) + # creating a text box with the number of observations
  theme(axis.text.x = element_blank(), # removing x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-10, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()


##############################################################################################
######## Section 3 – privacy concerns  #######################################################
##############################################################################################

# Creating a label variable
unique(privacy_by_country$question)
question_label<-c("Unauthorized \naccount access",
                                "Having your info \nused for ads",
                                "Who can see \nwhat you post",
                                "Unauthorized \naccount access",
                                "Having your info \nused for ads",
                                "Who can see \nwhat you post")

privacy_by_country$question_label<-factor(privacy_by_country$question,
                                                       levels=unique(privacy_by_country$question),
                                                       labels=c("Who can see \nwhat you post",
                                                                "Having your info \nused for ads",
                                                                "Unauthorized \naccount access",
                                                                "Who can see \nwhat you post",
                                                                "Having your info \nused for ads",
                                                                "Unauthorized \naccount access"))

privacy_by_country$value_order<-0
privacy_by_country$value_order[privacy_by_country$value=="Extremely concerned"]<-1
privacy_by_country$value_order[privacy_by_country$value=="Very concerned"]<-2
privacy_by_country$value_order[privacy_by_country$value=="Somewhat concerned"]<-3
privacy_by_country$value_order[privacy_by_country$value=="A little concerned"]<-4
privacy_by_country$value_order[privacy_by_country$value=="Not at all concerned"]<-5

privacy_by_country$present<-0
privacy_by_country$present[privacy_by_country$question=="privacy_who_can_see_posts"]<-1
privacy_by_country$present[privacy_by_country$question=="privacy_info_for_ads"]<-1
privacy_by_country$present[privacy_by_country$question=="privacy_account_access"]<-1

privacy_by_country$question_order[privacy_by_country$question=="privacy_who_can_see_posts"]<-1
privacy_by_country$question_order[privacy_by_country$question=="privacy_info_for_ads"]<-2
privacy_by_country$question_order[privacy_by_country$question=="privacy_account_access"]<-3

# Making sure that the variable was created properly
view(privacy_by_country)

# Creating a variable for the number of observations per country
privacy_by_country <- privacy_by_country %>% 
  mutate(n_country=round((n_responses/proportion),0)) %>%
  mutate(n_country_canada=(country=="Canada")*n_country) %>%
  mutate(n_country_germany=(country=="Germany")*n_country) %>%
  mutate(n_country_spain=(country=="Spain")*n_country) %>%
  mutate(n_country_us=(country=="US")*n_country)

# Making sure that the variable was created properly
view(privacy_by_country)

# Plotting privacy concerns in all countries

# Plotting privacy concerns in the US
png("JJ1_privacy_concerns_us.png", width = 900, height = 500)
privacy_by_country %>%
  filter(country=="US",
    present==1) %>%
  ggplot(aes(x = reorder(question_label, -question_order), y = proportion, fill=reorder(value, value_order))) + # setting the basic aesthetics
  geom_bar(position="dodge", stat = "identity", width=0.7) + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Privacy concerns of teens in the US") + # chart title
  labs(fill = "Privacy Issues") + # legend title 
  scale_x_discrete(labels=question_label) + # scaling the x axis 
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), # scaling y axis 1
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis 2
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), # creating labels above bars with the value of each bar 1
            vjust=-3.0, hjust=0.25, size=3.5, position=position_dodge(0.7))+ # creating labels above bars with the value of each bar 2
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp),  
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.7)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") + # setting the colors of the bars
  annotate("text", x = 2, y = 0.7, label = paste("n=", max(privacy_by_country$n_country_us), sep=""), # creating a text box with the number of observations 1
           size=6) + # creating a text box with the number of observations 2
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-20, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()

# Plotting privacy concerns in Canada
png("JJ1_privacy_concerns_canada.png", width = 900, height = 500)
privacy_by_country %>%
  filter(country=="Canada",
         present==1) %>%
  ggplot(aes(x = reorder(question_label, -question_order), y = proportion, fill=reorder(value, value_order))) + # setting the basic aesthetics
  geom_bar(position="dodge", stat = "identity", width=0.7) + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Privacy concerns of teens in Canada") + # chart title
  labs(fill = "Privacy Issues") + # legend title 
  scale_x_discrete(labels=question_label) + # scaling the x axis 
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), # scaling y axis 1
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis 2
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), # creating labels above bars with the value of each bar 1
            vjust=-3.1, hjust=0.25, size=3.5, position=position_dodge(0.7))+ # creating labels above bars with the value of each bar 2
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp),  
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.7)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") + # setting the colors of the bars
  annotate("text", x = 2, y = 0.7, label = paste("n=", max(privacy_by_country$n_country_canada), sep=""), # creating a text box with the number of observations 1
           size=6) + # creating a text box with the number of observations 2
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-20, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()

# Plotting privacy concerns in Germany
png("JJ1_privacy_concerns_germany.png", width = 900, height = 500)
privacy_by_country %>%
  filter(country=="Germany",
         present==1) %>%
  ggplot(aes(x = reorder(question_label, -question_order), y = proportion, fill=reorder(value, value_order))) + # setting the basic aesthetics
  geom_bar(position="dodge", stat = "identity", width=0.7) + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Privacy concerns of teens in Germany") + # chart title
  labs(fill = "Privacy Issues") + # legend title 
  scale_x_discrete(labels=question_label) + # scaling the x axis 
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), # scaling y axis 1
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis 2
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), # creating labels above bars with the value of each bar 1
            vjust=-3.1, hjust=0.25, size=3.5, position=position_dodge(0.7))+ # creating labels above bars with the value of each bar 2
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp),  
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.7)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") + # setting the colors of the bars
  annotate("text", x = 2, y = 0.7, label = paste("n=", max(privacy_by_country$n_country_germany), sep=""), # creating a text box with the number of observations 1
           size=6) + # creating a text box with the number of observations 2
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-20, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()

# Plotting privacy concerns in Spain
png("JJ1_privacy_concerns_spain.png", width = 900, height = 500)
privacy_by_country %>%
  filter(country=="Spain",
         present==1) %>%
  ggplot(aes(x = reorder(question_label, -question_order), y = proportion, fill=reorder(value, value_order))) + # setting the basic aesthetics
  geom_bar(position="dodge", stat = "identity", width=0.7) + # creating bar plot
  xlab("") + #removing label of x axis
  ylab("") + #removing label of y axis
  labs(title = "Privacy concerns of teens in Spain") + # chart title
  labs(fill = "Privacy Issues") + # legend title 
  scale_x_discrete(labels=question_label) + # scaling the x axis 
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), # scaling y axis 1
                     labels=scales::percent_format(accuracy=1)) + # scaling y axis 2
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), # creating labels above bars with the value of each bar 1
            vjust=-3.1, hjust=0.25, size=3.5, position=position_dodge(0.7))+ # creating labels above bars with the value of each bar 2
  geom_errorbar(aes(ymin=proportion_low,ymax=proportion_upp),  
                width=0.07, colour="black", alpha=0.5, size=0.5, position=position_dodge(0.7)) + # creating error bars
  scale_fill_tableau(palette ="Color Blind") + # setting the colors of the bars
  annotate("text", x = 2, y = 0.7, label = paste("n=", max(privacy_by_country$n_country_spain), sep=""), # creating a text box with the number of observations 1
           size=6) + # creating a text box with the number of observations 2
  theme(axis.text.x = element_text(angle = 0, vjust=1, hjust=0.5, size=15), # creating x axis lables
        axis.text.y = element_blank(), # removing y axis labels
        axis.ticks.x=element_blank(), # removing x axis ticks
        axis.ticks.y=element_blank(), # removing y axis ticks
        panel.background = element_rect(fill = "transparent", colour = NA), # creating blank background 1
        plot.background = element_rect(fill = "transparent", colour = NA), # creating blank background 2
        plot.title = element_text(hjust = 0.5, vjust=-20, size=20), # designing plot title
        axis.line.x = element_line(color="gray69", size = 0.5), # designing the x axis
        legend.title = element_text(size = 15), # setting the size of the legend title
        legend.text = element_text(size=15)) # setting the size of the legend text
dev.off()

unique(how_important)
