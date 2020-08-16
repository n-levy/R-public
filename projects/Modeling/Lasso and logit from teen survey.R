# Analyzing teen data

### Code from Andy ###

# Installing packages
fbr::with_proxy(install.packages("srvyr"))
fbr::with_proxy(install.packages("prediction"))
fbr::with_proxy(install.packages("stringi"))
fbr::with_proxy(install.packages("ggplot2"))
fbr::with_proxy(install.packages("ggthemes"))
fbr::with_proxy(install.packages("leaps"))
fbr::with_proxy(install.packages("bestglm"))
fbr::with_proxy(install.packages("effects"))


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
library(plyr)
library(corrplot)
library(gridExtra)
library(MASS)
library(leaps)
library(bestglm)
library(glmnet)
library(caret)
library(effects)
library(sjPlot)

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
save.image("~/projects/teen_off_platform/backup_workspaces/workspace_31.10.2019.RData")
save.image("~/projects/teen_off_platform/workspace_31.10.2019.RData")

# Exploring the data
dim(svy_unweighted)
ls()
class(svy_unweighted)
head(svy_unweighted, n=1)
names(svy_unweighted)

######################################## Modeling teen retention ########################################################

# Dependent variable: churned_facebook 

# Andy - key variables from the tenured cohorts analysis:
# 1. num_notifications_facebook
# 2. negative_positive_facebook
# 3. changed_overall_facebook (probably less relevant here)
# 4. how_often_unfriend_facebook
# 5. saw_amount_{connection_type}_facebook

# Exploring the dependent variable
svy_unweighted$variables %>%
  count(churned_facebook)

view(svy_unweighted$variables[1:2,])
svy_unweighted$variables$churned_facebook

churned_facebook<-svy_unweighted$variables$churned_facebook
table(churned_facebook)

# Andy's R code for retention analysis - converted to the teen dataset

get_complex_cols <- function(svy, col_stem) {
  list <- colnames(svy$variables[grep(col_stem, names(svy$variables))])
  list <- grep("ordered", list, value=TRUE, invert=TRUE)
  return(trim_apps(list))
}

# Create a used last 4 week variable for other apps that includes the NAs as 0
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_instagram =  case_when(
    is.na(app_usage_l4w_instagram) ~ 0,
    app_usage_l4w_instagram == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_twitter =  case_when(
    is.na(app_usage_l4w_twitter) ~ 0,
    app_usage_l4w_twitter == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_snapchat =  case_when(
    is.na(app_usage_l4w_snapchat) ~ 0,
    app_usage_l4w_snapchat == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_messenger =  case_when(
    is.na(app_usage_l4w_messenger) ~ 0,
    app_usage_l4w_messenger == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_whatsapp =  case_when(
    is.na(app_usage_l4w_whatsapp) ~ 0,
    app_usage_l4w_whatsapp == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_imessage =  case_when(
    is.na(app_usage_l4w_imessage) ~ 0,
    app_usage_l4w_imessage == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_youtube =  case_when(
    is.na(app_usage_l4w_youtube) ~ 0,
    app_usage_l4w_youtube == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_linkedin =  case_when(
    is.na(app_usage_l4w_linkedin) ~ 0,
    app_usage_l4w_linkedin == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_tiktok =  case_when(
    is.na(app_usage_l4w_tiktok) ~ 0,
    app_usage_l4w_tiktok == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))
svy_unweighted  <- svy_unweighted   %>% 
  mutate(app_usage_l4w_no_na_pinterest =  case_when(
    is.na(app_usage_l4w_pinterest) ~ 0,
    app_usage_l4w_pinterest == "Didn't use in the last four weeks" ~ 0,
    TRUE ~ 1))


how_often_col <- get_complex_cols(svy_unweighted, "how_often_online_")
for(i in how_often_col){
  temp = str_replace(paste(sym(i), "_often", sep=''), "how_often_online_", "")
  print(temp)
  svy_unweighted$variables[[temp]] = ifelse(svy_unweighted$variables[[i]] == 'Often', 1, 0)
}


churn_formula <- "churned_facebook ~ gender + age_bucket + marital_status + 
children_in_hh + urban_suburban_rural + education_level +
political_views*political_bias_fb_recode +
num_notifications_facebook +  negative_positive_facebook +
saw_amount_close_friends_facebook +
saw_amount_acquaintances_facebook + saw_amount_family_members_facebook +
saw_amount_suggested_friends_facebook + saw_amount_politics_facebook +
main_usage_method_facebook + how_often_unfriend_facebook +
privacy_pca + 
changed_more_difficult_facebook +
app_usage_l4w_no_na_instagram + app_usage_l4w_no_na_twitter + app_usage_l4w_no_na_snapchat +   
app_usage_l4w_no_na_whatsapp + app_usage_l4w_no_na_imessage + app_usage_l4w_no_na_youtube + app_usage_l4w_no_na_linkedin + 
app_usage_l4w_no_na_tiktok + app_usage_l4w_no_na_pinterest +
very_bad_exp_binary  + time_on_sns_vs_three_years_ago + 
sharing_about_daily_life_often + sharing_about_major_life_events_often +
sending_private_messages_often  + 
connecting_with_family_and_close_friends_often  + 
connecting_with_more_distant_friends_and_acquaintances_often + 
looking_at_photos_often + 
watching_videos_often + 
meeting_new_people_often + 
connecting_to_your_passions_and_interests_often + 
getting_news_often + 
buying_selling_goods_often + 
promoting_running_coordinating_a_business_often + 
planning_coordinating_events_often + 
playing_games_often + 
debating_often + 
promoting_a_cause_fundraising_often + 
viewing_memes_humor_often"
churn_unweighted_logit <- glm(churn_formula, data=svy_unweighted$variables,family = "binomial")
summary(churn_unweighted_logit)
nobs(churn_unweighted_logit)

# end of Andy's code

# Nir's code
# Exploring the whole dataset
class(svy_unweighted)
class(svy_unweighted$variables)
view(svy_unweighted)
dim(svy_unweighted$variables)

############ Identifying and recoding the variables ###############
##### Gender #####
table(svy_unweighted$variables$gender)

# recoding into a binary variable
female<-NA
female[svy_unweighted$variables$gender=="Female"]<-1
female[svy_unweighted$variables$gender=="Male"]<-0
table(female)

# plotting the correlations
female_and_churned<-cbind(female, churned_facebook)
cor_female_and_churned<-cor(female_and_churned, use="pairwise")

view(female_and_churned)
png("corrplot_female_and_churned.png", width = 1500, height = 500)
corrplot(cor_female_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_female_and_churned)
round(sort(cor_female_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_female_and_churned)[1],2)

##### Ethnicity #####
# collecting the variables
ethnicity_vars<-svy_unweighted$variables[,grep("ethnicity", names(svy_unweighted$variables), value=TRUE)]
dim(ethnicity_vars)
names(ethnicity_vars)
# converting to numeric dataframe
ethnicity_vars<-mutate_all(ethnicity_vars, function(x) as.numeric(as.character(x)))
# removing variables with no 1s
ethnicity_vars<-ethnicity_vars[, colSums(ethnicity_vars, na.rm=TRUE) != 0]
# checking
round(cor(ethnicity_vars,use="pairwise"),2)

# getting variable names
ethnicity_varnames<-names(ethnicity_vars)
view(ethnicity_varnames)

# plotting the correlations
ethnicity_and_churned<-cbind(ethnicity_vars, churned_facebook)
cor_ethnicity_and_churned<-cor(ethnicity_and_churned, use="pairwise")

png("corrplot_ethnicity_and_churned.png", width = 1500, height = 500)
corrplot(cor_ethnicity_and_churned, method = "circle")
dev.off()

ethnicity_varnames<-names(ethnicity_vars)
ethnicity_varnames

# finding maximum 
n <- length(cor_ethnicity_and_churned)
round(sort(cor_ethnicity_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_ethnicity_and_churned)[1],2)

# finding the ethnicity variables with the highest
# and lowest correlation with churned  
class(cor_ethnicity_and_churned)
cor_ethnicity_and_churned.df<-as.data.frame(t(cor_ethnicity_and_churned))
cor_ethnicity_and_churned.df[cor_ethnicity_and_churned.df==1]<-NA
min_ethnicity<-sort(cor_ethnicity_and_churned.df$churned_facebook)[1]
min_ethnicity
place_min<-which(cor_ethnicity_and_churned.df$churned_facebook == min_ethnicity)
max_ethnicity_var<-names(cor_ethnicity_and_churned.df)[place_min]

max_ethnicity<-(-1)*sort(-cor_ethnicity_and_churned.df$churned_facebook)[1]
max_ethnicity
place_max<-which(cor_ethnicity_and_churned.df$churned_facebook == max_ethnicity)
min_ethnicity_var<-names(cor_ethnicity_and_churned.df)[place_max]

#### Online activities that are often performed #####
#### Question: How often do you do {activity} online? ###
# The variable denotes those who answered "often". The other options were "rarely" and "never" ###
# Andy: "These are useful, but there is a lot of them. I would probably try a pass with and without them"

# collecting the variables that Andy created
activity_often_vars<-svy_unweighted$variables %>% select(ends_with("_often"))
view(activity_often_vars[,1:10])

# understanding Andy's variables
table(svy_unweighted$variables$how_often_online_sharing_about_daily_life,svy_unweighted$variables$sharing_about_daily_life_often)

# converting to numeric dataframe
activity_often_vars<-mutate_all(activity_often_vars, function(x) as.numeric(as.character(x)))

# removing variables with no 1s
activity_often_vars<-activity_often_vars[, colSums(activity_often_vars, na.rm=TRUE) != 0]

# checking
round(cor(activity_often_vars,use="pairwise"),2)

# getting variable names
activity_often_varnames<-names(activity_often_vars)
view(activity_often_varnames)

# plotting the correlations
activity_often_vars_and_churned<-cbind(activity_often_vars, churned_facebook)
cor_activity_often_vars_and_churned<-cor(activity_often_vars_and_churned, use="pairwise")

png("cor_activity_often_vars_and_churned.png", width = 1500, height = 500)
corrplot(cor_activity_often_vars_and_churned, method = "circle")
dev.off()

# collecting activity variables
how_often_vars<-as.data.frame(svy_unweighted$variables[,grep("how_often_online", names(svy_unweighted$variables), value=TRUE)])
how_often_vars_ordered<-as.data.frame(how_often_vars[,grep("ordered", names(how_often_vars), value=TRUE)])

how_often_varnames_ordered<-names(how_often_vars_ordered)
how_often_varnames_ordered

########## Privacy #######################

# creating a dataset of numeric privacy variables
# collecting all of the privacy variables
privacy_vars<-svy_unweighted$variables[,grep("privacy", names(svy_unweighted$variables), value=TRUE)]

# keeping only those that are not ordered
privacy_vars=select(privacy_vars,-c("privacy_who_can_see_posts_ordered","privacy_info_for_ads_ordered","privacy_account_access_ordered","privacy_who_can_see_posts_vs_three_years_ago_ordered","privacy_info_for_ads_vs_three_years_ago_ordered","privacy_account_access_vs_three_years_ago_ordered"))

names(privacy_vars)
dim(privacy_vars)
view(privacy_vars)

# converting to binary values
# converting the values for the question about concerns in the present
privacy_vars_binary<-privacy_vars
privacy_vars_binary[privacy_vars_binary=="Very concerned"]<-1
privacy_vars_binary[privacy_vars_binary=="Extremely concerned"]<-1
privacy_vars_binary[privacy_vars_binary=="Somewhat concerned"]<-0
privacy_vars_binary[privacy_vars_binary=="A little concerned"]<-0
privacy_vars_binary[privacy_vars_binary=="Not at all concerned"]<-0

privacy_vars_binary[privacy_vars_binary=="Much more concerned"]<-1
privacy_vars_binary[privacy_vars_binary=="Somewhat more concerned"]<-0
privacy_vars_binary[privacy_vars_binary=="About the same level of concerned"]<-0
privacy_vars_binary[privacy_vars_binary=="Somewhat less concerned"]<-0
privacy_vars_binary[privacy_vars_binary=="Much less concerned"]<-0

class(privacy_vars_binary)

# counting the values in the original privacy variables to make sure that the binary variables were created correctly
table(unlist(privacy_vars))
table(unlist(privacy_vars_binary))

# converting to numeric dataframe
privacy_vars_binary<-mutate_all(privacy_vars_binary, function(x) as.numeric(as.character(x)))

# removing variables with no 1s
privacy_vars_binary<-privacy_vars_binary[, colSums(privacy_vars_binary, na.rm=TRUE) != 0]
 
# checking
round(cor(privacy_vars_binary, na.rm=TRUE),2)

# getting variable names
privacy_vars_binary_varnames<-names(privacy_vars_binary)
view(privacy_vars_binary_varnames)

# plotting the correlations
privacy_vars_binary_and_churned<-cbind(privacy_vars_binary, churned_facebook)
cor_aprivacy_vars_binary_and_churned<-cor(privacy_vars_binary_and_churned, use="pairwise")

png("privacy_vars_binary_and_churned.png", width = 1500, height = 500)
corrplot(cor_aprivacy_vars_binary_and_churned, method = "circle")
dev.off()

# ordered variables
privacy_who_can_see_posts_ordered<-svy_unweighted$variables$privacy_who_can_see_posts_ordered
table(privacy_who_can_see_posts_ordered)

privacy_info_for_ads_ordered<-svy_unweighted$variables$privacy_info_for_ads_ordered
table(privacy_info_for_ads_ordered)

privacy_account_access_ordered<-svy_unweighted$variables$privacy_account_access_ordered
table(privacy_account_access_ordered)

privacy_who_can_see_posts_vs_three_years_ago_ordered<-svy_unweighted$variables$privacy_who_can_see_posts_vs_three_years_ago_ordered
table(privacy_who_can_see_posts_vs_three_years_ago_ordered)

privacy_info_for_ads_vs_three_years_ago_ordered<-svy_unweighted$variables$privacy_info_for_ads_vs_three_years_ago_ordered
table(privacy_info_for_ads_vs_three_years_ago_ordered)

privacy_account_access_vs_three_years_ago_ordered<-svy_unweighted$variables$privacy_account_access_vs_three_years_ago_ordered
table(privacy_account_access_vs_three_years_ago_ordered)

########## Urban / Suburban / Rural #######################
### Ordered variable ###
urban_suburban_rural_ordered <- svy_unweighted$variables$urban_suburban_rural
unique(urban_suburban_rural_ordered)
table(urban_suburban_rural_ordered)

# converting to numeric values
urban_suburban_rural_ordered[urban_suburban_rural_ordered=="Urban"]<-1
urban_suburban_rural_ordered[urban_suburban_rural_ordered=="Suburban"]<-2
urban_suburban_rural_ordered[urban_suburban_rural_ordered=="Rural"]<-3
urban_suburban_rural_ordered[urban_suburban_rural_ordered=="Prefer not to say"]<-NA
table(urban_suburban_rural_ordered)

# attaching churned_facebook
urban_suburban_rural_ordered_and_churned<-cbind(urban_suburban_rural_ordered,churned_facebook)
class(urban_suburban_rural_ordered_and_churned)
view(urban_suburban_rural_ordered_and_churned)

# converting to numeric dataframe
urban_suburban_rural_ordered_and_churned<-as.data.table(urban_suburban_rural_ordered_and_churned)
urban_suburban_rural_ordered_and_churned<-mutate_all(urban_suburban_rural_ordered_and_churned, function(x) as.numeric(as.character(x)))
view(urban_suburban_rural_ordered_and_churned)
class(urban_suburban_rural_ordered_and_churned)

# creating the correlation table
cor_urban_suburban_rural_ordered_and_churned<-cor(urban_suburban_rural_ordered_and_churned, use="pairwise")
# view(cor_urban_suburban_rural_ordered_and_churned)

# view(urban_suburban_rural_ordered_and_churned)

png("cor_urban_suburban_rural_ordered_and_churned.png", width = 1500, height = 500)
corrplot(cor_urban_suburban_rural_ordered_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_urban_suburban_rural_ordered_and_churned)
round(sort(cor_urban_suburban_rural_ordered_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_urban_suburban_rural_ordered_and_churned)[1],2)

### Binary variables ###
urban_suburban_rural <- svy_unweighted$variables$urban_suburban_rural
unique(urban_suburban_rural)
view(urban_suburban_rural)
table(urban_suburban_rural)

# converting to binary values
urban<-vector()
urban[urban_suburban_rural=="Urban"]<-1
urban[urban_suburban_rural!="Urban"]<-0
urban[urban_suburban_rural=="Prefer not to say"]<-NA
table(urban)

suburban<-vector()
suburban[urban_suburban_rural=="Suburban"]<-1
suburban[urban_suburban_rural!="Suburban"]<-0
suburban[urban_suburban_rural=="Prefer not to say"]<-NA
table(suburban)

rural<-vector()
rural[urban_suburban_rural=="Rural"]<-1
rural[urban_suburban_rural!="Rural"]<-0
rural[urban_suburban_rural=="Prefer not to say"]<-NA
table(rural)

urban_suburban_rural_vars<-cbind(urban,suburban,rural)
view(urban_suburban_rural_vars)

# checking
round(cor(urban_suburban_rural_vars,use="pairwise"),2)

# plotting the correlations
urban_suburban_rural_vars_and_churned<-cbind(urban_suburban_rural_vars, churned_facebook)
cor_urban_suburban_rural_vars_and_churned<-cor(urban_suburban_rural_vars_and_churned, use="pairwise")

png("urban_suburban_rural_vars_and_churned.png", width = 1500, height = 500)
corrplot(cor_urban_suburban_rural_vars_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_urban_suburban_rural_vars_and_churned)
round(sort(cor_urban_suburban_rural_vars_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_urban_suburban_rural_vars_and_churned)[1],2)

# creating aggregate variables
urban_suburban_rural_vars<-cbind(urban,suburban,rural)
urban_suburban_rural_varnames<-names(urban_suburban_rural)

########## How negative or positive are the things you see on Facebook #######################
negative_positive<-svy_unweighted$variables$negative_positive_facebook
table(negative_positive)

# converting to binary values
neg_pos_all_or_mostly_negative<-vector()
neg_pos_all_or_mostly_negative[negative_positive=="All or mostly negative"]<-1
neg_pos_all_or_mostly_negative[negative_positive!="All or mostly negative"]<-0
table(neg_pos_all_or_mostly_negative)

neg_pos_all_or_mostly_positive<-vector()
neg_pos_all_or_mostly_positive[negative_positive=="All or mostly positive"]<-1
neg_pos_all_or_mostly_positive[negative_positive!="All or mostly positive"]<-0
table(neg_pos_all_or_mostly_positive)

neg_pos_half_positive_negative<-vector()
neg_pos_half_positive_negative[negative_positive=="Half positive, half negative"]<-1
neg_pos_half_positive_negative[negative_positive!="Half positive, half negative"]<-0
table(neg_pos_half_positive_negative)

neg_pos_more_than_half_negative<-vector()
neg_pos_more_than_half_negative[negative_positive=="More than half negative"]<-1
neg_pos_more_than_half_negative[negative_positive!="More than half negative"]<-0
table(neg_pos_more_than_half_negative)

neg_pos_more_than_half_positive<-vector()
neg_pos_more_than_half_positive[negative_positive=="More than half positive"]<-1
neg_pos_more_than_half_positive[negative_positive!="More than half positive"]<-0
table(neg_pos_more_than_half_positive)

# creating matrix with all vars
negative_positive_vars<-cbind(all_or_mostly_negative,all_or_mostly_positive,half_positive_negative,more_than_half_negative,more_than_half_positive)

# checking
round(cor(negative_positive_vars,use="pairwise"),2)

# plotting the correlations
negative_positive_vars_and_churned<-cbind(negative_positive_vars, churned_facebook)
cor_negative_positive_vars_and_churned<-cor(negative_positive_vars_and_churned, use="pairwise")

png("negative_positive_vars_and_churned.png", width = 1500, height = 500)
corrplot(cor_negative_positive_vars_and_churned, method = "circle")
dev.off()

# ordered variable
negative_positive_ordered_facebook<-svy_unweighted$variables$negative_positive_ordered_facebook
table(negative_positive_ordered_facebook)

########## Do you feel you receive too many, too few, or the right number of notifications from [brand]? #######################
num_notifications_facebook<-svy_unweighted$variables$num_notifications_facebook
table(num_notifications_facebook)

# converting to binary values
num_notifications_about_right<-vector()
num_notifications_about_right[num_notifications_facebook=="About the right number of notifications"]<-1
num_notifications_about_right[num_notifications_facebook!="About the right number of notifications"]<-0
table(num_notifications_about_right)

num_notifications_didnt_receive<-vector()
num_notifications_didnt_receive[num_notifications_facebook=="I don't receive notifications"]<-1
num_notifications_didnt_receive[num_notifications_facebook!="I don't receive notifications"]<-0
table(num_notifications_didnt_receive)

num_notifications_too_few<-vector()
num_notifications_too_few[num_notifications_facebook=="Too few notifications"]<-1
num_notifications_too_few[num_notifications_facebook!="Too few notifications"]<-0
table(num_notifications_too_few)

num_notifications_too_many<-vector()
num_notifications_too_many[num_notifications_facebook=="Too many notifications"]<-1
num_notifications_too_many[num_notifications_facebook!="Too many notifications"]<-0
table(num_notifications_too_many)

# creating matrix with all vars
num_notifications_vars<-cbind(num_notifications_about_right,num_notifications_didnt_receive,num_notifications_too_few,num_notifications_too_many)

# checking
round(cor(num_notifications_vars,use="pairwise"),2)

# plotting the correlations
num_notifications_vars_and_churned<-cbind(num_notifications_vars, churned_facebook)
cor_num_notifications_vars_and_churned<-cor(num_notifications_vars_and_churned, use="pairwise")

png("num_notifications_vars_and_churned.png", width = 1500, height = 500)
corrplot(cor_num_notifications_vars_and_churned, method = "circle")
dev.off()

# ordered variable
num_notifications_ordered_facebook<-svy_unweighted$variables$num_notifications_ordered_facebook
table(num_notifications_ordered_facebook)


########## Cares about users #######################
cares_about_users_ordered_facebook<-svy_unweighted$variables$cares_about_users_ordered_facebook
table(cares_about_users_ordered_facebook)

########## Favorability #######################
app_favorability_ordered_facebook<-svy_unweighted$variables$app_favorability_ordered_facebook
table(app_favorability_ordered_facebook)

########## Favorability vs. three years ago#######################
app_favorability_vs_three_years_ago_ordered_facebook<-svy_unweighted$variables$app_favorability_vs_three_years_ago_ordered_facebook
table(app_favorability_vs_three_years_ago_ordered_facebook)

########## Value #######################
value_from_ordered_facebook<-svy_unweighted$variables$value_from_ordered_facebook
table(value_from_ordered_facebook)

########## Share of time spent on Facebook #######################
share_sns_time_ordered_facebook<-svy_unweighted$variables$share_sns_time_ordered_facebook
table(share_sns_time_ordered_facebook)

########## Share of time spent on Facebook vs. three years ago #######################
share_sns_time_three_years_ago_ordered_facebook<-svy_unweighted$variables$share_sns_time_three_years_ago_ordered_facebook
table(share_sns_time_three_years_ago_ordered_facebook)

########## How important is it to you that a social network site is #######################
# collecting the variables
how_important_vars_ordered<-svy_unweighted$variables[,grep("how_important_ordered_", names(svy_unweighted$variables), value=TRUE)]
dim(how_important_vars_ordered)
names(how_important_vars_ordered)
head(how_important_vars_ordered)

how_important_varnames<-names(how_important_vars_ordered)
view(how_important_varnames)

########## Do you associate Facebook with {adj} #######################
associate_vars<-as.data.frame(svy_unweighted$variables[,grep("associate_", names(svy_unweighted$variables), value=TRUE)])
associate_vars_facebook<-as.data.frame(associate_vars[,grep("facebook", names(associate_vars), value=TRUE)])

associate_varnames_facebook<-names(associate_vars_facebook)

########## Social media helps me  meet new people / maintain my existing connections #######################
seg_meet_vs_manage_connections<-svy_unweighted$variables$seg_meet_vs_manage_connections
unique(seg_meet_vs_manage_connections)
seg_meet_vs_manage_connections_num<-seg_meet_vs_manage_connections
seg_meet_vs_manage_connections_num[seg_meet_vs_manage_connections==unique(seg_meet_vs_manage_connections)[1]]<-0
seg_meet_vs_manage_connections_num[seg_meet_vs_manage_connections==unique(seg_meet_vs_manage_connections)[2]]<-1

table(seg_meet_vs_manage_connections)
table(seg_meet_vs_manage_connections_num)


########## I am a very private person / I'm open to share most details of my life  #######################
seg_sharer_vs_private_person<-svy_unweighted$variables$seg_sharer_vs_private_person

unique(seg_sharer_vs_private_person)
seg_sharer_vs_private_person_num<-seg_sharer_vs_private_person
seg_sharer_vs_private_person_num[seg_sharer_vs_private_person==unique(seg_sharer_vs_private_person)[1]]<-0
seg_sharer_vs_private_person_num[seg_sharer_vs_private_person==unique(seg_sharer_vs_private_person)[2]]<-1

table(seg_sharer_vs_private_person)
table(seg_sharer_vs_private_person_num)


########## Social media is mostly valuable to me as a way to make business contacts / to make personal contacts  #######################
seg_business_vs_personal<-svy_unweighted$variables$seg_business_vs_personal

unique(seg_business_vs_personal)
seg_business_vs_personal_num<-seg_business_vs_personal
seg_business_vs_personal_num[seg_business_vs_personal==unique(seg_business_vs_personal)[1]]<-0
seg_business_vs_personal_num[seg_business_vs_personal==unique(seg_business_vs_personal)[2]]<-1

table(seg_business_vs_personal)
table(seg_business_vs_personal_num)

########## I prefer to explore and have new experiences / sticking to familiar activities and traditions  #######################
seg_explore_vs_familiar<-svy_unweighted$variables$seg_explore_vs_familiar

seg_explore_vs_familiar<-svy_unweighted$variables$seg_explore_vs_familiar
unique(seg_explore_vs_familiar)
seg_explore_vs_familiar_num<-seg_explore_vs_familiar
seg_explore_vs_familiar_num[seg_explore_vs_familiar==unique(seg_explore_vs_familiar)[1]]<-0
seg_explore_vs_familiar_num[seg_explore_vs_familiar==unique(seg_explore_vs_familiar)[2]]<-1

table(seg_explore_vs_familiar)
table(seg_explore_vs_familiar_num)

########## I keep up with what’s popular so I can be part of the action or conversation / I follow my own personal interests regardless of what's trending  #######################
seg_popular_vs_own_interests<-svy_unweighted$variables$seg_popular_vs_own_interests

seg_popular_vs_own_interests<-svy_unweighted$variables$seg_popular_vs_own_interests
unique(seg_popular_vs_own_interests)
seg_popular_vs_own_interests_num<-seg_popular_vs_own_interests
seg_popular_vs_own_interests_num[seg_popular_vs_own_interests==unique(seg_popular_vs_own_interests)[1]]<-0
seg_popular_vs_own_interests_num[seg_popular_vs_own_interests==unique(seg_popular_vs_own_interests)[2]]<-1

table(seg_popular_vs_own_interests)
table(seg_popular_vs_own_interests_num)

########## I mostly use social media to connect with people / explore my interests  #######################
seg_people_vs_interests<-svy_unweighted$variables$seg_people_vs_interests

seg_people_vs_interests<-svy_unweighted$variables$seg_people_vs_interests
unique(seg_people_vs_interests)
seg_people_vs_interests_num<-seg_people_vs_interests
seg_people_vs_interests_num[seg_people_vs_interests==unique(seg_people_vs_interests)[1]]<-0
seg_people_vs_interests_num[seg_people_vs_interests==unique(seg_people_vs_interests)[2]]<-1

table(seg_people_vs_interests)
table(seg_people_vs_interests_num)

########## I wish the social media I use would stop changing the features or services they offer / I like it when the social media I use adds new features or services  #######################
seg_changing_features<-svy_unweighted$variables$seg_changing_features

seg_changing_features<-svy_unweighted$variables$seg_changing_features
unique(seg_changing_features)
seg_changing_features_num<-seg_changing_features
seg_changing_features_num[seg_changing_features==unique(seg_changing_features)[1]]<-0
seg_changing_features_num[seg_changing_features==unique(seg_changing_features)[2]]<-1

table(seg_changing_features)
table(seg_changing_features_num)

########## I like being the center of attention / I feel uncomfortable when attention is focused on me  #######################
seg_like_attention<-svy_unweighted$variables$seg_like_attention

seg_like_attention<-svy_unweighted$variables$seg_like_attention
unique(seg_like_attention)
seg_like_attention_num<-seg_like_attention
seg_like_attention_num[seg_like_attention==unique(seg_like_attention)[1]]<-0
seg_like_attention_num[seg_like_attention==unique(seg_like_attention)[2]]<-1

table(seg_like_attention)
table(seg_like_attention_num)
########## On social media, I prefer to express myself through photos and videos / On social media, I prefer to express myself through words  #######################
seg_photos_vs_words<-svy_unweighted$variables$seg_photos_vs_words

seg_photos_vs_words<-svy_unweighted$variables$seg_photos_vs_words
unique(seg_photos_vs_words)
seg_photos_vs_words_num<-seg_photos_vs_words
seg_photos_vs_words_num[seg_photos_vs_words==unique(seg_photos_vs_words)[1]]<-0
seg_photos_vs_words_num[seg_photos_vs_words==unique(seg_photos_vs_words)[2]]<-1

table(seg_photos_vs_words)
table(seg_photos_vs_words_num)

########## I’ve only made Facebook friends with people I already met in person / I've become Facebook friends with people I have never met offline  #######################
seg_met_vs_only_online_facebook<-svy_unweighted$variables$seg_met_vs_only_online_facebook

seg_met_vs_only_online_facebook<-svy_unweighted$variables$seg_met_vs_only_online_facebook
unique(seg_met_vs_only_online_facebook)
seg_met_vs_only_online_facebook_num<-seg_met_vs_only_online_facebook
seg_met_vs_only_online_facebook_num[seg_met_vs_only_online_facebook==unique(seg_met_vs_only_online_facebook)[1]]<-0
seg_met_vs_only_online_facebook_num[seg_met_vs_only_online_facebook==unique(seg_met_vs_only_online_facebook)[2]]<-1

table(seg_met_vs_only_online_facebook)
table(seg_met_vs_only_online_facebook_num)

########## When you have used Facebook, how often have you unfollowed, unsubscribed, or removed connections?  #######################
how_often_unfriend_ordered_facebook<-svy_unweighted$variables$how_often_unfriend_ordered_facebook
view(how_often_unfriend_ordered_facebook)

table(how_often_unfriend_ordered_facebook)

how_often_unfriend_ordered_facebook_varnames<-names(how_often_unfriend_ordered_facebook)

########## When using Facebook how bad did this experience make you feel?  #######################
main_reason_facebook_vars<-as.data.frame(svy_unweighted$variables[,grep("main_reason_facebook_", names(svy_unweighted$variables), value=TRUE)])
view(main_reason_facebook_vars)

main_reason_facebook_varnames<-names(main_reason_facebook_vars)

########## Compared with that time period, how often do you engage in the following online activities now?  #######################
how_often_vs_three_years_ago_ordered_vars<-as.data.frame(svy_unweighted$variables[,grep("how_often_vs_three_years_ago_ordered_", names(svy_unweighted$variables), value=TRUE)])
view(how_often_vs_three_years_ago_ordered_vars)

how_often_vs_three_years_ago_ordered_varnames<-names(how_often_vs_three_years_ago_ordered_vars)

##########  When using [brand], please rate how easy or difficult you find it to complete the following activities: #######################
how_easy_ordered_vars<-as.data.frame(svy_unweighted$variables[,grep("how_easy_ordered_", names(svy_unweighted$variables), value=TRUE)])
how_easy_ordered_facebook_vars<-as.data.frame(how_easy_ordered_vars[,grep("facebook", names(how_easy_ordered_vars), value=TRUE)])
view(names(how_easy_ordered_facebook_vars))

how_easy_ordered_facebook_varnames<-names(how_easy_ordered_facebook_vars)

########## Would you say that [brand] generally loads quickly or loads slowly?   #######################
load_time_ordered_facebook<-svy_unweighted$variables$load_time_ordered_facebook
table(load_time_ordered_facebook)

########## What is the main way you currently use Facebook? #######################
main_usage_method_facebook<-svy_unweighted$variables$main_usage_method_facebook
table(main_usage_method_facebook)

########## Do you currently have the mobile app version of Facebook on your phone? #######################
app_currently_installed_facebook<-svy_unweighted$variables$app_currently_installed_facebook
table(app_currently_installed_facebook)

########## Have you ever used the mobile app version of Facebook? #######################
ever_used_app_facebook<-svy_unweighted$variables$ever_used_app_facebook
table(ever_used_app_facebook)

########## Thinking about the last time you used [brand], would you say that you saw too many, too few, or about the right amount of things from each of the following? #######################
saw_amount_vars<-as.data.frame(svy_unweighted$variables[,grep("saw_amount_", names(svy_unweighted$variables), value=TRUE)])
saw_amount_vars_facebook<-as.data.frame(saw_amount_vars[,grep("_facebook", names(saw_amount_vars), value=TRUE)])
saw_amount_vars_ordered_facebook<-as.data.frame(saw_amount_vars_facebook[,grep("ordered", names(saw_amount_vars_facebook), value=TRUE)])

saw_amount_varnames_ordered_facebook<-names(saw_amount_vars_ordered_facebook)

######################################################################################
###### creating  correlation plots   #################################################
######################################################################################

### shared_sns_time ###
shared_sns_time_all_vars_and_churned<-cbind(share_sns_time_ordered_facebook, share_sns_time_three_years_ago_ordered_facebook, churned_facebook) 
cor_shared_sns_time_all_vars_and_churned<-cor(shared_sns_time_all_vars_and_churned, use="pairwise")

png("corrplot_shared_sns_time_all_vars_and_churned.png", width = 1500, height = 500)
corrplot(cor_shared_sns_time_all_vars_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_shared_sns_time_all_vars_and_churned)
round(sort(cor_shared_sns_time_all_vars_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_shared_sns_time_all_vars_and_churned)[1],2)

#####################
### segmentation  ###
#####################

seg_all_vars_and_churned<-cbind(seg_sharer_vs_private_person_num,seg_popular_vs_own_interests_num,
                                  seg_photos_vs_words_num,seg_people_vs_interests_num,seg_met_vs_only_online_facebook_num,
                                  seg_meet_vs_manage_connections_num,seg_like_attention_num,
                                  seg_explore_vs_familiar_num,seg_changing_features_num,seg_business_vs_personal_num)
head(seg_all_vars_and_churned)

# converting to numeric dataframe
seg_all_vars_and_churned<-as.data.table(seg_all_vars_and_churned)
seg_all_vars_and_churned<-mutate_all(seg_all_vars_and_churned, function(x) as.numeric(as.character(x)))

cor_seg_all_vars_and_churned<-cor(seg_all_vars_and_churned, use="pairwise")
cor_seg_all_vars_and_churned

png("corrplot_seg_all_vars_and_churned.png", width = 1500, height = 500)
corrplot(cor_seg_all_vars_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_seg_all_vars_and_churned)
round(sort(cor_seg_all_vars_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_seg_all_vars_and_churned)[1],2)

# finding maximum 
n <- length(cor_seg_all_vars_and_churned)
round(sort(cor_seg_all_vars_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_seg_all_vars_and_churned)[1],2)


################
### privacy  ###
################

privacy_and_churned<-cbind(privacy_who_can_see_posts_ordered,privacy_info_for_ads_ordered,privacy_account_access_ordered,privacy_who_can_see_posts_vs_three_years_ago_ordered,
                           privacy_info_for_ads_vs_three_years_ago_ordered,privacy_account_access_vs_three_years_ago_ordered,churned_facebook)
head(privacy_and_churned)

# creating the correlation variable
cor_privacy_and_churned<-cor(privacy_and_churned, use="pairwise")
#cor_privacy_and_churned

png("corrplot_cor_privacy_and_churned.png", width = 1500, height = 500)
corrplot(cor_privacy_and_churned, method = "circle")
dev.off()

rounded_cor_privacy_and_churned<-round(cor_privacy_and_churned,2)
view(rounded_cor_privacy_and_churned)

png("corrtable_cor_privacy_and_churned.png", width = 1500, height = 500)
p<-tableGrob(rounded_cor_privacy_and_churned)
grid.arrange(p)
dev.off()

# finding maximum 
n <- length(cor_privacy_and_churned)
round(sort(cor_privacy_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_privacy_and_churned)[1],2)

#############################
### app favorability vars ###
#############################

app_favorability_all_vars_and_churned<-cbind(app_favorability_vs_three_years_ago_ordered_facebook,app_favorability_ordered_facebook,churned_facebook)
head(app_favorability_all_vars_and_churned)

# creating the correlation variable
cor_app_favorability_all_vars_and_churned<-cor(app_favorability_all_vars_and_churned, use="pairwise")
#cor_app_favorability_all_vars_and_churned

png("corrplot_cor_app_favorability_all_vars_and_churned.png", width = 1500, height = 500)
corrplot(cor_app_favorability_all_vars_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_app_favorability_all_vars_and_churned)
round(sort(cor_app_favorability_all_vars_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_app_favorability_all_vars_and_churned)[1],2)

#####################################################
### saw_amount_{connection_type}_ordered_facebook ###
#####################################################
# shorteneing the variable names
saw_amount_vars_ordered_facebook_short_names<-saw_amount_vars_ordered_facebook

names(saw_amount_vars_ordered_facebook_short_names)
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_close_friends_facebook"] <- "close_friends_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_acquaintances_facebook"] <- "acquaintances_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_family_members_facebook"] <- "family_members_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_work_colleagues_facebook"] <- "work_colleagues_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_business_org_celebrities_facebook"] <- "business_org_celebrities_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_news_facebook"] <- "news_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_politics_facebook"] <- "politics_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_sports_facebook"] <- "sports_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_advertisements_facebook"] <- "advertisements_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_memes_humor_facebook"] <- "memes_humor_facebook"
names(saw_amount_vars_ordered_facebook_short_names)[names(saw_amount_vars_ordered_facebook_short_names)=="saw_amount_ordered_suggested_friends_facebook"] <- "suggested_friends_facebook"

names(saw_amount_vars_ordered_facebook_short_names)

saw_amount_vars_ordered_facebook_short_names<-cbind(saw_amount_vars_ordered_facebook_short_names,churned_facebook)
names(saw_amount_vars_ordered_facebook_short_names)

# creating the correlation variable
cor_saw_amount_vars_ordered_facebook_short_names<-cor(saw_amount_vars_ordered_facebook_short_names, use="pairwise")
#cor_saw_amount_vars_ordered_facebook_short_names

png("corrplot_cor_saw_amount_vars_ordered_facebook_short_names.png", width = 1500, height = 500)
corrplot(cor_saw_amount_vars_ordered_facebook_short_names, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_saw_amount_vars_ordered_facebook_short_names)
round(sort(cor_saw_amount_vars_ordered_facebook_short_names)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_saw_amount_vars_ordered_facebook_short_names)[1],2)

##############################################################
### How often have you engaged in the following activities ###
##############################################################

# shorteneing the variable names
how_often_ordered_vars_short_names<-how_often_vs_three_years_ago_ordered_vars

names(how_often_ordered_vars_short_names)
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_sharing_about_daily_life"] <- "sharing_about_daily_life"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_sharing_about_major_life_events"] <- "sharing_about_major_life_events"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_sharing_photos"] <- "sharing_photos"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_sharing_videos"] <- "sharing_videos"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_sending_private_messages"] <- "sending_private_messages"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_connecting_with_family_and_close_friends"] <- "connecting_with_family_and_close_friends"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_connecting_with_more_distant_friends_and_acquaintances"] <- "conn_with_more_distant_friends._and_acq."
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_looking_at_photos"] <- "looking_at_photos"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_watching_videos"] <- "watching_videos"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_meeting_new_people"] <- "meeting_new_people"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_connecting_to_your_passions_and_interests"] <- "connecting_to_your_passions_and_interests"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_getting_news"] <- "getting_news"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_buying_selling_goods"] <- "buying_selling_goods"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_promoting_running_coordinating_a_business"] <- "promoting_running_coordinating_a_business"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_making_professional_connections"] <- "making_professional_connections"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_planning_coordinating_events"] <- "planning_coordinating_events"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_playing_games"] <- "playing_games"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_passing_time"] <- "passing_time"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_debating"] <- "debating"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_promoting_a_cause_fundraising"] <- "promoting_a_cause_fundraising"
names(how_often_vars_ordered_short_names)[names(how_often_vars_ordered_short_names)=="how_often_vs_three_years_ago_ordered_viewing_memes_humor"] <- "viewing_memes_humor"

names(how_often_vars_ordered_short_names)

how_often_vars_ordered_short_names_and_churned<-cbind(how_often_vars_ordered_short_names[1:21],churned_facebook)
names(how_often_vars_ordered_short_names_and_churned)

# creating the correlation variable
cor_how_often_vars_ordered_short_names_and_churned<-cor(how_often_vars_ordered_short_names_and_churned, use="pairwise")
#view(cor_how_often_vars_ordered_short_names_and_churned)

png("corrplot_cor_how_often_vars_ordered_short_names_and_churned.png", width = 1500, height = 500)
corrplot(cor_how_often_vars_ordered_short_names_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_how_often_vars_ordered_short_names_and_churned)
round(sort(cor_how_often_vars_ordered_short_names_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_how_often_vars_ordered_short_names_and_churned)[1],2)

##################################################################################
### How often have you engaged in the following activities vs. three years ago ###
##################################################################################

# shorteneing the variable names
how_often_vars_ordered_short_names_v3<-how_often_vs_three_years_ago_ordered_vars

names(how_often_vars_ordered_short_names_v3)
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_sharing_about_daily_life"] <- "sharing_about_daily_life_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_sharing_about_major_life_events"] <- "sharing_about_major_life_events_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_sharing_photos"] <- "sharing_photos_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_sharing_videos"] <- "sharing_videos_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_sending_private_messages"] <- "sending_private_messages_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_connecting_with_family_and_close_friends"] <- "connecting_with_family_and_close_friends_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_connecting_with_more_distant_friends_and_acquaintances"] <- "conn_with_more_dist_fr._and_acq._v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_looking_at_photos"] <- "looking_at_photos_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_watching_videos"] <- "watching_videos_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_meeting_new_people"] <- "meeting_new_people_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_connecting_to_your_passions_and_interests"] <- "connecting_to_your_passions_and_interests_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_getting_news"] <- "getting_news_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_buying_selling_goods"] <- "buying_selling_goods_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_promoting_running_coordinating_a_business"] <- "promoting_running_coordinating_a_business_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_making_professional_connections"] <- "making_professional_connections_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_planning_coordinating_events"] <- "planning_coordinating_events_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_playing_games"] <- "playing_games_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_passing_time"] <- "passing_time_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_debating"] <- "debating_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_promoting_a_cause_fundraising"] <- "promoting_a_cause_fundraising_v3"
names(how_often_vars_ordered_short_names_v3)[names(how_often_vars_ordered_short_names_v3)=="how_often_vs_three_years_ago_ordered_viewing_memes_humor"] <- "viewing_memes_humor_v3"

names(how_often_vars_ordered_short_names_v3)

how_often_vars_ordered_short_names_v3_and_churned<-cbind(how_often_vars_ordered_short_names_v3[1:21],churned_facebook)
names(how_often_vars_ordered_short_names_v3_and_churned)

# creating the correlation variable
cor_how_often_vars_ordered_short_names_v3_and_churned<-cor(how_often_vars_ordered_short_names_v3_and_churned, use="pairwise")
#view(cor_how_often_vars_ordered_short_names_v3_and_churned)

png("cor_how_often_vars_ordered_short_names_v3_and_churned.png", width = 1500, height = 500)
corrplot(cor_how_often_vars_ordered_short_names_v3_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_how_often_vars_ordered_short_names_v3_and_churned)
round(sort(cor_how_often_vars_ordered_short_names_v3_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_how_often_vars_ordered_short_names_v3_and_churned)[1],2)

##################################################################################
### How important is it to you that a social network site is :   #################
##################################################################################

# shorteneing the variable names
how_important_vars_ordered_short_names<-how_important_vars_ordered

names(how_important_vars_ordered_short_names)
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_innovative"] <- "innovative"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_visually_pleasing"] <- "visually_pleasing"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_easy_to_use"] <- "easy_to_use"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_non_commercial"] <- "non_commercial"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_authentic"] <- "authentic"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_safe"] <- "safe"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_customizable"] <- "customizable"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_inclusive"] <- "inclusive"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_exclusive"] <- "exclusive"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_engaging"] <- "engaging"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_interesting"] <- "interesting"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_uncluttered"] <- "uncluttered"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_modern"] <- "modern"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_unique"] <- "unique"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_fun"] <- "fun"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_useful"] <- "useful"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_trustworthy"] <- "trustworthy"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_relaxing"] <- "relaxing"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_provocative"] <- "provocative"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_uncensored"] <- "uncensored"
names(how_important_vars_ordered_short_names)[names(how_important_vars_ordered_short_names)=="how_important_ordered_private"] <- "private"

names(how_important_vars_ordered_short_names)

# attaching churned_facebook
how_important_vars_ordered_and_churned_short_names<-cbind(how_important_vars_ordered,churned_facebook)


# creating the correlation variable
cor_how_important_vars_ordered_and_churned_short_names<-cor(how_important_vars_ordered_and_churned_short_names, use="pairwise")
#view(cor_how_important_vars_ordered_and_churned_short_names)

png("cor_how_important_vars_ordered_and_churned_short_names.png", width = 1500, height = 500)
corrplot(cor_how_important_vars_ordered_and_churned_short_names, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_how_important_vars_ordered_and_churned_short_names)
round(sort(cor_how_important_vars_ordered_and_churned_short_names)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_how_important_vars_ordered_and_churned_short_names)[1],2)

##################################################################################
### How often have you engaged in the following activities vs. three years ago ###
##################################################################################

# shorteneing the variable names
how_easy_ordered_facebook_vars_short_names<-how_easy_ordered_facebook_vars

names(how_easy_ordered_facebook_vars_short_names)
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_sharing_photos_facebook"] <- "sharing_photos_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_sharing_videos_facebook"] <- "sharing_videos_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_sending_private_messages_facebook"] <- "sending_private_messages_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_looking_at_photos_facebook"] <- "looking_at_photos_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_watching_videos_facebook"] <- "watching_videos_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_getting_news_facebook"] <- "getting_news_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_buying_selling_goods_facebook"] <- "buying_selling_goods_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_planning_coordinating_events_facebook"] <- "planning_coordinating_events_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_playing_games_facebook"] <- "playing_games_facebook"
names(how_easy_ordered_facebook_vars_short_names)[names(how_easy_ordered_facebook_vars_short_names)=="how_easy_ordered_promoting_a_cause_fundraising_facebook"] <- "promoting_a_cause_fundraising_facebook"

names(how_easy_ordered_facebook_vars_short_names)

how_easy_ordered_facebook_vars_short_names_and_churned<-cbind(how_easy_ordered_facebook_vars_short_names,churned_facebook)
names(how_easy_ordered_facebook_vars_short_names_and_churned)

# creating the correlation variable
cor_how_easy_ordered_facebook_vars_short_names_and_churned<-cor(how_easy_ordered_facebook_vars_short_names_and_churned, use="pairwise")
#view(cor_how_easy_ordered_facebook_vars_short_names_and_churned)

png("cor_how_easy_ordered_facebook_vars_short_names_and_churned.png", width = 1500, height = 500)
corrplot(cor_how_easy_ordered_facebook_vars_short_names_and_churned, method = "circle")
dev.off()

view(cor_how_easy_ordered_facebook_vars_short_names_and_churned)

# finding maximum 
n <- length(cor_how_easy_ordered_facebook_vars_short_names_and_churned)
round(sort(cor_how_easy_ordered_facebook_vars_short_names_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_how_easy_ordered_facebook_vars_short_names_and_churned)[1],2)

##################################################################################
### Do you associate Facebook with...  ###########################################
##################################################################################

# shorteneing the variable names
associate_vars_short_names<-associate_vars_facebook
names(associate_vars_short_names)

# shortening the variable names
names(associate_vars_short_names) <- sub("associate_", "", names(associate_vars_short_names))
names(associate_vars_short_names)

# attaching churned_facebook
associate_vars_short_names_and_churned<-cbind(associate_vars_short_names,churned_facebook)
names(associate_vars_short_names_and_churned)

# creating the correlation variable
cor_associate_vars_short_names_and_churned<-cor(associate_vars_short_names_and_churned, use="pairwise")
#view(associate_vars_short_names_and_churned)

png("cor_associate_vars_short_names_and_churned.png", width = 1500, height = 500)
corrplot(cor_associate_vars_short_names_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_associate_vars_short_names_and_churned)
round(sort(cor_associate_vars_short_names_and_churned)[n-sqrt(n)-1],2)

# finding minimum
round(sort(cor_associate_vars_short_names_and_churned)[1],2)

##################################################################################
### Other variables  #############################################################
##################################################################################

# creating the data table
other_variables<-data.table()
other_variables<-cbind(num_notifications_ordered_facebook,negative_positive_ordered_facebook,how_often_unfriend_ordered_facebook,ever_used_app_facebook,app_currently_installed_facebook,cares_about_users_ordered_facebook,value_from_ordered_facebook)
head(other_variables)

# attaching churned_facebook
other_variables_and_churned<-cbind(other_variables,churned_facebook)
view(other_variables_and_churned)

# creating the correlation variable
cor_other_variables_and_churned<-cor(other_variables_and_churned, use="pairwise")
#view(associate_vars_short_names_and_churned)

png("cor_other_variables_and_churned.png", width = 1500, height = 500)
corrplot(cor_other_variables_and_churned, method = "circle")
dev.off()

# finding maximum 
n <- length(cor_other_variables_and_churned)
round(sort(cor_other_variables_and_churned)[n-sqrt(n)-sum(is.na(cor_other_variables_and_churned))],2)

# finding minimum
round(sort(cor_other_variables_and_churned)[1],2)


##################################################################################
### All variables together #######################################################
##################################################################################

all_indeps_together<-data.table()
all_indeps_together<-cbind(share_sns_time_three_years_ago_ordered_facebook,share_sns_time_ordered_facebook,seg_sharer_vs_private_person_num,seg_popular_vs_own_interests_num,seg_photos_vs_words_num,seg_people_vs_interests_num,seg_met_vs_only_online_facebook_num,seg_meet_vs_manage_connections_num,seg_like_attention_num,seg_explore_vs_familiar_num,seg_changing_features_num,seg_business_vs_personal_num,privacy_who_can_see_posts_ordered,privacy_info_for_ads_ordered,privacy_account_access_ordered,privacy_who_can_see_posts_vs_three_years_ago_ordered,privacy_info_for_ads_vs_three_years_ago_ordered,privacy_account_access_vs_three_years_ago_ordered,app_favorability_vs_three_years_ago_ordered_facebook,app_favorability_ordered_facebook,ethnicity_vars,urban_suburban_rural_ordered,saw_amount_vars_ordered_facebook,how_often_vars_ordered_short_names,how_easy_ordered_facebook_vars_short_names,associate_vars_short_names,female,num_notifications_ordered_facebook,negative_positive_ordered_facebook,how_often_unfriend_ordered_facebook,ever_used_app_facebook,app_currently_installed_facebook,cares_about_users_ordered_facebook,value_from_ordered_facebook)
names(all_indeps_together)

# converting to numeric dataframe
all_indeps_together<-mutate_all(all_indeps_together, function(x) as.numeric(as.character(x)))

# creating correlation matrix
cor_all_indeps_together<-cor(all_indeps_together, use="pairwise")
view(cor_all_indeps_together)

# adding churned
all_together<-cbind(all_indeps_together,churned_facebook)
names(all_together)

# creating correlation matrix
cor_all_together<-cor(all_together, use="pairwise")
names(cor_all_together)
view(cor_all_together)

# finding maximum 
n <- length(cor_all_together)
round(sort(cor_all_together)[n-sqrt(n)-sum(is.na(cor_all_together))],2)

sum(is.na(cor_all_together))

# finding minimum
round(sort(cor_all_together)[1],2)

# creating excel table
# write.csv(cor_all_together,"cor_all_together.csv", row.names = FALSE)

############# creating histogram  ##############
# creating one vector
one_vector_cor_all_together<-as.vector(cor_all_together)
# removing the ones
one_vector_cor_all_together[one_vector_cor_all_together==1]<-NA

# exploring the vector
head(one_vector_cor_all_together)
class(one_vector_cor_all_together)
length(one_vector_cor_all_together)

# creating the plot
png("one_vector_cor_all_together.png", width = 1500, height = 500)
qplot(one_vector_cor_all_together,
      geom="histogram",
      binwidth = 0.01,  
      main = "Pearson correlation coefficients for all variables together, including churned",
      xlab="All Pearson coefficients",
      fill="blue") + theme(legend.position="none")
dev.off()

view(names(all_indeps_together))

# creating excel table of all indeps
write.csv(all_indeps_together,"all_indeps_together.csv", row.names = FALSE)

# creating excel table of all indeps and churned
write.csv(all_together,"all_together.csv", row.names = FALSE)

# creating a vector of correlations between variables and churned
all_corrs<-as.data.frame(cor(all_together, use="pairwise"))
dim(all_corrs)
head(all_corrs)
churned_corrs<-as.data.frame(all_corrs$churned_facebook)
rownames(churned_corrs)<-names(all_together)
view(churned_corrs)

# creating a vector of correlations between variables and churned
churned_corrs_abs<-abs(churned_corrs)
view(churned_corrs_abs)

# removing the correlation between churned and itself
churned_corrs_abs[churned_corrs_abs==1]<-NA
view(churned_corrs_abs)

#############################################################
###############  Regression anaysis  ########################
#############################################################

####### Regression step 1 - preparing the data #########

# removing columns with over 40% of missing values
all_together_no40<-all_together[ , colMeans(is.na(all_together)) <= 0.4]
names(all_together_no40)
colMeans(is.na(all_together_no40))
colMeans(!is.na(all_together_no40))
dim(all_together_no40)

# removing rows with missing values
all_together_comp<-all_together[complete.cases(all_together),]
dim(all_together_comp)
all_together_no40_complete <- na.omit(all_together_no40)

dim(all_together_no40)
dim(all_together_no40_complete)
class(all_together_no40_complete)

# Creating a design matrix
x <- model.matrix(churned_facebook~., all_together_no40_complete)
y<-all_together_no40_complete$churned_facebook
class(x)
dim(x)


####### Regression step 2 - selecting variables with LASSO logistic regression #########

# LASSO logistic regression, all reg40 variables

lasso_40_lambda0<-glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

# running the LASSO regression with cv.
cv.out <- cv.glmnet(x, y, family = "binomial", alpha = 1,type.measure = "mse")
plot(cv.out)

# running a regular LASSO regression
lasso_40_lambdamin<-glmnet(x, y, family = "binomial", alpha = 1, lambda = lambda_min)
lasso_40_lambdamin
lasso_40_lambdamin$beta
lasso_40_lambda0

# choosing lambda
chosen_lambda<-lasso_40_lambda0$lambda[23]
chosen_lambda

# getting different values of lambda
lambda_min <- cv.out$lambda.min
lambda_1se<-cv.out$lambda.1se
lambda_min

# checking different regression coefficients
coef(cv.out,s=lambda_min)
coef(cv.out,s=lambda_1se)
coef(cv.lasso, cv.lasso$lambda.1se)

# extracting chosen variables
coef<-coef(cv.out,s=chosen_lambda)
sum<-summary(coef)
dim(sum)
sum[,1]
chosen_varnames_lasso<-names(all_together_no40_complete)[sum[,1]-1]

# removing churned_facebook
chosen_varnames_lasso
len<-length(chosen_varnames_lasso)
chosen_varnames_lasso_no_y<-chosen_varnames_lasso[1:len-1]
chosen_varnames_lasso_no_y

# adding variables from Andy's adult model
adult_varnames<-c("num_notifications_ordered_facebook",
              "negative_positive_ordered_facebook",
              "how_often_unfriend_ordered_facebook")
adult_varnames

all_together_comp$num_notifications_ordered_facebook


chosen_varnames<-c(adult_varnames,chosen_varnames_lasso)
chosen_varnames
chosen_varnames_no_y<-c(adult_varnames,chosen_varnames_lasso_no_y)
chosen_varnames

####### Regression step 2 - plotting predicted probabilities using the  #########
####### logistic regression with the chosen variabl   ###########################

# creating the dataset
# creating dataset with chosen varnames
reduced_data<-all_together[chosen_varnames]

# removing rows with missing variables
reduced_data_complete <- na.omit(reduced_data)
dim(reduced_data_complete)

# running the regression
fit <- glm(churned_facebook~.,data = reduced_data_complete, family = binomial)
summary(fit)

png("predicted_probability_plots.png", width = 1500, height = 500)
p<-plot(allEffects(fit))
dev.off()



