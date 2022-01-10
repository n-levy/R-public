########################################################################################
#### Checking the RMSE in case of the "constant effects with regularization" model #####
########################################################################################


### Loading libraries ###
library(tidyverse)
library(caret)
# library(data.table)
library(dplyr)
# library(kableExtra)
# library(recommenderlab)
library(Matrix)
# library(BBmisc)
# library(DT)
# library(pander)

# cleaning the environment
# rm(list = ls())

### Increasing memory
# Checking memory limit
# memory.limit()
# Change memory limit
memory.limit(size = 10^10)

# cleaning memory
invisible(gc())

# # loading  data files
setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
edx<-readRDS("edx")
validation<-readRDS("validation")

### Loading libraries ###
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)

# # removing all files
# rm(list = ls())

# clearing unused memory
invisible(gc())
# 
# user, movie and genre fixed effects with regularization
# Regularizing with lambda = 3
lambda <- 3
mu <- mean(edx$rating)

# user fixed effects
user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

# movie fixed effects with regularization
movie_avgs <- edx %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu - b_u)/(n()+lambda), n_i = n())

# head(user_avgs)
# head(movie_avgs)

# genre fixed effects with regularization
# creating numeric genre column
edx$genresnum<-as.numeric(as.factor(edx$genres))

genre_avgs <- edx %>% 
  left_join(user_avgs) %>%
  left_join(movie_avgs) %>%
  group_by(genresnum) %>% 
  summarize(b_g = sum(rating - mu-b_u-b_i)/(n()+lambda), n_i = n())

# creating a numeric user_genres column in the
# validation dataset
validation$genresnum<-as.numeric(as.factor(validation$genres))

### Calculating RMSE ###
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genresnum') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
movie_user_genre_effects<-RMSE(predicted_ratings, validation$rating)

# checking the RMSE
# rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_user_genre_effects,5)))
rmse_results <- c("Movie, user and genre effects with regularization", round(movie_user_genre_effects,5))
names(rmse_results)<-c("method", "RMSE")
rmse_results

