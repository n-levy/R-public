### Checking the effect of regularization ###

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
# # loading the data files
 setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# # ratings<-readRDS("ratings")
# # movies<-readRDS("movies")
# movielens<-readRDS("movielens")
 core<-readRDS("core")
 sub<-readRDS("sub")
# validation<-readRDS("validation")
# edx<-readRDS("edx")
 rmse_results<-readRDS("rmse_results")
 
# user and movie fixed effects
# Regularizing with lambda = 3
lambda <- 3
mu <- mean(core$rating)
mu

user_avgs <- core %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

movie_avgs <- core %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu - b_u)/(n()+lambda), n_i = n())

# Making a plot to see how the estimates shrunk
# tibble(original = movie_avgs$b_i, 
#        regularlized = movie_reg_avgs$b_i, 
#        n = movie_reg_avgs$n_i) %>%
#   ggplot(aes(original, regularlized, size=sqrt(n))) + 
#   geom_point(shape=1, alpha=0.5)

### Looking up the top 10 best movies ###
# movie_titles <- movielens %>% 
#   select(movieId, title) %>%
#   distinct()
# 
# core %>%
#   count(movieId) %>% 
#   left_join(movie_reg_avgs, by = "movieId") %>%
#   left_join(movie_titles, by = "movieId") %>%
#   arrange(desc(b_i)) %>% 
#   slice(1:10) %>% 
#   pull(title)

### Looking up the 10 worst movies ###
# core %>%
#   count(movieId) %>% 
#   left_join(movie_reg_avgs, by = "movieId") %>%
#   left_join(movie_titles, by="movieId") %>%
#   arrange(b_i) %>% 
#   select(title, b_i, n) %>% 
#   slice(1:10) %>% 
#   pull(title)

### Calculating RMSE ###
predicted_ratings <- sub %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
movie_and_user_effects_with_regularization_rmse<-RMSE(predicted_ratings, sub$rating)

# checking the RMSE
rmse_results <- c("Movie and user effects with regularization", round(movie_and_user_effects_with_regularization_rmse,5))
# rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_and_user_effects_with_regularization_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

# user and movie fixed effects
# Regularizing with lambda = 3
lambda <- 3
mu <- mean(core$rating)

# user fixed effects
user_avgs <- core %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

# movie fixed effects with regularization
movie_avgs <- core %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu - b_u)/(n()+lambda), n_i = n())

head(user_avgs)
head(movie_avgs)

# genre fixed effects with regularization
# creating numeric genre column
core$genresnum<-as.numeric(as.factor(core$genres))

genre_avgs <- core %>% 
  left_join(user_avgs) %>%
  left_join(movie_avgs) %>%
  group_by(genresnum) %>% 
  summarize(b_g = mean(rating - mu-b_u-b_i)/(n()+lambda), n_i = n())

# creating a numeric user_genres column in the
# sub dataset
sub$genresnum<-as.numeric(as.factor(sub$genres))

### Calculating RMSE ###
predicted_ratings <- sub %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genresnum') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
movie_user_genre_effects<-RMSE(predicted_ratings, sub$rating)

# checking the RMSE
rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_user_genre_effects,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results


# user*genre fixed effects with regularization
# creating numeric user*genre column
core$user_genre<-as.numeric(as.factor(core$userId))*as.numeric(as.factor(core$genres))

user_genre_avgs <- core %>% 
  left_join(user_avgs) %>%
  left_join(movie_avgs) %>%
  left_join(genre_avgs) %>%
  group_by(user_genre) %>% 
  summarize(b_ug = mean(rating - mu-b_u-b_i-b_g)/(n()+lambda), n_i = n())

### Calculating RMSE ###
# creating a numeric user_genres column in the
# sub dataset
sub$user_genre<-as.numeric(as.factor(sub$userId))*as.numeric(as.factor(sub$genres))

predicted_ratings <- sub %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genresnum') %>%
  left_join(user_genre_avgs, by='user_genre') %>%
  mutate(pred = mu + b_i + b_u + b_g+b_ug) %>%
  pull(pred)
movie_and_user_effects_with_regularization_rmse<-RMSE(predicted_ratings, sub$rating)

# checking the RMSE
rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_and_user_effects_with_regularization_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

# running a regression
# fit <- lm(rating ~ as.factor(movieId), data = core)
# fit <- lm(rating ~ as.factor(movieId) + as.factor(userId) + as.factor(genres) + as.factor(userId)*as.factor(genres), data = core)

