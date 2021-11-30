### Checking the effect of regularization ###

### Loading libraries ###
# library(tidyverse)
# library(caret)
# library(data.table)
# library(dplyr)
# 
# # loading the data files
# setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# # ratings<-readRDS("ratings")
# # movies<-readRDS("movies")
# movielens<-readRDS("movielens")
# core<-readRDS("core")
# sub<-readRDS("sub")
# validation<-readRDS("validation")
# edx<-readRDS("edx")


# Regularizing with lambda = 3
lambda <- 3
mu <- mean(core$rating)

user_avgs <- core %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

movie_avgs <- core %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu - b_u)/(n()+lambda), n_i = n())

# Maing a plot to see how the estimates shrunk
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
rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_and_user_effects_with_regularization_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

