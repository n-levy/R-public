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
####################################################################################
####### Running a simple linear model, with constants for users and movies #########
####################################################################################

# loading the data files
setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# ratings<-readRDS("ratings")
# movies<-readRDS("movies")
# movielens<-readRDS("movielens")
# core<-readRDS("core")
# sub<-readRDS("sub")
validation<-readRDS("validation")
edx<-readRDS("edx")

# Validation method is "Leave One Out Cross Validation (LOOCV)"

### creating a subset of the training set, for testing the model ###
# I am avoiding calling this a validation set, 
# since edx called the test set 'validation set'.
# To avoid confusion, the core training set will be called 'core' and the subset of the training set
# for validation will be called 'sub'.

# The 'sub' set will be 15% of the training set
 sub_index <- createDataPartition(y = edx$rating, times = 1, p = 0.15, list = FALSE)
 core <- edx[-sub_index,]
 temp <- edx[sub_index,]

# Make sure userId and movieId in sub set are also in core set
sub <- temp %>%
  semi_join(core, by = "movieId") %>%
  semi_join(core, by = "userId")

# Add rows removed from sub set back into core set
removed <- anti_join(temp, sub)
core <- rbind(core, removed)
# 
# rm(sub_index, temp, removed)

### The first model ###
# Predicting only according to the average rating in the dataset ###
mu <- mean(core$rating)
mu

# checking the root mean squared error
naive_rmse <- RMSE(sub$rating, mu)
naive_rmse

# creating a results table
rmse_results <- c("Just the average", round(naive_rmse,5))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding movie effects ###
movie_avgs <- core %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# examining the distributions of constant movie effects
# qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- mu + sub %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
movie_effects_rmse<-RMSE(predicted_ratings, sub$rating)

rmse_results <- rbind.data.frame(rmse_results, c("With movie effects", round(movie_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding user effects ###
# examining the average rating per user
# core %>% 
#   group_by(userId) %>% 
#   filter(n()>=100) %>%
#   summarize(b_u = mean(rating)) %>% 
#   ggplot(aes(b_u)) + 
#   geom_histogram(bins = 30, color = "black")

# estimating the user effects, by computing 
# the overall average and the item effect, and then 
# the user effect is the average of the remainder, after they
# are substacted from the rating (user effect= average of (rating - overall average - item effect)
user_avgs <- core %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
movie_and_user_effects_rmse<-RMSE(predicted_ratings, sub$rating)

rmse_results <- rbind.data.frame(rmse_results, c("With movie and user effects", round(movie_and_user_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

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

# user, movie and genre fixed effects
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

### Exploring the training set ###
dim(edx)
names(edx)
head(edx)
summary(edx)
sum(is.na(edx)) # counting missing values

# counting the unique values
n_distinct(edx$userId) # users
n_distinct(edx$movieId) # movies
n_distinct(edx$rating) # ratings
n_distinct(edx$genres) # genres

### examining some distributions of the ratings ###
# distribution of the ratings
histogram(edx$rating) 
ratings_per_movie<-edx %>%
  count(movieId)

# distribution of the ratings per movie
histogram(ratings_per_movie$n, breaks=30)  
boxplot(ratings_per_movie$n)
summary(ratings_per_movie$n) 

rm(ratings_per_movie)

# distribution of the ratings per user
ratings_per_user<-edx %>%
  filter(!is.na(rating))  %>%
  count(userId)

histogram(ratings_per_user$n, breaks=30)  
boxplot(ratings_per_user$n)
summary(ratings_per_user$n) 

rm(ratings_per_user)

### Creating a matrix of ratings with userIds (rows) by movieIds (columns) ###
trainmat<-pivot_wider(edx, id_cols = userId, names_from = title, values_from = rating, values_fn=length, names_sep = "_", names_repair = "check_unique")
dim(trainmat)
head(trainmat)[1:5,1:5]

# exploring the matrix #
n_missing<-sum(is.na(trainmat)) # counting mising values
n_nonmissing<-sum(!is.na(trainmat)) # counting nonmissing values
p_nonmissing<-n_nonmissing/(n_missing+n_nonmissing) # calculating the percentage of nonmissing values
p_nonmissing 
rm(n_missing, n_nonmissing, p_nonmissing) # removing these variables

# rm(trainmat)
