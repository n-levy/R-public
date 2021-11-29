####################################################################################
####### Running a simple linear model, with constants for users and movies #########
####################################################################################


### Loading libraries ###
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)


# Version 1 - validation method is "Leave One Out Cross Validation (LOOCV)"

### creating a subset of the training set, for testing the model ###
# I am avoiding calling this a validation set, 
# so it will not be confused with the test set, which edx called a 'validation set'
# will simply be called the core test set 'core' and the subset 'sub'
# Validation set will be 15% of the training set
sub_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
core <- edx[-sub_index,]
temp <- edx[sub_index,]

# Make sure userId and movieId in sub set are also in core set
sub <- temp %>% 
  semi_join(core, by = "movieId") %>%
  semi_join(core, by = "userId")

# Add rows removed from sub set back into core set
removed <- anti_join(temp, sub)
core <- rbind(core, removed)

rm(sub_index, temp, removed)

### The first model ###
# Predicting only according to the average rating in the dataset ###
mu <- mean(core$rating)
mu

# checking the root mean squared error
naive_rmse <- RMSE(sub$rating, mu)
naive_rmse

# creating a results table
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

### adding movie effects ###
movie_avgs <- core %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# examining the distributions of constant movie effects
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- mu + sub %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
movie_effects_rmse<-RMSE(predicted_ratings, sub$rating)

rmse_results <- rbind(rmse_results, c("With movie effects", movie_effects_rmse))
rmse_results

### adding user effects ###
# examining the average rating per user
core %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

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

rmse_results <- rbind(rmse_results, c("With movie and user effects", movie_and_user_effects_rmse))
rmse_results


# Version 2 - validation method is "k-fold cross-validation"

# Define training control
set.seed(123) 

