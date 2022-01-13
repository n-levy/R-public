#########################
### 4. A simple model ###
#########################

### To keep things simple, we will use the method of "Leave One Out Cross Validation" (LOOCV)

### creating a subset of the training set, for evaluating the model ###
### creating index of 20% 
suppressWarnings(set.seed(1, sample.kind="Rounding")) # setting the seed
test_index <- createDataPartition(y = train$rating, times = 1, p = 0.2, list = FALSE) # defining a 50% split
core <- train[-test_index,]
temp <- train[test_index,]

# making sure that the temporary split is 80-20
length(test_index)/nrow(train)

# creating a 'sub' set that includes only userids and bookids that are also in the training set
sub <- temp %>% 
  semi_join(train, by = "bookid") %>%
  semi_join(train, by = "userid")

# adding rows that were included in the index, but not in the test set, to the training set
removed <- anti_join(temp, sub)
core <- rbind(core, removed)

### making sure that the number of rows in the training and test sets is equal to 
### the number of rows in the full dataset
nrow(train)-(nrow(core)+nrow(sub)) # this should be zero

### checking the final relative sizes of the training and test sets
nrow(core)/nrow(train) # the core set is ~80% of the training data
nrow(sub)/nrow(train) # the sub set is ~20% of the training data

### The first model ###
# Predicting only according to the average rating in the core dataset ###
mu <- mean(core$rating)
mu

# checking the root mean squared error
core$average<-mu
naive_rmse <- RMSE(core$rating, core$average)
naive_rmse

# creating a results table
rmse_results <- c("Just the average", round(naive_rmse,3))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding user effects ###
user_avgs <- core %>% 
  group_by(userid) %>% 
  summarize(b_u = mean(rating - mu))

# examining the distributions of user effects
qplot(b_u, data = user_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- mu + sub %>% 
  left_join(user_avgs, by='userid') %>%
  pull(b_u)
user_effects_rmse<-RMSE(predicted_ratings, sub$rating)

rmse_results <- rbind.data.frame(rmse_results, c("With user effects", round(user_effects_rmse,3)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding book effects ###
# estimating the book effects, by computing 
# the overall average and the user effect, and then 
# the book effect is the average of the remainder, after they
# are substracted from the rating (book effect= average of (rating - overall average - user effect)
book_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(bookid) %>%
  summarize(b_i = mean(rating - mu - b_u))

# examining the distributions of book effects
qplot(b_i, data = book_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(book_avgs, by='bookid') %>%
  mutate(pred = mu + b_u + b_i) %>%
  pull(pred)
user_and_book_effects_rmse<-RMSE(predicted_ratings, sub$rating)

rmse_results <- rbind.data.frame(rmse_results, c("With user and book effects", round(user_and_book_effects_rmse,3)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding age bracket effects
age_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(book_avgs, by='bookid') %>%
  group_by(age_bracket) %>% 
  summarize(b_a = mean(rating - mu-b_i-b_u))

# examining the distributions of book effects
qplot(b_a, data = age_avgs, bins = 10, color = I("black"))
# reached here

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(book_avgs, by='bookid') %>%
  left_join(age_avgs, by='age_bracket') %>%
  mutate(pred = mu + b_u + b_i + b_a) %>%
  pull(pred)
user_book_age_effects_rmse<-RMSE(predicted_ratings, sub$rating)

rmse_results <- rbind.data.frame(rmse_results, c("With user, book and age effects", round(user_book_age_effects_rmse,3)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

