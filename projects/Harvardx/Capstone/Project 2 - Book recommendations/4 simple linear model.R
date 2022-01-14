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
core$mean<-mu
naive_rmse <- RMSE(core$rating, core$mean)
naive_rmse

# creating a results table
rmse_results <- c("Just the average", round(naive_rmse,5))
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
user_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User effects", round(user_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding book effects ###
# estimating the book effects, by computing 
# the overall average and the user effect, and then 
# the book effect is the average of the remainder, after they
# are substracted from the rating (book effect= average of (rating - overall average - user effect)
age_bracket_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(age_bracket) %>%
  summarize(b_a = mean(rating - mu - b_u))

# examining the distributions of age_bracket effects
qplot(b_a, data = age_bracket_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(age_bracket_avgs, by='age_bracket') %>%
  mutate(pred = mu + b_u + b_a) %>%
  pull(pred)
user_and_age_bracket_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and age_bracket effects", round(user_and_age_bracket_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### examining book effects ###
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
user_and_book_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and book effects", round(user_and_book_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding country effects ###
country_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(country) %>%
  summarize(b_c = mean(rating - mu - b_u))

# examining the distributions of country effects
qplot(b_c, data = country_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(country_avgs, by='country') %>%
  mutate(pred = mu + b_u + b_c) %>%
  pull(pred)
user_and_country_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and country effects", round(user_and_country_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding author effects ###
author_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(author) %>%
  summarize(b_au = mean(rating - mu - b_u))

# examining the distributions of author effects
qplot(b_au, data = author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(author_avgs, by='author') %>%
  mutate(pred = mu + b_u + b_au) %>%
  pull(pred)
user_and_author_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and author effects", round(user_and_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding year effects ###
year_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_u))

# examining the distributions of year effects
qplot(b_y, data = year_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(year_avgs, by='year') %>%
  mutate(pred = mu + b_u + b_y) %>%
  pull(pred)
user_and_year_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and year effects", round(user_and_year_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding publisher effects ###
publisher_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(publisher) %>%
  summarize(b_p = mean(rating - mu - b_u))

# examining the distributions of publisher effects
qplot(b_p, data = publisher_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(publisher_avgs, by='publisher') %>%
  mutate(pred = mu + b_u + b_p) %>%
  pull(pred)
user_and_publisher_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and publisher effects", round(user_and_publisher_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding userid_author effects ###
userid_author_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(userid_author) %>%
  summarize(b_ua = mean(rating - mu - b_u))

# examining the distributions of userid_author effects
qplot(b_ua, data = userid_author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  mutate(pred = mu + b_u + b_ua) %>%
  pull(pred)
user_and_userid_author_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and User*Author effects", round(user_and_userid_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding author effects ###
author_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  group_by(author) %>%
  summarize(b_a = mean(rating - mu - b_u - b_ua))

# examining the distributions of userid_author effects
qplot(b_a, data = author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  left_join(author_avgs, by='author') %>%
  mutate(pred = mu + b_u + b_ua +b_a) %>%
  pull(pred)
user_and_userid_author_and_author_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and userid_author and author effects", round(user_and_userid_author_and_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

names(train)
