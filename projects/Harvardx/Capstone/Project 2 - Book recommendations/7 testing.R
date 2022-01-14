########################################################
### 7. Applying the chosen algorithm to the test set ###
########################################################

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### loading the datasets
train<-readRDS("train")
test<-readRDS("test")
rmse_results<-readRDS("rmse_results")

### calculating the mean rating
mu <- mean(train$rating)
mu

### adding user effects ###
user_avgs <- train %>% 
  group_by(userid) %>% 
  summarize(b_u = mean(rating - mu))

# examining the distributions of user effects
qplot(b_u, data = user_avgs, bins = 10, color = I("black"))

### adding userid_author effects ###
userid_author_avgs <- train %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(userid_author) %>%
  summarize(b_ua = mean(rating - mu - b_u))

# examining the distributions of userid_author effects
qplot(b_ua, data = userid_author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- test %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  mutate(pred = mu + b_u + b_ua) %>%
  pull(pred)
user_and_userid_author_effects_rmse<-RMSE(predicted_ratings, test$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("Test set - user and User*Author effects", round(user_and_userid_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

