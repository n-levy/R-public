################################
### 5. Adding regularization ###
################################

### We will use the model with user and userid_author effects,
### since it produced the lowest RMSE (and adding author effects did not make any difference)

### Regularizing with lambda = 3
lambda <- 3
mu <- mean(core$rating)
mu

### user effects are calculated in the same way as without regularization
user_avgs <- core %>% 
  group_by(userid) %>% 
  summarize(b_u = mean(rating - mu))

### userid_author effects are calculated differently 
userid_author_avgs_reg <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(userid_author) %>%
  summarize(b_ua = sum(rating - mu - b_u)/(n()+lambda), n_ua = n())

# Making a plot to see how the estimates shrunk
tibble(original = userid_author_avgs$b_ua,
       regularlized = userid_author_avgs_reg$b_ua,
       n = userid_author_avgs_reg$n_ua) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) +
  geom_point(shape=1, alpha=0.5)

### Calculating RMSE ###
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs_reg, by='userid_author') %>%
    mutate(pred = mu + b_u + b_ua) %>%
  pull(pred)
user_and_userid_author_effects_with_regularization_rmse<-RMSE(predicted_ratings, sub$rating)

# checking the RMSE
rmse_results <- rbind.data.frame(rmse_results, c("User and User*Author effects with regularization", round(user_and_userid_author_effects_with_regularization_rmse,5)))
# rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_and_user_effects_with_regularization_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

