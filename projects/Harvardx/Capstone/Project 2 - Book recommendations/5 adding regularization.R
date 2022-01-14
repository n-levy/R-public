################################
### 5. Adding regularization ###
################################

### We will use the model with user and userid_author effects,
### since it produced the lowest RMSE (and adding author effects did not make any difference)

### Choosing lambda
lambdas <- seq(0, 10, 0.25)

mu <- mean(core$rating)
just_the_sum <- core %>% 
  group_by(bookid) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- sub %>% 
    left_join(just_the_sum, by='bookid') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, sub$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

### Regularizing with lambda = 3
lambda <- 8.5
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

### Calculating RMSE ###
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs_reg, by='userid_author') %>%
    mutate(pred = mu + b_u + b_ua) %>%
  pull(pred)
user_and_userid_author_effects_with_regularization_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

# checking the RMSE
rmse_results <- rbind.data.frame(rmse_results, c("User and User*Author effects with regularization", round(user_and_userid_author_effects_with_regularization_rmse,5)))
# rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_and_user_effects_with_regularization_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### saving files
saveRDS(core, "core")
saveRDS(sub, "sub")
saveRDS(train, "train")
saveRDS(rmse_results, "rmse_results")
