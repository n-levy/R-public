###########################
### 6. Other algorithms ###
###########################

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### reloading the train set
train<-readRDS("train")
test<-readRDS("test")
rmse_results<-readRDS("rmse_results")

### Preparing the data ###
# removing books in the training set that do not appear in the test set
trainmat_final <- train %>% 
  semi_join(test, by = "bookid") 

### exploring the datasets
dim(train)
dim(trainmat_final)

### saving
saveRDS(trainmat_final, file="trainmat_final")

### converting the training set into a matrix
users_and_ratings_train_set<-cbind.data.frame(trainmat_final$userid, trainmat_final$bookid, trainmat_final$rating)
names(users_and_ratings_train_set)<-c("userid", "bookid", "rating")

### exploring
dim(users_and_ratings_train_set)
head(users_and_ratings_train_set)

### counting missing values
n_missing<-sum(is.na(users_and_ratings_train_set))

### removing rows with missing bookids 
### (that is the only column that contains missing values)
index<-which(is.na(users_and_ratings_train_set$bookid))
head(index)
length(index)
users_and_ratings_train_set<-users_and_ratings_train_set[-index,]

### making sure that it worked
dim(users_and_ratings_train_set)

### making sure that there are no missing values left
sum(is.na(users_and_ratings_train_set)) # this should be zero

### converting the matrix into a "realRatingMatrix")
trainmat_final <- as(users_and_ratings_train_set, "realRatingMatrix")
dim(trainmat_final)

### saving
saveRDS(trainmat_final, file="trainmat_final")

### creating a regular matrix
trainmat_final_reg<-as(trainmat_final, "matrix")

### saving
saveRDS(trainmat_final_reg, file="trainmat_final_reg")

# exploring the matrix #
class(trainmat_final_reg)
n_missing<-sum(is.na(trainmat_final_reg)) # counting missing values
n_missing
n_non_missing<-sum(!is.na(trainmat_final_reg)) # counting non-missing values
n_non_missing
all<-nrow(trainmat_final_reg)*ncol(trainmat_final_reg) # counting all values
all
p_missing<-n_missing/all # calculating the percentage of missing values
p_missing 
p_non_missing<-1-p_missing
p_non_missing
rm(trainmat_final_reg, n_missing, all) # removing the matrix and other unnecessary objects

# Increasing memory size
memory.limit(size = 10^10)

# cleaning memory
invisible(gc())

# checking number of ratings per item
number_of_ratings<-colCounts(trainmat_final)
min(number_of_ratings)
max(number_of_ratings)

# exploring a sample of the matrix
trainmat_final@data[1500:1510, 2001:2009]

# normalizing the values
normalize(trainmat_final, method = "Z-score")

# saving
saveRDS(trainmat_final, file="trainmat_final")

# Setting up the evaluation scheme (leave one out cross validation (LOOCV), 
# reserving 10% of the training set for validation)

Sys.time() # recording the time in order to see how long each step takes

scheme <- trainmat_final %>% 
  evaluationScheme(method = "split",
                   k=1,
                   train  = 0.9,  # 90% data train
                   given  = 1,
                   goodRating = 7.5
  )

Sys.time() # recording the time in order to see how long each step takes

# saving
saveRDS(scheme, file="scheme")

Sys.time()

##################### Evaluating the models ################################

### Popular ###
# evaluating the "popular" model

Sys.time() # recording the time in order to see how long each step takes

result_rating_popular <- evaluate(scheme,
                                  method = "popular",
                                  parameter = list(normalize = "Z-score"),
                                  type  = "ratings"
)

# examining the results
results_pop<-result_rating_popular@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_pop

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("Popular model", round(results_pop$RMSE,5)))
rmse_results

Sys.time() # recording the time in order to see how long each step takes

# saving 
saveRDS(result_rating_popular, file="result_rating_popular")

### svd ###
# evaluating the svd model
result_rating_svd <- evaluate(scheme,
                              method = "svd",
                              parameter = list(normalize = "Z-score", k = 5),
                              type  = "ratings"
)

# examining the results
results_svd<-result_rating_svd@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_svd

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("Singular Value Decomposition", round(results_svd$RMSE,5)))
rmse_results

# saving 
saveRDS(result_rating_svd, file="result_rating_svd")

Sys.time() # recording the time in order to see how long each step takes

### ibcf ###

# evaluating
result_rating_ibcf <- evaluate(scheme,
                               method = "ibcf",
                               parameter = list(normalize = "Z-score"),
                               type  = "ratings"
)

Sys.time() # recording the time in order to see how long each step takes

# examining the results
results_ibcf<-result_rating_ibcf@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_ibcf

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("Item Based Collaborative Filtering", round(results_ibcf$RMSE,5)))
rmse_results

Sys.time() # recording the time in order to see how long each step takes

# saving 
saveRDS(result_rating_ibcf, file="result_rating_ibcf")

### ubcf ###

# evaluating
result_rating_ubcf <- evaluate(scheme,
                               method = "ubcf",
                               parameter = list(normalize = "Z-score", k = 5),
                               type  = "ratings"
)

Sys.time() # recording the time in order to see how long each step takes

# examining the results
results_ubcf<-result_rating_ubcf@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_ubcf

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("User Based Collaborative Filtering", round(results_ubcf$RMSE,5)))
rmse_results

Sys.time() # recording the time in order to see how long each step takes

# saving 
saveRDS(result_rating_ubcf, file="result_rating_ubcf")

### svdf ###

# evaluating
result_rating_svdf <- evaluate(scheme,
                               method = "svdf",
                               parameter = list(normalize = "Z-score", k = 5),
                               type  = "ratings"
)

Sys.time() # recording the time in order to see how long each step takes

# examining the results
results_svdf<-result_rating_svdf@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_svdf

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("Fund Singular Value Decomposition", round(results_svdf$RMSE,5)))
rmse_results

Sys.time() # recording the time in order to see how long each step takes

# saving 
saveRDS(result_rating_svdf, file="result_rating_svdf")
