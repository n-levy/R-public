######################################
####### Matrix factorization #########
######################################

### Loading libraries ###
library(tidyverse)
library(caret)
# library(data.table)
library(dplyr)
# library(kableExtra)
library(recommenderlab)
library(Matrix)
# library(BBmisc)
# library(DT)
# library(pander)

# cleaning the environment
# rm(list = ls())

### Increasing memory
# Checking memory limit
# memory.limit()
# Change memory limit
# memory.limit(size = 10^10)

# cleaning memory
invisible(gc())

# # loading  data files
# setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# ratings<-readRDS("ratings")
# movies<-readRDS("movies")
# movielens<-readRDS("movielens")
# core<-readRDS("core")
# sub<-readRDS("sub")
validation<-readRDS("validation")
edx<-readRDS("edx")
# trainmat<-readRDS("trainmat")
# testmat<-readRDS("testmat")
# scheme_10<-readRDS("scheme_10")
# full_scheme<-readRDS("full_scheme")
trainmat_reduced<-readRDS("trainmat_reduced")
recommendations_pop_10<-readRDS("recommendations_pop_10")
testmat<-readRDS("testmat")

### Preparing the data ###
### *** Begin with a small sample of 10K out of the 10M dataset, only afterwards proceed to the full sample *** ###
# Creating a sample of 0.1% of the training data, to try out the method #
# set.seed(123) 
# sampling_rate<-1
# sample_index <- createDataPartition(y = edx$rating, times = 1, p = sampling_rate, list = FALSE)
# samp <- edx[sample_index,]
# samp<-edx

# exploring the sample
# dim(samp)
# names(samp)
# head(samp)
# class(samp$userId)
# class(samp$movieId)
# class(samp$rating)

# removing movies in the training set that do not appear in the test set
edx_reduced <- edx %>% 
  semi_join(validation, by = "movieId") 

# converting the training set into a matrix
users_and_ratings_train_set<-cbind.data.frame(edx_reduced$userId, edx_reduced$movieId, edx_reduced$rating)
dim(users_and_ratings_train_set)

# converting the matrix into a "realRatingMatrix")
trainmat_reduced <- as(users_and_ratings_train_set, "realRatingMatrix")
dim(trainmat_reduced)

# converting the matrix into a "realRatingMatrix")
trainmat_reduced <- as(users_and_ratings_train_set, "realRatingMatrix")
dim(trainmat_reduced)

# saving
saveRDS(trainmat_reduced, file="trainmat_reduced")

# removing unnecessary files
rm(edx, edx_reduced, users_and_ratings_train_set)

# class(trainmat)

# removing items with few ratings because of low confidence in these ratings
# min_n_movies <- quantile(rowCounts(trainmat_full), 0.9)
# print(min_n_movies)

min_n_users <- quantile(colCounts(trainmat_reduced), 0.9)
min_n_users

trainmat_final_10 <- trainmat_reduced[colCounts(trainmat_reduced) > min_n_users,]
dim(trainmat_final_10)

# saving
saveRDS(trainmat_final_10, file="trainmat_final_10")

# trainmat <- trainmat_full[rowCounts(trainmat_full) > min_n_movies,
#                           colCounts(trainmat_full) > min_n_users]

# checking number of ratings per item
# number_of_ratings<-colCounts(trainmat_final)
# min(number_of_ratings)
# max(number_of_ratings)

# exploring the matrix
# dim(trainmat_final)
# trainmat_final@data[1500:1510, 2001:2009]

# normalizing the values
# normalize(trainmat, method = "Z-score")

# saving
# saveRDS(trainmat, file="trainmat")

# checking the initial/default parameters of the SVDF model
# recommenderRegistry$get_entry("SVDF", dataType = "realRatingMatrix")

# recommending
# recom_svdf <- Recommender(data = trainmat,
#                           method = "SVDF",
#                           parameter = list(normalize = "Z-score")
# )

# Evaluating the model by cross-validation
# set.seed(1, sample.kind="Rounding")

# Setting up the evaluation scheme
scheme_10 <- trainmat_final_10 %>% 
  evaluationScheme(method = "split",
                   k=1,
                   train  = 0.9,  # 90% data train
                   given  = -8,
                   goodRating = 3.5
  )

# scheme
scheme_10

# saving
saveRDS(scheme_10, file="scheme_10")

Sys.time()

##################### Evaluating the models ################################

### Popular ###
evaluating
result_rating_popular_10 <- evaluate(scheme_10,
                                     method = "popular",
                                     parameter = list(normalize = "Z-score"),
                                     type  = "ratings"
)

# examining the results
result_rating_popular_10@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

Sys.time()

# saving 
# saveRDS(result_rating_popular, file="result_rating_popular")

### svd ###
# evaluating
# result_rating_svd <- evaluate(scheme,
#                               method = "svd",
#                               parameter = list(normalize = "Z-score", k = 5),
#                               type  = "ratings"
# )
# 
# # examining the results
# result_rating_svd@results %>% 
#   map(function(x) x@cm) %>% 
#   unlist() %>% 
#   matrix(ncol = 3, byrow = T) %>% 
#   as.data.frame() %>% 
#   summarise_all(mean) %>% 
#   setNames(c("RMSE", "MSE", "MAE"))
# 
# Sys.time()

# saving 
# saveRDS(result_rating_svd, file="result_rating_svd")

### als ###
# evaluating
# result_rating_als <- evaluate(scheme,
#                               method = "als",
#                               parameter = list(normalize = "Z-score"),
#                               type  = "ratings"
# )
# 
# # examining the results
# result_rating_als@results %>% 
#   map(function(x) x@cm) %>% 
#   unlist() %>% 
#   matrix(ncol = 3, byrow = T) %>% 
#   as.data.frame() %>% 
#   summarise_all(mean) %>% 
#   setNames(c("RMSE", "MSE", "MAE"))
# 
# Sys.time()
# 
# # saving 
# saveRDS(result_rating_als, file="result_rating_als")

### svdf ###

# evaluating
result_rating_svdf_10 <- evaluate(scheme_10,
                                  method = "svdf",
                                  parameter = list(normalize = "Z-score", k = 5),
                                  type  = "ratings"
)

Sys.time()

# saving 
saveRDS(result_rating_svdf_10, file="result_rating_svdf_10")

# examining the results
result_rating_svdf@results %>% 
  map(function(x) x@cm) %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = T) %>% 
  as.data.frame() %>% 
  summarise_all(mean) %>% 
  setNames(c("RMSE", "MSE", "MAE"))


Sys.time()

##################################################################################
##################### Test Set ###################################################
##################################################################################


# converting the testing set into a matrix
users_and_ratings_test_set<-cbind.data.frame(validation$userId, validation$movieId, validation$rating)
dim(users_and_ratings_test_set)
# head(users_and_ratings_test_set)

testmat <- as(users_and_ratings_test_set, "realRatingMatrix")
dim(testmat)

# checking number of ratings per item
number_of_ratings<-colCounts(testmat)
min(number_of_ratings)
max(number_of_ratings)

# exploring the matrix
dim(testmat)
# testmat@data[1500:1510, 2001:2009]

# normalizing the values
# normalize(testmat, method = "Z-score")

# saving
saveRDS(testmat, file="testmat")

# checking the initial/default parameters of the SVDF model
# recommenderRegistry$get_entry("SVDF", dataType = "realRatingMatrix")

# recommending
# recom_svdf <- Recommender(data = testmat,
#                           method = "SVDF",
#                           parameter = list(normalize = "Z-score")
# )

Sys.time()

# Creating the recommendations

Sys.time()

recommendations_pop_10 <- Recommender(trainmat_final_10, method = "popular")
recommendations_pop_10

Sys.time()

# saving
saveRDS(recommendations_pop_10, file="recommendations_pop_10")

Sys.time()

recommendations_svdf_10 <- Recommender(trainmat_final_10, method = "svdf")
recommendations_svdf_10

saveRDS(recommendations_svdf_10, file="recommendations_pop")

recommendations_svdf_10<-recommendations_svdf

Sys.time()

#Making prediction on validation set:
predictions_pop_10 <- predict(recommendations_pop_10, testmat, type="ratings")
predictions

Sys.time()

class(predictions)

# saving
saveRDS(predictions_pop_10, file="predictions")

# turning the results into a matrix
predmat<-as(predictions, "matrix")
class(predmat)

# calculating RMSE
rmse_svdf<-RMSE(testmat, predmat, na.rm=T)
rmse_svdf

### end of script ###
