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
memory.limit(size = 10^10)

# cleaning memory
invisible(gc())

# # loading  data files
setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# ratings<-readRDS("ratings")
# movies<-readRDS("movies")
# movielens<-readRDS("movielens")
# core<-readRDS("core")
# sub<-readRDS("sub")
validation<-readRDS("validation")
 edx<-readRDS("edx")
 edx_reduced<-readRDS("edx_reduced")
# trainmat<-readRDS("trainmat")
# testmat<-readRDS("testmat")
# scheme_10<-readRDS("scheme_10")
# full_scheme<-readRDS("full_scheme")
# trainmat_reduced<-readRDS("trainmat_reduced")
trainmat_final_10<-readRDS("trainmat_final_10")

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

dim(edx)
dim(validation)
head(edx)
head(validation)
names(edx)
names(validation)

dim(edx$reduced)

# saving
saveRDS(edx_reduced, file="edx_reduced")

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
scheme_10<-scheme
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
saveRDS(recommendations_pop, file="recommendations_pop")


############   svdf    #################

Sys.time()

recommendations_svdf_10 <- Recommender(trainmat_final_10, method = "svdf")
recommendations_svdf_10<-recommendations_svdf

# rm(recommendations_svdf, scheme_10, trainmat_final_10, trainmat_reduced, validation)

saveRDS(recommendations_svdf_10, file="recommendations_svdf_10")

Sys.time()

#Making prediction on validation set:
predictions_svdf_10 <- predict(recommendations_svdf_10, testmat, type="ratingsMatrix")
predictions_svdf_10

Sys.time()

class(predictions_svdf_10)

# saving
saveRDS(predictions_svdf_10, file="predictions_svdf_10")

# turning the results into a matrix
predmat_svdf_10<-as(predictions_svdf_10, "matrix")
class(predictions_svdf_10)

# calculating RMSE
rmse_svdf_10<-RMSE(testmat, predmat_svdf_10, na.rm=T)
rmse_svdf_10

# saving
saveRDS(rmse_svdf_10, file="rmse_svdf_10")

############   ibcf    #################

Sys.time()

recommendations_ibcf_10 <- Recommender(trainmat_final_10, method = "ibcf")

# rm(recommendations_ibcf, scheme_10, trainmat_final_10, trainmat_reduced, validation)

saveRDS(recommendations_ibcf_10, file="recommendations_ibcf_10")

Sys.time()

#Making prediction on validation set:
predictions_ibcf_10 <- predict(recommendations_ibcf_10, testmat, type="ratingMatrix")
predictions_ibcf_10

Sys.time()

class(predictions)

# saving
saveRDS(predictions_ibcf_10, file="predictions_ibcf_10")

# turning the results into a matrix
predmat_ibcf_10<-as(predictions_ibcf_10, "matrix")
class(predmat_ibcf_10)

# saving
saveRDS(predmat_ibcf_10, file="predmat_ibcf_10")

dim(predmat_ibcf_10)
dim(testmat)

# turning testmat into a matrix
testmat_matrix<-as(testmat, "matrix")

# saving
saveRDS(testmat_matrix, file="testmat_matrix")

# calculating RMSE
rmse_ibcf_10<-RMSE(testmat_matrix, predmat_ibcf_10, na.rm=T)
rmse_ibcf_10

sum(!is.na(testmat_matrix))
sum(!is.na(predmat_ibcf_10))
nrow(predmat_ibcf_10)*ncol(predmat_ibcf_10)

diff<-testmat_matrix-predmat_ibcf_10
sum(!is.na(diff))

# saving
saveRDS(diff, file="diff")

diff_squared<-diff^2
sum(!is.na(diff_squared))
number_of_ratings_in_test<-sum(!is.na(diff_squared))

# saving
saveRDS(diff_squared, file="diff_squared")
min(diff_squared)
min(diff_squared, na.rm=T)
min(validation$rating)
sum(!is.na(validation$rating))

rmse_ibcf_10<-sum(diff_squared,na.rm=T)/number_of_ratings_in_test
rmse_ibcf_10

# saving
saveRDS(rmse_ibcf_10, file="rmse_ibcf_10")

### end of script ###

# checking for the number of identical users in both matrices
dim(edx_reduced)

identical <- edx_reduced %>% 
  semi_join(validation, by = "userId") 

dim(identical)

movieIds_training<-colnames(trainmat_final_10)
movieIds_test<-colnames(testmat_matrix)
same<-intersect(movieIds_training, movieIds_test)
length(same)

userIds_training<-rownames(trainmat_final_10)
userIds_test<-rownames(testmat_matrix)
same<-intersect(userIds_training, userIds_test)
length(same)
dim(trainmat_final_10)
