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
edx<-readRDS("edx")
# trainmat<-readRDS("trainmat")
# testmat<-readRDS("testmat")
# scheme<-readRDS("scheme")
# full_scheme<-readRDS("full_scheme")
# trainmat<-readRDS("trainmat")
trainmat_final<-readRDS("trainmat_final")

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


# converting the training set into a matrix
users_and_ratings_train_set<-cbind.data.frame(edx$userId, edx$movieId, edx$rating)
dim(users_and_ratings_train_set)

# converting the matrix into a "realRatingMatrix")
trainmat <- as(users_and_ratings_train_set, "realRatingMatrix")
dim(trainmat)

# converting the matrix into a "realRatingMatrix")
trainmat <- as(users_and_ratings_train_set, "realRatingMatrix")
dim(trainmat)

# saving
saveRDS(trainmat, file="trainmat")

# class(trainmat)


###################  validation ######################


# removing items with few ratings because of low confidence in these ratings
# min_n_movies <- quantile(rowCounts(trainmat_full), 0.9)
# print(min_n_movies)

min_n_users <- quantile(colCounts(trainmat), 0.9)
min_n_users

trainmat_final <- trainmat[colCounts(trainmat) > min_n_users,]
dim(trainmat_final)

# saving
saveRDS(trainmat_final, file="trainmat_final")

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
scheme <- trainmat_final %>% 
  evaluationScheme(method = "split",
                   k=1,
                   train  = 0.9,  # 90% data train
                   given  = -8,
                   goodRating = 3.5
  )

# scheme
scheme<-scheme
# saving
saveRDS(scheme, file="scheme")

Sys.time()

##################### Evaluating the models ################################

### Popular ###
evaluating
result_rating_popular <- evaluate(scheme,
                                     method = "popular",
                                     parameter = list(normalize = "Z-score"),
                                     type  = "ratings"
)

# examining the results
result_rating_popular@results %>%
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
result_rating_svdf <- evaluate(scheme,
                                  method = "svdf",
                                  parameter = list(normalize = "Z-score", k = 5),
                                  type  = "ratings"
)

Sys.time()

# saving 
saveRDS(result_rating_svdf, file="result_rating_svdf")

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

recommendations_pop <- Recommender(trainmat, method = "popular")
recommendations_pop

Sys.time()

# saving
saveRDS(recommendations_pop, file="recommendations_pop")

#Making prediction on validation set:
predictions_pop <- predict(recommendations_pop, testmat, type="ratingMatrix")
predictions_pop

Sys.time()

class(predictions_pop)

# saving
saveRDS(predictions_pop, file="predictions_pop")

# turning the results into a matrix
predmat_pop<-as(predictions_pop, "matrix")
class(predictions_pop)

# calculating RMSE
rmse_pop<-RMSE(testmat, predmat_pop, na.rm=T)
rmse_pop

# saving
saveRDS(rmse_pop, file="rmse_pop")


############   svdf    #################

Sys.time()

recommendations_svdf <- Recommender(trainmat_final, method = "svdf")
recommendations_svdf<-recommendations_svdf

# rm(recommendations_svdf, scheme, trainmat_final, trainmat, validation)

saveRDS(recommendations_svdf, file="recommendations_svdf")

Sys.time()

#Making prediction on validation set:
predictions_svdf <- predict(recommendations_svdf, testmat, type="ratingMatrix")
predictions_svdf

Sys.time()

class(predictions_svdf)

# saving
saveRDS(predictions_svdf, file="predictions_svdf")

# turning the results into a matrix
predmat_svdf<-as(predictions_svdf, "matrix")
class(predictions_svdf)

# calculating RMSE
rmse_svdf<-RMSE(testmat, predmat_svdf, na.rm=T)
rmse_svdf

# saving
saveRDS(rmse_svdf, file="rmse_svdf")

############   ibcf    #################

Sys.time()

recommendations_ibcf <- Recommender(trainmat_final, method = "ibcf")

# rm(recommendations_ibcf, scheme, trainmat_final, trainmat, validation)

saveRDS(recommendations_ibcf, file="recommendations_ibcf")

Sys.time()

#Making prediction on validation set:
predictions_ibcf <- predict(recommendations_ibcf, testmat, type="ratingMatrix")
predictions_ibcf

Sys.time()

class(predictions)

# saving
saveRDS(predictions_ibcf, file="predictions_ibcf")

# turning the results into a matrix
predmat_ibcf<-as(predictions_ibcf, "matrix")
class(predmat_ibcf)

# saving
saveRDS(predmat_ibcf, file="predmat_ibcf")

dim(predmat_ibcf)
dim(testmat)

# turning testmat into a matrix
testmat_matrix<-as(testmat, "matrix")

# saving
saveRDS(testmat_matrix, file="testmat_matrix")

# calculating RMSE
rmse_ibcf<-RMSE(testmat_matrix, predmat_ibcf, na.rm=T)
rmse_ibcf

sum(!is.na(testmat_matrix))
sum(!is.na(predmat_ibcf))
nrow(predmat_ibcf)*ncol(predmat_ibcf)

diff<-testmat_matrix-predmat_ibcf
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

rmse_ibcf<-sum(diff_squared,na.rm=T)/number_of_ratings_in_test
rmse_ibcf

# saving
saveRDS(rmse_ibcf, file="rmse_ibcf")

### end of script ###

# checking for the number of identical users in both matrices
dim(edx)

identical <- edx %>% 
  semi_join(validation, by = "userId") 

dim(identical)

movieIds_training<-colnames(trainmat_final)
movieIds_test<-colnames(testmat_matrix)
same<-intersect(movieIds_training, movieIds_test)
length(same)

userIds_training<-rownames(trainmat_final)
userIds_test<-rownames(testmat_matrix)
same<-intersect(userIds_training, userIds_test)
length(same)
dim(trainmat_final)
