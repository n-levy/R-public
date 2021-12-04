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
# memory.limit(size = 10^9)

# cleaning memory
invisible(gc())

# # loading  data files
# setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# # ratings<-readRDS("ratings")
# # movies<-readRDS("movies")
# # movielens<-readRDS("movielens")
#  core<-readRDS("core")
#  sub<-readRDS("sub")
# validation<-readRDS("validation")
edx<-readRDS("edx")
# trainmat<-readRDS("trainmat")
# testmat<-readRDS("testmat")
# scheme_10<-readRDS("scheme_10")

### Preparing the data ###
### *** Begin with a small sample of 10K out of the 10M dataset, only afterwards proceed to the full sample *** ###
# Creating a sample of 0.1% of the training data, to try out the method #
set.seed(123) 
sampling_rate<-1
sample_index <- createDataPartition(y = edx$rating, times = 1, p = sampling_rate, list = FALSE)
# samp <- edx[sample_index,]
samp<-edx

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

# class(trainmat)

# removing items with few ratings because of low confidence in these ratings
# min_n_movies <- quantile(rowCounts(trainmat_full), 0.9)
# print(min_n_movies)

min_n_users <- quantile(colCounts(trainmat_full), 0.95)
min_n_users

trainmat_final <- trainmat_reduced[colCounts(trainmat_full) > min_n_users,]
dim(trainmat_final)

# saving
saveRDS(trainmat_final, file="trainmat_final")

# trainmat <- trainmat_full[rowCounts(trainmat_full) > min_n_movies,
#                           colCounts(trainmat_full) > min_n_users]

# checking number of ratings per item
number_of_ratings<-colCounts(trainmat_final)
min(number_of_ratings)
max(number_of_ratings)

# exploring the matrix
dim(trainmat_final)
trainmat_final@data[1500:1510, 2001:2009]

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
set.seed(123, sample.kind="Rounding")

# Setting up the evaluation scheme
scheme <- trainmat_final %>% 
  evaluationScheme(method = "split",
                   k=1,
                   train  = 0.9,  # 90% data train
                   given  = -8,
                   goodRating = 3
  )

scheme

# saving
saveRDS(scheme, file="scheme")

# measuring the rating error
result_rating_svdf <- evaluate(scheme,
                                  method = "svdf",
                                  parameter = list(normalize = "Z-score", k = 5),
                                  type  = "ratings"
)

# saving 
saveRDS(result_rating_svdf, file="result_rating_svdf_10")


# result_rating_svd <- evaluate(scheme,
#                               method = "svd",
#                               parameter = list(normalize = "Z-score", k = 5),
#                               type  = "ratings"
# )
# 
# 
# result_rating_popular <- evaluate(scheme, 
#                                   method = "popular",
#                                   parameter = list(normalize = "Z-score"),
#                                   type  = "ratings"
# )
# 
# result_rating_als <- evaluate(scheme,
#                               method = "als",
#                               parameter = list(normalize = "Z-score", k = 5),
#                               type  = "ratings"
# )


result_rating_svdf@results %>% 
  map(function(x) x@cm) %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = T) %>% 
  as.data.frame() %>% 
  summarise_all(mean) %>% 
  setNames(c("RMSE", "MSE", "MAE"))

###############################################################################


