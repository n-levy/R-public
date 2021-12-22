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
validation<-readRDS("validation")
edx<-readRDS("edx")
trainmat_reduced_reg<-readRDS("trainmat_reduced_reg")

### Preparing the data ###
# removing movies in the training set that do not appear in the test set
edx_reduced <- edx %>% 
  semi_join(validation, by = "movieId") 

# exploring the datasets
dim(edx)
dim(validation)
head(edx)
head(validation)
names(edx)
names(validation)
dim(edx_reduced)

# saving
saveRDS(edx_reduced, file="edx_reduced")

# converting the training set into a matrix
users_and_ratings_train_set<-cbind.data.frame(edx_reduced$userId, edx_reduced$movieId, edx_reduced$rating)
dim(users_and_ratings_train_set)

# converting the matrix into a "realRatingMatrix")
trainmat_reduced <- as(users_and_ratings_train_set, "realRatingMatrix")
dim(trainmat_reduced)

# saving
saveRDS(trainmat_reduced, file="trainmat_reduced")

# creating a regular matrix
trainmat_reduced_reg<-as(trainmat_reduced, "matrix")

# saving
saveRDS(trainmat_reduced_reg, file="trainmat_reduced_reg")

# exploring the matrix #
class(trainmat_reduced_reg)
n_missing<-sum(is.na(trainmat_reduced_reg)) # counting missing values
n_missing
all<-nrow(trainmat_reduced_reg)*ncol(trainmat_reduced_reg) # counting all values
all
p_nonmissing<-n_nonmissing/(all) # calculating the percentage of nonmissing values
p_nonmissing 
rm(trainmat_reduced_reg, n_missing, all) # removing the matrix and these variables

# Calculating the 90th percentile of the number of ratings per user
min_n_users <- quantile(rowCounts(trainmat_reduced), 0.9)
min_n_users

# making sure the calculation is correct
nrow(trainmat_reduced)
length(rowCounts(trainmat_reduced))

# Keeping only users who gave many ratings (the top 10% of the number of ratings)
# to reduce computation time. Otherwise the analysis runs
# very slowly on my computer

trainmat_final_10 <- trainmat_reduced[rowCounts(trainmat_reduced) > min_n_users,]
dim(trainmat_final_10)

# saving
saveRDS(trainmat_final_10, file="trainmat_final_10")

# checking number of ratings per item
number_of_ratings<-colCounts(trainmat_final_10)
min(number_of_ratings)
max(number_of_ratings)

# exploring the matrix
 dim(trainmat_final_10)
 trainmat_final_10@data[1500:1510, 2001:2009]

# normalizing the values
 normalize(trainmat_final_10, method = "Z-score")

# saving
saveRDS(trainmat_final_10, file="trainmat")

# Setting up the evaluation scheme (Leave one out cross validation)
scheme_10 <- trainmat_final_10 %>% 
  evaluationScheme(method = "split",
                   k=1,
                   train  = 0.9,  # 90% data train
                   given  = -8,
                   goodRating = 3.5
  )

# saving
saveRDS(scheme_10, file="scheme_10")

Sys.time()

##################### Evaluating the models ################################

### Popular ###
# evaluating the "popular" model
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
saveRDS(result_rating_popular_10, file="result_rating_popular_10")

### svd ###
# evaluating the svd model
result_rating_svd_10 <- evaluate(scheme_10,
                              method = "svd",
                              parameter = list(normalize = "Z-score", k = 5),
                              type  = "ratings"
)

# examining the results
result_rating_svd_10@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))
 
# Sys.time()

# saving 
saveRDS(result_rating_svd_10, file="result_rating_svd_10")

Sys.time()
 
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
result_rating_svdf_10@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

### ibcf ###

# evaluating
result_rating_ibcf_10 <- evaluate(scheme_10,
                                  method = "ibcf",
                                  parameter = list(normalize = "Z-score", k = 5),
                                  type  = "ratings"
)

Sys.time()

# saving 
saveRDS(result_rating_ibcf_10, file="result_rating_ibcf_10")

# examining the results
result_rating_ibcf_10@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

### ubcf ###

# evaluating
result_rating_ubcf_10 <- evaluate(scheme_10,
                                  method = "ubcf",
                                  parameter = list(normalize = "Z-score", k = 5),
                                  type  = "ratings"
)

Sys.time()

# saving 
saveRDS(result_rating_ubcf_10, file="result_rating_ubcf_10")

# examining the results
result_rating_ubcf_10@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))
