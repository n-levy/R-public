######################################
####### Matrix factorization #########
######################################

### Loading libraries ###
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
# library(kableExtra)
library(recommenderlab)
library(Matrix)
# library(BBmisc)
# library(DT)
# library(pander)

### Increasing memory
# Checking memory limit
memory.limit()
# Change memory limit
memory.limit(size = 10^9)

# # loading  data files
setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# # ratings<-readRDS("ratings")
# # movies<-readRDS("movies")
# # movielens<-readRDS("movielens")
#  core<-readRDS("core")
#  sub<-readRDS("sub")
# validation<-readRDS("validation")
edx<-readRDS("edx")
# trainmat<-readRDS("trainmat")
# testmat<-readRDS("testmat")

### Preparing the data ###
### *** Begin with a small sample of 10K out of the 10M dataset, only afterwards proceed to the full sample *** ###
# Creating a sample of 0.1% of the training data, to try out the method #
set.seed(123) 
sampling_rate<-1
sample_index <- createDataPartition(y = edx$rating, times = 1, p = sampling_rate, list = FALSE)
samp <- edx[sample_index,]

# exploring the sample
# dim(samp)
# names(samp)
# head(samp)
# class(samp$userId)
# class(samp$movieId)
# class(samp$rating)

# converting the training set into a matrix
users_and_ratings_training_set<-cbind.data.frame(samp$userId, samp$movieId, samp$rating)
dim(users_and_ratings_training_set)
# head(users_and_ratings_training_set)

# converting the matrix into a "realRatingMatrix")
trainmat <- as(users_and_ratings_training_set, "realRatingMatrix")
dim(trainmat)
class(trainmat)

# removing items with few ratings because of low confidence in these ratings
# min_num_ratings<-30
# trainmat<- trainmat[colCounts(trainmat) > min_num_ratings]

# exploring the matrix
dim(trainmat)
# trainmat@data[1500:1510, 2001:2009]

# normalizing the values
normalize(trainmat, method = "Z-score")

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
set.seed(123)

# Setting up the evaluation scheme
scheme <- trainmat %>% 
  evaluationScheme(method = "split",
                   k=1,
                   train  = 0.9,  # 90% data train
                   given  = -1,
                   goodRating = 0
  )

scheme

# saving
saveRDS(scheme, file="full_scheme")

# measuring the rating error
result_rating_svdf <- evaluate(scheme,
                          method = "svdf",
                          parameter = list(normalize = "Z-score", k = 5),
                          type  = "ratings"
)
 
# result_rating_svd <- evaluate(scheme,
#                           method = "svd",
#                           parameter = list(normalize = "Z-score", k = 1),
#                           type  = "ratings"
# )

result_rating_popular <- evaluate(scheme, 
                          method = "popular",
                          parameter = list(normalize = "Z-score"),
                          type  = "ratings"
)

result_rating_als <- evaluate(scheme,
                               method = "als",
                               parameter = list(normalize = "Z-score", k = 5),
                               type  = "ratings"
)


###############################################################################
# Alternative method
#Calculation of rmse for popular method 
set.seed(123)
# a. POPULAR , UBCF and IBCF algorithms of the recommenderlab package

model_pop <- Recommender(ratings_movies, method = "POPULAR", 
                         param=list(normalize = "center"))

#prediction example on the first 10 users
pred_pop <- predict(model_pop, ratings_movies[1:10], type="ratings")
as(pred_pop, "matrix")[,1:10]

#Calculation of rmse for popular method 
set.seed(1)
e <- evaluationScheme(ratings_movies, method="split", train=0.7, given=-5)
#5 ratings of 30% of users are excluded for testing

model_pop <- Recommender(getData(e, "train"), "POPULAR")

prediction_pop <- predict(model_pop, getData(e, "known"), type="ratings")

rmse_popular <- calcPredictionAccuracy(prediction_pop, getData(e, "unknown"))[1]
rmse_popular

e <- evaluationScheme(trainmat, method="split", train=0.7, given=-5)
#5 ratings of 30% of users are excluded for testing

model_pop <- Recommender(getData(e, "train"), "POPULAR")

prediction_pop <- predict(model_pop, getData(e, "known"), type="ratings")

rmse_popular <- calcPredictionAccuracy(prediction_pop, getData(e, "unknown"))[1]
rmse_popular

# # Trying without normalization, see if the RMSE changes
# 
# scheme2 <- trainmat %>% 
#   evaluationScheme(method = "cross-validation",
#                    k=1,
#                    train  = 0.9,  # 90% data train
#                    given  = -1,
#                    goodRating = 3
#   )
# 
# scheme2
# 
# # saving
# saveRDS(scheme2, file="scheme2")
# 
# 
# result_rating_popular_not_normalized <- evaluate(scheme2, 
#                                                  method = "popular",
#                                                  parameter = list(normalize = "Z-score", k = 5),
#                                                  type  = "ratings"
# )



# summarizing the mean of three performance measures from each fold
# result_rating_svdf@results %>% 
#   map(function(x) x@cm) %>% 
#   unlist() %>% 
#   matrix(ncol = 3, byrow = T) %>% 
#   as.data.frame() %>% 
#   summarise_all(mean) %>% 
#   setNames(c("RMSE", "MSE", "MAE"))
# 
# result_rating_svd@results %>% 
#   map(function(x) x@cm) %>% 
#   unlist() %>% 
#   matrix(ncol = 3, byrow = T) %>% 
#   as.data.frame() %>% 
#   summarise_all(mean) %>% 
#   setNames(c("RMSE", "MSE", "MAE"))

result_rating_popular@results %>% 
  map(function(x) x@cm) %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = T) %>% 
  as.data.frame() %>% 
  summarise_all(mean) %>% 
  setNames(c("RMSE", "MSE", "MAE"))

### Comparing models
# specifying the algorithms
# algorithms <- list(
#   "Random items" = list(name = "RANDOM"),
#   "Popular items" = list(name = "POPULAR"),
#   "SVD" = list(name = "SVD"),
#   "UBCF" = list(name = "UBCF"),
#   "item-based CF" = list(name = "IBCF"))
# 
# ### Running the algorithms
# result_error <- evaluate(scheme, 
#                          algorithms, 
#                          type  = "ratings"
# )
# 
# ### Visualizing the results
# get_error <- function(x){
#   x %>% 
#     map(function(x) x@cm) %>% 
#     unlist() %>% 
#     matrix(ncol = 3, byrow = T) %>% 
#     as.data.frame() %>% 
#     summarise_all(mean) %>% 
#     setNames(c("RMSE", "MSE", "MAE"))
# }
# 
# result_error_svdf <- result_rating@results %>% 
#   get_error() %>% 
#   mutate(method = "Funk SVD")
# 
# map2_df(.x = result_error@.Data, 
#         .y = c("Random", "Popular", "SVD", "UBCF", "IBCF"), 
#         .f = function(x,y) x@results %>% get_error() %>% mutate(method = y)) %>% 
#   bind_rows(result_error_svdf) %>%
#   pivot_longer(-method) %>% 
#   mutate(method = tidytext::reorder_within(method, -value, name)) %>% 
#   ggplot(aes(y =  method, 
#              x =  value)) +
#   geom_segment(aes(x = 0, xend = value, yend = method)) +
#   geom_point(size = 2.5, color = "firebrick" ) +
#   tidytext::scale_y_reordered() +
#   labs(y = NULL, x = NULL, title = "Model Comparison") +
#   facet_wrap(~name, scales = "free_y") +
#   theme_minimal()
  
# <This is where I reached> *

# # converting the test set into a matrix
# users_and_ratings_test_set<-cbind.data.frame(validation$userId, validation$movieId, validation$rating)
# dim(users_and_ratings_test_set)
# 
# # converting the matrix into a "realRatingMatrix")
# testmat <- as(users_and_ratings_test_set, "realRatingMatrix")
# dim(testmat)
# 
# # saving
# saveRDS(testmat, file="testmat")
# 
# rm(users_and_ratings_training_set, users_and_ratings_test_set)
# 
### Matrix factorization of training matrix ###
train <- as(trainmat_regularized, "matrix")

# exploring the matrix
head(train)
train@data[1:9, 2001:2009]

# 
# # saving
# saveRDS(train, file="train")

### running Funk Singular Value Decomposition
fsvd <- funkSVD(train, verbose = TRUE)
class(fsvd)

# saving
saveRDS(fsvd, file="fsvd")





### preparingn for k-fold evaluation ###
set.seed(123)

n_fold <- 10  # k value for k fold cross validation
items_to_keep <- 15  # Items to consider in training set (less than min no of ratings )
rating_threshold <- 3.5  # Considering a rating of 3.5 as good rating across all movies

eval_sets <- evaluationScheme(data = trainmat_regularized, method = "cross-validation", k = n_fold, 
                              given = items_to_keep, goodRating = rating_threshold)


### training the model ###
# User based collaborative filtering #
rec=Recommender(trainmat,method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5))
rec

# predicting
recom10k <- predict(rec, trainmat[1:10^4], type="ratings")

# checking the accuracy
model10k_accuracy <- calcPredictionAccuracy(x = recom10k, data = testmat, 
                                            byUser = FALSE)
model10k_accuracy


# items_to_recommend<-10
# UBCF_prediction <- predict(object = trainmat, newdata = testmat, n = items_to_recommend, 
#                              type = "ratings")
rm(edx)
rm(users_and_ratings)
rm(validation)
rm(user_avgs)
rm(movie_avgs, movie_reg_avgs)
rm(testmat)
rm(trainmat)

# Source 1: https://rpubs.com/Argaadya/recommender-svdf
# Source 2: https://mono33.github.io/MovieLensProject/
