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
library(DT)
library(pander)

# # loading the data files
setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# # ratings<-readRDS("ratings")
# # movies<-readRDS("movies")
# # movielens<-readRDS("movielens")
#  core<-readRDS("core")
#  sub<-readRDS("sub")
validation<-readRDS("validation")
edx<-readRDS("edx")
trainmat<-readRDS("trainmat")
testmat<-readRDS("testmat")


### Preparing the data ###
# converting the training set into a matrix
users_and_ratings_training_set<-cbind.data.frame(edx$userId,edx$rating)
dim(users_and_ratings_training_set)

# converting the matrix into a "realRatingMatrix")
trainmat <- as(users_and_ratings_training_set, "realRatingMatrix")
dim(trainmat)

# saving
saveRDS(trainmat, file="trainmat")

# converting the test set into a matrix
users_and_ratings_test_set<-cbind.data.frame(validation$userId,validation$rating)
dim(users_and_ratings_test_set)

# converting the matrix into a "realRatingMatrix")
testmat <- as(users_and_ratings_test_set, "realRatingMatrix")
dim(testmat)

rm(users_and_ratings_test_set)

# saving
saveRDS(testmat, file="testmat")

# removing items with less than 50 ratings, for regularization 
# reached here. the matrix should be bigger, I don't understand
# why it has only 10 columns
trainmat <- trainmat[colCounts(trainmat) > 50]
minrowcnt <- min(rowCounts(trainmat))
dim(trainmat)


### preparingn for k-fold evaluation ###
set.seed(123)

n_fold <- 10  # k value for k fold cross validation
items_to_keep <- 15  # Items to consider in training set (less than min no of ratings )
rating_threshold <- 3.5  # Considering a rating of 3.5 as good rating across all movies

eval_sets <- evaluationScheme(data = trainmat, method = "cross-validation", k = n_fold, 
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
