##################
### validation ###
##################

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
rm(edx, result_rating_svdf_10, samp, scheme_10, testmat_full, users_and_ratings_test_set)
rm(rec)

### Increasing memory
# Checking memory limit
# memory.limit()
# Change memory limit
# memory.limit(size = 10^9)

# cleaning memory
invisible(gc())

# # loading  data files
# setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
 # rec<-readRDS("rec")
# edx<-readRDS("edx")

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
# saveRDS(testmat, file="testmat")

# checking the initial/default parameters of the SVDF model
# recommenderRegistry$get_entry("SVDF", dataType = "realRatingMatrix")

# recommending
# recom_svdf <- Recommender(data = testmat,
#                           method = "SVDF",
#                           parameter = list(normalize = "Z-score")
# )

# Evaluating the model by cross-validation
set.seed(123, sample.kind="Rounding")

recommendations <- Recommender(trainmat_final, method = "svdf")
recommendations

# saving
saveRDS(rec, file="recommendations")

#Making prediction on validation set:
predictions <- predict(recomendations, testmat, type="ratings")
predictions

class(predictions)
# saving
saveRDS(predictions, file="predictions")

# turning the results into a matrix
predmat<-as(predictions, "matrix")
class(predmat)

# calculating RMSE
RMSE(testmat, predictions, na.rm=T)

#####

rm(edx)
rm(users_and_ratings)
rm(validation)
rm(user_avgs)
rm(movie_avgs, movie_reg_avgs)
rm(testmat)
rm(testmat_full)
rm(samp)
rm(users_and_ratings_test_set)
# rm(list = ls())

# Source 1: https://rpubs.com/Argaadya/recommender-svdf
# Source 2: https://mono33.github.io/MovieLensProject/