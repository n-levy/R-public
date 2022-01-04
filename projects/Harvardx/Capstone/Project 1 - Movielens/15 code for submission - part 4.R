###############################
####### Other methods #########
###############################

### Loading libraries ###
library(tidyverse)
library(caret)
library(dplyr)
library(recommenderlab)
library(Matrix)

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
# edx<-readRDS("edx")
# trainmat<-readRDS("trainmat")
# testmat<-readRDS("testmat")
# scheme_10<-readRDS("scheme_10")
# full_scheme<-readRDS("full_scheme")
# trainmat_reduced<-readRDS("trainmat_reduced")
# trainmat_final_10<-readRDS("trainmat_final_10")
# recommendations_svdf_10<-readRDS("recommendations_svdf_10")
# recommendations_pop_10<-readRDS("recommendations_pop_10")

##################################################################################
##################### Test Set ###################################################
##################################################################################

### converting the testing set into a reaRatingmatrix ###
# creating userId,  movieId and rating columns
users_and_ratings_test_set<-cbind.data.frame(validation$userId, validation$movieId, validation$rating)
dim(users_and_ratings_test_set)
# head(users_and_ratings_test_set)

# creating the matrix
testmat <- as(users_and_ratings_test_set, "realRatingMatrix")
dim(testmat)
 
# checking number of ratings per item
number_of_ratings<-colCounts(testmat)
min(number_of_ratings)
max(number_of_ratings)
 
# creating an index of rows in the test matrix
index<-seq(1:nrow(testmat))
length(index) # making sure that the index was created properly

### Splitting the test matrix intro 10 equal parts ###
### to shorten processing time ###

# splitting the index into 10 equal parts
splitted_index<-split(index,             # Applying split() function
                      cut(seq_along(index),
                          10,
                          labels = FALSE))
# splitted_index
tenth_1<-as.vector(splitted_index[[1]])
tenth_2<-as.vector(splitted_index[[2]])
tenth_3<-as.vector(splitted_index[[3]])
tenth_4<-as.vector(splitted_index[[4]])
tenth_5<-as.vector(splitted_index[[5]])
tenth_6<-as.vector(splitted_index[[6]])
tenth_7<-as.vector(splitted_index[[7]])
tenth_8<-as.vector(splitted_index[[8]])
tenth_9<-as.vector(splitted_index[[9]])
tenth_10<-as.vector(splitted_index[[10]])

# verifying that all of the 10ths together comprise the total number of users
# in the test set
total<-length(tenth_1)+length(tenth_2)+length(tenth_3)+length(tenth_4)+length(tenth_5)+
 length(tenth_6)+length(tenth_7)+length(tenth_8)+length(tenth_9)+length(tenth_10)
total
nrow(testmat)

# splitting the test matrix into ten parts
testmat_1<-testmat[tenth_1]
testmat_2<-testmat[tenth_2]
testmat_3<-testmat[tenth_3]
testmat_4<-testmat[tenth_4]
testmat_5<-testmat[tenth_5]
testmat_6<-testmat[tenth_6]
testmat_7<-testmat[tenth_7]
testmat_8<-testmat[tenth_8]
testmat_9<-testmat[tenth_9]
testmat_10<-testmat[tenth_10]

# examining some of the tenths, to make sure that
# they were created properly
dim(testmat_1)
dim(testmat_7)
dim(testmat_9)
dim(testmat_10)

# Creating the predictions, using the 'popular' method

Sys.time() # noting the time in order to measure the time the next command takes

# training the algorithm on one tenth of the training set
# (training it on the full set took too much time)
recommendations_pop_10 <- Recommender(trainmat_final_10, method = "popular")

Sys.time()

# saving
# saveRDS(recommendations_pop_10, file="recommendations_pop_10")

# Predicting the ratings in the validation set, one tenth at a time:
predictions_pop_10_1 <- predict(recommendations_pop_10, testmat_1, type="ratingMatrix")
saveRDS(predictions_pop_10_1, file="predictions_pop_10_1")
predictions_pop_10_2 <- predict(recommendations_pop_10, testmat_2, type="ratingMatrix")
saveRDS(predictions_pop_10_2, file="predictions_pop_10_2")
predictions_pop_10_3 <- predict(recommendations_pop_10, testmat_3, type="ratingMatrix")
saveRDS(predictions_pop_10_3, file="predictions_pop_10_3")
predictions_pop_10_4 <- predict(recommendations_pop_10, testmat_4, type="ratingMatrix")
saveRDS(predictions_pop_10_4, file="predictions_pop_10_4")
predictions_pop_10_5 <- predict(recommendations_pop_10, testmat_5, type="ratingMatrix")
saveRDS(predictions_pop_10_5, file="predictions_pop_10_5")
predictions_pop_10_6 <- predict(recommendations_pop_10, testmat_6, type="ratingMatrix")
saveRDS(predictions_pop_10_6, file="predictions_pop_10_6")
predictions_pop_10_7 <- predict(recommendations_pop_10, testmat_7, type="ratingMatrix")
saveRDS(predictions_pop_10_7, file="predictions_pop_10_7")
predictions_pop_10_8 <- predict(recommendations_pop_10, testmat_8, type="ratingMatrix")
saveRDS(predictions_pop_10_8, file="predictions_pop_10_8")
predictions_pop_10_9 <- predict(recommendations_pop_10, testmat_9, type="ratingMatrix")
saveRDS(predictions_pop_10_9, file="predictions_pop_10_9")
predictions_pop_10_10 <- predict(recommendations_pop_10, testmat_10, type="ratingMatrix")
saveRDS(predictions_pop_10_10, file="predictions_pop_10_10")

Sys.time()

# reached here

# turning the results into a matrix
predmat_pop_10<-as(predictions_pop_10, "matrix")
class(predmat_pop_10)

# saving
saveRDS(predmat_pop_10, file="predmat_pop_10")

dim(testmat_first)
testmat_first

dim(predmat_pop_10)

# examining the matrices
testmat_first[1000:1100, 2000:2100]

# turning testmat into a matrix
testmat_first_matrix<-as(testmat_first, "matrix")

# saving
saveRDS(testmat_first_matrix, file="testmat_first_matrix")

diffmat_1<-testmat_first_matrix-predmat_pop_10

diffmat_1[1000:1100, 2000:2100]

# sum(!is.na(diffmat_1[1000:1100, 2000:2100]))
# sum(!is.na(testmat_first_matrix[1000:1100, 2000:2100]))
# sum(is.na(predmat_pop_10[1000:1100, 2000:2100]))

# predmat_pop_10[1500:1510, 2001:2009]

# calculating the difference

# calculating RMSE
number_of_ratings_in_test<-sum(!is.na(testmat_first_matrix))
squared_differences_1<-diffmat_1^2
rmse_manual<-sqrt(sum(squared_differences_1, na.rm=T)/number_of_ratings_in_test)
rmse_manual

rmse_pop_10<-RMSE(testmat_first_matrix, predmat_pop_10, na.rm=T)
rmse_pop_10


# saving
saveRDS(predictions_pop_10_1, file="predictions_pop_10")

# turning the results into a matrix
predmat_pop_10<-as(predictions_pop_10, "matrix")
class(predmat_pop_10)

# saving
saveRDS(predmat_pop_10, file="predmat_pop_10")

dim(testmat_first)
testmat_first

dim(predmat_pop_10)

# examining the matrices
testmat_first[1000:1100, 2000:2100]

# turning testmat into a matrix
testmat_first_matrix<-as(testmat_first, "matrix")

# saving
saveRDS(testmat_first_matrix, file="testmat_first_matrix")

diffmat_1<-testmat_first_matrix-predmat_pop_10

diffmat_1[1000:1100, 2000:2100]

# sum(!is.na(diffmat_1[1000:1100, 2000:2100]))
# sum(!is.na(testmat_first_matrix[1000:1100, 2000:2100]))
# sum(is.na(predmat_pop_10[1000:1100, 2000:2100]))

# predmat_pop_10[1500:1510, 2001:2009]

# calculating the difference

# calculating RMSE
number_of_ratings_in_test<-sum(!is.na(testmat_first_matrix))
squared_differences_1<-diffmat_1^2
rmse_manual<-sqrt(sum(squared_differences_1, na.rm=T)/number_of_ratings_in_test)
rmse_manual

rmse_pop_10<-RMSE(testmat_first_matrix, predmat_pop_10, na.rm=T)
rmse_pop_10

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
# validation<-readRDS("validation")
# edx<-readRDS("edx")
# trainmat<-readRDS("trainmat")
testmat<-readRDS("testmat")
# scheme_10<-readRDS("scheme_10")
# full_scheme<-readRDS("full_scheme")
# trainmat_reduced<-readRDS("trainmat_reduced")
trainmat_final_10<-readRDS("trainmat_final_10")
# recommendations_svdf_10<-readRDS("recommendations_svdf_10")
# recommendations_pop_10<-readRDS("recommendations_pop_10")
# predictions_pop_10_1<-readRDS("predictions_pop_10_1")
# predictions_pop_10_2<-readRDS("predictions_pop_10_2")
# predictions_pop_10_3<-readRDS("predictions_pop_10_3")
predmat_pop_10_1<-readRDS("predmat_pop_10_1")
predmat_pop_10_2<-readRDS("predmat_pop_10_2")
predmat_pop_10_3<-readRDS("predmat_pop_10_3")
predmat_pop_10_4<-readRDS("predmat_pop_10_4")
predmat_pop_10_5<-readRDS("predmat_pop_10_5")
predmat_pop_10_6<-readRDS("predmat_pop_10_6")
predmat_pop_10_7<-readRDS("predmat_pop_10_7")
predmat_pop_10_8<-readRDS("predmat_pop_10_8")
predmat_pop_10_9<-readRDS("predmat_pop_10_9")
predmat_pop_10_10<-readRDS("predmat_pop_10_10")

testmat_10_1_matrix<-readRDS("testmat_10_1_matrix")
testmat_10_2_matrix<-readRDS("testmat_10_2_matrix")
testmat_10_3_matrix<-readRDS("testmat_10_3_matrix")
testmat_10_4_matrix<-readRDS("testmat_10_4_matrix")
testmat_10_5_matrix<-readRDS("testmat_10_5_matrix")
testmat_10_6_matrix<-readRDS("testmat_10_6_matrix")
testmat_10_7_matrix<-readRDS("testmat_10_7_matrix")
testmat_10_8_matrix<-readRDS("testmat_10_8_matrix")
testmat_10_9_matrix<-readRDS("testmat_10_9_matrix")
testmat_10_10_matrix<-readRDS("testmat_10_10_matrix")

diffmat_10_1<-readRDS("diffmat_10_1")
diffmat_10_2<-readRDS("diffmat_10_2")
diffmat_10_3<-readRDS("diffmat_10_3")
diffmat_10_4<-readRDS("diffmat_10_4")
diffmat_10_5<-readRDS("diffmat_10_5")
diffmat_10_6<-readRDS("diffmat_10_6")
diffmat_10_7<-readRDS("diffmat_10_7")
diffmat_10_8<-readRDS("diffmat_10_8")
diffmat_10_9<-readRDS("diffmat_10_9")
diffmat_10_10<-readRDS("diffmat_10_10")

predmat_pop_all<-readRDS("predmat_pop_all")
testmat_all<-readRDS("testmat_all")
rmse<-readRDS("rmse")

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


##################################################################################
##################### Test Set ###################################################
##################################################################################

index<-seq(1:nrow(testmat))

# splitting index into 10 equal parts
splitted_index<-split(index,             # Applying split() function
                      cut(seq_along(index),
                          10,
                          labels = FALSE))

tenth_1<-as.vector(splitted_index[[1]])
tenth_2<-as.vector(splitted_index[[2]])
tenth_3<-as.vector(splitted_index[[3]])
tenth_4<-as.vector(splitted_index[[4]])
tenth_5<-as.vector(splitted_index[[5]])
tenth_6<-as.vector(splitted_index[[6]])
tenth_7<-as.vector(splitted_index[[7]])
tenth_8<-as.vector(splitted_index[[8]])
tenth_9<-as.vector(splitted_index[[9]])
tenth_10<-as.vector(splitted_index[[10]])

# verifying that all of the 10ths together comprise the total number of users
# in the test set
# total<-length(tenth_1)+length(tenth_2)+length(tenth_3)+length(tenth_4)+length(tenth_5)+
#   length(tenth_6)+length(tenth_7)+length(tenth_8)+length(tenth_9)+length(tenth_10)
#  total
#  nrow(testmat)

# splitting the test set into ten parts
testmat_10_1<-testmat[tenth_1]
testmat_10_2<-testmat[tenth_2]
testmat_10_3<-testmat[tenth_3]
testmat_10_4<-testmat[tenth_4]
testmat_10_5<-testmat[tenth_5]
testmat_10_6<-testmat[tenth_6]
testmat_10_7<-testmat[tenth_7]
testmat_10_8<-testmat[tenth_8]
testmat_10_9<-testmat[tenth_9]
testmat_10_10<-testmat[tenth_10]

# removing unnecessary files from the workspace
rm(tenth_1, tenth_2, tenth_3, tenth_4, tenth_5)
rm(tenth_6, tenth_7, tenth_8, tenth_9, tenth_10)


# examining the tenths
# dim(testmat_1)
# dim(testmat_7)
# dim(testmat_9)
# dim(testmat_10)

Sys.time()

# Creating the recommendations

############   popular    #################

Sys.time()

recommendations_pop_10 <- Recommender(trainmat_final_10, method = "popular")

Sys.time()

# saving
saveRDS(recommendations_pop_10, file="recommendations_pop_10")

#Making prediction on validation set:
predictions_pop_10_1 <- predict(recommendations_pop_10, testmat_1, type="ratingMatrix")
saveRDS(predictions_pop_10_1, file="predictions_pop_10_1")
predictions_pop_10_2 <- predict(recommendations_pop_10, testmat_2, type="ratingMatrix")
saveRDS(predictions_pop_10_2, file="predictions_pop_10_2")
predictions_pop_10_3 <- predict(recommendations_pop_10, testmat_3, type="ratingMatrix")
saveRDS(predictions_pop_10_3, file="predictions_pop_10_3")
predictions_pop_10_4 <- predict(recommendations_pop_10, testmat_4, type="ratingMatrix")
saveRDS(predictions_pop_10_4, file="predictions_pop_10_4")
invisible(gc())
predictions_pop_10_5 <- predict(recommendations_pop_10, testmat_5, type="ratingMatrix")
saveRDS(predictions_pop_10_5, file="predictions_pop_10_5")
rm(predictions_pop_10_1, predictions_pop_10_2, predictions_pop_10_3,predictions_pop_10_4, predictions_pop_10_5)
predictions_pop_10_6 <- predict(recommendations_pop_10, testmat_6, type="ratingMatrix")
saveRDS(predictions_pop_10_6, file="predictions_pop_10_6")
predictions_pop_10_7 <- predict(recommendations_pop_10, testmat_7, type="ratingMatrix")
saveRDS(predictions_pop_10_7, file="predictions_pop_10_7")
predictions_pop_10_8 <- predict(recommendations_pop_10, testmat_8, type="ratingMatrix")
saveRDS(predictions_pop_10_8, file="predictions_pop_10_8")
predictions_pop_10_9 <- predict(recommendations_pop_10, testmat_9, type="ratingMatrix")
saveRDS(predictions_pop_10_9, file="predictions_pop_10_9")
predictions_pop_10_10 <- predict(recommendations_pop_10, testmat_10, type="ratingMatrix")
saveRDS(predictions_pop_10_10, file="predictions_pop_10_10")
rm(predictions_pop_10_6, predictions_pop_10_7, predictions_pop_10_8,predictions_pop_10_9, predictions_pop_10_10)

# turning the results into a matrix
predictions_pop_10_1<-readRDS("predictions_pop_10_1")
predmat_pop_10_1<-as(predictions_pop_10_1, "matrix")
saveRDS(predmat_pop_10_1, file="predmat_pop_10_1")
rm(predictions_pop_10_1, predmat_pop_10_1)

predictions_pop_10_2<-readRDS("predictions_pop_10_2")
predmat_pop_10_2<-as(predictions_pop_10_2, "matrix")
saveRDS(predmat_pop_10_2, file="predmat_pop_10_2")
rm(predictions_pop_10_2, predmat_pop_10_2)

predictions_pop_10_3<-readRDS("predictions_pop_10_3")
predmat_pop_10_3<-as(predictions_pop_10_3, "matrix")
saveRDS(predmat_pop_10_3, file="predmat_pop_10_3")
rm(predictions_pop_10_3, predmat_pop_10_3)

predictions_pop_10_4<-readRDS("predictions_pop_10_4")
predmat_pop_10_4<-as(predictions_pop_10_4, "matrix")
saveRDS(predmat_pop_10_4, file="predmat_pop_10_4")
rm(predictions_pop_10_4, predmat_pop_10_4)

predictions_pop_10_5<-readRDS("predictions_pop_10_5")
predmat_pop_10_5<-as(predictions_pop_10_5, "matrix")
saveRDS(predmat_pop_10_5, file="predmat_pop_10_5")
rm(predictions_pop_10_5, predmat_pop_10_5)

predictions_pop_10_6<-readRDS("predictions_pop_10_6")
predmat_pop_10_6<-as(predictions_pop_10_6, "matrix")
saveRDS(predmat_pop_10_6, file="predmat_pop_10_6")
rm(predictions_pop_10_6, predmat_pop_10_6)

predictions_pop_10_7<-readRDS("predictions_pop_10_7")
predmat_pop_10_7<-as(predictions_pop_10_7, "matrix")
saveRDS(predmat_pop_10_7, file="predmat_pop_10_7")
rm(predictions_pop_10_7, predmat_pop_10_7)

predictions_pop_10_8<-readRDS("predictions_pop_10_8")
predmat_pop_10_8<-as(predictions_pop_10_8, "matrix")
saveRDS(predmat_pop_10_8, file="predmat_pop_10_8")
rm(predictions_pop_10_8, predmat_pop_10_8)

predictions_pop_10_9<-readRDS("predictions_pop_10_9")
predmat_pop_10_9<-as(predictions_pop_10_9, "matrix")
saveRDS(predmat_pop_10_9, file="predmat_pop_10_9")
rm(predictions_pop_10_9, predmat_pop_10_9)

predictions_pop_10_10<-readRDS("predictions_pop_10_10")
predmat_pop_10_10<-as(predictions_pop_10_10, "matrix")
saveRDS(predmat_pop_10_10, file="predmat_pop_10_10")
rm(predictions_pop_10_10, predmat_pop_10_10)

dim(testmat_first)
testmat_first

# examining the matrices
testmat_first[1000:1100, 2000:2100]

# turning testmats into a matrices
testmat_10_1_matrix<-as(testmat_10_1, "matrix")
saveRDS(testmat_10_1_matrix, file="testmat_10_1_matrix")

testmat_10_2_matrix<-as(testmat_10_2, "matrix")
saveRDS(testmat_10_2_matrix, file="testmat_10_2_matrix")

testmat_10_3_matrix<-as(testmat_10_3, "matrix")
saveRDS(testmat_10_3_matrix, file="testmat_10_3_matrix")

testmat_10_4_matrix<-as(testmat_10_4, "matrix")
saveRDS(testmat_10_4_matrix, file="testmat_10_4_matrix")

testmat_10_5_matrix<-as(testmat_10_5, "matrix")
saveRDS(testmat_10_5_matrix, file="testmat_10_5_matrix")

testmat_10_6_matrix<-as(testmat_10_6, "matrix")
saveRDS(testmat_10_6_matrix, file="testmat_10_6_matrix")

testmat_10_7_matrix<-as(testmat_10_7, "matrix")
saveRDS(testmat_10_7_matrix, file="testmat_10_7_matrix")

testmat_10_8_matrix<-as(testmat_10_8, "matrix")
saveRDS(testmat_10_8_matrix, file="testmat_10_8_matrix")

testmat_10_9_matrix<-as(testmat_10_9, "matrix")
saveRDS(testmat_10_9_matrix, file="testmat_10_9_matrix")

testmat_10_10_matrix<-as(testmat_10_10, "matrix")
saveRDS(testmat_10_10_matrix, file="testmat_10_10_matrix")

# cleaning the workspace
rm(testmat_10_1, testmat_10_2, testmat_10_3, testmat_10_4, testmat_10_5)
rm(testmat_10_6, testmat_10_7, testmat_10_8, testmat_10_9, testmat_10_10)
rm(splitted_index, testmat)

# calculating the differences
diffmat_10_1<-predmat_pop_10_1-testmat_10_1_matrix
saveRDS(diffmat_10_1, file="diffmat_10_1")

diffmat_10_2<-predmat_pop_10_2-testmat_10_2_matrix
saveRDS(diffmat_10_2, file="diffmat_10_2")

diffmat_10_3<-predmat_pop_10_3-testmat_10_3_matrix
saveRDS(diffmat_10_3, file="diffmat_10_3")

diffmat_10_4<-predmat_pop_10_4-testmat_10_4_matrix
saveRDS(diffmat_10_4, file="diffmat_10_4")

diffmat_10_5<-predmat_pop_10_5-testmat_10_5_matrix
saveRDS(diffmat_10_5, file="diffmat_10_5")

diffmat_10_6<-predmat_pop_10_6-testmat_10_6_matrix
saveRDS(diffmat_10_6, file="diffmat_10_6")

diffmat_10_7<-predmat_pop_10_7-testmat_10_7_matrix
saveRDS(diffmat_10_7, file="diffmat_10_7")

diffmat_10_8<-predmat_pop_10_8-testmat_10_8_matrix
saveRDS(diffmat_10_8, file="diffmat_10_8")

diffmat_10_1<-predmat_pop_10_1-testmat_10_1_matrix
saveRDS(diffmat_10_1, file="diffmat_10_1")

diffmat_10_9<-predmat_pop_10_9-testmat_10_9_matrix
saveRDS(diffmat_10_9, file="diffmat_10_9")

diffmat_10_10<-predmat_pop_10_10-testmat_10_10_matrix
saveRDS(diffmat_10_10, file="diffmat_10_10")

# cleaning the workspace
rm(predmat_pop_10_1, predmat_pop_10_2, predmat_pop_10_3, predmat_pop_10_4, predmat_pop_10_5)
rm(predmat_pop_10_6, predmat_pop_10_7, predmat_pop_10_8, predmat_pop_10_9, predmat_pop_10_10)

rm(testmat_10_1_matrix, testmat_10_2_matrix, testmat_10_3_matrix, testmat_10_4_matrix, testmat_10_5_matrix)
rm(testmat_10_6_matrix, testmat_10_7_matrix, testmat_10_8_matrix, testmat_10_9_matrix, testmat_10_10_matrix)

# combining the matrices of the differences
diffmat_all<-rbind(
  diffmat_10_1, 
  diffmat_10_2,
  diffmat_10_3,
  diffmat_10_4,
  diffmat_10_5,
  diffmat_10_6,
  diffmat_10_7,
  diffmat_10_8,
  diffmat_10_9,
  diffmat_10_10
)

dim(diffmat_all)

# cleaning the workspace
rm(diffmat_10_1, diffmat_10_2, diffmat_10_3, diffmat_10_4, diffmat_10_5)
rm(diffmat_10_6, diffmat_10_7, diffmat_10_8, diffmat_10_9, diffmat_10_10)

# saving
Sys.time()
saveRDS(diffmat_all, file="diffmat_all")
Sys.time()

# calculating RMSE
number_of_ratings_in_test_set<-sum(!is.na(diffmat_all))
number_of_ratings_in_test_set

# saving
saveRDS(number_of_ratings_in_test_set, file="number_of_ratings_in_test_set")

squared_differences_all<-diffmat_all^2

# saving
saveRDS(squared_differences_all, file="squared_differences_all")

rmse<-sqrt(sum(squared_differences, na.rm=T)/number_of_ratings_in_test_set)
rmse

# saving
saveRDS(rmse, file="rmse")

### validating by calculating RMSE in an alternative way ###
# creating one unified matrix of predictions
predmat_pop_all<-rbind(
  predmat_pop_10_1, 
  predmat_pop_10_2,
  predmat_pop_10_3,
  predmat_pop_10_4,
  predmat_pop_10_5,
  predmat_pop_10_6,
  predmat_pop_10_7,
  predmat_pop_10_8,
  predmat_pop_10_9,
  predmat_pop_10_10
)

# verifying that all users in the validation set are included
dim(predmat_pop_all)

# cleaning the workspace
rm(predmat_pop_10_1, predmat_pop_10_2, predmat_pop_10_3, predmat_pop_10_4, predmat_pop_10_5)
rm(predmat_pop_10_6, predmat_pop_10_7, predmat_pop_10_8, predmat_pop_10_9, predmat_pop_10_10)

# saving
Sys.time()
saveRDS(predmat_pop_all, file="predmat_pop_all")
Sys.time()

# creating one unified matrix of real ratings
testmat_all<-rbind(
  testmat_10_1_matrix, 
  testmat_10_2_matrix,
  testmat_10_3_matrix,
  testmat_10_4_matrix,
  testmat_10_5_matrix,
  testmat_10_6_matrix,
  testmat_10_7_matrix,
  testmat_10_8_matrix,
  testmat_10_9_matrix,
  testmat_10_10_matrix
)

# verifying that all users in the validation set are included
dim(testmat_all)

# cleaning the workspace
rm(testmat_10_1_matrix, testmat_10_2_matrix, testmat_10_3_matrix, testmat_10_4_matrix, testmat_10_5_matrix)
rm(testmat_10_6_matrix, testmat_10_7_matrix, testmat_10_8_matrix, testmat_10_9_matrix, testmat_10_10_matrix)

# cleaning memory
invisible(gc())

# saving
Sys.time()
saveRDS(testmat_all, file="testmat_all")
Sys.time()

# verifying that all observations are included
dim(predmat_pop_all)
dim(testmat_all)

# calculating RMSE through the built-in function
rmse_alternative<-RMSE(testmat_all, predmat_pop_all, na.rm=T)

# comparing the two RMSEs
rmse
rmse_alternative
difference<-rmse-rmse_alternative
difference

#cleaning the dataset
rm(breaks, diffmat_1, g, h, k, mtrx, predictions_pop_10, predmat_pop_10, recommendations_pop_10)
rm(recommendations_svdf_10, squared_differences_1, testmat_first, testmat_first_matrix)
rm(recommendations_pop, testmat_real, trainmat_real)
rm(testmat_all)
rm(list=ls())

# cleaning memory
invisible(gc())

# Allocating memory
memory.limit(size = 10^10)

########################################################################################
#### Checking the RMSE in case of the "constant effects with regularization" model #####
########################################################################################


### Loading libraries ###
library(tidyverse)
library(caret)
# library(data.table)
library(dplyr)
# library(kableExtra)
# library(recommenderlab)
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
edx<-readRDS("edx")
validation<-readRDS("validation")

### Loading libraries ###
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)

# # removing all files
# rm(list = ls())

# clearing unused memory
invisible(gc())
# 
# user, movie and genre fixed effects with regularization
# Regularizing with lambda = 3
lambda <- 3
mu <- mean(edx$rating)

# user fixed effects
user_avgs <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu))

# movie fixed effects with regularization
movie_avgs <- edx %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu - b_u)/(n()+lambda), n_i = n())

# head(user_avgs)
# head(movie_avgs)

# genre fixed effects with regularization
# creating numeric genre column
edx$genresnum<-as.numeric(as.factor(edx$genres))

genre_avgs <- edx %>% 
  left_join(user_avgs) %>%
  left_join(movie_avgs) %>%
  group_by(genresnum) %>% 
  summarize(b_g = sum(rating - mu-b_u-b_i)/(n()+lambda), n_i = n())

# creating a numeric user_genres column in the
# validation dataset
validation$genresnum<-as.numeric(as.factor(validation$genres))

### Calculating RMSE ###
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genresnum') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
movie_user_genre_effects<-RMSE(predicted_ratings, validation$rating)

# checking the RMSE
# rmse_results <- rbind(rmse_results, c("Movie and user effects with regularization", round(movie_user_genre_effects,5)))
rmse_results <- c("Movie, user and genre effects with regularization", round(movie_user_genre_effects,5))
names(rmse_results)<-c("method", "RMSE")
rmse_results


