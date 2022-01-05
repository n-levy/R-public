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

# examining some of the tenths, to make sure that
# they were created properly
dim(testmat_10_1)
dim(testmat_10_7)
dim(testmat_10_9)
dim(testmat_10_10)

# removing unnecessary files from the workspace
rm(tenth_1, tenth_2, tenth_3, tenth_4, tenth_5)
rm(tenth_6, tenth_7, tenth_8, tenth_9, tenth_10)

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

# Creating the predictions, using the 'popular' method

# Predicting the ratings in the validation set, one tenth at a time:
predictions_pop_10_1 <- predict(recommendations_pop_10, testmat_1, type="ratingMatrix")
saveRDS(predictions_pop_10_1, file="predictions_pop_10_1") # saving the file
rm(predictions_pop_10_1) # removing the file to save memory

predictions_pop_10_2 <- predict(recommendations_pop_10, testmat_2, type="ratingMatrix")
saveRDS(predictions_pop_10_2, file="predictions_pop_10_2") # saving the file
rm(predictions_pop_10_2) # removing the file to save memory

predictions_pop_10_3 <- predict(recommendations_pop_10, testmat_3, type="ratingMatrix")
saveRDS(predictions_pop_10_3, file="predictions_pop_10_3") # saving the file
rm(predictions_pop_10_3) # removing the file to save memory

predictions_pop_10_4 <- predict(recommendations_pop_10, testmat_4, type="ratingMatrix")
saveRDS(predictions_pop_10_4, file="predictions_pop_10_4") # saving the file
rm(predictions_pop_10_4) # removing the file to save memory

predictions_pop_10_5 <- predict(recommendations_pop_10, testmat_5, type="ratingMatrix")
saveRDS(predictions_pop_10_5, file="predictions_pop_10_5") # saving the file
rm(predictions_pop_10_5) # removing the file to save memory

predictions_pop_10_6 <- predict(recommendations_pop_10, testmat_6, type="ratingMatrix")
saveRDS(predictions_pop_10_6, file="predictions_pop_10_6") # saving the file
rm(predictions_pop_10_6) # removing the file to save memory

predictions_pop_10_7 <- predict(recommendations_pop_10, testmat_7, type="ratingMatrix")
saveRDS(predictions_pop_10_7, file="predictions_pop_10_7") # saving the file
rm(predictions_pop_10_7) # removing the file to save memory

predictions_pop_10_8 <- predict(recommendations_pop_10, testmat_8, type="ratingMatrix")
saveRDS(predictions_pop_10_8, file="predictions_pop_10_8") # saving the file
rm(predictions_pop_10_8) # removing the file to save memory

predictions_pop_10_9 <- predict(recommendations_pop_10, testmat_9, type="ratingMatrix")
saveRDS(predictions_pop_10_9, file="predictions_pop_10_9") # saving the file
rm(predictions_pop_10_9) # removing the file to save memory

predictions_pop_10_10 <- predict(recommendations_pop_10, testmat_10, type="ratingMatrix")
saveRDS(predictions_pop_10_10, file="predictions_pop_10_10") # saving the file
rm(predictions_pop_10_10) # removing the file to save memory

Sys.time()

# turning the results into matrices
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

# loading the prediction files
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

# making sure that the dimensions of the matrix 
# fit the number of users in the validation set
dim(diffmat_all)
length(unique(validation$userId))

# saving
Sys.time()
saveRDS(diffmat_all, file="diffmat_all")
Sys.time()

# cleaning the workspace
rm(diffmat_10_1, diffmat_10_2, diffmat_10_3, diffmat_10_4, diffmat_10_5)
rm(diffmat_10_6, diffmat_10_7, diffmat_10_8, diffmat_10_9, diffmat_10_10)

### calculating RMSE
# calculating the number of non-empty cells in the test set
number_of_ratings_in_test_set<-sum(!is.na(diffmat_all))
number_of_ratings_in_test_set

# saving
saveRDS(number_of_ratings_in_test_set, file="number_of_ratings_in_test_set")

# calculating the squared differences
squared_differences_all<-diffmat_all^2

# saving
saveRDS(squared_differences_all, file="squared_differences_all")

# dividing the sum of the squared differences by the number of ratings
rmse<-sqrt(sum(squared_differences_all, na.rm=T)/number_of_ratings_in_test_set)
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

# making sure that all users in the validation set are included
dim(predmat_pop_all)
length(unique(validation$userId))

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

# making sure that all users in the validation set are included
dim(testmat_all)
length(unique(validation$userId))

# saving
Sys.time()
saveRDS(testmat_all, file="testmat_all")
Sys.time()

# cleaning the workspace
rm(testmat_10_1_matrix, testmat_10_2_matrix, testmat_10_3_matrix, testmat_10_4_matrix, testmat_10_5_matrix)
rm(testmat_10_6_matrix, testmat_10_7_matrix, testmat_10_8_matrix, testmat_10_9_matrix, testmat_10_10_matrix)

# cleaning memory
invisible(gc())

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
