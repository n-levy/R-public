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
 # testmat<-readRDS("testmat")
# scheme_10<-readRDS("scheme_10")
# full_scheme<-readRDS("full_scheme")
# trainmat_reduced<-readRDS("trainmat_reduced")
trainmat_final_10<-readRDS("trainmat_final_10")
# recommendations_svdf_10<-readRDS("recommendations_svdf_10")
recommendations_pop_10<-readRDS("recommendations_pop_10")
predictions_pop_10_1<-readRDS("predictions_pop_10_1")
predictions_pop_10_2<-readRDS("predictions_pop_10_2")
predictions_pop_10_3<-readRDS("predictions_pop_10_3")


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
# total
# nrow(testmat)

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
testmat_10_2_matrix<-as(testmat_10_2, "matrix")
testmat_10_3_matrix<-as(testmat_10_3, "matrix")
testmat_10_4_matrix<-as(testmat_10_4, "matrix")
testmat_10_5_matrix<-as(testmat_10_5, "matrix")
testmat_10_6_matrix<-as(testmat_10_6, "matrix")
testmat_10_7_matrix<-as(testmat_10_7, "matrix")
testmat_10_8_matrix<-as(testmat_10_8, "matrix")
testmat_10_9_matrix<-as(testmat_10_9, "matrix")
testmat_10_10_matrix<-as(testmat_10_10, "matrix")

# removing unnecessary files from the workspace
rm(testmat_10_1, testmat_10_2, testmat_10_3, testmat_10_4, testmat_10_5)
rm(testmat_10_6, testmat_10_7, testmat_10_8, testmat_10_9, testmat_10_10)
rm(splitted_index, testmat)

# calculating the differences
predictions_pop_10_1 <- predict(recommendations_pop_10, testmat_10_1, type="ratingMatrix")
predmat_pop_10_1<-as(predictions_pop_10_1, "matrix")
rm(predictions_pop_10_1)
diffmat_10_1<-predmat_pop_10_1-testmat_10_1_matrix



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



# sum(!is.na(diffmat_1[1000:1100, 2000:2100]))
# sum(!is.na(testmat_first_matrix[1000:1100, 2000:2100]))
# sum(is.na(predmat_pop_10[1000:1100, 2000:2100]))

# predmat_pop_10[1500:1510, 2001:2009]

# calculating the differences
readRDS()
diffmat_1<-testmat_10_1-predmat_pop_10_1


# calculating RMSE
number_of_ratings_in_test<-sum(!is.na(testmat_first_matrix))
squared_differences_1<-diffmat_1^2
rmse_manual<-sqrt(sum(squared_differences_1, na.rm=T)/number_of_ratings_in_test)
rmse_manual

rmse_pop_10<-RMSE(testmat_first_matrix, predmat_pop_10, na.rm=T)
rmse_pop_10

rm(breaks, diffmat_1, g, h, k, mtrx, predictions_pop_10, predmat_pop_10, recommendations_pop_10)
rm(recommendations_svdf_10, squared_differences_1, testmat_first, testmat_first_matrix)
