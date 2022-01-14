########################################################
### 7. Applying the chosen algorithm to the test set ###
########################################################

### Increasing memory size
memory.limit(size = 10^10)

### cleaining the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### loading the test set
test<-readRDS("test")

### converting the testing set into a reaRatingmatrix ###
### creating userid,  bookid and rating columns
users_and_ratings_test_set<-cbind.data.frame(test$userid, test$bookid, test$rating)
names(users_and_ratings_test_set)<-c("userid", "bookid", "rating")

### making sure that all columns are numeric
class(users_and_ratings_test_set$userid)
class(users_and_ratings_test_set$bookid)
class(users_and_ratings_test_set$rating)

### exploring users_and_ratings_test_set
dim(users_and_ratings_test_set)
head(users_and_ratings_test_set)

### turning users_and_ratings_test_set into a matrix
### (if I try to convert the dataframe into a realRatingMatrix I get an error)
mat<-as.matrix(users_and_ratings_test_set)

# creating the matrix
testmat <- as(mat, "realRatingMatrix")

### examining the realRatingMatrix object
dimnames(testmat)
colCounts(testmat) ## number of ratings per item
colMeans(testmat) ## average item rating
nratings(testmat) ## total number of ratings
# View(testmat)

image(testmat[1:5,1:5])

### restoring missing values in the matrix
testmat[testmat==99999999]

# checking the number of ratings per item
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

# turning the tenths of the test set into matrices
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

# Creating the predictions, using the 'popular' method

# Predicting the ratings in the test set, one tenth at a time:
predictions_pop_10_1 <- predict(recommendations_pop_10, testmat_10_1, type="ratingMatrix")
saveRDS(predictions_pop_10_1, file="predictions_pop_10_1") # saving the file

predictions_pop_10_2 <- predict(recommendations_pop_10, testmat_10_2, type="ratingMatrix")
saveRDS(predictions_pop_10_2, file="predictions_pop_10_2") # saving the file

predictions_pop_10_3 <- predict(recommendations_pop_10, testmat_10_3, type="ratingMatrix")
saveRDS(predictions_pop_10_3, file="predictions_pop_10_3") # saving the file

predictions_pop_10_4 <- predict(recommendations_pop_10, testmat_10_4, type="ratingMatrix")
saveRDS(predictions_pop_10_4, file="predictions_pop_10_4") # saving the file

predictions_pop_10_5 <- predict(recommendations_pop_10, testmat_10_5, type="ratingMatrix")
saveRDS(predictions_pop_10_5, file="predictions_pop_10_5") # saving the file

predictions_pop_10_6 <- predict(recommendations_pop_10, testmat_10_6, type="ratingMatrix")
saveRDS(predictions_pop_10_6, file="predictions_pop_10_6") # saving the file

predictions_pop_10_7 <- predict(recommendations_pop_10, testmat_10_7, type="ratingMatrix")
saveRDS(predictions_pop_10_7, file="predictions_pop_10_7") # saving the file

predictions_pop_10_8 <- predict(recommendations_pop_10, testmat_10_8, type="ratingMatrix")
saveRDS(predictions_pop_10_8, file="predictions_pop_10_8") # saving the file

predictions_pop_10_9 <- predict(recommendations_pop_10, testmat_10_9, type="ratingMatrix")
saveRDS(predictions_pop_10_9, file="predictions_pop_10_9") # saving the file

predictions_pop_10_10 <- predict(recommendations_pop_10, testmat_10_10, type="ratingMatrix")
saveRDS(predictions_pop_10_10, file="predictions_pop_10_10") # saving the file

Sys.time()

# removing files to free up memory
rm(testmat_10_1, testmat_10_2, testmat_10_3, testmat_10_4, testmat_10_5)
rm(testmat_10_6, testmat_10_7, testmat_10_8, testmat_10_9, testmat_10_10)
rm(splitted_index, testmat)

# cleaning memory
invisible(gc())

# turning the results into matrices
predictions_pop_10_1<-readRDS("predictions_pop_10_1")
predmat_pop_10_1<-as(predictions_pop_10_1, "matrix")
saveRDS(predmat_pop_10_1, file="predmat_pop_10_1")
rm(predictions_pop_10_1, predmat_pop_10_1) # removing files to free up memory

predictions_pop_10_2<-readRDS("predictions_pop_10_2")
predmat_pop_10_2<-as(predictions_pop_10_2, "matrix")
saveRDS(predmat_pop_10_2, file="predmat_pop_10_2")
rm(predictions_pop_10_2, predmat_pop_10_2) # removing files to free up memory

predictions_pop_10_3<-readRDS("predictions_pop_10_3")
predmat_pop_10_3<-as(predictions_pop_10_3, "matrix")
saveRDS(predmat_pop_10_3, file="predmat_pop_10_3") 
rm(predictions_pop_10_3, predmat_pop_10_3) # removing files to free up memory

predictions_pop_10_4<-readRDS("predictions_pop_10_4")
predmat_pop_10_4<-as(predictions_pop_10_4, "matrix")
saveRDS(predmat_pop_10_4, file="predmat_pop_10_4")
rm(predictions_pop_10_4, predmat_pop_10_4) # removing files to free up memory

predictions_pop_10_5<-readRDS("predictions_pop_10_5")
predmat_pop_10_5<-as(predictions_pop_10_5, "matrix")
saveRDS(predmat_pop_10_5, file="predmat_pop_10_5")
rm(predictions_pop_10_5, predmat_pop_10_5) # removing files to free up memory

predictions_pop_10_6<-readRDS("predictions_pop_10_6")
predmat_pop_10_6<-as(predictions_pop_10_6, "matrix")
saveRDS(predmat_pop_10_6, file="predmat_pop_10_6")
rm(predictions_pop_10_6, predmat_pop_10_6) # removing files to free up memory

predictions_pop_10_7<-readRDS("predictions_pop_10_7")
predmat_pop_10_7<-as(predictions_pop_10_7, "matrix")
saveRDS(predmat_pop_10_7, file="predmat_pop_10_7")
rm(predictions_pop_10_7, predmat_pop_10_7) # removing files to free up memory

predictions_pop_10_8<-readRDS("predictions_pop_10_8")
predmat_pop_10_8<-as(predictions_pop_10_8, "matrix")
saveRDS(predmat_pop_10_8, file="predmat_pop_10_8")
rm(predictions_pop_10_8, predmat_pop_10_8) # removing files to free up memory

predictions_pop_10_9<-readRDS("predictions_pop_10_9")
predmat_pop_10_9<-as(predictions_pop_10_9, "matrix")
saveRDS(predmat_pop_10_9, file="predmat_pop_10_9")
rm(predictions_pop_10_9, predmat_pop_10_9) # removing files to free up memory

predictions_pop_10_10<-readRDS("predictions_pop_10_10")
predmat_pop_10_10<-as(predictions_pop_10_10, "matrix")
saveRDS(predmat_pop_10_10, file="predmat_pop_10_10")
rm(predictions_pop_10_10, predmat_pop_10_10) # removing files to free up memory

# cleaning memory
invisible(gc())

# loading the prediction files that were created in the code above
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

# saving
saveRDS(diffmat_all, file="diffmat_all")

# cleaning the working space to free up memory
rm(diffmat_10_1, diffmat_10_2, diffmat_10_3, diffmat_10_4, diffmat_10_5)
rm(diffmat_10_6, diffmat_10_7, diffmat_10_8, diffmat_10_9, diffmat_10_10)

# making sure that the dimensions of the matrix 
# fit the number of users in the test set
dim(diffmat_all)
length(unique(test$userid))

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
