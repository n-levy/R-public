########################################################################
### Learning how to perform Matrix Factorization with Recommenderlab ###
########################################################################

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

# Loading the Movielens 10K data set 
# That is built into Recommenderlab
data(MovieLense)

# exploring the data
dim(MovieLense)
class(MovieLense)

## look at the first few ratings of the first user
head(as(MovieLense[1,], "list")[[1]])

## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))

## number of ratings per movie
hist(colCounts(MovieLense))

## mean rating (averaged over users)
mean(rowMeans(MovieLense))

## mean rating (averaged over items)
mean(colMeans(MovieLense))

## available movie meta information
head(MovieLenseMeta)

## available user meta information
head(MovieLenseUser)



### matrix factorization ###
# Loading the example data
data("Jester5k")
class(Jester5k)

# Converting the file to a matrix
train <- as(Jester5k[1:100], "matrix")
class(train)

### Performing Funk Singular Value Decomposition
fsvd <- funkSVD(train, verbose = TRUE)
