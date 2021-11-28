######################################################
###########  Predicting Movie Ratings   ##############
############     Movielens 10M Data      #############
######################################################

### Loading libraries ###
library(tidyverse)
library(caret)
library(data.table)

### Increasing the amount of memory allocated to RStudio ###
memory.limit()
memory.limit(size = 10^13)

### Exploring the training set ###
dim(edx)
names(edx)
head(edx)
summary(edx)
sum(is.na(edx)) # counting missing values

# counting the unique values
n_distinct(edx$userId) # users
n_distinct(edx$movieId) # movies
n_distinct(edx$rating) # ratings
n_distinct(edx$genres) # genres

# examining the distribution of the ratings
# histogram(edx$rating)

### Creating a matrix of ratings with userIds (rows) by movieIds (columns) ###
trainmat<-pivot_wider(edx, id_cols = userId, names_from = title, values_from = rating, values_fn=length, names_sep = "_", names_repair = "check_unique")
dim(trainmat)
head(trainmat)[1:5,1:5]

# exploring the matrix #
n_missing<-sum(is.na(trainmat)) # counting mising values
n_nonmissing<-sum(!is.na(trainmat)) # counting nonmissing values
p_nonmissing<-n_nonmissing/(n_missing+n_nonmissing) # calculating the percentage of nonmissing values
p_nonmissing 
rm(n_missing, n_nonmissing, p_nonmissing) # removing these variables

