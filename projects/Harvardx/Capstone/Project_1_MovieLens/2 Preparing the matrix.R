######################################################
###########  Predicting Movie Ratings   ##############
############     Movielens 10M Data      #############
######################################################

### Loading libraries ###
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)

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

### examining some distributions of the ratings ###
# distribution of the ratings
histogram(edx$rating) 
ratings_per_movie<-edx %>%
  count(movieId)

# distribution of the ratings per movie
histogram(ratings_per_movie$n, breaks=30)  
boxplot(ratings_per_movie$n)
summary(ratings_per_movie$n) 

rm(ratings_per_movie)

# distribution of the ratings per user
ratings_per_user<-edx %>%
  filter(!is.na(rating))  %>%
  count(userId)

histogram(ratings_per_user$n, breaks=30)  
boxplot(ratings_per_user$n)
summary(ratings_per_user$n) 

rm(ratings_per_user)

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

# rm(trainmat)
