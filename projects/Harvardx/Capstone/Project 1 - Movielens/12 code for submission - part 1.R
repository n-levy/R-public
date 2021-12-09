##############################################################
# Create edx set, validation set (final hold-out test set) ###
##############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# install.packages("caret")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

### saving the training and test sets ###
getwd()
setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# 
saveRDS(movies, file="movies")
saveRDS(ratings, file="ratings")
saveRDS(edx, file="edx")
saveRDS(validation, file="validation")
saveRDS(movielens, file="movielens")
# saveRDS(core, file="core")
# saveRDS(sub, file="sub")
# saveRDS(trainmat, file="trainmat")
getwd()
# # removing unnecessary files
rm(movielens, ratings, movies, test_index, temp, movielens, removed)
# 
# # removing large files so R will start faster
# rm(validation, ratings)
# 
# # removing all files
rm(list = ls())

# loading the data files
setwd("H:/My Drive/sync/data analytics and machine learning/harvardx/Capstone/Github project/public/ml-10M100K")
# ratings<-readRDS("ratings")
# movies<-readRDS("movies")
# movielens<-readRDS("movielens")
# core<-readRDS("core")
# sub<-readRDS("sub")
# validation<-readRDS("validation")
edx<-readRDS("edx")

###################################################################
####### Assessment 1: Quiz: MovieLens Dataset #####################
###################################################################

dim(edx)
class(edx)
nrow(edx)
ncol(edx)
names(edx)
sum(edx$rating==0)
sum(edx$rating==3)

# their answer:
edx %>% filter(rating == 3) %>% tally()

length(unique(edx$movieId))

# their answer:
n_distinct(edx$movieId)

length(unique(edx$userId))

# their answer:
n_distinct(edx$userId)

sum(grepl( "Drama", edx$genres, fixed = TRUE))
sum(grepl( "Comedy", edx$genres, fixed = TRUE))
sum(grepl( "Thriller", edx$genres, fixed = TRUE))
sum(grepl( "Romance", edx$genres, fixed = TRUE))

# their answer:
# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# separate_rows, much slower!
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

sum(is.na(edx$rating))
df<-as.data.frame(table(edx$movieId))
head(df)
index<-which(df$Freq==max(df$Freq))
index
edx$title[index]
sum(grepl("Forrest Gump",edx$title, fixed=TRUE))
sum(grepl("Jurassic Park",edx$title, fixed=TRUE))
sum(grepl("Pulp Fiction",edx$title, fixed=TRUE))
sum(grepl("Shawshank",edx$title, fixed=TRUE))
sum(grepl("Speed 2: Cruise Control",edx$title, fixed=TRUE))

# their answer:
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

df<-as.data.frame(table(edx$rating))
df1<-df[order(-df$Freq),]
df1

# their answer:
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

# their answer:
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

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


