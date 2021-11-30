##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

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
# saveRDS(movies, file="movies")
# saveRDS(ratings, file="ratings")
# saveRDS(edx, file="edx")
# saveRDS(validation, file="validation")
# saveRDS(movielens, file="movielens")
# saveRDS(core, file="core")
# saveRDS(sub, file="sub")
# saveRDS(trainmat, file="trainmat")

# # removing unnecessary files
# rm(dl, ratings, movies, test_index, temp, movielens, removed)
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

