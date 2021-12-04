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

### Increasing memory
# Checking memory limit
# memory.limit()
# Change memory limit
# memory.limit(size = 10^9)

# cleaning memory
invisible(gc())

# converting the testing set into a matrix
users_and_ratings_test_set<-cbind.data.frame(validation$userId, validation$movieId, validation$rating)
dim(users_and_ratings_test_set)
# head(users_and_ratings_test_set)

# converting the matrix into a "realRatingMatrix")
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

# Setting up the evaluation scheme
scheme_test <- testmat %>% 
  evaluationScheme(method = "split",
                   k=1,
                   test  = 0.9,  # 90% data test
                   given  = -8,
                   goodRating = 3
  )

scheme_10

# saving
saveRDS(scheme_10, file="scheme_10")

# measuring the rating error
result_rating_svdf_10 <- evaluate(scheme_10,
                                  method = "svdf",
                                  parameter = list(normalize = "Z-score", k = 5),
                                  type  = "ratings"
)

# saving 
saveRDS(result_rating_svdf_10, file="result_rating_svdf_10")


result_rating_svd <- evaluate(scheme,
                              method = "svd",
                              parameter = list(normalize = "Z-score", k = 5),
                              type  = "ratings"
)


result_rating_popular <- evaluate(scheme, 
                                  method = "popular",
                                  parameter = list(normalize = "Z-score"),
                                  type  = "ratings"
)

result_rating_als <- evaluate(scheme,
                              method = "als",
                              parameter = list(normalize = "Z-score", k = 5),
                              type  = "ratings"
)


result_rating_svdf_10@results %>% 
  map(function(x) x@cm) %>% 
  unlist() %>% 
  matrix(ncol = 3, byrow = T) %>% 
  as.data.frame() %>% 
  summarise_all(mean) %>% 
  setNames(c("RMSE", "MSE", "MAE"))
# items_to_recommend<-10
# UBCF_prediction <- predict(object = testmat, newdata = testmat, n = items_to_recommend, 
#                              type = "ratings")
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