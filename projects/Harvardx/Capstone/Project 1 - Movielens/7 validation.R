### validation ###


# converting the matrix into a "realRatingMatrix")
trainmat_full <- as(users_and_ratings_training_set, "realRatingMatrix")
dim(trainmat_full)

# class(trainmat)

# removing items with few ratings because of low confidence in these ratings
min_n_movies <- quantile(rowCounts(trainmat_full), 0.9)
print(min_n_movies)

min_n_users <- quantile(colCounts(trainmat_full), 0.9)
print(min_n_users)

trainmat <- trainmat_full[rowCounts(trainmat_full) > min_n_movies,
                          colCounts(trainmat_full) > min_n_users]

dim(trainmat_full)
dim(trainmat)

# checking number of ratings per item
number_of_ratings<-colCounts(trainmat)
min(number_of_ratings)
max(number_of_ratings)

# exploring the matrix
dim(trainmat)
# trainmat@data[1500:1510, 2001:2009]

# normalizing the values
# normalize(trainmat, method = "Z-score")

# saving
# saveRDS(trainmat, file="trainmat")

# checking the initial/default parameters of the SVDF model
# recommenderRegistry$get_entry("SVDF", dataType = "realRatingMatrix")

# recommending
# recom_svdf <- Recommender(data = trainmat,
#                           method = "SVDF",
#                           parameter = list(normalize = "Z-score")
# )

# Evaluating the model by cross-validation
set.seed(123, sample.kind="Rounding")

# Setting up the evaluation scheme
scheme_10 <- trainmat %>% 
  evaluationScheme(method = "split",
                   k=1,
                   train  = 0.9,  # 90% data train
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
# UBCF_prediction <- predict(object = trainmat, newdata = testmat, n = items_to_recommend, 
#                              type = "ratings")
rm(edx)
rm(users_and_ratings)
rm(validation)
rm(user_avgs)
rm(movie_avgs, movie_reg_avgs)
rm(testmat)
rm(trainmat_full)
rm(samp)
rm(users_and_ratings_training_set)
# rm(list = ls())

# Source 1: https://rpubs.com/Argaadya/recommender-svdf
# Source 2: https://mono33.github.io/MovieLensProject/