####################################################################################
####### Running a simple linear model, with constants for users and movies #########
####################################################################################


### creating a subset of the training set, for testing the model ###
# I am avoiding calling this a validation set, 
# so it will not be confused with the test set, which edx called a 'validation set'
# will simply be called the core test set 'core' and the subset 'sub'
# Validation set will be 15% of the training set
sub_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
core <- edx[-sub_index,]
temp <- edx[sub_index,]

# Make sure userId and movieId in sub set are also in core set
sub <- temp %>% 
  semi_join(core, by = "movieId") %>%
  semi_join(core, by = "userId")

# Add rows removed from sub set back into core set
removed <- anti_join(temp, sub)
core <- rbind(core, removed)

rm(sub_index, temp, removed)

