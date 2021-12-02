#Before to perform MF method, i clear unusued memory
invisible(gc())

#c. Matrix Factorization with parallel stochastic gradient descent

#i create a copy of training(edx) and validation sets where i retain only userId, movieId and rating features. i rename the three columns.

edx.copy <-  edx %>%
  select(-c("genres","title","timestamp"))

names(edx.copy) <- c("user", "item", "rating")


valid.copy <-  validation %>%
  select(-c("genres","title","timestamp"))

names(valid.copy) <- c("user", "item", "rating")


#as matrix
edx.copy <- as.matrix(edx.copy)
valid.copy <- as.matrix(valid.copy)


#write edx.copy and valid.copy tables on disk 
write.table(edx.copy , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(valid.copy, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)


#  data_file(): Specifies a data set from a file in the hard disk. 

set.seed(123) # This is a randomized algorithm
train_set <- data_file(system.file( "dat" ,"trainset.txt" , package = "recosystem"))
valid_set <- data_file(system.file( "dat" ,"validset.txt" , package = "recosystem"))


#Next step is to build Recommender object
r = Reco()


# Matrix Factorization :  tuning training set

opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts

######################################################################################################

# Matrix Factorization : trains the recommender model

r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

pred_file = tempfile()

r$predict(valid_set, out_file(pred_file))  

#Matrix Factorization : show first 10 predicted values
print(scan(pred_file, n = 10))

#valid_set
scores_real <- read.table("validset.txt", header = FALSE, sep = " ")$V3
scores_pred <- scan(pred_file)

#remove copies of training and validation sets
rm(edx.copy, valid.copy)

rmse_mf <- RMSE(scores_real,scores_pred)
rmse_mf

Source: https://mono33.github.io/MovieLensProject/