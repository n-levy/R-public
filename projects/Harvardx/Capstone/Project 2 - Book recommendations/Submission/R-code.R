####################################################################
####################  Predicting Book Ratings ######################
####  Harvardx Data Science Capstone - Create Your Own Project  ####
###################   Nir Levy, January 2022   #####################
####################################################################

###############################
### 1. Downloading the data ###
###############################

### Installing packages
suppressWarnings(suppressMessages(if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")))
suppressWarnings(suppressMessages(if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")))
suppressWarnings(suppressMessages(if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")))
suppressWarnings(suppressMessages(if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")))
suppressWarnings(suppressMessages(if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")))
suppressWarnings(suppressMessages(if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")))
suppressWarnings(suppressMessages(if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")))
suppressWarnings(suppressMessages(if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")))

### loading libraries
suppressWarnings(suppressMessages(library(downloader)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(recommenderlab)))
suppressWarnings(suppressMessages(library(stringr)))

### description of the data set
### by publisher: "http://www2.informatik.uni-freiburg.de/~cziegler/BX/"
### in Kaggle: "https://www.kaggle.com/somnambwl/bookcrossing-dataset"
### (the website: "https://www.bookcrossing.com/")

### downloading the dataset
download("http://www2.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip", dest="dataset.zip", mode="wb") 
unzip ("dataset.zip")

### converting the files to data frame format
books<-read.csv(file = 'BX-Books.csv', sep=";")
users<-read.csv(file = 'BX-Users.csv', sep=";")
ratings<-read.csv(file = 'BX-Book-Ratings.csv', sep=";")

### saving the data frames
saveRDS(books, "books")
saveRDS(users, "users")
saveRDS(ratings, "ratings")

#############################
### 2. Preparing the data ###
#############################

### cleaning the working space
rm(list=ls())

### loading the data
books<-readRDS("books")
users<-readRDS("users")
ratings<-readRDS("ratings")

### exploring the three data files
dim(books)
names(books)

dim(users)
names(users)
head(users)

dim(ratings)
names(ratings)
head(ratings)

### extracting country names from the 'users' data frame
users_processed<-users # duplicating the raw 'users' dataset

### examining the first 10 values in the users$Location column
users$Location[1:10]

### The users$Location column contains three entries, separated by commas.
### In our model we will only use the country

### Extracting the country names
### extracting country names
users_processed$country<-sub(".*, ", "", users_processed$Location)

### making sure that it worked
### first 10 rows
users_processed$country[1:10] 
users_processed$Location[1:10]

### an arbitrary sample of rows
users_processed$country[647:658] 
users_processed$Location[647:658]

names(users_processed)

### merging all three datasets to one file, on the rating level
### i.e. each row will contain a rating, and information
### about both the user and the book that were involved

### adding user information
names(users_processed)
class(users_processed$User.ID) 
class(users_processed$User.ID)

### converting the users$User.ID column to numeric form, since that is its' form in the ratings dataset
suppressWarnings(users_processed$User.ID<-as.numeric(users_processed$User.ID))

### merging the files
all_together <- left_join(ratings, users_processed, by = "User.ID") 

### making sure that the merged file is fine
head(all_together)
dim(all_together)

### adding books information
names(books)
class(books$ISBN) 
class(ratings$ISBN)

### merging the files
all_together <- left_join(all_together, books, by = "ISBN") 

### making sure that the merged file is fine
names(all_together)
dim(all_together)

### keeping the relevant columns only
colnames<-names(all_together)
colnames
keep<-c(colnames[1], colnames[2], colnames[3], colnames[5], colnames[6], colnames[8], colnames[9], colnames[10])
dat<-all_together[keep]
head(dat)

### simplifying the names
names(dat)<- c("userid", "bookid", "rating", "age", "country", "author", "year", "publisher")
names(dat) # making sure that the renaming worked properly

### checking the classes of the columns that are supposed to be numeric
class(dat$userid)
class(dat$bookid)
class(dat$rating)
class(dat$age)
class(dat$year)

### converting columns that should be numeric to numeric class
dat_backup<-dat # creating a backup 
saveRDS(dat_backup, "dat_backup") # saving it

### converting to numeric
suppressWarnings(dat$bookid<-as.numeric(dat$bookid))
suppressWarnings(dat$age<-as.numeric(dat$age))
suppressWarnings(dat$year<-as.numeric(dat$year))

### making sure that it worked
class(dat$userid)
class(dat$bookid)
class(dat$rating)
class(dat$age)
class(dat$year)

### preparing a column with the concatenation of userid and author
### (see the rationale in the beginning of this section)

### first, let's check for missing values
n_missing_userid<-sum(is.na(dat$userid)) # the number of missing values in the userid column
n_missing_userid

n_missing_author<-sum(is.na(dat$author)) # the number of missing values in the author column
n_missing_author
nrow(dat)

### the author columns contains missing values
### indexing the rows that do not have a missing value in the author column
index<-which(!is.na(dat$author))

### making sure that the indexing worked properly
head(index)
length(index)+n_missing_author-nrow(dat) # this should be zero

### concatenating userid and author in rows that contain both
dat$userid_author<-"missing" # creating default value
dat$userid_author[index]<-paste(dat$userid[index], dat$author[index]) # concatenating
head(dat$userid_author)
dat$userid_author[dat$userid_author=="missing"]<-NA # replacing "missing" with NAs

### making sure that it worked properly
dat$userid_author[4300:4400] # examining
nonmissing1<-sum(!is.na(dat$userid_author)) # counting non-missing values
nonmissing2<-nrow(dat)-n_missing_author 
nonmissing1-nonmissing2 # this should be zero

### creating age brackets for the analysis
### (see rationale in the beginning of this section)
dat$age<-round(dat$age,0) # rounding the values of the age variable, in case they aren't rounded yet
dat$age_bracket[dat$age<=10]<-"0-10"
dat$age_bracket[dat$age>=11 & dat$age<=20]<-"11-20"
dat$age_bracket[dat$age>=21 & dat$age<=30]<-"21-30"
dat$age_bracket[dat$age>=31 & dat$age<=40]<-"31-40"
dat$age_bracket[dat$age>=41 & dat$age<=50]<-"41-50"
dat$age_bracket[dat$age>=51 & dat$age<=60]<-"51-60"
dat$age_bracket[dat$age>=61 & dat$age<=70]<-"61-70"
dat$age_bracket[dat$age>=71 & dat$age<=80]<-"71-80"
dat$age_bracket[dat$age>=81 & dat$age<=90]<-"81-90"
dat$age_bracket[dat$age>=91 & dat$age<=100]<-"91-100"

### examining the distribution of ratings
table(dat$rating)

### there seem to be many rows with rating = 0
### in the book crossing website, books are rated from 1 to 10
### with an option to mark "haven't read" as well
### (see the website: "https://www.bookcrossing.com/")
### so we need to remove the rows with ratings of zero

### removing the rows with ratings of zero
dim(dat)
dat<-dat[dat$rating!=0,]
dim(dat)

### saving
saveRDS(dat, file="dat")

### cleaning up the working space
rm(list=ls())

### reloading the dataset
dat<-readRDS("dat")

### cleaning memory
invisible(gc())

### checking the percentage of missing values in each variable

# creating a function that computes the percentage of missing values in a vector
percent_missing<-function(x){
  n_missing<-sum(is.na(x)) # calculate the number of missing values
  p_missing<-n_missing/length(x) # calculate the percentage of missing values 
  p_missing # print the percentage
}

# applying the function to each of the columns
apply(dat, MARGIN = 2, percent_missing)

# examining the distribution of the number of ratings per user
ratings_per_user<-dat %>%
  filter(!is.na(rating))  %>%
  count(userid)

ggplot(ratings_per_user, aes(x=n)) + 
        geom_histogram(binwidth=1) +
        xlim(0,20)

### splitting the data into training and test sets
### since the number of ratings per user is fairly low
### and some variables have a high percentage of missing values,
### I choose a 50-50 initial split to make sure that the test set is large enough.
### In the final split, the percentage of users in the training set will be larger than 50
### (and less than 50 in the test set) since we are going to remove users who appear only in the test set
### from the test set and add them to the training set.

# creating index of 50% 
suppressWarnings(set.seed(1, sample.kind="Rounding")) # setting the seed
test_index <- createDataPartition(y = dat$rating, times = 1, p = 0.5, list = FALSE) # defining a 50% split
train <- dat[-test_index,]
temp <- dat[test_index,]

# making sure that the temporary split is 50-50
length(test_index)/nrow(dat)

# creating a test set that includes only userids and bookids that are also in the training set
test <- temp %>% 
  semi_join(train, by = "bookid") %>%
  semi_join(train, by = "userid")

# adding rows that were included in the index, but not in the test set, to the training set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

### making sure that the number of rows in the training and test sets is equal to 
### the number of rows in the full dataset
nrow(dat)-(nrow(train)+nrow(test)) # this should be zero

### checking the final relative sizes of the training and test sets
nrow(train)/nrow(dat) # the train set is ~70% of the full data
nrow(test)/nrow(dat) # the test set is ~30% of the full data

### saving the files
saveRDS(train, file="train")
saveRDS(test, file="test")

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### reloading the training set
train<-readRDS("train")

#############################
### 3. Exploring the data ###
#############################

### we already examined the percentage of missing values in each column
### and the distribution of the number of users above, in the full data set,
### in order to determine the split between the training and test sets.
### from this point until the final validation, we will proceed with the train data only.
### let us examine some properties of the train data.

### examining the number of unique values in each column
dim(train)
names(train)

### examining the number of unique users
n_distinct(train$userid)
nrow(train)

### examining the number of unique books
n_distinct(train$bookid)
nrow(train)

### examining the distribution of ratings
table(train$rating)

train %>% ggplot(aes(x=rating)) + 
  geom_histogram(binwidth=1)

### the distribution is skewed to the right.

### counting the countries
n_distinct(train$country)

### counting the authors
n_distinct(train$author)

### counting the years
n_distinct(train$year)

### counting the publishers
n_distinct(train$publisher)

### counting the usierid-author combinations
n_distinct(train$userid_author)

### checking the average number of times each value appears in each column
### creating the function
avpu_func<-function(x){
  x<-x[!is.na(x)] # removing missing values
  n_unique_units<-n_distinct(x) # counting distinct values
  n_values<-length(x) # counting the overall number of values
  avpu<-round(n_values/n_unique_units,1) # calculating the number of times each distinct value appears in the vector
  avpu
}

### applying the function to each of the columns
apply(train, MARGIN = 2, avpu_func)

### examining the distribution of the average rating per user
train %>%
  group_by(userid) %>%
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")


### examining the distribution of the number of ratings per user
ratings_per_user<-train %>%
  filter(!is.na(rating) & !is.na(userid)) %>%
  count(userid)

ratings_per_user %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,10)

boxplot(ratings_per_user$n)

### examining the distribution of the number of ratings per book
ratings_per_book<-train %>%
  filter(!is.na(rating) & !is.na(bookid)) %>%
  count(bookid)

ratings_per_book %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,10)

boxplot(ratings_per_book$n)

### examining the distribution of the number of ratings per publisher
ratings_per_publisher<-train %>%
  filter(!is.na(rating) & !is.na(publisher)) %>%
  count(publisher)

ratings_per_publisher %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,10)

boxplot(ratings_per_publisher$n)

### examining the distribution of the number of ratings per year
hist(train$year)

ratings_per_year<-train %>%
  filter(!is.na(rating) & !is.na(year)) %>%
  count(year)

ratings_per_year %>% ggplot(aes(x=n)) + 
  geom_histogram()

boxplot(ratings_per_year$n)

### examining the distribution of the number of ratings per user_author pair
ratings_per_userid_author<-train %>%
  filter(!is.na(rating) & !is.na(userid_author)) %>%
  count(userid_author)

ratings_per_userid_author %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,10)

boxplot(ratings_per_userid_author$n)

### examining the distribution of age
age_without_nas<-train$age[!is.na(train$age) & train$age!="NULL"]
length(age_without_nas)
length(train$age)
n_distinct(age_without_nas)
n_distinct(train$age)

head(age_without_nas)
class(age_without_nas)
length(age_without_nas)

hist(age_without_nas)

sum(age_without_nas>100, na.rm=T)

### removing rows with age>100 since that is probably false
train$age[train$age>100]<-NA

### examining the age distribution
hist(train$age)

ratings_per_age<-train %>%
  filter(!is.na(rating) & !is.na(age) & train$age!="NULL") %>%
  count(age)

ratings_per_age %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,100)

boxplot(ratings_per_age$n)

### examining the distribution of the age brackets
table(train$age_bracket)
train %>% ggplot(aes(x=age_bracket)) + 
  geom_bar() 

### saving the train set
saveRDS(train, "train")

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### reloading the train set
train<-readRDS("train")

#########################
### 4. A simple model ###
#########################

### To keep things simple, we will use the method of "Leave One Out Cross Validation" (LOOCV)

### creating a subset of the training set, for evaluating the model ###
### creating index of 20% 
suppressWarnings(set.seed(1, sample.kind="Rounding")) # setting the seed
test_index <- createDataPartition(y = train$rating, times = 1, p = 0.2, list = FALSE) # defining a 20% split
core <- train[-test_index,]
temp <- train[test_index,]

# making sure that the temporary split is 80-20
length(test_index)/nrow(train)

# creating a 'sub' set that includes only userids and bookids that are also in the training set
sub <- temp %>% 
  semi_join(train, by = "bookid") %>%
  semi_join(train, by = "userid")

# adding rows that were included in the index, but not in the test set, to the training set
removed <- anti_join(temp, sub)
core <- rbind(core, removed)

### making sure that the number of rows in the training and test sets is equal to 
### the number of rows in the full dataset
nrow(train)-(nrow(core)+nrow(sub)) # this should be zero

### checking the final relative sizes of the training and test sets
nrow(core)/nrow(train) # the core set is ~80% of the training data
nrow(sub)/nrow(train) # the sub set is ~20% of the training data

### The first model ###
# Predicting only according to the average rating in the core dataset ###
mu <- mean(core$rating)
mu

# checking the root mean squared error
core$mean<-mu
naive_rmse <- RMSE(core$rating, core$mean)
naive_rmse

# creating a results table
rmse_results <- c("Just the average", round(naive_rmse,5))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### adding user effects ###
user_avgs <- core %>% 
  group_by(userid) %>% 
  summarize(b_u = mean(rating - mu))

# examining the distributions of user effects
qplot(b_u, data = user_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- mu + sub %>% 
  left_join(user_avgs, by='userid') %>%
  pull(b_u)
user_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User effects", round(user_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### examining book effects ###
# estimating the book effects, by computing 
# the overall average and the user effect, and then 
# the book effect is the average of the remainder, after they
# are substracted from the rating (book effect= average of (rating - overall average - user effect)
book_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(bookid) %>%
  summarize(b_i = mean(rating - mu - b_u))

# examining the distributions of book effects
qplot(b_i, data = book_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(book_avgs, by='bookid') %>%
  mutate(pred = mu + b_u + b_i) %>%
  pull(pred)
user_and_book_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and book effects", round(user_and_book_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### Adding book effects does not improve the RMSE
### Let us try to add other 'side information'

### adding country effects ###
country_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(country) %>%
  summarize(b_c = mean(rating - mu - b_u))

# examining the distributions of country effects
qplot(b_c, data = country_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(country_avgs, by='country') %>%
  mutate(pred = mu + b_u + b_c) %>%
  pull(pred)
user_and_country_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and country effects", round(user_and_country_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### Adding country effects does not improve the RMSE
### Let us try to add other 'side information'

### adding author effects ###
author_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(author) %>%
  summarize(b_au = mean(rating - mu - b_u))

# examining the distributions of author effects
qplot(b_au, data = author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(author_avgs, by='author') %>%
  mutate(pred = mu + b_u + b_au) %>%
  pull(pred)
user_and_author_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and author effects", round(user_and_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### Adding author effects does not improve the RMSE
### Let us try to add other 'side information'

### adding year effects ###
year_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - mu - b_u))

# examining the distributions of year effects
qplot(b_y, data = year_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(year_avgs, by='year') %>%
  mutate(pred = mu + b_u + b_y) %>%
  pull(pred)
user_and_year_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and year effects", round(user_and_year_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### Adding year effects does not improve the RMSE
### Let us try to add other 'side information'

### adding publisher effects ###
publisher_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(publisher) %>%
  summarize(b_p = mean(rating - mu - b_u))

# examining the distributions of publisher effects
qplot(b_p, data = publisher_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(publisher_avgs, by='publisher') %>%
  mutate(pred = mu + b_u + b_p) %>%
  pull(pred)
user_and_publisher_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and publisher effects", round(user_and_publisher_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### Adding publisher effects does not improve the RMSE
### Let us try to add other 'side information'

### adding age bracket effects ###
age_bracket_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(age_bracket) %>%
  summarize(b_a = mean(rating - mu - b_u))

# examining the distributions of age_bracket effects
qplot(b_a, data = age_bracket_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(age_bracket_avgs, by='age_bracket') %>%
  mutate(pred = mu + b_u + b_a) %>%
  pull(pred)
user_and_age_bracket_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and age_bracket effects", round(user_and_age_bracket_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### Adding country effects does not improve the RMSE.
### Let us try to add an interaction between users and authors.
### it seems reasonable that a users' rating of a book by a certain author 
### would be rather similar to their rating of other books by the same author, 
### and different than the mean rating of all books by a certain user

### adding userid_author effects ###
userid_author_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(userid_author) %>%
  summarize(b_ua = mean(rating - mu - b_u))

# examining the distributions of userid_author effects
qplot(b_ua, data = userid_author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  mutate(pred = mu + b_u + b_ua) %>%
  pull(pred)
user_and_userid_author_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and User*Author effects", round(user_and_userid_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### Adding country effects somewhat improves the RMSE.
### Now let us add author effects
### Are there authors who receive higher ratings, or lower
### ratings, on average?

### adding author effects ###
author_avgs <- core %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  group_by(author) %>%
  summarize(b_a = mean(rating - mu - b_u - b_ua))

# examining the distributions of userid_author effects
qplot(b_a, data = author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  left_join(author_avgs, by='author') %>%
  mutate(pred = mu + b_u + b_ua +b_a) %>%
  pull(pred)
user_and_userid_author_and_author_effects_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("User and userid_author and author effects", round(user_and_userid_author_and_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

################################
### 5. Adding regularization ###
################################

### We will use the model with user and userid_author effects,
### since it produced the lowest RMSE (and adding author effects did not make any difference)

### Choosing lambda
lambdas <- seq(0, 10, 0.25)

mu <- mean(core$rating)
just_the_sum <- core %>% 
  group_by(bookid) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- sub %>% 
    left_join(just_the_sum, by='bookid') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, sub$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

### Regularizing with lambda = 3
lambda <- 8.5
mu <- mean(core$rating)
mu

### user effects are calculated in the same way as without regularization
user_avgs <- core %>% 
  group_by(userid) %>% 
  summarize(b_u = mean(rating - mu))

### userid_author effects are calculated differently 
userid_author_avgs_reg <- core %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(userid_author) %>%
  summarize(b_ua = sum(rating - mu - b_u)/(n()+lambda), n_ua = n())

### Calculating RMSE ###
predicted_ratings <- sub %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs_reg, by='userid_author') %>%
    mutate(pred = mu + b_u + b_ua) %>%
  pull(pred)
user_and_userid_author_effects_with_regularization_rmse<-RMSE(predicted_ratings, sub$rating, na.rm = T)

# checking the RMSE
rmse_results <- rbind.data.frame(rmse_results, c("User and User*Author effects with regularization", round(user_and_userid_author_effects_with_regularization_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### saving files
saveRDS(core, "core")
saveRDS(sub, "sub")
saveRDS(train, "train")
saveRDS(rmse_results, "rmse_results")

###########################
### 6. Other algorithms ###
###########################

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### reloading the train set
train<-readRDS("train")
test<-readRDS("test")
rmse_results<-readRDS("rmse_results")

### Preparing the data ###
### removing books in the training set that do not appear in the test set
trainmat_final <- train %>% 
  semi_join(test, by = "bookid") 

### exploring the datasets
dim(train)
dim(trainmat_final)

### saving
saveRDS(trainmat_final, file="trainmat_final")

### converting the training set into a matrix
users_and_ratings_train_set<-cbind.data.frame(trainmat_final$userid, trainmat_final$bookid, trainmat_final$rating)
names(users_and_ratings_train_set)<-c("userid", "bookid", "rating")

### exploring
dim(users_and_ratings_train_set)
head(users_and_ratings_train_set)

### counting missing values
n_missing<-sum(is.na(users_and_ratings_train_set))

### removing rows with missing bookids 
### (that is the only column that contains missing values)
index<-which(is.na(users_and_ratings_train_set$bookid))
head(index)
length(index)
users_and_ratings_train_set<-users_and_ratings_train_set[-index,]

### making sure that it worked
dim(users_and_ratings_train_set)

### making sure that there are no missing values left
sum(is.na(users_and_ratings_train_set)) # this should be zero

### converting the matrix into a "realRatingMatrix")
trainmat_final <- as(users_and_ratings_train_set, "realRatingMatrix")
dim(trainmat_final)

### saving
saveRDS(trainmat_final, file="trainmat_final")

### creating a regular matrix
trainmat_final_reg<-as(trainmat_final, "matrix")

### saving
saveRDS(trainmat_final_reg, file="trainmat_final_reg")

### exploring the matrix 
class(trainmat_final_reg)
n_missing<-sum(is.na(trainmat_final_reg)) # counting missing values
n_missing
n_non_missing<-sum(!is.na(trainmat_final_reg)) # counting non-missing values
n_non_missing
all<-nrow(trainmat_final_reg)*ncol(trainmat_final_reg) # counting all values
all
p_missing<-n_missing/all # calculating the percentage of missing values
p_missing 
p_non_missing<-1-p_missing
p_non_missing
rm(trainmat_final_reg, n_missing, all) # removing the matrix and other unnecessary objects

### Increasing memory size
memory.limit(size = 10^10)

### cleaning memory
invisible(gc())

### checking number of ratings per item
number_of_ratings<-colCounts(trainmat_final)
min(number_of_ratings)
max(number_of_ratings)

### exploring a sample of the matrix
trainmat_final@data[1500:1510, 2001:2009]

### normalizing the values
normalize(trainmat_final, method = "Z-score")

### saving
saveRDS(trainmat_final, file="trainmat_final")

### Setting up the evaluation scheme.
### We will use cross-validation

### calculating the mean of all ratings again, in order to determine 
### the value of a "good" rating for the evaluation function
train<-readRDS("train")
mean(train$rating)
rm(train) # removing the dataset from the workspace to save memory

Sys.time() # recording the time in order to see how long each step takes

scheme <- trainmat_final %>% 
  evaluationScheme(method = "cross-validation",
                   k=2, # 2-fold cross validation
                   given  = 1, 
                   goodRating = 8
  )

Sys.time() # recording the time in order to see how long each step takes

### saving
saveRDS(scheme, file="scheme")

Sys.time()

##################### Evaluating the models ################################

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### loading the scheme and the rmse_results
scheme<-readRDS("scheme")
rmse_results<-readRDS("rmse_results")

### Popular ###
### evaluating the "Popular" model
Sys.time() # recording the time in order to see how long each step takes

result_rating_popular <- evaluate(scheme,
                                  method = "popular",
                                  parameter = list(normalize = "Z-score"),
                                  type  = "ratings"
)

### examining the results
results_pop<-result_rating_popular@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_pop

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("Popular model", round(results_pop$RMSE,5)))
rmse_results

Sys.time() # recording the time in order to see how long each step takes

### saving 
saveRDS(result_rating_popular, file="result_rating_popular")

Sys.time() # recording the time in order to see how long each step takes

### saving 
### saveRDS(result_rating_ubcf, file="result_rating_ubcf")

### svd ###
### evaluating the svd model
result_rating_svd <- evaluate(scheme,
                              method = "svd",
                              parameter = list(normalize = "Z-score", k = 5),
                              type  = "ratings"
)

### examining the results
results_svd<-result_rating_svd@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_svd

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("Singular Value Decomposition", round(results_svd$RMSE,5)))
rmse_results

### saving 
saveRDS(result_rating_svd, file="result_rating_svd")

Sys.time() # recording the time in order to see how long each step takes

### svdf ###

### evaluating
result_rating_svdf <- evaluate(scheme,
                               method = "svdf",
                               parameter = list(normalize = "Z-score", k = 5),
                               type  = "ratings"
)

Sys.time() # recording the time in order to see how long each step takes

### examining the results
results_svdf<-result_rating_svdf@results %>%
  map(function(x) x@cm) %>%
  unlist() %>%
  matrix(ncol = 3, byrow = T) %>%
  as.data.frame() %>%
  summarise_all(mean) %>%
  setNames(c("RMSE", "MSE", "MAE"))

results_svdf

### adding the results to the list
rmse_results <- rbind.data.frame(rmse_results, c("Funk Singular Value Decomposition", round(results_svdf$RMSE,5)))
rmse_results

### Identifying the model with the lowest RMSE
### finding the row numbers
rows<-which(rmse_results$RMSE==min(rmse_results$RMSE))
rows
### finding the models
rmse_results$method[c(rows)]

### both models produce the same RMSE (when rounded), so we will choose the simpler one:
chosen_model<-rmse_results$method[c(rows)[1]]
chosen_model

### saving 
saveRDS(result_rating_svdf, file="result_rating_svdf")
saveRDS(rmse_results, file="rmse_results")
 
########################################################
### 7. Applying the chosen algorithm to the test set ###
########################################################

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### loading the datasets
train<-readRDS("train")
test<-readRDS("test")
rmse_results<-readRDS("rmse_results")

### calculating the mean rating
mu <- mean(train$rating)
mu

### adding user effects ###
user_avgs <- train %>% 
  group_by(userid) %>% 
  summarize(b_u = mean(rating - mu))

# examining the distributions of user effects
qplot(b_u, data = user_avgs, bins = 10, color = I("black"))

### adding userid_author effects ###
userid_author_avgs <- train %>% 
  left_join(user_avgs, by='userid') %>%
  group_by(userid_author) %>%
  summarize(b_ua = mean(rating - mu - b_u))

# examining the distributions of userid_author effects
qplot(b_ua, data = userid_author_avgs, bins = 10, color = I("black"))

# checking the rmse
predicted_ratings <- test %>% 
  left_join(user_avgs, by='userid') %>%
  left_join(userid_author_avgs, by='userid_author') %>%
  mutate(pred = mu + b_u + b_ua) %>%
  pull(pred)
user_and_userid_author_effects_rmse<-RMSE(predicted_ratings, test$rating, na.rm = T)

rmse_results <- rbind.data.frame(rmse_results, c("Test set: user and User*Author effects", round(user_and_userid_author_effects_rmse,5)))
names(rmse_results)<-c("method", "RMSE")
rmse_results

### The End ###