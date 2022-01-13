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
head(books)

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
### this will be useful for the model

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


