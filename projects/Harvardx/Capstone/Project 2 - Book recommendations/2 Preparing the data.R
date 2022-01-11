##########################
### Preparing the data ###
##########################

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
names(users)
class(users$User.ID) 
class(ratings$User.ID)

### converting the users$User.ID column to numeric form, since that is its' form in the ratings dataset
users$User.ID<-as.numeric(users$User.ID) 

### merging the files
all_together <- left_join(ratings, users, by = "User.ID") 

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
keep<-c(colnames[2], colnames[3], colnames[5], colnames[6], colnames[9], colnames[10], colnames[11])
dat<-all_together[keep]
head(dat)

### converting the names of the columns to lowercase letters (I'm used to coding with lowercase letters only)
names(dat)<-tolower(names(dat))

### making sure that it worked
names(dat)

### saving
saveRDS(dat, file="dat")

### removing all files except for the relevant one ('dat')
rm (all_together, books, comma_locations, ratings, users, users_processed)

### cleaning memory
invisible(gc())
