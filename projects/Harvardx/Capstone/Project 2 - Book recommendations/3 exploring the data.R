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

### examining the distribution of ratings per user
ratings_per_user<-train %>%
  filter(!is.na(rating) & !is.na(userid)) %>%
  count(userid)

ratings_per_user %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,10)

boxplot(ratings_per_user$n)

### examining the distribution of the ratings per book
ratings_per_book<-train %>%
  filter(!is.na(rating) & !is.na(bookid)) %>%
  count(bookid)

ratings_per_book %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,10)

boxplot(ratings_per_book$n)

### examining the distribution of the ratings per publisher
ratings_per_publisher<-train %>%
  filter(!is.na(rating) & !is.na(publisher)) %>%
  count(publisher)

ratings_per_publisher %>% ggplot(aes(x=n)) + 
  geom_histogram() +
  xlim(0,10)

boxplot(ratings_per_publisher$n)

### examining the distribution of the ratings per year
hist(train$year)

ratings_per_year<-train %>%
  filter(!is.na(rating) & !is.na(year)) %>%
  count(year)

ratings_per_year %>% ggplot(aes(x=n)) + 
  geom_histogram()

boxplot(ratings_per_year$n)

### examining the distribution of the ratings per user_author pair
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

### creating age brackets for the analysis

### creating one variable
train$age<-round(train$age,0) # rounding the values of the age variable, in case they aren't rounded yet
train$age_bracket[train$age<=10]<-"0-10"
train$age_bracket[train$age>=11 & train$age<=20]<-"11-20"
train$age_bracket[train$age>=21 & train$age<=30]<-"21-30"
train$age_bracket[train$age>=31 & train$age<=40]<-"31-40"
train$age_bracket[train$age>=41 & train$age<=50]<-"41-50"
train$age_bracket[train$age>=51 & train$age<=60]<-"51-60"
train$age_bracket[train$age>=61 & train$age<=70]<-"61-70"
train$age_bracket[train$age>=71 & train$age<=80]<-"71-80"
train$age_bracket[train$age>=81 & train$age<=90]<-"81-90"
train$age_bracket[train$age>=91 & train$age<=100]<-"91-100"

### examining the distribution of the age brackets
table(train$age_bracket)
train %>% ggplot(aes(x=age_bracket)) + 
  geom_bar() 

### creating dummy variables for the regression analysis
train$age_0_10<-0
train$age_0_10[train$age<=10]<-1

train$age_11_20<-0
train$age_11_20[train$age>=11 & train$age<=20]<-1

train$age_21_30<-0
train$age_21_30[train$age>=21 & train$age<=30]<-1

train$age_31_40<-0
train$age_31_40[train$age>=31 & train$age<=40]<-1

train$age_41_50<-0
train$age_41_50[train$age>=41 & train$age<=50]<-1

train$age_51_60<-0
train$age_51_60[train$age>=51 & train$age<=60]<-1

train$age_61_70<-0
train$age_61_70[train$age>=61 & train$age<=70]<-1

train$age_71_80<-0
train$age_71_80[train$age>=71 & train$age<=80]<-1

train$age_81_90<-0
train$age_81_90[train$age>=81 & train$age<=90]<-1

train$age_91_100<-0
train$age_91_100[train$age>=91 & train$age<=100]<-1

### making sure that it worked properly
table(train$age_bracket, train$age_0_10)
table(train$age_bracket, train$age_11_20)
table(train$age_bracket, train$age_21_30)
table(train$age_bracket, train$age_31_40)
table(train$age_bracket, train$age_41_50)
table(train$age_bracket, train$age_51_60)
table(train$age_bracket, train$age_61_70)
table(train$age_bracket, train$age_71_80)
table(train$age_bracket, train$age_81_90)
table(train$age_bracket, train$age_91_100)

### saving the train set
saveRDS(train, "train")

### cleaning the working space
rm(list=ls())

### cleaning memory
invisible(gc())

### reloading the train set
train<-readRDS("train")
