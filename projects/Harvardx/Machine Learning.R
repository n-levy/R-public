# Harvadx course: Machine Learning
# Course lessons
# installing the caret package (that requires ggplot2)
# install.packages("caret")
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("magrittr")
# install.packages("purrr")
# install.packages("MASS")
# install.packages("pdftools")
# install.packages("dslabs")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("matrixStats")
# devtools::install_bioc("genefilter")
# install.packages("rpart")
# install.packages("randomForest")
# install.packages("__Rborist__")

#loading the libraries that will be used for the lesson - caret and dslabs
library(caret)
library(dslabs)
# library(magrittr)
# library(purrr)
library(dplyr)
# library(matrixStats)
# library(devtools)
# library(genefilter)
# library(rpart)
# library(Rborist)

# loading the libraries with the code provided by the course
library(maps)## load maps first to avoid map conflict with purrr
library(MASS) ## load MASS and matrixStats first to avoid select and count conflict
library(matrixStats) 
library(tidyverse)
library(dslabs)
ds_theme_set()

## Copied from Hadley Wickham and Garrett Grolemund's r4ds
options(digits = 3)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  out.width = "70%",
  fig.align = 'center',
  fig.width = 6,
  fig.height = 3.708,  # width * 1 / phi
  fig.show = "hold")

options(dplyr.print_min = 6, dplyr.print_max = 6)



# loading the data
data("heights")

# defining the outcome variable and the predictor variable
y<-heights$sex
x<-heights$height

length(x)
head(x)
levels(x)
class(x)

length(y)
head(y)
levels(y)
class(y)

# Randomly partitioning the data into a training set and a test set 
set.seed(2)
test_index<-createDataPartition(y, times=1, p=0.5, list=FALSE)
# that means: partition the data once, 
# use half of it for training and half for testing,
# and do not return indexes as a list

#my addition: exploring the datasets: heights and test_index
head(heights)
class(heights)
heights
length(heights)
levels(heights$sex)
 
head(test_index)
test_index
length(test_index)
 
# Defining the training set and the test set
train_set<-heights[-test_index,]
test_set<-heights[test_index,]

# A simple attempt to guess if a person is 
# male or female based on their height:
# simply guessing, like flipping a coin
y_hat<-sample(c("Male", "Female"), length(test_index),replace=TRUE)

# It is recommended or required to code categorical
# outcomes as factors, for packages like caret
# Coding the outcome as a factor
y_hat<-sample(c("Male", "Female"), length(test_index),replace=TRUE) %>%
  factor(levels=levels(test_set$sex))

# Computing the 'overall accuracy', that is
# the overall proportion that is computed correctly
mean(y_hat==test_set$sex)

# refining our method of prediction
# introducing method: 
# "predict 'male' if height is larger than two standard deviations
# below the average height of males
y_hat<-ifelse(x>62, "Male", "Female") %>%
  factor(levels=levels(test_set$sex))
# checking the overall accuracy
mean(y_hat==y)

# choosing a different cutoff point (not 62 inches as before)
# We examine the accuracy we obtain with 10 different cutoffs
# and pick the one yielding the best result
cutoff<-seq(61,70)
accuracy<-map_dbl(cutoff, function(x){
  y_hat<-ifelse(train_set$height>x, "Male", "Female") %>%
    factor(levels=levels(test_set$sex))
  mean(y_hat==test_set$sex)
})

# in the lesson he shows a chart
# showing that the best cutoff point is
# 64 inches, and that the accuracy rate in that case is .817 percent

# testing our new cutoff point on the test data
# it's code that is based on new variables that they created without showing, 
# so i didn't copy it.

# creating a 'confusion matrix',
# i.e. a table that  shows false positives and false negatives
data("heights")

set.seed(2)
test_index<-createDataPartition(y, times=1, p=0.5, list=FALSE)

train_set<-heights[-test_index,]
test_set<-heights[test_index,]

head(train_set)

y<-heights$sex
x<-heights$height

test_set$y_hat<-ifelse(test_set$x>64, "Male", "Female") %>%
  factor(levels=levels(test_set$sex))

test_set$y_hat
class(test_set$height)

length(test_set$y_hat)
length(test_set$y_hat)

table(predicted=test_set$y_hat, actual=test_set$sex)

#computing the accuracy separately for each sex
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarize(accuracy=mean(y_hat==sex))

# checking the prevalence of males
prev<-mean(y=="Male")
prev

# the accuracy was high even though there was a big error predicting women,
# because the dataset happened to contain more men than women
# therefore we need to study sensitivity and specificity separately

# studying sensitivity and sepcificity separately
# sensitivity = (amount of true positives) / (amount of true positives + amount of false negatives)
# or: the proportion of actual positives that are called positives
# this quantity is referred to as the 'true positive rate' or 'recall'

# specificity = (amount of true negatives) / (amount of true negatives + amount of false positives)
# or: the proportion of actual negatives that are called negatives
# this quantity is referred to as the 'true negative rate' 

# precision or 'positive predictive value' = the proportion of predicted positives that are actually positives
# unlike the true positive rate or the true relative rate, precision depends on prevalence.
# (nir: that is not good, since prevalence in the data may be different than
# prevalence in reality)

# the command 'confusionMatrix' uses the first level of a factor as "1" unless defined otherwise
# it shows the different type of errors for a certain level, i.e. for females.

# examining the different type of errors for females:
confusionMatrix(data=test_set$y_hat,reference=test_set$sex)

# sometimes it is useful to have a one number summary of the accuracy
# for instance, for optimization
# balanced accuracy = the average of sensitivity and specificity 
# the harmonic average is computed, because sensitivity and specificity are rates:
# F1 score= 1 / (0.5(1/recall + 1/precision))
# this is called the F1-score. It is a widely used one-number summary of the accuracy.

# the F1 score can be adopted to weigh specificity and sensitivity differently
# for this 'beta' is used
# see the formula in the video

# the F_meas function computes it with beta defaulting to 1

# re-building our prediction algorithm to maximize the F score
# instead of overall accuracy

cutoff<-seq(61,70)
F_1<-map_dbl(cutoff, function(x){
  y_hat<-ifelse(train_set$height>x, "Male", "Female") %>%
    factor(levels=levels(test_set$sex))
  F_meas(data=y_hat, reference=factor(train_set$sex))
})

table(F_1, cutoff)
max(F_1)

# finding the best cutoff point
best_cutoff<-cutoff[which.max(F_1)]
best_cutoff

# up to this point we used two methods for measuring the accuracy
# of our prediction: measuring the percentage of accurate predictions
# and F_1
# which one is better?
# a common approach for comparing two measures is plotting them both 
# and comparing the plots
# a widely used plot is the ROC curve
# it plots sensitivity (True Positive Ratio) vs. 1-specificity (False Positive Ratio)

# constructing an ROC curve for our second approach
cutoffs<-c(50,seq(60,75),80)
height_cutoff<-map_df(cutoffs,function(x){
  y_hat<-ifelse(test_set$height>x,"Male","Female") %>%
    factor(levels=c("Female","Male"))
  list(method="Height cutoff",
       FPR=1-specificity(y_hat,test_set$sex),
       TPR=sensitivity(y_hat,test_set$sex))
})

# ROC curves have one weakness - neither of the meausres
# plotted depends on prevalence
# in cases in which prevalence matters, we may make a
# "precision-recall plot"

# creating the plot
guessing<-map_df(probs,
                 function(p){
  y_hat<-sample(c("Male","Female"),length(test_index),
                replace=TRUE,prob=c(p,1-p)) %>%
    factor(levels=c("Female","Male"))
  list(method="Guess",
       recall=sensitivity(y_hat,test_set$sex),
       precision=precision(y_hat,test_set$sex))
                 })

height_cutoff<-map_df(cutoffs,
                 function(p){
  y_hat<-if_else(test_set$height>x,"Male","Female") %>%
    factor(levels=c("Female","Male"))
  list(method="Height cutoff",
      recall=sensitivity(y_hat,test_set$sex),
      precision=precision(y_hat,test_set$sex))
                 })

# the curve shows that the precision of guessing is low.
# this is because of the low prevalence of females
# if we change positive to mean "male"
# the precision-recall curve will change, showing high precision
# but the ROC curve will not change

# Comprehension Check: Confusion Matrix
library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# exploring the dataset
head(dat)
class(dat)
dim(dat)
nrow(dat)
ncol(dat)
names(dat)
summary(dat)

length(dat$sex)
length(dat$type)
mean(dat$sex=="Female")
mean(dat$sex=="Male")
summary(dat$type)
unique(dat$type)

# creating separate datasets for inclass and online students
dat_inclass<-dat%>%
  filter(type=="inclass")
summary(dat_inclass)

dat_online<-dat%>%
  filter(type=="online")
summary(dat_online)

# calculating the percentages of females among inclass and online students
mean(dat_inclass$sex=="Female")
mean(dat_online$sex=="Female")
table(dat)
print(26/39)
print(42/111)

# calculating the accuracy if we would use type to predict sex
print((26+69)/150)

# writing a line of code to show the confusion matrix, using
# the 'table' function and assuming the prediction is y_hat and the truth is y
table(predicted=y_hat, actual=y)

# calculating the sensitivity of this prediction
# sensitivity = (amount of true positives) / (amount of true positives + amount of false negatives)
# Female in this data is defined as positive
# calculating the amount of true positives (correct classification of females)
print(26)
# calculating the amount of false negatives (incorrect classification of males)
print(42)
# calculatin the sensitivity
print(26/(26+42))

# calculating the specificity of this prediction
# specificity = (amount of true negatives) / (amount of true negatives + amount of false positives)
# Female in this data is defined as positive
# displaying the amount of true negatives (correct classification of males)
print(69)
# displaying the amount of false negatives (incorrect classification of females)
print(13)
# calculatin the specificity
print(69/(69+13))

# calculating the prevalence of females in the dataset
mean(dat$sex=="Female")

# Comprehension Check: Practice with Machine Learning
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# creating an even split of the data
set.seed(2)
test_index<-createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# intermediary step 1 - exploring the dataset
summary(iris)
head(iris)
class(iris)
dim(iris)
nrow(iris)
ncol(iris)
names(iris)

# intermediary step 2 - Defining the training set and the test set
train_set<-iris[-test_index,]
test_set<-iris[test_index,]

summary(train_set)
summary(test_set)

# intermediary step 3 - creating the predictive variables
# Sepal Length
cutoff_sl<-(seq(2*(min(train_set$Sepal.Length)),2*(max(train_set$Sepal.Length))))/2
cutoff_sl            
accuracy_sl<-map_dbl(cutoff_sl, function(x){
  y_hat<-ifelse(train_set$Sepal.Length>x, "virginica", "versicolor") %>%
    factor(levels=levels(train_set$Species))
  mean(y_hat==test_set$Species)
})
summary(accuracy_sl)

# Sepal Width
cutoff_sw<-(seq(3*(min(train_set$Sepal.Width)),3*(max(train_set$Sepal.Width))))/3
cutoff_sw            
accuracy_sw<-map_dbl(cutoff_sw, function(x){
  y_hat<-ifelse(train_set$Sepal.Width>x, "virginica", "versicolor") %>%
    factor(levels=levels(train_set$Species))
  mean(y_hat==test_set$Species)
})
summary(accuracy_sw)

# Petal length
cutoff_pl<-(seq(100*(min(train_set$Petal.Length)),100*(max(train_set$Petal.Length))))/100
cutoff_pl            
accuracy_pl<-map_dbl(cutoff_pl, function(x){
  y_hat<-ifelse(train_set$Petal.Length>x, "virginica", "versicolor") %>%
    factor(levels=levels(train_set$Species))
  mean(y_hat==test_set$Species)
})
summary(accuracy_pl)

# Petal width
cutoff_pw<-(seq(5*(min(train_set$Petal.Width)),5*(max(train_set$Petal.Width))))/5
cutoff_pw            
accuracy_pw<-map_dbl(cutoff_pw, function(x){
  y_hat<-ifelse(train_set$Petal.Width>x, "virginica", "versicolor") %>%
    factor(levels=levels(train_set$Species))
  mean(y_hat==test_set$Species)
})
summary(accuracy_pw)

# finding the overall accuracy, using the smart cutoff point
best_cutoff_pl<-cutoff_pl[which.max(accuracy_pl)]
best_cutoff_pl
# checking:
table (cutoff_pl,accuracy_pl)

test_set_with_prediction<-test_set %>%
  mutate(prediction=
           ifelse(Petal.Length>best_cutoff_pl, "virginica", "versicolor"))
head(test_set_with_prediction)
mean(test_set_with_prediction$Species==test_set_with_prediction$prediction)

# the code that they supplied for answering the previous question
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)


remove(a)

a<-c(1,2,3)
class(a)
table(a)
b<-c(0,3,4)
b
dat<-data.frame(a=a,b=b)
head(dat)
dat_with_c<-dat %>% 
  mutate(c=ifelse(a>2,"yes","no"))
dat_with_c

# finding the feature that best optimizes the accuracy of all the data
# (both training and test sets)
# Sepal Length
cutoff_sl<-seq(range(iris$Sepal.Length)[1],range(iris$Sepal.Length)[2],by=0.1)
cutoff_sl            
accuracy_sl<-map_dbl(cutoff_sl, function(x){
  y_hat<-ifelse(iris$Sepal.Length>x, "virginica", "versicolor") %>%
    factor(levels=levels(iris$Species))
  mean(y_hat==iris$Species)
})
summary(accuracy_sl)

# Sepal Width
cutoff_sw<-seq(min(iris$Sepal.Width),max(iris$Sepal.Width),by=0.1)
cutoff_sw            
accuracy_sw<-map_dbl(cutoff_sw, function(x){
  y_hat<-ifelse(iris$Sepal.Width>x, "virginica", "versicolor") %>%
    factor(levels=levels(iris$Species))
  mean(y_hat==iris$Species)
})
summary(accuracy_sw)

# Petal length
cutoff_pl<-seq(min(iris$Petal.Length),max(iris$Petal.Length),by=0.1)
cutoff_pl            
accuracy_pl<-map_dbl(cutoff_pl, function(x){
  y_hat<-ifelse(iris$Petal.Length>x, "virginica", "versicolor") %>%
    factor(levels=levels(iris$Species))
  mean(y_hat==iris$Species)
})
summary(accuracy_pl)

# Petal width
cutoff_pw<-seq(min(iris$Petal.Width),max(iris$Petal.Width),by=0.1)
cutoff_pw            
accuracy_pw<-map_dbl(cutoff_pw, function(x){
  y_hat<-ifelse(iris$Petal.Width>x, "virginica", "versicolor") %>%
    factor(levels=levels(iris$Species))
  mean(y_hat==iris$Species)
})
summary(accuracy_pw)

# combining information from variables 'petal width' and 'petal length'
# to predict species 
# Petal length
cutoff_pl<-(seq(100*(min(train_set$Petal.Length)),100*(max(train_set$Petal.Length))))/100
cutoff_pl            
accuracy_pl<-map_dbl(cutoff_pl, function(x){
  y_hat<-ifelse(train_set$Petal.Length>x, "virginica", "versicolor") %>%
    factor(levels=levels(train_set$Species))
  mean(y_hat==test_set$Species)
})
summary(accuracy_pl)

# Petal width
cutoff_pw<-(seq(5*(min(train_set$Petal.Width)),5*(max(train_set$Petal.Width))))/5
cutoff_pw            
accuracy_pw<-map_dbl(cutoff_pw, function(x){
  y_hat<-ifelse(train_set$Petal.Width>x, "virginica", "versicolor") %>%
    factor(levels=levels(train_set$Species))
  mean(y_hat==test_set$Species)
})
summary(accuracy_pw)

best_cutoff_pl<-cutoff_pl[which.max(accuracy_pl)]
best_cutoff_pl

best_cutoff_pw<-cutoff_pw[which.max(accuracy_pw)]
best_cutoff_pw

test_set_with_prediction<-test_set %>%
  mutate(prediction=
           ifelse(test_set$Petal.Length>best_cutoff_pl, "virginica",
                  ifelse(test_set$Petal.Width>best_cutoff_pw,
                         "virginica", "versicolor")))
head(test_set_with_prediction)
mean(test_set_with_prediction$Species==test_set_with_prediction$prediction)

# their solution:

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)

id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species)

# assessment - conditional probabilities
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# exploring the data
tail(disease)
summary(disease)
class(disease)

# probability that the test is positive:
mean(test==1)

# probability that an individual has the disease
# if the test is negative
mean(test==0 & disease==1)

# probability that an individual has the disease
# if the test is positive
mean(test==1 & disease==1)/(mean(test==1 & disease==1)+mean(test==1 & disease==0))

# the "relative risk" of having the disease if the test is positive
# i.e. how many times higher is a person's chance of having the disease
# given the he was tested positive, compared with someone who wasn't tested
(mean(test==1 & disease==1)/(mean(test==1 & disease==1)+mean(test==1 & disease==0)))/mean(disease==1)

# Comprehension Check: Conditional Probabilities Practice
library(dslabs)
library(dplyr)
library(ggplot2)
data("heights")

# Round the heights to the closest inch and 
# plot the estimated conditional probability of being male for each height
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# use the quantile (\ 0.1,0.2,\dots,0.9 \) and the cut function 
# to assure each group has the same number of points.
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# generate data from a bivariate normal distrubution using the MASS package

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

# estimate the conditional expectations and make a plot
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

# Comprehension Check: Linear Regression
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

class(Sigma)
Sigma
names(dat)
head(dat)
class(dat)
summarize(dat)

remove(index,test,train)
# Use the caret package to partition the dataset into test and training sets 
# of equal size. 
length(dat$y)
test_index<-createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test_set <- dat %>% slice(index)
train_set <- dat %>% slice(-index)

length(test$y)
length(train$y)

#Train a linear model 
fit<-lm(y~x,data=train_set)
fit$coefficients

# and calculate the RMSE. 
y_hat<-fit$coeff[1]+fit$coeff[2]*test_set$x
mean((y_hat-test_set$x)^2)

#Repeat this exercise 100 times 
#and report the mean and standard deviation of the RMSEs. 
#(Hint: You can use the code shown in a previous course 
#inside a call to replicate using a seed of 1.
set.seed(1)
n<-100
sigma<-9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), sigma) %>% data.frame() %>% setNames(c("x", "y"))
set.seed(1)
rmse <- replicate(n, { index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test_set <- dat %>% slice(index)
train_set <- dat %>% slice(-index)
fit<-lm(y~x,data=train_set)
y_hat<- fit$coeff[1]+fit$coeff[2]*test_set$x
sqrt(mean((y_hat-test_set$y)^2))
})
mean(rmse)
sd(rmse)

# their answer:
set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

# Now we will repeat the above but using larger datasets. 
#Repeat the previous exercise but for datasets with n <- c(100, 500, 1000, 5000, 10000). 
#Save the average and standard deviation of RMSE from the 100 repetitions 
#using a seed of 1. Hint: use the sapply or map functions.

n<- c(100,500, 1000, 5000,10000)
set.seed(1)
function_Rmses<-function(n){sigma<-9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), sigma) %>% data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, { index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  test_set <- dat %>% slice(index)
  train_set <- dat %>% slice(-index)
  fit<-lm(y~x,data=train_set)
  y_hat<- fit$coeff[1]+fit$coeff[2]*test_set$x
  sqrt(mean((y_hat-test_set$y)^2))
  })
}

results<-sapply(n, function_Rmses)
results

#100
mean(results[1:100,1])
sd(results[1:100,1])

#500
mean(results[1:100,2])
sd(results[1:100,2])

#1000
mean(results[1:100,3])
sd(results[1:100,3])

#5000
mean(results[1:100,4])
sd(results[1:100,4])

#10000
mean(results[1:100,5])
sd(results[1:100,5])

# their answer:
set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

# Now repeat the exercise from Q1, this time making the correlation between x and y larger, as in the following code:
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

# Create a data set using the following code.

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


# exploring the dataset
head(dat)
class(dat)
dim(dat)
nrow(dat)
ncol(dat)
names(dat)
summary(dat)

# Note that y is correlated with both x_1 and x_2 
# but the two predictors are independent of each other, as seen by cor(dat).

cor(dat)

# Use the caret package to partition into a test and training set of equal size. 
set.seed(1)
index<-createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test_set <- dat %>% slice(index)
train_set <- dat %>% slice(-index)

# exploring the two datasets
dim(test_set)
dim(train_set)


# Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a linear model for each.

#Calculating RMSE only for x_1
#Training a linear model  
fit1<-lm(y~x_1,data=train_set)
fit1$coefficients

# predicting on the test set
y_hat1 <- predict(fit1, newdata = test_set)
y_hat1

length(y_hat1)
length(test_set$y)

# and calculate the RMSE. 
rmse1<-sqrt(mean((y_hat1-test_set$y)^2))
rmse1

# making sure:
error<-y_hat-test_set$y
squared_error<-error^2
squared_error
mean_squared_error<-mean(squared_error)
mean_squared_error
root_mean_squared_error=mean_squared_error^.5
root_mean_squared_error
rmse
length(rmse)


#Calculating RMSE only for x_2
#Training a linear model  
fit2<-lm(y~x_2,data=train_set)
fit2$coefficients

# predicting on the test set
y_hat2 <- predict(fit2, newdata = test_set)
y_hat2
# and calculate the RMSE. 
rmse2<-sqrt(mean((y_hat2-test_set$y)^2))
rmse2

#Calculating RMSE for both x_1 and x_2
#Training a linear model
fit12<-lm(y~x_1+x_2,data=train_set)
fit12$coefficients

# predicting on the test set
y_hat12 <- predict(fit12, newdata = test_set)

# and calculate the RMSE. 
rmse12<-sqrt(mean((y_hat12-test_set$y)^2))
rmse12

# Which of the three models performs the best (has the lowest RMSE)?
rmse1
rmse2
rmse12

# Repeat the exercise from q6 but now create an example in which x_1 and x_2 are highly correlated.

# set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Use the caret package to partition into a test and training set of equal size. 
# Use the caret package to partition into a test and training set of equal size. 
set.seed(1)
index<-createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
test_set <- dat %>% slice(index)
train_set <- dat %>% slice(-index)

# Compare the RMSE when using just x_1, just x_2, and both x_1 and x_2.
#Calculating RMSE only for x_1
#Training a linear model  
fit1<-lm(y~x_1,data=train_set)
fit1$coefficients

# predicting on the test set
y_hat1 <- predict(fit1, newdata = test_set)
y_hat1

# and calculate the RMSE. 
rmse1<-sqrt(mean((y_hat1-test_set$y)^2))
rmse1

#Calculating RMSE only for x_2
#Training a linear model  
fit2<-lm(y~x_2,data=train_set)
fit2$coefficients

# predicting on the test set
y_hat2 <- predict(fit2, newdata = test_set)
y_hat2
# and calculate the RMSE. 
rmse2<-sqrt(mean((y_hat2-test_set$y)^2))
rmse2

#Calculating RMSE for both x_1 and x_2
#Training a linear model
fit12<-lm(y~x_1+x_2,data=train_set)
fit12$coefficients

# predicting on the test set
y_hat12 <- predict(fit12, newdata = test_set)

# and calculate the RMSE. 
rmse12<-sqrt(mean((y_hat12-test_set$y)^2))
rmse12

# Which of the three models performs the best (has the lowest RMSE)?
rmse1
rmse2
rmse12


# logistic regression excercise
# "Define a dataset using the following code:"

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()


# "Note that we have defined a variable x that is predictive of a binary outcome y:" 
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# exploring the data
head(dat$train)
summarise(dat$train)
dim(dat$train)
class(dat$train)


# Generate 25 different datasets changing the difference between the two classes
#using delta <- seq(0, 3, len=25) and plot accuracy vs mu_1.
delta <- seq(0, 3, len=25)

#their answer:
set.seed(1)
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)

# smoothing
# "In the Wrangling course of this series, PH125.6x, 
# we used the following code to obtain mortality counts for Puerto Rico for 2015-2018:"

library(tidyverse)
library(purrr)
library(pdftools)
library(lubridate)
library(stringr)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

# Use the loess function to obtain a smooth estimate of the expected number of deaths as a function
# of date. Plot this resulting smooth function. Make the span about two months long.

#exploring the data
dim(dat)
names(dat)
head(dat)
class(dat)
class(dat$deaths)
class(dat$date)

# creating a numeric date variable
dat<-dat %>% mutate(date_num=as.numeric(date)) %>%
  filter(!is.na(deaths))
head(dat)
dim(dat)

#plotting the number of deaths as a function of date
length(dat$date_num)
length(dat$deaths)
dat %>% ggplot(aes(date_num,deaths)) +
                 geom_point(size=3,alpha=.5,color="blue")

# producing a kernel smoothed line with a two month span, without loess
span<-60
fit<-with(dat,
          ksmooth(date_num,deaths,kernel="box",bandwidth =span,x.points=date_num))
dat %>% mutate(smooth=fit$y) %>% 
  ggplot(aes(date_num,deaths)) +
  geom_point(size=3,alpha=.5,color="blue") + 
  geom_line(aes(date_num,smooth),color="red",size=2)

# changing the smoothing from 'box' to 'normal'
# therefore using the 'normal' or 'Gaussian' density to assign weights
span<-60
fit<-with(dat,
          ksmooth(date_num,deaths,kernel="normal",bandwidth =span,x.points=date_num))
dat %>% mutate(smooth=fit$y) %>% 
  ggplot(aes(date_num,deaths)) +
  geom_point(size=3,alpha=.5,color="blue") + 
  geom_line(aes(date_num,smooth),color="red",size=2)

# smoothing with a local weighted regression
total_days<-diff(range(dat$date_num))
total_days
span<-60/total_days
span
fit<-loess(deaths~date_num,degree=1,span=span,data=dat)
dat %>% mutate(smooth=fit$fitted) %>% 
  ggplot(aes(date_num,deaths)) +
  geom_point(size=3,alpha=.5,color="blue") + 
  geom_line(aes(date_num,smooth),color="red",size=2)

#their answer:
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = 2)

# Work with the same data as in Q1 to plot 
# smooth estimates against day of the year, 
# all on the same plot, but with different colors for each year.
# Which code produces the desired plot?

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

# Suppose we want to predict 2s and 7s in the mnist_27 dataset with just 
# the second covariate. Can we do this? 
# On first inspection it appears the data does not have much predictive power.
# In fact, if we fit a regular logistic regression the coefficient for x_2 is not significant!
# This can be seen using this code:
library(dslabs)
data("mnist_27")
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

# Plotting a scatterplot here is not useful since y is binary:
qplot(x_2, y, data = mnist_27$train)
  
# The loess line can be plotted using the following code:
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

#Note that there is indeed predictive power, 
# but that the conditional probability is non-linear.

# Matrices lessons
mnist<-read_mnist()
class(mnist)
names(mnist)
class(mnist$train)
length(mnist$train)
length(mnist$test)
class(mnist$train$images)

# checking the dimensions of the training set
dim(mnist$train$images)

# creating a smaller, more manageable dataset
# with only the first 1,000 images and their labels (the 'guess' of what number they represent)
x<-mnist$train$images[1:1000,]
y<-mnist$train$labels[1:1000]

class(x)
class(y)
head(x)
head(y)
mean(y)
length(y)
dim(x)
dim(as.matrix(y))

# combining two vectors to form a matrix
x_1<-1:5
x_2<-6:10
cbind(x_1,x_2)

# the 'dim' function in r does not report dimensions of vectors
dim(x_1)
# but their dimensions can be checked with the following code
dim(as.matrix(x_1))

# checking the dimensions of our training dataset
dim(x)
length(y)
dim(as.matrix(y))

# converting the row of predictors into a matrix 
# that represents the grid used for analyzing 
# the handwriting on envelopes

# first - a simple example with a short vector
my_vector<-1:15
mat<-matrix(my_vector,5,3)
mat

# the 'matrix' function fills in the matrix by column (the first number) and then by row
# we can fill it first by row (or 'transpose' the matrix above) using this code:
mat_t<-matrix(my_vector,3,5,byrow=TRUE)
mat_t

# we can use the function t to directly transpose the matrix
t(mat)
identical(mat_t,t(mat))

# warning: the matrix function in r recycles values without warning,
# in case the product of columns and rows does not match
# the length of a vector. 
# Demonstration:
matrix(my_vector,5,5)

# now analyzing the digits data.
# let us examine the third row of the train set.
# first, let us check which digit it represents:
y[3]
# it represents a 4.
# we know that the analysis uses a matrix of pixels
# of dimension 28 by 28
grid<-matrix(x[3,],28,28)
dim(grid)
head(grid)

# confirming that we have done this correctly by showing the image
image(1:28,1:28,grid)
# the image is upside-down because r shows pixel number 1 at the bottom
# we can flip the image like this:
image(1:28,1:28,grid[,28:1])

# checking the total pixel darkness by digit
# we want to sum the values of each row
# and then visualize how the values vary by digit

sums<-rowSums(x)

# exploring the 'sums' vector
class(sums)
length(sums)
plot(sums)
histogram(sums)
range(sums)
table(sums)

# computing the averages of the pixel intensity in our training set
avg<-rowMeans(x)

# exploring the 'averages' vector
class(avg)
length(avg)
plot(avg)
histogram(avg)
range(avg)
table(avg)

# generating a box plot to see how the average pixel intensity
# changes from digit to digit
# 1. making sure that both vectors have the same length
length(avg)
length(y)
# 2. combining them into a data frame
x_name <- "avg"
y_name <- "y"

require(reshape2)
df <- data.frame(avg,y)
colnames(df) <- c(x_name, y_name)
dim(df)
head(df)
# e. creating the boxplot
boxplot<-ggplot(df,aes(x=y,y=avg,group=y))+
  geom_boxplot()+
  scale_x_continuous(breaks=seq(0,9,1))
boxplot


# note: we could also use the apply function for calculating row means or column means.
# the first argument of the apply function is the matrix.
# the second argument is the dimension - 1 for rows, 2 for columns
# for example, row means can be calculated like this:
avgs<-apply(x,1,mean)
head(avgs)
identical(avg,avgs)
# the standard deviation for each column:
sd<-apply(x,2,sd)
class(sd)
range(sd)
length(sd)

# next task: "study the variation of each pixel
# and remove columns associated with pixels
# that don't change much"

# compute the standard deviation of each pixel across all entries
# "since each column represents a pixel,
# we use the colSds function from the matrixStats package like this:
library(matrixStats)
sds<-colSds(x)
# checking the distribution of the standard deviations
df<-data.frame(sds)
head(df)
chart<-ggplot(df,aes(x=sds))+
  geom_histogram()
chart
# we will set the cutoff point at standard deviation=60.
# removing uninformative predictors from our matrix
new_x<-x[ ,colSds(x)>60]
# checking how many columns remain
dim(new_x)
dim(x)
# 314 columns remain. originally we had 784

# note: if we perform an operation that leaves only 1 column
# or 1 row, the result will not be classified as a matrix.
# but we can preserve the matrix class by using the argument 'drop'
# like this:
class(x[ ,1,drop=FALSE])
dim(x[ ,1,drop=FALSE])
class(x[1,,drop=FALSE])
dim(x[1,,drop=FALSE])

# now we want to look at a histogram of all our pixels
# in order to do that we need to turn the matrix into vectors
# we can do it like this:
mat<-matrix(1:15,5,3)
mat
as.vector(mat)

# creating a historgram of all our predictors in the training set
qplot(as.vector(x),bins=30,color=I("black"))

# we see a clear dichotomy. it can be explanied by 
# some parts having ink on them and some parts not having ink.
# we will assume the parts with under 50 are just smudges.
# we will turn them to zero.
new_x<-x
new_x[x<50]<-0

# now we want to binarize the data. 
# our cutoff point will be the middle of the range, 255/2=122.5
bin_x<-x
bin_x[bin_x<255/2]<-0
bin_x[bin_x>255/2]<-1
class(bin_x)
table(bin_x)
class(x)
# we can also convert the vector x into a matrix
# and coerce it using logicals
# like this
bin_x<-(x>255/2)*1
class(bin_x)

# our final task is to standardize the rows or the columns
# in R, when you subtract a vector from a matrix,
# it subtracts the first element of the vector from each element of the first row
# of the matrix. the secod element of the vector is subtracted from each element
# of the secod row of the matrix, and so on.
# the same holds true for other arithmetic operations between matrices and vectors
# So we can scale each row of a matrix using this code:
(x-rowMeans(x))/rowSds(x)
# this does not work for columns. For columns we would have to transpose the matrix, 
# standardize the rows, and then transpose it back.

# we can also use a function called sweep, which works in a similar
# way to apply
# it takes each entry of a vector and subtracts it from 
# the corresponding row or column.
# for example, here is the code for subtracting the mean of each column
# from the values in each column:
X_mean_0<-sweep(x,2,colMeans(x))
# (the '2' denotes column. if it would have been '1' it would have referred to rows)

# using the function 'sweep' for dividing by standard deviation:
x_standardized<-sweep(X_mean_0,2,colSds(x),FUN="/")
# (Fun="/" denotes calculating the standard deviation. the default is subtracting.)

# the course does not cover matrix multiplication
# but here is the code, for those who want to do it.
# the command is '%*%'
# for example, the cross product can be written like this:
t(x)%*%x
# we could also use the function with that name:
crossprod(x)

# to compute the inverse of a function, we use solve
solve(crossprod(x))

# the qr decomposition can be done with the qr function:
qr(x)

# comprehension check
x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)
ncol(x)

# For each digit in the mnist training data, 
# compute the proportion of pixels that are in the grey area, 
# defined as values between 50 and 205. 
# (To visualize this, you can make a boxplot by digit class.)

mnist<-read_mnist()
x<-mnist$train$images
y<-mnist$train$labels
dim(x)
length(y)


# creating a more manageable dataset
x<-mnist$train$images[1:1000,]
y<-mnist$train$labels[1:1000]

# marking the grey pixels
grey_x<-x
grey_x[grey_x<50]<-0
grey_x[grey_x>205]<-0
grey_x[(grey_x>=50 & grey_x<=205)]<-1
table(grey_x)
prop<-rowSums(grey_x)/ncol(grey_x) 
length(prop)
mean(prop)
print(48503/(48503+735497))

#combining prop with y
comb<-cbind(prop,y)
dim(comb)

# plotting the proportions by digits
x_name <- "prop"
y_name <- "y"
require(reshape2)
df <- data.frame(prop,y)
colnames(df) <- c(x_name, y_name)
dim(df)
head(df)
# creating the boxplot
boxplot<-ggplot(df,aes(x=y,y=prop,group=y))+
  geom_boxplot()+
  scale_x_continuous(breaks=seq(0,9,1))
boxplot

# caculating the means
means<-aggregate(prop~y, data=df, FUN=function(x) c(mean=mean(x)))
means

# now doing the same thing with the whole dataset
mnist<-read_mnist()
x<-mnist$train$images
y<-mnist$train$labels
dim(x)
length(y)

# marking the grey pixels
grey_x<-x
grey_x[grey_x<50]<-0
grey_x[grey_x>205]<-0
grey_x[(grey_x>=50 & grey_x<=205)]<-1
table(grey_x)
prop<-rowSums(grey_x)/ncol(grey_x) 
length(prop)
mean(prop)
print(48503/(48503+735497))

#combining prop with y
comb<-cbind(prop,y)
dim(comb)

# plotting the proportions by digits
x_name <- "prop"
y_name <- "y"
require(reshape2)
df <- data.frame(prop,y)
colnames(df) <- c(x_name, y_name)
dim(df)
head(df)
# creating the boxplot
boxplot<-ggplot(df,aes(x=y,y=prop,group=y))+
  geom_boxplot()+
  scale_x_continuous(breaks=seq(0,9,1))
boxplot

# caculating the means
means<-aggregate(prop~y, data=df, FUN=function(x) c(mean=mean(x)))
means

# their answer:
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y)

# Lesson - "Distance"
# Generating the digist training set with only 2 and 7
set.seed(0)
if(!exists("mnist")) mnist<-read_mnist()
ind<-which(mnist$train$labels %in% c(2,7)) %>% sample (500)
x<-mnist$train$images[ind,]
y<-mnist$train$labels[ind]

dim(x)
dim(as.matrix(y))
class(x)
class(y)
length(y)

# checking the first three labels
y[1:3]
x_1<-x[1,]
x_2<-x[2,]
x_3<-x[3,]

# calculating the euclidean distance between the x's
sqrt(sum((x_1-x_2)^2))
sqrt(sum((x_1-x_3)^2))
sqrt(sum((x_2-x_3)^2))

# computing it through matrix algebra, with the cross product
sqrt(crossprod(x_1-x_2))
sqrt(crossprod(x_1-x_3))
sqrt(crossprod(x_2-x_3))

# computing it using the function 'dist'
d<-dist(x)
class(d)
head(d)
dim(as.matrix(d))
d[1]
d[2]
as.matrix(d)[1:3,1:3]
image(as.matrix(d))
image(as.matrix(d)[order(y),order(y)])

# until this point we computed the distance between two rows.
# we may think of this as the distance between two 'cases', or two 'people' 
# in the dataset. (in our example - two handwritten digits)
# but we can also predict the distance between predictors.
# to do that we simply transpose the matrix, and then 
# use the 'dist' function. like this:
d<-dist(t(x))
dim(as.matrix(d))

# checking the distance between the 492nd pixel and all the rest of the pixels
c<-as.matrix(d)[492,]
class(c)
length(c)
e<-matrix(c,28,28)
dim(e)
image(e)

#comprehension check

#"Load the following dataset:"
library(dslabs)
data("tissue_gene_expression")

#"This dataset includes a matrix x:"
dim(tissue_gene_expression$x)

#"This matrix has the gene expression levels of 500 genes 
# from 189 biological samples representing seven different tissues. 
# The tissue type is stored in y:"
table(tissue_gene_expression$y)

# Which of the following lines of code computes the Euclidean distance
# between each observation and stores it in the object d?
d <- dist(tissue_gene_expression$x)
head(d)
# "Compare the distances between observations 1 and 2 (both cerebellum), 
# observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium)."
as.matrix(d)[1,2]
as.matrix(d)[1,39]
as.matrix(d)[1,40]
as.matrix(d)[1,73]
as.matrix(d)[1,74]
as.matrix(d)[2,39]
as.matrix(d)[2,40]
as.matrix(d)[2,73]
as.matrix(d)[2,74]
as.matrix(d)[39,40]
as.matrix(d)[39,73]
as.matrix(d)[39,74]
as.matrix(d)[40,73]
as.matrix(d)[40,74]
as.matrix(d)[73,74]

# their answer:
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# "Make a plot of all the distances 
# using the image function to see if the pattern you observed in Q2 is general."
# "Which code would correctly make the desired plot?"
image(as.matrix(d))

# making sure
sqrt(sum((tissue_gene_expression$x[39,]-tissue_gene_expression$x[40,])^2))

# k nearest neighbor
# starting with logisitc regression. this would be the baseline that we need to beat.
library(dslabs)
data("mnist_27")
fit_glm<-glm(y~x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic<-predict(fit_glm, mnist_27$test)
y_hat_logistic<-factor(ifelse(p_hat_logistic>0.5, 7, 2))
confusionMatrix(data=y_hat_logistic, reference=mnist_27$test$y)$overall[1]

# now we will estimate with knn
# there are two ways to call the function 'knn3' in R.
# first way:
knn_fit<-knn3(y~., data=mnist_27$train)

# second way:
# define a matrix with the predictors 
x<-as.matrix(mnist_27$train[,2:3])
# define a vector with the outcomes
y<-mnist_27$train$y
# call the function like this:
knn_fit<-knn3(x,y)

# the first way is quickest, but once we have large datasests with many predictors
# we will want to use the second approach. 

# now we pick the number of neighbors to include.
# the default is k=5.
# we can write it explicitly, like this: k=5

knn_fit<-knn3(y~., data=mnist_27$train, k=5)

# creating y_hat:
y_hat_knn<-predict(knn_fit, mnist_27$test, type="class")
# specifiying 'type="class"' produces the actual outcomes that are predicted, by a 
# majority 'vote' of the neighbors. Another way of thinking of it is 
# that the average probability is calculated, based on the outcomes
# of the neighbors, and then the outcome is chosen
# with the cutoff probability 0.5. 
# if we would not write 'type="class'" we would get the probabilities for each class.

# computing our accuracy
confusionMatrix(data=y_hat_knn, reference=mnist_27$test$y)$overall["Accuracy"]
# (this is equivalent to:)
confusionMatrix(data=y_hat_knn, reference=mnist_27$test$y)$overall[1]

# overtraining and oversmoothing
# reminder: here is the accuracy of the prediction in our test set
data("mnist_27")
knn_fit<-knn3(y~., data=mnist_27$train, k=5)
y_hat_knn<-predict(knn_fit, mnist_27$test, type="class")
confusionMatrix(data=y_hat_knn, reference=mnist_27$test$y)$overall["Accuracy"]

# here is the accuracy in our training set
y_hat_knn<-predict(knn_fit, mnist_27$train, type="class")
confusionMatrix(data=y_hat_knn, reference=mnist_27$train$y)$overall["Accuracy"]

# the accuracy is higher in our training set because of overfitting, since 
# the number of nearest neighbors (k) is too small. it is set to 5 as default.
# when we use k=1 our accuracy is almost 1 because the predictors are almost unique
# i.e. there are almost no identical x's
knn_fit<-knn3(y~., data=mnist_27$train, k=1)
y_hat_knn<-predict(knn_fit, mnist_27$train, type="class")
confusionMatrix(data=y_hat_knn, reference=mnist_27$train$y)$overall["Accuracy"]

# but when we check on the test set, the accuracy is worse than before
# and even worse than logistic regression
# because of extreme overfitting
y_hat_knn<-predict(knn_fit, mnist_27$test, type="class")
confusionMatrix(data=y_hat_knn, reference=mnist_27$test$y)$overall["Accuracy"]

# let's check different k's, i.e. different numbers of neighbors
# create a vector of all the uneven numbers between 2 and 251:
ks<-seq(2,251,2)
# these will be all the ks we try.

# trying those ks:
library(purrr)
accuracy<-map_df(ks, function(k){
  fit<-knn3(y~., data=mnist_27$train, k=k)
  y_hat<-predict(fit, mnist_27$train, type="class")
  train_error<-confusionMatrix(data=
                                 y_hat, 
        reference=mnist_27$train$y)$overall["Accuracy"]
  
  y_hat<-predict(fit, mnist_27$test, type="class")
  test_error<-confusionMatrix(data=
                                 y_hat, 
                               reference=mnist_27$test$y)$overall["Accuracy"]
  
  list(train=train_error, test=test_error)
})

# Comprehension check
# Previously, we used logistic regression to predict sex based on height. 
# Now we are going to use knn to do the same. 
# Use the code described in these videos to select the F_1 measure 
# and plot it against k. 
# Compare to the F_1 of about 0.6 we obtained with regression. 
# Set the seed to 1.

# loading the data
data("heights")

# defining x and y
y<-heights$sex
x<- heights$height
 
# partitioning into training and test sets

set.seed(1)
test_index<-createDataPartition(y, times=1, p=0.5, list=FALSE)

train_set<-heights[test_index,]
test_set<-heights[-test_index,]
head(train_set)

# checking if calling knn works
fit<- knn3(sex~height, data=train_set, k=5)
y_hat<-predict(fit, test_set, type="class")
head(y_hat)

# trying ks - from 1 to 101 in gaps of 3:
ks<-seq(1,101,3)
accuracy <- map_df(ks, function(k) {
  fit<- knn3(sex~height, data=train_set, k=k)
  y_hat<-predict(fit, test_set, type="class")
  F_val<-F_meas(data=y_hat, reference=factor(test_set$sex))
  list(k=k, F_val=F_val)
})

# plotting F_1 against k
accuracy %>% ggplot(aes(k,F_val)) + geom_line()

# computing the maximum F_val and the k associated with it
max(accuracy$F_val)
accuracy$k[which.max(accuracy$F_val)]

# their answer:
set.seed(1)
data("heights")
library(caret)
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
  test_set <- heights[test_index, ]
  train_set <- heights[-test_index, ]
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)

# "Next we will use the same gene expression example used 
# in the Comprehension Check: Distance exercises. 
# You can load it like this:

library(dslabs)
data("tissue_gene_expression")

length(tissue_gene_expression$y)
dim(tissue_gene_expression$x)
class(tissue_gene_expression)

# Split the data into training and test sets, 
# and report the accuracy you obtain. 
# Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.

x<-tissue_gene_expression$x
y<-tissue_gene_expression$y

set.seed(1)
train_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
train_set= x[-train_index]
test_set = x[train_index,] 
train_set_y = as.numeric(y[-train_index] )
test_set_y = as.numeric(y[train_index] )
class(train_set)

ks <- seq(1, 11, 2)
accuracy <- map_df(ks, function(k){ 
    knn_fit <- knn3(train_set,train_set_y, k = k) 
  y_hat <- predict(knn_fit, test_set, type = "class") 
  test_error<- confusionMatrix(data = y_hat, reference = test_set_y, mode = "everything")
overall["Accuracy"] 
list(k=k,test=test_error) }) 
accuracy

# another try


accuracy <- map_df(ks, function(k){ 
  data_matrix <- tissue_gene_expression$x
  
  y <- tissue_gene_expression$y
  
  set.seed(1)
  
  test_index <- createDataPartition(y,times=1,list=FALSE)
  
  train_matrix <- data.matrix[test_index,]
  
  y_train <- y[test_index]
  
  test_matrix <- data.matrix[-test_index,]
  
  y_test <- y[-test_index]
  
  ks <- c(1, 3, 5, 7, 9, 11)
  
  knn_fit <- knn3(train_matrix,y_train, k = k) 
  y_hat <- predict(knn_fit, test_matrix, type = "class") 
  test_error <- confusionMatrix(data = y_hat, reference = y_test, mode = "everything")$overall["Accuracy"] 
  list(k = k, test = test_error) 
})
accuracy

# their answer:
set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})

# k-fold Cross Validation
# comprehension check

# "Generate a set of random predictors and outcomes using the following code:"
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# exploring the data
dim(x)
class(x)
head(x)
ncol(x)
nrow(x)
mean(x)[1]
x[527,7898]
mean(x[,4])
mean(x[4,])
testing<-rnorm(3)
testing2<-rbinom(7,1,0.5)
length(y)
class(y)
dim(as.matrix(y))
dim(x_subset)

# Because x and y are completely independent, 
# you should not be able to predict y using x with accuracy greater than 0.5. 
# Confirm this by running cross-validation using logistic regression to fit the model. 
# Because we have so many predictors, we selected a random sample x_subset. 
# Use the subset when training the model.


# Which code correctly performs this cross-validation?
fit <- train(x_subset, y, method = "glm")
fit$results

# Now, instead of using a random selection of predictors, 
# we are going to search for those that are most predictive of the outcome. 
# We can do this by comparing the values for the y=1 group to those in the y=0  group, 
# for each predictor, using a t-test. You can perform this step like this:

install.packages("BiocManager")
BiocManager::install("genefilter",version = "3.8")
library(genefilter)
tt <- colttests(x, y)
head(tt)

# Which of the following lines of code correctly creates a vector of the p-values called pvals?
pvals <- tt$p.value

# Create an index ind with the column numbers of the predictors
# that were "statistically significantly" associated with y. 
# Use a p-value cutoff of 0.01 to define "statistically significantly."
ind <- which(pvals <= 0.01)

# How many predictors survive this cutoff?
length(ind)
# another way to count:
signif<-pvals<0.01
sum(signif)

# Now re-run the cross-validation after redefinining x_subset
# to be the subset of x defined by the columns showing 
# "statistically significant" association with y.
x_subset <- x[ ,ind]
dim(x_subset)

# What is the accuracy now?
fit <- train(x_subset, y, method = "glm")
fit$results

# Re-run the cross-validation again, but this time using kNN. 
# Try out the following grid k = seq(101, 301, 25) of tuning parameters. 
# Make a plot of the resulting accuracies.

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Use the train function to predict tissue from gene expression 
# in the tissue_gene_expression dataset. Use kNN.
# What value of k works best?

#their answer:

data("tissue_gene_expression")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results

# Bootstrap

# Comprehension check
#"The createResample function can be used to create bootstrap samples. 
# For example, we can create 10 bootstrap samples for the mnist_27 dataset like this:

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
length(mnist_27$train$y)
indexes[1]

# their answer:
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

# "What is the total number of times that 3 appears in all of the resampled indexes?"
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

# "Generate a random dataset using the following code:"
set.seed(1)
y <- rnorm(100, 0, 1)

# Estimate the 75th quantile, which we know is qnorm(0.75), 
# with the sample quantile: quantile(y, 0.75).
# Run a Monte Carlo simulation with 10,000 repetitions 
# to learn the expected value and standard error of this random variable. 
# Set the seed to 1.
qnorm(0.75)
quantile(y, 0.75)

# their answer:
set.seed(1)
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q_75)
sd(q_75)

# In practice, we can't run a Monte Carlo simulation. Use 10 bootstrap
# samples to estimate the standard error using just the initial sample y. 
# Set the seed to 1.

# Set up the variables:
B <- 10

# create the dataset
set.seed(1)
y <- rnorm(100, 0, 1)
mean(y)
sd(y)

# Getting the indexes:
set.seed(1)
indexes <- createResample(y, 10, list=FALSE)
dim(indexes)
y[indexes[,3]]
ev<-data.frame(100,10)

# creating the 10-sample dataset
for(i in 1:10){
  ev[1:100,i]<-y[indexes[1:100,i]]
}
dim(ev)

mean(ev[1:100,3])

q75<-1:10

# calculating the column 75th percentile of each column
for(i in 1:10){
  q75[i]<-quantile(ev[1:100,i], 0.75)
  }
q75

# calculating the mean, i.e. the expected value, and the standard error
mean(q75)
sd(q75)

# their answer:
set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

# Repeat the exercise from Q4 
# but with 10,000 bootstrap samples instead of 10. Set the seed to 1.
 
# their answer:
set.seed(1)
indexes <- createResample(y, 10^5)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

# lesson: Naive Bayes
# loading the heights dataset 
library(caret)
data("heights")
y <- heights$height

# partitioning the data
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# In this case, the Naive Bayes approach is particularly appropriate 
# because we know that the normal distribution is a good approximation 
# for the conditional distributions of height given sex for both classes
# \(Y=1\) (female) and \(Y=0\) (Male). 
# This implies that we can approximate the conditional distributions 
# \(f_{X|Y=1}\) and \(f_{X|Y=0}\) 
# by simply estimating averages and standard deviations from the data:

params <- train_set %>% 
  group_by(sex) %>% 
  summarize(avg = mean(height), sd = sd(height))
params

# The prevalence, which we will denote with \(\pi = \mbox{Pr}(Y=1)\), 
# can be estimated from the data by computing the proportion of females:

pi <- train_set %>% 
  summarize(pi=mean(sex=="Female")) %>% 
  .$pi
pi

# Now we can use our estimates of average and standard deviation to get an actual rule.
# We get the conditional distributions, f0 and f1,
# and then we use Bayes theorem to compute the naive Bayes estimate
# of the conditional probability.

x <- test_set$height

f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

# lesson: quadratic discriminative analysis (qda)
# loading the example data
data("mnist_27")

# In this case, we have two predictors so we assume each one is bivariate normal. 
# This implies that we need to estimate two averages, two standard deviations, 
# and a correlation for each case \(Y=1\) and \(Y=0\). 
# Once we have these, we can approximate the distributions 
# \(f_{X_1,X_2|Y=1}\) and \(f_{X_1, X_2|Y=0}\). 
# We can easily estimate parameters from the data:

params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), 
            sd_1= sd(x_1), sd_2 = sd(x_2), 
            r = cor(x_1,x_2))
params


# Here we provide a visual way of showing the approach. 
# We plot the data and use contour plots to give an idea 
# of what the two estimated normal densities look like 
# (we show the curve representing a region that includes 95% of the points):
  
mnist_27$train %>% mutate(y = factor(y)) %>% 
ggplot(aes(x_1, x_2, fill = y, color=y)) + 
geom_point(show.legend = FALSE) + 
stat_ellipse(type="norm", lwd = 1.5)

# We can use the caret package to fit the model and obtain predictors:
library(caret)
train_qda <- train(y ~ ., 
                   method = "qda",
                   data = mnist_27$train)

# We see that we obtain relatively good accuracy:
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]  

# QDA worked well here, but it becomes harder to use as the number of predictors increases.

# A relatively simple solution to the problem of having too many parameters 
# is to assume that the correlation structure is the same for all classes, 
# which reduces the number of parameters we need to estimate.

# In this case, we would compute just one pair of standard deviations 
# and one correlation, so the parameters would look something like this:

params <- mnist_27$train %>% 
  group_by(y) %>% 
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1= sd(x_1), sd_2 = sd(x_2), r = cor(x_1,x_2))

params <-params %>% mutate(sd_1 = mean(sd_1), sd_2=mean(sd_2), r=mean(r))
params 

# n this case, the lack of flexibility does not permit us 
# to capture the nonlinearity in the true conditional probability function. 
# We can fit the model using caret:

train_lda <- train(y ~ .,
                   method = "lda",
                   data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# Comprehension Check: Generative Models

# In the following exercises, 
# we are going to apply LDA and QDA to the tissue_gene_expression dataset.

# Create a dataset of samples from just cerebellum and hippocampus, 
# two parts of the brain, and a predictor matrix with 10 randomly selected columns 
# using the following code:

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# exploring the data
class(y)
length(y)
levels(y)
table(y)

dim(x)
class(x)


# Use the train function to estimate the accuracy of LDA
train_lda<-train(x,y, method="lda")
train_lda

# their answer:
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

# Look at the fitted model by looking at the finalModel 
# component of the result of train. 
# Notice there is a component called means that includes the estimated means 
# of both distributions. 
# Plot the mean vectors against each other and determine 
# which predictors (genes) appear to be driving the algorithm.

train_lda$finalModel
train_lda$finalModel$means

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Repeat the exercise in Q1 with QDA
# Create a dataset of samples from just cerebellum and hippocampus, 
# two parts of the brain, and a predictor matrix with 10 randomly selected columns 
# using the following code:

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]


# Use the train function to estimate the accuracy of QDA.
train_qda<-train(x,y, method="qda")
train_qda

fit_qda <- train(x, y, method = "qda")

# What is the accuracy?
fit_qda$results["Accuracy"]

# Which TWO genes drive the algorithm when using QDA instead of LDA?

train_qda$finalModel
train_qda$finalModel$means

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


# Re-run LDA with preProcessing = "scale". 
# Note that accuracy does not change, but it is now easier to identify 
# the predictors that differ more between groups than based on the plot made in Q2.

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda<-train(x,y, method="lda")

train_lda<-train(x,y, method="lda",preProcess = "center")
train_lda

train_lda$finalModel
train_lda$finalModel$means

t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Now we are going to increase the complexity of the challenge slightly: 
# we will consider all the tissue types. Use the following code to create your dataset:

set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

# What is the accuracy using LDA?
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

# Lesson -  Classification and Regression Trees (CART)
# we will use a new dataset that includes the breakdown of 
# the composition of olive oil into 8 fatty acids:

data("olive")
head(olive)
class(olive)
dim(olive)
names(olive)

# we will try to predict the region using the fatty acid composition values as predictors.
table(olive$region)

# We remove the area column because we won't use it as a predictor.
olive <- select(olive, -area)

# Let's very quickly try to predict the region using kNN:
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Let's look at the distribution of each predictor stratified by region:
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free")

# We see that eicosenoic is only present in Southern Italy 
# and that linolenic separates Northern Italy from Sardinia. 
# This implies that we should be able to build an algorithm 
# that predicts perfectly! We can see this clearly 
# by plotting the values for these two predictors:

p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p

# We can, by eye, construct a prediction rule that partitions the predictor space like this:

p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.535, xend = 0.065, yend = 10.535, color = "black", lty = 2)

# Regression and decision trees operate
# by predicting an outcome variable by partitioning predictor space.

# When the outcome is continuous, we call this method regression trees.

# We will use a continuous case, the 2008 poll data introduced earlier,
# to describe the basic idea of how we build these algorithms.

# We will try to estimate the conditional expectation
# with the poll margin and the day.

data("polls_2008")
qplot(day, margin, data = polls_2008)

# Let's take a look at what this algorithm does 
# on the 2008 presidential election poll data. 
# We will use the rpart function in the rpart package.
fit <- rpart(margin ~ ., data = polls_2008)

# We can visually see where the splits were made:
  
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# Parameters:
# 1. Complexity parameter - a minimum for how much the RSS must improve for another partition to be added.
# 2. minsplit: The algorithm sets a minimum number of observations to be partitioned. The default is 20.
# 3. minbucket: The algorithm also sets a minimum on the number of observations in each partition. 
# The default is round(minsplit/3)

# As expected, if we set cp = 0 and minsplit=2, then our prediction is our original data:

fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# We can prune tree by snipping of partitions that do not meet a cp criterion:

pruned_fit <- prune(fit, cp = 0.01)
polls_2008 %>% 
  mutate(y_hat = predict(pruned_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# To choose CP we can use cross validation just like with any tuning parameter.

library(caret)
train_rpart <- train(margin ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)

# To see the resulting tree, we access the finalModel and plot it:

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# And because we only have one predictor, we can actually plot fhat of x:

polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# Lesson: Classification (Decision) Trees

# the algorithm does not minimze rsquare, 
# it minimizes measurements of accuracy 
# sucha as 'Gini' or 'entropy'.

# using a tree to determine which digit is written in the mnist dataset
# training the model
train_rpart<-train(y~.,
                   method="rpart",
                   tuneGrid=data.frame(cp=seq(0.0,0.1,len=25)),
                   data=mnist_27$train)
plot(train_rpart)

# using the tree that has the best accuracy
confusionMatrix(predict(train_rpart,
                        mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

# Lesson: Random forests
# The code for applying random forest
# to the 2008 polls data

library(randomForest)
fit <- randomForest(margin~., data = polls_2008)
plot(fit)
# we can see that the algorithm doesn't change much after about 200 trees
# we may say that it 'converges'at around 200 trees.

# plotting the prediction
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

# fitting a random forest to our 2 or 7 digit example
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)

confusionMatrix(predict(train_rf, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]

# Here is what the conditional probabilities look like:

plot_cond_prob(predict(train_rf, mnist_27$true_p, type = "prob")[,2])

# the plot is quite wiggly, we want it to be smoother.
# until this point we did not optimize the parameters in any way.
# we can do it with the caret package, using this code:
fit <- train(y ~ .,
             method = "Rborist",
             tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
             data = mnist_27$train)
confusionMatrix(predict(fit, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# Comprehension Check: Trees and Random Forests
# Create a simple dataset where the outcome grows 0.75 units on average 
# for every increase in a predictor, using this code:
set.seed(1)
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Which code correctly uses rpart to fit a regression tree and saves the result to fit?

fit <- rpart(y ~ ., data = dat)

# Which of the following plots 
# correctly shows the final tree obtained in Q1?
  
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# make a scatter plot of y versus x 
# along with the predicted values based on the fit.

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#Now run Random Forests instead of a regression tree using randomForest from the __randomForest__ package, 
# and remake the scatterplot with the prediction line.
library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

# Use the plot function to see if the Random Forest 
# from Q4 has converged or if we need more trees.
plot(fit)

# It seems that the default values for the Random Forest result
# in an estimate that is too flexible (unsmooth). 
# Re-run the Random Forest but this time with a node size of 50
# and a maximum of 25 nodes. Remake the plot.

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

# lesson: the caret package
data("mnist_27")

# The train function lets us train different 
# algorithms using similar syntax. So, for example, we can type:
library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

# To make predictions, we can use the output of this
# function directly without needing to look at the 
# specifics of predict.glm and predict.knn. 
# Instead, we can learn how to obtain predictions 
# from predict.train.

# So the code looks the same for both methods:
  
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

# This permits us to quickly compare the algorithms. For example, we can compare the accuracy like this:
  
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]

confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

# When an algorithm includes a tuning parameter, train automatically uses cross validation to decide among a few default values. To find out what parameter or parameters are optimized, you can read this or study the output of:
  
getModelInfo("knn")

# We can also use a quick look up like this:
  
modelLookup("knn")

# If we run it with default values:
  
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
# you can quickly see the results of the cross-validation 
# using the ggplot function. The argument highlight highlights the max:
  
ggplot(train_knn, highlight = TRUE)

# By default, the cross validation is performed 
# by taking 25 bootstrap samples comprised of 25% of the observations. 
# For the kNN method, the default is to try  k=5,7,9
# To change this we use the tuneGrid parameters. 
# The grid of values must be supplied by a data frame 
# with the parameter names as specified in the modelLookup output.

# Here, we present an example where we try out 30 values
# between 9 and 67. To do this with caret, we need to define 
# a column named k, so we use this:
data.frame(k = seq(9, 67, 2))

# Note that when running this code, 
# we are fitting 30 versions of kNN to 25 bootstrapped samples. 
# Since we are fitting 30 * 25 = 750 knn models, 
# running this code will take several seconds:

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

# To access the parameter that maximized the accuracy, you can use this:

train_knn$bestTune

#and the best performing model like this:
  
train_knn$finalModel

# The function predict will use this best performing model. 
# Here is the accuracy of the best model when applied to 
# the test set, which we have not used at all yet because 
# the cross validation was done on the training set:

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

# If we want to change of how we perform cross validation, 
# we can use the trainControl function. 
# We can make the code above go a bit faster by using, 
# for example, 10-fold cross validation. 
# This means we have 10 samples using 10% of the observations each. 
# We accomplish this using the following code:
  
  control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

# We notice that the accuracy estimates are more variable, which is expected since we changed the number of samples used to estimate accuracy.

# We can also use the standard deviation bars obtained from the cross validation samples:
  
  train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD, 
                    ymax = Accuracy + AccuracySD))
  
# Comprehension check: The Caret Package
# Q1 and Q2 are is based on the same dataset used 
# in the previous comprehension check:
set.seed(1)
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# in the previous comprehension check we ran:
library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

# "In the exercise in Q6 from Comprehension Check: Trees and Random Forests, we saw that changing nodesize to 50 and setting maxnodes to 25 yielded smoother results. 
# Let's use the train function to help us pick what the values of nodesize and maxnodes should be.
# From the caret description of methods, we see that we can't tune the maxnodes parameter or the nodesize argument with randomForests. So we will use the __Rborist__ package and tune the minNode argument. Use the train function to try values minNode <- seq(25, 100, 25). Set the seed to 1.

# Which value minimizes the estimated RMSE?
library(Rborist)
fit <- train(y ~ ., method = "Rborist", 
                   tuneGrid = data.frame(predFixed=1, 
                         minNode = seq(25, 100, 25)),
                    dat=dat)
ggplot(fit)

# To access the parameter that maximized the accuracy, you can use this:

fit$bestTune

# Make a scatterplot along with the prediction 
# from the best fitted model

library(caret)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

# Use the rpart function to fit a classification tree 
# to the tissue_gene_expression dataset. 
# Use the train function to estimate the accuracy. 
# Try out cp values of seq(0, 0.1, 0.01). 
# Plot the accuracies to report the results of the best model. 
# Set the seed to 1991.

data("tissue_gene_expression")

fit <- rpart(tissue_gene_expression$y ~ tissue_gene_expression$x, data = tissue_gene_expression)

# checking where the splits were made:

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# training the model
set.seed(1991)
train_rpart <- train(tissue_gene_expression$x , tissue_gene_expression$y, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))                     )
ggplot(train_rpart)

# their answer:
set.seed(1991)
data("tissue_gene_expression")

fit_rpart <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
ggplot(fit)            

confusionMatrix(train_rpart)

# Note that there are only 6 placentas in the dataset. 
# By default, rpart requires 20 observations before splitting a node. 
# That means that it is difficult to have a node in which placentas 
# are the majority. Rerun the analysis you did in the exercise in Q3, 
# but this time, allow rpart to split any node 
# by using the argument control = rpart.control(minsplit = 0). 
# Look at the confusion matrix again to determine 
# whether the accuracy increases. Again, set the seed to 1991.

set.seed(1991)
train_rpart <- train(tissue_gene_expression$x , tissue_gene_expression$y, 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                     control = rpart.control(minsplit = 0))                     )

confusionMatrix(train_rpart)

# Plot the tree from the best fitting model of the analysis you ran.

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# We can see that with just seven genes, 
# we are able to predict the tissue type. 
# Now let's see if we can predict the tissue type 
# with even fewer genes using a Random Forest. 
# Use the train function and the rf method to train a Random Forest. 
# Try out values of mtry ranging from seq(50, 200, 25) 
# (you can also explore other values on your own). 
# What mtry value maximizes accuracy? 
# To permit small nodesize to grow as we did with
# the classification trees, 
# use the following argument: nodesize = 1.

library(randomForest)
set.seed(1991)
data("tissue_gene_expression")

set.seed(1991)
library(randomForest)
fit_rf <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

fit_rf
fit_rf$bestTune

# checking variable importance

imp<-varImp(fit_rf)
imp

# Lesson: Case Study: MNIST

# loading the mnist dataset
mnist <- read_mnist()


# The dataset includes two components, a training set and test set:
names(mnist)

# Each of these components includes a matrix with features in the columns:
dim(mnist$train$images)
 
# and vector with the classes as integers:
class(mnist$train$labels)

table(mnist$train$labels)

# Because we want this example to run on a small laptop 
# and in less than one hour, we will consider a subset 
# of the dataset. We will sample 10,000 random rows 
# from the training set and 1,000 random rows from the test set:

set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])

# computing the standard deviation of each column
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

# The caret packages includes a function that recommends 
# features to be removed due to near zero variance:
  
library(caret)
nzv <- nearZeroVar(x)

# We can see the columns that are removed:
  
image(matrix(1:784 %in% nzv, 28, 28))

# we end up keeping these many columns:
  
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

# Now we are ready to fit some models. 

# Before we start, we need to add column names 
# to the feature matrices as these are required by caret:
  
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)

# Let's start with kNN. The first step is to optimize for k.
# Keep in mind that when we run the algorithm, 
# we will have to compute a distance between 
# each observation in the test set and each observation 
# in the training set. 
# These are a lot of computations. 
# We will therefore use k-fold cross validation to improve speed.

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y, 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
ggplot(train_knn)

# In general, it is a good idea to try a test run 
# with a subset of the data to get an idea of timing 
# before we start running code that might take hours to complete. 
# You can do this as follows:
  
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index,] 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

# Then increase n and b to get an idea 
# of how long it takes a function of these values.

# Once we optimize our algorithm, we can fit it to the entire dataset:
  
fit_knn<- knn3(x[ ,col_index], y,  k = 5)

y_hat_knn <- predict(fit_knn, 
                     x_test[, col_index], 
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

# The accuracy is almost 0.95!

# We now achieve an accuracy of about 0.95. 
# From the specificity and sensitivity, we also see 
# that 8s are the hardest to detect and the most 
# commonly incorrectly predicted digit is 7.

cm$byClass[,1:2]

# Now let's see if we can do even better with Random Forest.

# With Random Forest, computation time is a challenge. 
# For each Forest, we need to build hundreds of trees. 
# We also have several parameters we can tune. 
# We use the Random Forest implementation in the Rborist package 
# which is faster than the one in the randomForest package

# Because with Random Forest, the fitting is the slowest part 
# of the procedure rather than the predicting (as with kNN), 
# we will use only 5 fold cross validation.

# We will also reduce the number of trees that are fit 
# since we are not yet building our final model.

# Finally, to compute on a smaller dataset, 
# we will take a random sample of the observations 
# when constructing each tree. We can change this number 
# with the nSamp argument.

library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1) , predFixed = c(10, 15, 35))

train_rf <-  train(x[ , col_index], 
                   y, 
                   method = "Rborist", 
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

ggplot(train_rf)
train_rf$bestTune

# Now that we have optimized our tree, 
# we are ready to fit our final model.
# Now we will set the number of trees to a larger number:
  
fit_rf <- Rborist(x[, col_index], y, 
                nTree = 1000,
                minNode = train_rf$bestTune$minNode,
                predFixed = train_rf$bestTune$predFixed)
  
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

# Variable importance
# Unfortunately, the Rborist implementation of Random Forest 
# does not yet support importance calculations. 
# So we demonstrate with a quick fit using the randomForest package.
# This time we will use all of the columns.

library(randomForest)
rf <- randomForest(x, y,  ntree = 50)
The following function computes the importance of each feature:
  
  imp <- importance(rf)

# We can see which features are most being used by plotting an image:
  
  image(matrix(imp, 28, 28))

  # An important part of data science is visualizing results 
  # to determine why we are failing. 
  # How we do this depends on the application. 
  # Below we show the images of digits for which we made 
  # an incorrect prediction. 
  # We can compare what we get with kNN to Random Forest.

# calculating for knn:
p_max<-predict(fit_knn, x_test[,col_index])
p_max<-apply(p_max,1,max)
ind<-which(y_hat_knn!=y_test)  
ind<-ind[order(p_max[ind],decreasing=TRUE)]

# And then create images to see where we made a mistake.

# Ensembles
# The idea of an ensemble is similar to the idea of combining data 
# from different pollsters to obtain a better estimate 
# of the true support for each candidate.

# In Machine Learning, one can usually greatly improve the final results 
# by combining the results of different algorithms.

# Here is a simple example where we compute new class probabilities 
# by taking the average of Random Forest and kNN. 
# We can see that the accuracy improves to 0.96:
  
p_rf <- predict(fit_rf, x_test[,col_index])$census  
p_rf<- p_rf / rowSums(p_rf)
p_knn  <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)

# Comprehension check - Ensembles

# Use the training set to build a model with several of the models 
# available from the caret package. 
# We will test out all of the following models in this exercise:
  
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

# Run the following code to train the various models:

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# Now that you have all the trained models in a list, 
# use sapply or map to create a matrix of predictions 
# for the test set. 
# You should end up with a matrix with [length(mnist_27$test$y)] rows
# and [length(models)] columns.

# their answer:
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

# checking:
length(mnist_27$test$y)
length(models)

# Now compute accuracy for each model on the test set. 
# Report the mean accuracy across all models.

same<-matrix(as.numeric(pred)==mnist_27$test$y,200,23)
dim(same)
avgs<-apply(same,1,mean)
avgs
mean(avgs)

# their answer:
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

# Next, build an ensemble prediction by majority vote 
# and compute the accuracy of the ensemble.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# trying the function:
temp<-c(1,2,2,3)
getmode(temp)

# trying it on the 'pred' matrix
getmode(pred[1,])
getmode(pred[2,])
getmode(pred[3,])
getmode(pred[201,])

# calculating the mode of the rows
dim(pred)
i<-seq(1,200)
modes <- sapply(i, function(i) 
  getmode(pred[i,]))
modes
length(modes)
ensemble_acc<-mean(modes==mnist_27$test$y)
ensemble_acc

# their answer:
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

# How many of the individual methods do better than the ensemble?
acc
sum(acc>ensemble_acc)

# Which individual methods perform better than the ensemble?
# their answer:
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

# It is tempting to remove the methods that do not perform well
# and re-do the ensemble. The problem with this approach is 
# that we are using the test data to make a decision. 
# However, we could use the accuracy estimates obtained from 
# cross validation with the training data. 
# Obtain these estimates and save them in an object. 
# Report the mean accuracy of the new estimates.

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

# Now let's only consider the methods with 
# an estimated accuracy of greater than or equal to 0.8 
# when constructing the ensemble.

ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

# Dimension Reduction
# Preserving distance

# We consider an example with twin heights. 
# Some pairs are adults the others are children. 
# Here we simulate 100 two dimensional points 
# that represent the number of standard deviations 
# each individual is from the mean height. 
# Each point is a pair of twins. 
# We use the mvrnorm function from the MASS package 
# to simulate bivariate normal data.

set.seed(1)
library(MASS)
n <- 100
x <- rbind(mvrnorm(n/2, c(69,69), matrix(c(9, 9*0.9, 9*0.92, 9*1),2,2)),
           mvrnorm(n/2, c(55,55), matrix(c(9, 9*0.9, 9*0.92, 9*1),2,2)))

# A scatter-plot quickly reveals 
# that the correlation is high and that 
# there are two groups of twins:

plot(x)

# computing the distance between observations
# 1 and 2, and between 2 and 51

d <- dist(x)
as.matrix(d)[1,2]
as.matrix(d)[2,51]

# calculating the distance based on only one dimension
z <- x[,1]
z

plot(dist(x), dist(z))
abline(0,1, col = "red")

# the 'average' distance, using only the first column, is
# the square of the difference between the coordinates
# on this column times two, and all this under square root.
# so it's the distance on one coordinate times the square
# root of two.

plot(dist(x), dist(z)*sqrt(2))
abline(0, 1, col = "red")

# the standard deviation of the difference between x and the 'average':
sd(dist(x) - dist(z)*sqrt(2))


# plotting the difference vs. the average
z  <- cbind((x[,2] + x[,1])/2,  x[,2] - x[,1])
z

# Using the first dimension of this transformed matrix 
# we obtain an even better approximation:
plot(dist(x), dist(z[,1])*sqrt(2))
abline(0,1)

# with the typical difference improved by about 35%:
sd(dist(x) - dist(z[,1])*sqrt(2))
b<-dist(x)
b

# We reduced the number of dimensions from two to one 
# with very little loss of information.

# The reason we were able to do this is because the columns of  
# X were very correlated:
  
cor(x[,1], x[,2])

# and the transformation produced 
# uncorrelated columns with "independent" information in each column:
  
cor(z[,1], z[,2])
 
# One way this insight may be useful 
# in a machine learning application 
# is that we can reduce the complexity of a model by using just Z1
# rather than both X1 and X2.

# It is actually common 
# to obtain data with several highly correlated predictors. 
# In these cases PCA, 
# which we describe next, 
# can be quite useful for reducing the complexity 
# of the model being fit.

# Comprehension check - dimension reduction
# We want to explore the tissue_gene_expression predictors 
# by plotting them.

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

# We want to get an idea of which observations are close to each other,
# but, as you can see from the dimensions, the predictors
# are 500-dimensional, making plotting difficult.
# Plot the first two principal components 
# with color representing tissue type.

pca<-prcomp(tissue_gene_expression$x)

data.frame(pca$x[,1:2], tissue=tissue_gene_expression$y) %>% 
  ggplot(aes(PC1,PC2, fill = tissue))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

# their answer:
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# The predictors for each observation 
# are measured using the same device and experimental procedure. 
# This introduces biases that can affect all the predictors 
# from one observation. 
# For each observation, compute the average 
# across all predictors, 
# and then plot this against the first PC 
# with color representing tissue. Report the correlation.

avg_ob<-rowMeans(tissue_gene_expression$x)
length(avg_ob)

data.frame(pc_1 = pc$x[,1], avg_ob=avg_ob, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, avg_ob, color = tissue)) +
  geom_point()

cor(pc$x[,1],avg_ob)

# their answer:
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

# We see an association with the first PC 
# and the observation averages. 
# Redo the PCA but only after removing the center. 
# Part of the code is provided for you.

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# For the first 10 PCs, 
# make a boxplot showing the values for each tissue.
seq<-(1:10)
data.frame(pc7=pc$x[,7], tissue = tissue_gene_expression$y) %>%
    ggplot(aes(y=pc7, x=tissue)) +
    geom_boxplot()

# their answer:
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

# Plot the percent variance explained by PC number. 
summary(pc)

# their answer:
plot(summary(pc)$importance[3,])

# lesson - recommendation systems
# The Netflix data is not publicly available, 
# but the GroupLens research lab generated their own database 
# with over 20 million ratings for over 27,000 movies by more than 138,000 users. 
# We make a small subset of this data available via the dslabs package:

library(dslabs)
data("movielens")
head(movielens)
class(movielens)

# Each row represents a rating given by one user to one movie.
# We can see the number of unique users that provided ratings 
# and for how many unique movies they provided them for:
  
  movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
  
# Let's look at some of the general properties of the data 
# to better understand the challenges.
# The first thing we notice is that some movies get rated more than others. 
# Here is the distribution:
    
    movielens %>% 
    count(movieId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins = 30, color = "black") + 
    scale_x_log10() + 
    ggtitle("Movies")

# This should not surprise us given that there are blockbuster movies 
# watched by millions and artsy, independent movies watched by just a few.
# Our second observation is that some users are more active than others 
# at rating movies:
      
      movielens %>% 
      count(userId) %>% 
      ggplot(aes(n)) + 
      geom_histogram(bins = 30, color = "black") + 
      scale_x_log10() + 
      ggtitle("Users")    
      
# Let's create a test set to assess the accuracy of the models we implement.

      library(caret)
      set.seed(755)
      test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
      train_set <- movielens[-test_index,]
      test_set <- movielens[test_index,]
      
# To make sure we don't include users and movies in the test set 
# that do not appear in the training set, 
# we remove these entries using the semi_join function:
      
        test_set <- test_set %>% 
        semi_join(train_set, by = "movieId") %>%
        semi_join(train_set, by = "userId")
        
# Let's write a function that computes the RMSE
# for vectors of ratings and their corresponding predictors:
        
        RMSE <- function(true_ratings, predicted_ratings){
          sqrt(mean((true_ratings - predicted_ratings)^2))
        }

# assume that in realith the same rating is given to all movies 
# and users with all the differences explained by random variation.
# in this case our estimate would be the average:
        
mu_hat <- mean(train_set$rating)
mu_hat

# now we will predict the rmse on the test set data
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

# creating a table that stores rmse results from different methods:
rmse_results <- data.frame(method = "Just the average", RMSE = naive_rmse)
rmse_results

# Some movies are just generally rated higher than others.        
# We can again use least squared to estimate movie averages:

# fit <- lm(rating ~ as.factor(movieId), data = movielens)
# but this will be very slow.

# so we compute it differently:

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# We can see that these estimates vary substantially:

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# Let's see how much our prediction improves when we add movie averages

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

# now Let's compute the average rating for user u, 
# for those that have rated over 100 movies.

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# To fit a model with constants
# for both user and movie effects, we could again use lm:
# lm(rating ~ as.factor(movieId) + as.factor(userId))
# but that would probably crash the computer, it requires
# a lot of computing.
# so instead, we will compute an approximation:

user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# We can now construct predictors and see how much the RMSE improves:

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# Comprehension Check: Recommendation Systems

# The following exercises all work with the movielens data, 
# which can be loaded using the following code:

library(dslabs)
data("movielens")

# Compute the number of ratings for each movie 
# and then plot it against the year the movie came out. 
# Use the square root transformation on the counts.

class(movielens)
head(movielens)
names(movielens)

p <- movielens %>% count(year)
p %>% ggplot(aes(x=year, y=sqrt(n))) + geom_point()
p[which(p$n==max(p$n)),]


# their answer:
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# What is the average rating for the movie The Shawshank Redemption?
data("movielens")

TSR<-movielens %>% 
  filter(title == unique(grep('Shawshank Redemption', 
                              movielens$title, ignore.case = TRUE, value= TRUE)))
TSR %>% filter (year>=1993) %>% summarize(mr=mean(rating))

# What is the average number of ratings per year for the movie Forrest Gump?
FG <- movielens %>% filter(title == "Forrest Gump")     # Grab all the FG data
counts <- FG %>% count(rating)    # Simple table of ratings and counts
sum(counts$n) / (2018-1994)  # Total counts / Time span
counts
sum(counts$n)

# their answer:
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 

# stratify the post-1993 movies by ratings per year 
# and compute their average ratings. 
# Make a plot of average rating versus ratings per year 
# and show an estimate of the trend.

group<- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId, year) %>%
  summarize(title = title[1],
            n=n(),
            years = 2018 - first(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 
  
group
group %>% ggplot(aes(x=rate,y=rating)) + geom_point()

# their answer:
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2017 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# The movielens dataset also includes a time stamp. 
# This variable represents the time and data in which the rating was provided. 
# The units are seconds since January 1, 1970. 
# Create a new column date with the date.
library(lubridate)
movielens <- mutate(movielens, date = as.POSIXct(timestamp, origin="1970-01-01"))  
head(movielens$date)
tail(movielens$date)
class(movielens$date)

# Compute the average rating for each week and plot this average against day.
movielens<- movielens %>% mutate(week = (year(date) - year(min(date)))*52 + 
                 week(date) - week(min(date))) 
movielens<- movielens %>% group_by(week) %>%
  summarize(avg_rating=mean(rating))

class(movielens$avg_rating)
head(movielens$avg_rating)
movielens %>% ggplot(aes(x=week, y=avg_rating)) + 
 geom_point() +
 geom_smooth()

# their answer:
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

# The movielens data also has a genres column. 
# This column includes every genre that applies to the movie. 
# Some movies fall under several genres. 
# Define a category as whatever combination appears in this column. 
# Keep only categories with more than 1,000 ratings. 
# Then compute the average and standard error for each category. 
# Plot these as error bar plots.
library(dslabs)
data("movielens")
names(movielens)

by_genre<-movielens %>%
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), standarderror = sd(rating)/sqrt(n)) %>%
  filter(n >= 1000) 

head(by_genre)

by_genre %>%
ggplot (aes(x=genres, y=avg)) +
  geom_point() +
  geom_errorbar(aes(ymin=avg-standarderror, ymax=avg+standarderror),
                width=0.4, colour="red", alpha=0.8, size=1.3) +
  geom_bar(stat="identity", fill="skyblue", alpha=0.7) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# their answer:
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

