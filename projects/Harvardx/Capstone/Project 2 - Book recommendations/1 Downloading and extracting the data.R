####################################################################
####################  Predicting Book Ratings ######################
####  Harvardx Data Science Capstone - Create Your Own Project  ####
###################   Nir Levy, January 2022   #####################
####################################################################

###############################
### 1. Downloading the data ###
###############################

### Installing packages
if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")


### loading libraries
library(downloader)
library(dplyr)
library(tidyverse)
library(caret)
library(data.table)
library(ggplot2)
library(recommenderlab)
library(stringr)

### description of the data set
### by publisher: "http://www2.informatik.uni-freiburg.de/~cziegler/BX/"
### in Kaggle: "https://www.kaggle.com/somnambwl/bookcrossing-dataset"
### (the website: "https://www.bookcrossing.com/")

### downloading the dataset
# download("http://www2.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip", dest="dataset.zip", mode="wb") 
# unzip ("dataset.zip")

### converting the files to data frame format
# books<-read.csv(file = 'BX-Books.csv', sep=";")
# users<-read.csv(file = 'BX-Users.csv', sep=";")
# ratings<-read.csv(file = 'BX-Book-Ratings.csv', sep=";")

### saving the data frames
# saveRDS(books, "books")
# saveRDS(users, "users")
# saveRDS(ratings, "ratings")



