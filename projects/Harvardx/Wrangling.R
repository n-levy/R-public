# Assessment 1.2
# 
library(tidyverse)
url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
test<-read.csv(url(url), header = F)
dim(test)
