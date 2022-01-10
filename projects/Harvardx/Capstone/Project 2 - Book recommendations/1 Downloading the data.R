# Downloading the data #

if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org")

# install.packages("downloader")
library(downloader)

# downloading
download("http://www2.informatik.uni-freiburg.de/~cziegler/BX/BX-CSV-Dump.zip", dest="dataset.zip", mode="wb") 
unzip ("dataset.zip")

#converting the files to data frame format
books<-read.csv(file = 'BX-Books.csv', sep=";")
users<-read.csv(file = 'BX-Users.csv', sep=";")
ratings<-read.csv(file = 'BX-Book-Ratings.csv', sep=";")

# saving
saveRDS(books, "books")
saveRDS(users, "users")
saveRDS(ratings, "ratings")

# exploring
dim(books)
names(books)
head(books)

dim(users)
names(users)
head(users)

dim(ratings)
names(ratings)
head(ratings)
