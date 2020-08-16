# Harvadx course: R basics - syntax for assessment in Datacamp 

# Variable names
# Load package and data

library(dslabs)
data(murders)

# Use the function names to extract the variable names 
names(murders)

# Examining Variables
# To access the population variable from the murders dataset use this code:
p <- murders$population 

# To determine the class of object `p` we use this code:
class(p)

# Use the accessor to extract state abbreviations and assign it to a

# Determine the class of a
a<-murders$abb
class(a)

# Multiple ways to access variables
# We extract the population like this:
p <- murders$population

# This is how we do the same with the square brackets:
o <- murders[["population"]] 

# We can confirm these two are the same
identical(o, p)

# Use square brackets to extract `abb` from `murders` and assign it to b
b<-murders[["abb"]]
# Check if `a` and `b` are identical 
identical(a,b)
identical(a,b)

# Factors
# We can see the class of the region variable using class
class(murders$region)

# Determine the number of regions included in this variable 
length(levels(murders$region))

# Tables
# Here is an example of what the table function does
x <- c("a", "a", "b", "b", "b", "c")
table(x)

# Write one line of code to show the number of states per region
table(murders$region)

# Numeric Vectors
# Here is an example creating a numeric vector named cost
cost <- c(50, 75, 90, 100, 150)

# Create a numeric vector to store the temperatures listed in the instructions into a vector named temp
# Make sure to follow the same order in the instructions
temp<-c(35, 88, 42, 84, 81, 30)

# Character vectors
# here is an example of how to create a character vector
food <- c("pizza", "burgers", "salads", "cheese", "pasta")

# Create a character vector called city to store the city names
# Make sure to follow the same order as in the instructions
city<-c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Connecting Numeric and Character Vectors
# Associate the cost values with its corresponding food item
cost <- c(50, 75, 90, 100, 150)
food <- c("pizza", "burgers", "salads", "cheese", "pasta")
names(cost) <- food

# You already wrote this code
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Associate the temperature values with its corresponding city
names(temp)<-city

# Subsetting vectors
# cost of the last 3 items in our food list:
cost[3:5]

# temperatures of the first three cities in the list:
temp[1:3]

# Subsetting vectors continued...
# Access the cost of pizza and pasta from our food list 
cost[c(1,5)]

# Define temp
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
names(temp) <- city

# Access the temperatures of Paris and San Juan
temp[c(3,5)]

# Sequences
# Create a vector m of integers that starts at 32 and ends at 99.
m <- 32:99

# Determine the length of object m.
length(m)

# Create a vector x of integers that starts 12 and ends at 73.
x<-12:73
# Determine the length of object x.
length(x)

# Sequences continued...
# Create a vector with the multiples of 7, smaller than 50.
seq(7, 49, 7) 

# Create a vector containing all the positive odd numbers smaller than 100.
# The numbers should be in ascending order
seq(1,99,2)

# Sequences and length
# We can a vector with the multiples of 7, smaller than 50 like this 
seq(7, 49, 7) 

# But note that the second argument does not need to be last number.
# It simply determines the maximum value permitted.
# so the following line of code produces the same vector as seq(7, 49, 7)
seq(7, 50, 7)

# Create a sequence of numbers from 6 to 55, with 4/7 increments and determine its length
seq(6,55,4/7)
length(seq(6,55,4/7))

# Sequences of certain length
# Store the sequence in the object a
a<-seq(1,10,length.out=100)

# Determine the class of a
class(a)

# Integers
# Store the sequence in the object a
a<-seq(1,10)

# Determine the class of a
class(a)

# Integers and Numerics
# Check the class of 1, assigned to the object a
class(1)

# Confirm the class of 1L is integer
class(1L)

# Coercion
# Define the vector x
x <- c(1, 3, 5,"a")

# Note that the x is character vector
x

# Typecast the vector to get an integer vector
# You will get a warning but that is ok
x<-as.numeric(x)

# sort
# Access the `state` variable and store it in an object 
states <- murders$state 

# Sort the object alphabetically and redefine the object 
states <- sort(states) 

# Report the first alphabetical value  
states[1]

# Access population values from the dataset and store it in pop
pop<-murders$population
# Sort the object and save it in the same object 
pop<-sort(pop)
# Report the smallest population size 
pop[1]

# order
# Access population from the dataset and store it in pop
pop<-murders$population
# Use the command order, to order pop and store in object o
o<-order(pop)
# Find the index number of the entry with the smallest population size
i_min<-which.min(murders$population)
i_min

# New Codes
# Find the smallest value for variable total 
which.min(murders$total)

# Find the smallest value for population
which.min(murders$population)

# Using the output of order
# Define the variable i to be the index of the smallest state
i <- which.min(murders$population)

# Define variable states to hold the states
states<-(murders$state)

# Use the index you just defined to find the state with the smallest population
states[i]

# Ranks
# Store temperatures in an object 
temp <- c(35, 88, 42, 84, 81, 30)

# Store city names in an object 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Create data frame with city names and temperature 
city_temps <- data.frame(name = city, temperature = temp)

# Define a variable states to be the state names 
states<-(murders$state)

# Define a variable ranks to determine the population size ranks 
ranks<-rank(murders$population)


# Create a data frame my_df with the state name and its rank
my_df<-data.frame(my_state=states, my_poprank=ranks)

# Data Frames, Ranks and Orders
# Define a variable states to be the state names from the murders data frame
states<-(murders$state)

# Define a variable ranks to determine the population size ranks 
ranks<-rank(murders$population)

# Define a variable ind to store the indexes needed to order the population values
ind<-order(murders$population)

# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
states[ind]
ranks[ind]

my_df<-data.frame(mystate=states[ind], myrank=ranks[ind])

# missing values (NA)
# Using new dataset 
library(dslabs)
data(na_example)

# Checking the structure 
str(na_example)

# Find out the mean of the entire dataset 
mean(na_example)

# Use is.na to create a logical index ind that tells which entries are NA
ind<-is.na(na_example)
# Determine how many NA ind has using the sum function
sum(ind)

# Removing NAs
# Note what we can do with the ! operator
x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind]

# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)

# We saw that this gives an NA
mean(na_example)

# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])

# Vectorized operations
# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)

# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp<-5/9*(temp-32)
# Create a data frame `city_temps` 
city_temps<-data.frame(city, temp)


# Vectorized operations continued...
# Define an object `x` with the numbers 1 through 100
x<-seq(1,100)
# Compute the sum 
sum((1/x)^2)

# Vectorized operation continued...
# Load the data
library(dslabs)
data(murders)
# Store the per 100,000 murder rate for each state in murder_rate
murder_rate=(murders$total/murders$population)*100000
# Calculate the average murder rate in the US 
mean(murder_rate)

# Logical Vectors
# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total / murders$population * 100000

# Store the `murder_rate < 1` in `low` 
low<-murder_rate<1

# which
# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000

# Store the murder_rate < 1 in low 
low <- murder_rate < 1

# Get the indices of entries that are below 1
which(low)

# Ordering vectors
# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000

# Store the murder_rate < 1 in low 
low <- murder_rate < 1

# Names of states with murder rates lower than 1
murders$state[low]

# Filtering
# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total/murders$population*100000

# Store the `murder_rate < 1` in `low` 
low <- murder_rate < 1

# Create a vector ind for states in the Northeast and with murder rates lower than 1. 
ind<-low&murders$region=="Northeast"
# Names of states in `ind` 
murders$state[ind]

# Filtering continued
# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000

# Compute average murder rate and store in avg using `mean` 
avg<-mean(murder_rate)

# How many states have murder rates below avg ? Check using sum 
sum(murder_rate<avg)

# Match
# Store the 3 abbreviations in abbs in a vector (remember that they are character vectors and need quotes)
abbs<-c("AK","MI","IA")
# Match the abbs to the murders$abb and store in ind
ind<-match(abbs,murders$abb)
# Print state names from ind
murders$state[ind]

# %in%
# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs<-c("MA", "ME", "MI", "MO", "MU")

# Use the %in% command to check if the entries of abbs are abbreviations in the the murders data frame
abbs%in%murders$abb

# Logical operator
# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 

# Use the `which` command and `!` operator to find out which abbreviation are not actually part of the dataset and store in ind
ind<-which(abbs%in%murders$abb!=1)
# What are the entries of abbs that are not actual abbreviations
abbs[ind]

# dplyr
# Loading data
library(dslabs)
data(murders)

# Loading dplyr
library(dplyr)

# Redefine murders so that it includes column named rate with the per 100,000 murder rates
murders<-mutate(murders,rate=total/population*100000)

# mutate
# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)

# Defining rate
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders<-mutate(murders,rank=rank(-rate))

# select
# Load dplyr
library(dplyr)

# Use select to only show state names and abbreviations from murders
select(murders,state,abb)

# filter
# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))

# Filter to show the top 5 states with the highest murder rates
filter(murders,rank<=5)

# filter with !=
# Use filter to create a new data frame no_south
no_south<-filter(murders,region!="South")
# Use nrow() to calculate the number of rows
nrow(no_south)

# filter with %in%
# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw<-filter(murders,region %in% c("Northeast", "West"))
# Number of states (rows) in this category 
nrow(murders_nw)

# filtering by two conditions
# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# Create a table, call it my_states, that satisfies both the conditions 
my_states<-filter(murders,region %in% c("Northeast","West") & rate<1)
# Use select to show only the state name, the murder rate and the rank
select(my_states,state,rate,rank)

# Using the pipe %>%
# Load library
library(dplyr)

## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# show the result and only include the state, rate, and rank columns, all in one line
filter(murders,region %in% c("Northeast","West") & rate<1) %>% select(state, rate,rank)

# mutate, filter and select
# Loading the libraries
library(dplyr)
data(murders)

# Create new data frame called my_states (with specifications in the instructions)
my_states<-murders %>% mutate(rate=(total/population*100000),rank=rank(-rate)) %>% filter(region %in% c("Northeast", "West"),rate<1) %>% select(state,rate,rank)

# Scatterplots scatter plots
# Load the datasets and define some variables
library(dslabs)
data(murders)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions, total_gun_murders)

# Transform population using the log10 transformation and save to object log10_population
log10_population<-log(murders$population,base=10)
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders<-log(total_gun_murders,base=10)
# Create a scatterplot with the log scale transformed population and murders 
plot(log10_population,log10_total_gun_murders)

# Histograms
# Store the population in millions and save to population_in_millions 
population_in_millions <- murders$population/10^6


# Create a histogram of this variable
hist(population_in_millions)

# Boxplots box plots
# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region,data=murders)

# ifelse
# Assign the state abbreviation when the state name is longer than 8 characters 
new_names<-ifelse(nchar(murders$state)<=8,murders$state,murders$abb)

# Defining functions
# Create function called `sum_n`
sum_n<-function(x){
  y<-seq(1,n)
  z<-sum(y)
  z
}
# Use the function to determine the sum of integers from 1 to 5000
n<-5000
sum_n(n)

# Defining functions continued...
# Create `altman_plot` 
altman_plot<-function(x,y){
  plot(x+y,y-x)
}

# Lexical scope
# Run this code 
x <- 3
my_func <- function(y){
  x <- 5
  y+5
}

# Print value of x 
print(x)

# For loops
# Here is an example of function that adds numbers from 1 to n
example_func <- function(n){
  x <- 1:n
  sum(x)
}

# Here is the sum of the first 100 numbers
example_func(100)

# Write a function compute_s_n that with argument n and returns of 1 + 2^2 + ...+ n^2
compute_s_n<-function(x){
  x<-seq(1,n)
  y<-sum(x^2)
  print(y)
}
# Report the value of the sum when n=10
n<-10

compute_s_n(10)

# For loops continued...
# Define a function and store it in `compute_s_n`
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Create a vector for storing results
s_n <- vector("numeric", 25)

# write a for-loop to store the results in s_n
n <- 25
for(i in 1:n){
  s_n[i] <- compute_s_n(i)
}


# Checking our math
# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

#  Create the plot 
plot(n,s_n)

# Checking our math continued
# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

# Check that s_n is identical to the formula given in the instructions.
formula<-vector("numeric",25)
for(i in n){
  formula[i]<-i*(i+1)*(2*i+1)/6
}
identical(s_n, formula)


