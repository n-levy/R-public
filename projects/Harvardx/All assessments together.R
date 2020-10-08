# R Script for Datacamp assessments of all Harvardx courses together

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
my_states<-murders %>% mutate(rate=(total/population*100000),rank=rank(-rate))
%>% filter(region %in% c("Northeast", "West"),rate<1) %>% select(state,rate,rank)

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

# Harvadx course: Data Visualization - syntax for assessment in Datacamp 

#preparation
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)

#part 1
# variable names
library(dslabs)
data(heights)
names(heights)

# variable type
library(dslabs)
data(heights)
head(heights)

# Numerical values
library(dslabs)
data(heights)
x <- heights$height
length(unique(x))

# tables
library(dslabs)
data(heights)
x <- heights$height
tab<-table(x)
tab

# indicator variables
library(dslabs)
data(heights)
tab <- table(heights$height)
sum(tab==1)

#part 2
# Vector lengths
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)

# Percentiles
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
female_percentiles<-quantile(female,seq(0.1,0.9,0.2))
male_percentiles<-quantile(male,seq(0.1,0.9,0.2))
df<-data.frame(female=female_percentiles,male=male_percentiles)
df

# Proportions
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
mean(x>69 & x<=72)

# Averages and Standard Deviations
library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72,mean=avg,sd=stdev)-pnorm(69,mean=avg,sd=stdev)

# Approximations
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 81)
avg<-mean(x)
sd<-sd(x)
approx<-pnorm(81,mean=avg,sd=sd)-pnorm(79,mean=avg,sd=sd)
exact/approx

# Average and Median
library(HistData)
data(Galton)
x <- Galton$child
mean(x)
median(x)

# MAD
library(HistData)
data(Galton)
x <- Galton$child
sd(x)
mad(x)

# Standard Deviation
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
sd(x_with_error)-sd(x)

# writing a function
x <- Galton$child
error_avg <- function(k){
  x[1]<-k
  mean(x)
}
error_avg(k=10000)
error_avg(k=-10000)

# ggplot2 basics
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)
p <- ggplot(murders)
class(p)

# pipes
data(heights)
# define ggplot object called p like in the previous exercise but using a pipe 
p<-heights %>% ggplot()

# geom_point 1
## Fill in the blanks
murders %>% ggplot(aes(x = population, y =total )) +
  geom_point()

# geom_point 2
murders %>% ggplot(aes(total, population)) +
  geom_point()

# geom_point text
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
## edit the next line to add the label
murders %>% ggplot(aes(population, total,label=abb)) +
  geom_point()+
  geom_label()

# geom_point colors 2
murders %>% ggplot(aes(population, total,label= abb)) +
  geom_label(color="blue")

# geom_label colors
murders %>% ggplot(aes(population, total, label = abb,color=region)) + 
  geom_label()

# Log-scale
p <- murders %>% 
  ggplot(aes(population, total, label = abb, color = region)) + 
  geom_label()  
p + scale_x_log10()
p + scale_y_log10()

# Titles
p <- murders %>% ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()
# add a layer to add title to the next line
p + scale_x_log10() + 
  scale_y_log10()+
  ggtitle("Gun murder data")

# Histograms
p <- heights %>% 
  ggplot(aes(height))
## add a layer to p
p+geom_histogram()

# Histogram binwidth
p <- heights %>% 
  ggplot(aes(height))
## add the geom_histogram layer but with the requested argument
p+geom_histogram(binwidth=1)

# Smooth density plot
heights %>% 
  ggplot(aes(height))+
  geom_density()

# Two smooth density plots
heights %>% 
  ggplot(aes(height,group=sex))+
  geom_density()

# Two smooth density plots 2
heights %>% 
  ggplot(aes(height, color = sex))+
  geom_density()

# Two smooth density plots 3
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha=.2)

# filter
library(dplyr)
library(NHANES)
data(NHANES)
## fill in what is needed
tab <- NHANES %>%
  filter(Gender=="female", AgeDecade==" 20-29")

# missing values
library(dplyr)
library(NHANES)
data(NHANES)
## complete this line of code.
ref <- NHANES %>% 
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve, na.rm=TRUE))

# Summarizing averages
library(dplyr)
library(NHANES)
data(NHANES)
## modify the code we wrote for previous exercise.
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE))  %>% .$average

# Min and max
library(dplyr)
library(NHANES)
data(NHANES)
## complete the line
NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(min = min(BPSysAve, na.rm = TRUE), max=max(BPSysAve, na.rm = TRUE))

# group
library(dplyr)
library(NHANES)
data(NHANES)
##complete the line with group_by and summarize
NHANES %>%
  filter(Gender == "female") %>% 
  group_by(AgeDecade) %>%
  summarize(average=mean(BPSysAve, na.rm = TRUE), standard_deviation=sd(BPSysAve, na.rm = TRUE))

# group 2
library(dplyr)
library(NHANES)
data(NHANES)
NHANES %>%
  filter(Gender == "male") %>% 
  group_by(AgeDecade) %>%
  summarize(average=mean(BPSysAve, na.rm = TRUE), standard_deviation=sd(BPSysAve, na.rm = TRUE))

# group 3
library(NHANES)
data(NHANES)
NHANES %>%
  group_by(AgeDecade, Gender) %>%
  summarize(average=mean(BPSysAve, na.rm = TRUE), standard_deviation=sd(BPSysAve, na.rm = TRUE))

# Arrange
library(dplyr)
library(NHANES)
data(NHANES)
NHANES %>%
  filter(AgeDecade==" 40-49" & Gender=="male") %>%
  group_by(Race1) %>%
  summarize(average=mean(BPSysAve, na.rm = TRUE), 
            standard_deviation=sd(BPSysAve, na.rm = TRUE)) %>%
  arrange(average)

# geom_point
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
## fill out the missing parts in filter and aes
gapminder %>% filter(continent=="Africa" & year==2012) %>%
  ggplot(aes(x=fertility,y=life_expectancy)) +
  geom_point()

# color
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder %>% filter(continent=="Africa" & year==2012) %>%
  ggplot(aes(fertility,life_expectancy,color=region))+ geom_point()

# select
library(dplyr)
library(dslabs)
data(gapminder)
df<-gapminder %>% 
  filter (year==2012 & continent=="Africa" & fertility<=3 & life_expectancy>=70) %>%
  select (country, region)

# filter
library(dplyr)
library(dslabs)
data(gapminder)
tab<-gapminder %>% 
  filter (year>=1960 & year<=2010 & country %in% c("United States","Vietnam"))

# geom_line
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
ggplot(gapminder %>% filter (country=="Cambodia" & year>=1960 & year<=2010),aes(year,life_expectancy))+
  geom_line()

# mutate
library(dplyr)
library(dslabs)
data(gapminder)
daydollars <- gapminder %>% 
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day) & continent=="Africa" & year==2010) 

# multiple density plots 
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
subset<-gapminder %>%
  filter(continent=="Africa" & year %in% c(1970,2010))
head(subset)
tail(subset)

daydollars <- subset %>% 
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day)) 
head(daydollars)

ggplot (daydollars,aes(dollars_per_day)) + geom_density()+
  scale_x_continuous(trans="log2")+
  facet_grid(daydollars$year)

# stacked histograms
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
subset<-gapminder %>%
  filter(continent=="Africa" & year %in% c(1970,2010))
head(subset)
tail(subset)

daydollars <- subset %>% 
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day)) 
head(daydollars)

ggplot (daydollars,aes(dollars_per_day,fill=region)) + geom_density(bw=.5,position="stack")+
  scale_x_continuous(trans="log2")+
  facet_grid(.~daydollars$year)

# scatter plot - part 1
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_Africa_2010 <- gapminder %>%
  filter(continent=="Africa" & year==2010) %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day))
head(gapminder_Africa_2010)

# now make the scatter plot
gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day, infant_mortality, color = region))+geom_point()

# scatter plot - part 2 - logarithmic axis
gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region))+
  geom_point()+scale_x_continuous(trans="log2")

# scatter plot - part 3 - adding labels
gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, label=country, color=region))+
  scale_x_continuous(trans="log2")+geom_text()

# scatter plot - part 4 - comparison of scatter plots
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_Africa_1970_2010 <- gapminder %>%
  filter(continent=="Africa" & year %in% c(1970,2010)) %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(!is.na(dollars_per_day))
head(gapminder_Africa_1970_2010)
gapminder_Africa_1970_2010_nona<-gapminder_Africa_1970_2010 %>% 
  filter(!is.na(country) & !is.na(year) & !is.na(infant_mortality))

gapminder_Africa_1970_2010_nona %>% 
  ggplot(aes(dollars_per_day, infant_mortality, label=country, color=region))+
  scale_x_continuous(trans="log2")+
  geom_text()+
  facet_grid(year~.)

# Customizing plots
library(dplyr)
library(ggplot2)
library(dslabs)
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state<-reorder(state,rate)
print(state)
levels(state)

# Customizing plots - redefining
library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
dat <- us_contagious_diseases %>% 
  filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state=reorder(state,rate))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

# Box plot, boxplot
library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders_with_rate<-murders %>%
  mutate(rate=total/population*100000)
murders_with_rate %>% 
  ggplot(aes(region,rate))+
  geom_boxplot()
murders_with_rate$region<-reorder(murders_with_rate$region,murders_with_rate$rate)
murders_with_rate %>% 
  ggplot(aes(murders_with_rate$region,murders_with_rate$rate))+
  geom_boxplot()+
  geom_point()

# Tile plot
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease, weeks_reporting>=10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

# Time series plot 1
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease, weeks_reporting>=10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

# Time series plot 2
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>% filter(state=="California", weeks_reporting>=10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate,color=disease)) + 
  geom_line()

# Time series plot 3
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

us_contagious_diseases %>%
  filter(!is.na(count) & !is.na(population)) %>%
  group_by(year,disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate,color=disease)) + 
  geom_line()


############ Machine Learning Course ###############
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

# scout24 assignment

# Pasta Delivery challenge for Scout24
# Nir Levy, 7.4.2019

###################### Step 0 - Preparaing and exploring the data ###############################

# Loading libraries 
library(caret)
library(dslabs)
library(dplyr)
library(ggplot2)
library(NHANES)
library(Hmisc)

# Importing the data 
getwd()
dat=read.csv("Scout24AG_pastadelivery_casestudy.csv", sep=';', check.names=FALSE, header = TRUE)

# Converting the 'pasta_price' variable from 'factor' to 'numeric'
dat$pasta_price<-as.numeric(sub(",",".",dat$pasta_price, fixed = TRUE))

# Checking the conversion 
class(dat$pasta_price)
head(dat$pasta_price)
summary(dat$pasta_price)

# Exploring the data
head(dat)
names(dat)
class(dat)
sapply(dat,class)

hist(dat$order_id)
hist(dat$pasta_price)
hist(dat$voucher)
hist(dat$customer_age)
hist(dat$yearly_income)
hist(dat$area_code)

# Examining the distribution of the number of transactions per household
number_of_transactions<- dat %>%
  group_by(hh_id) %>%
  summarise(number_of_transactions=max(order_id))
plot(number_of_transactions)


################################ Step 1 - checking the variation between areas ################
################################ Adding variables of interest #################################

# Do households buy more or less pasta as the number of their orders grows?
# Adding two variables to check this: 
# 1. Pearson's correlation between sales and order_id
# 2. The difference between sales between each order and the previous order 
dat_new <- dat %>%
  group_by(hh_id) %>%
  mutate(sales_cor=cor(order_id,pasta_price)) %>%
  mutate(sales_diff=c(0,diff(pasta_price))) 

# Creating  household data
hh_dat<- dat_new %>%
  group_by(hh_id) %>%
  summarise(hh_total_sales=sum(pasta_price),
            hh_mean_sales=mean(pasta_price),
            hh_voucher=mean(voucher),
            hh_customer_age=mean(customer_age),
            hh_yearly_income=mean(yearly_income),
            hh_area_code=mean(area_code),
            hh_sales_cor=mean(sales_cor),
            hh_sales_diff=mean(sales_diff),
            hh_sales_share=hh_total_sales/hh_yearly_income)

# Labeling the new variables
var.labels<-c(
  hh_id="Household ID",
  hh_total_sales="Household total sales",
  hh_mean_sales="Household mean sales",
  hh_voucher="Household percentage of sales with voucher",
  hh_customer_age="Household customer age",
  hh_yearly_income="Household yearly income",
  hh_area_code="Household area code",
  hh_sales_cor="Household correlation between sales and order_id",
  hh_sales_diff="Household difference in price between consecutive sales",
  hh_sales_share="Household share of sales in yearly income")

hh_dat<-Hmisc::upData(hh_dat,labels=var.labels) 
Hmisc::label(hh_dat)  
Hmisc::contents(hh_dat)  

# Creating area by household data
area_hh_dat<- hh_dat %>%
  group_by(hh_area_code) %>%
  summarise(area_total_sales=sum(hh_total_sales),
            area_hh_total_sales=mean(hh_total_sales),
            area_hh_voucher=mean(hh_voucher),
            area_hh_customer_age=mean(hh_customer_age),
            area_hh_yearly_income=mean(hh_yearly_income),
            area_hh_sales=mean(hh_total_sales),
            area_hh_sales_cor=mean(hh_sales_cor),
            area_hh_sales_diff=mean(hh_sales_diff),
            area_hh_sales_share=mean(hh_sales_share),
            area_code=mean(hh_area_code),
            area_corr_share_income=cor(hh_yearly_income,hh_sales_share))

# Labeling the new variables
var.labels<-c(
  area_code="Area code",
  area_total_sales="Area total sales",
  area_hh_sales="Area average sales per household",
  area_hh_voucher="Area percentage of sales with voucher per household",
  area_hh_customer_age="Area average customer age per household",
  area_hh_yearly_income="Area yearly income per household",
  area_hh_sales="Area average total sales per household",
  area_hh_sales_cor="Area average correlation between sales and order_id per household",
  area_hh_sales_diff="Area average difference in price between consecutive sales per household",
  area_hh_sales_share="Area average share of sales in yearly income per household",
  area_corr_share_income="Area correlation between yearly income of households and share of sales in yearly income")

area_hh_dat<-Hmisc::upData(area_hh_dat,labels=var.labels) 
Hmisc::label(area_hh_dat)

# Exploring area data
dim(area_hh_dat)
ncol(area_hh_dat)
area_hh_dat[1:6]
area_hh_dat[7:12]

# computing coefficients of variation for area characteristics
area_hh_cvs<-area_hh_dat %>%
  summarise(area_total_sales_cv=(sd(area_total_sales)/mean(area_total_sales)),
            area_hh_voucher_cv=(sd(area_hh_voucher)/mean(area_hh_voucher)),
            area_hh_age_cv=(sd(area_hh_customer_age)/mean(area_hh_customer_age)),
            area_hh_yearly_income_cv=(sd(area_hh_yearly_income)/mean(area_hh_yearly_income)),
            area_hh_sales_cv=(sd(area_hh_sales)/mean(area_hh_sales)),          
            area_hh_sales_cor_cv=(sd(area_hh_sales_cor)/mean(area_hh_sales_cor)),
            area_hh_sales_diff_cv=(sd(area_hh_sales_diff)/mean(area_hh_sales_diff)),
            area_hh_sales_share_cv=(sd(area_hh_sales_share)/mean(area_hh_sales_share)),
            area_corr_share_income_cv=(sd(area_corr_share_income)/mean(area_corr_share_income)))

# Examining the coefficients of variation                        
ncol(area_hh_cvs)
area_hh_cvs[,1:5]
area_hh_cvs[,6:9] 

# Visualizing the average yearly income of households in the area 
area_hh_dat %>% 
  ggplot(aes(hh_area_code,area_hh_yearly_income)) +
  scale_x_continuous()+
  geom_bar(stat="identity")

# Choice of areas
# Among the original variables in the dataset, the highest variation between the areas
# is in average yearly income per household. 
# Some of the variables I created have a higher variance, but small  
# differences in absolute terms.
# Therefore I would recommend to publish on billboards in areas 3 and 4.
# If the relationship between the effect of billboards and the
# average yearly income per household is monotonic, 
# this will provide us with an indication of the sign (positive or negative). 

############ Step 2 - randomly selecting 30% of the households for receiving mail ################

# Creating dummy variable for mail
hh_dat$mail<-with(hh_dat,sample(100,size=nrow(hh_dat),replace=TRUE))
hh_dat$mail<-ifelse(hh_dat$mail<=30,1,0)

# Verifying that the variable was created correctly
# checking the percentage of mails in each area
hh_dat %>%
  group_by(hh_area_code) %>%
  summarise(hh_dat_mail_percent=mean(mail))

############ Step 3 - creating fictional effect of billboards and mails ##########################

# Creating dummy variables for areas
hh_dat$area_1<- with(hh_dat,ifelse(hh_area_code==1,1,0)) 
hh_dat$area_2<- with(hh_dat,ifelse(hh_area_code==2,1,0)) 
hh_dat$area_3<- with(hh_dat,ifelse(hh_area_code==3,1,0)) 
hh_dat$area_4<- with(hh_dat,ifelse(hh_area_code==4,1,0)) 
hh_dat$area_5<- with(hh_dat,ifelse(hh_area_code==5,1,0)) 

# Verifying that the variables were created correctly
area_dummies<-data.frame(hh_dat$area_1,hh_dat$area_2,hh_dat$area_3,hh_dat$area_4, hh_dat$area_5)
sapply(area_dummies, sum)
table(hh_dat$hh_area_code)

# creating the potential billboard effect - random addition of 0 to 2 to total sales per household
# (normal distribution)
hh_dat$potential_billboard_effect=with(hh_dat,rnorm(nrow(hh_dat),0,2))

# Verifying that the variable was created correctly
head(hh_dat$potential_billboard_effect)
mean(hh_dat$potential_billboard_effect)

# checking the mean of billboard effect in each area
hh_dat %>%
  group_by(hh_area_code) %>%
  summarise(billboard_effect_average=mean(potential_billboard_effect))

# creating a variable denoting publication on a billboard in the area
hh_dat$billboard_exists<-with(hh_dat,ifelse((hh_area_code==3 | hh_area_code==4),1,0)) 

# Verifying that the variable was created correctly
hh_dat %>%
  group_by(hh_area_code) %>%
  count(billboard_exists)

# Simplifying assumptions about effects of billboards and mails:
# 1. There are no interactions between billboard effects and characteristics of households or areas.
# 2. Mail has both a constant effect and an effect that grows with household income.

# Creating variable for total sales before introducing billboards and mails
hh_dat$hh_total_sales_t0<-hh_dat$hh_total_sales

# Creating the fictional effect variable of billboards and mails
hh_dat$y=with(hh_dat,billboard_exists*potential_billboard_effect+0.5*mail+0.05*mail*hh_total_sales_t0)

# Creating variable for total sales after introducing billboards and mails
hh_dat$hh_total_sales_t1<-hh_dat$hh_total_sales_t0+hh_dat$y

################################ Step 4 - estimating the effect with an OLS regression ################

# Running the OLS regression model   
ols_t1<-lm(hh_total_sales_t1~ 
             hh_total_sales_t0+
             hh_total_sales_t0*mail+
             hh_yearly_income*mail+
             hh_voucher*mail+
             hh_customer_age*mail+
             hh_sales_cor*mail+
             hh_sales_diff*mail+
             hh_sales_share*mail+
             hh_total_sales_t0*billboard_exists+
             hh_yearly_income*billboard_exists+
             hh_voucher*billboard_exists+
             hh_customer_age*billboard_exists+
             hh_sales_cor*billboard_exists+
             hh_sales_diff*billboard_exists+
             hh_sales_share*billboard_exists+
             area_1*billboard_exists+
             area_2*billboard_exists+
             area_3*billboard_exists+
             area_4*billboard_exists,
           data=hh_dat)
summary(ols_t1)

################ Step 5 - calculating the marginal revenue from billboards and mails ####################

# Displaying the marginal revenue from billboards 
names(coef(ols_t1))
names(coef(ols_t1))[4]
summary(ols_t1)$coefficients[4]

# Calculating the marginal revenue from sending mail to the household
# Displaying constant effect of mail
summary(ols_t1)$coefficients[3]
# Displaying effect of mail that depends on household total income (mail$household_total_income_to)
summary(ols_t1)$coefficients[14]
# Calculating the marginal revenue
hh_dat$mail_mr<-summary(ols_t1)$coefficients[3]+summary(ols_t1)$coefficients[14]*hh_dat$hh_total_sales_t0

# Verifying that that the variable was created correctly
summary(ols_t1)$coefficients[3]+summary(ols_t1)$coefficients[14]*mean(hh_dat$hh_total_sales)
mean(hh_dat$mail_mr)

# summarizing the variable
summary(hh_dat$mail_mr)

# Recommendation
# First send mails to all housholds in which the marginal revenue from mails (hh_dat$mail_mr) is
# larger than the average revenue per househod from billboards (summary(ols_t1)$coefficients[4]).
# If some budget remains, spend it on billboards in all five areas. 
# If some budget still remains, spend it on mails to the households
# that did not receive them in the first step.

# Note: I would continue the analysis in the following ways:
# 1. Checking for interaction effecs between household characteristics,
# between area characteristics and between household and area characteristics
# and billboards and mails.
# 2. Checking for non-linear relationships between the variables. 
# For example, by changing the dependent variable to ln(y), 
# raising the values of variables by the power of 2 or the power of 0.5, 
# or splitting variables according to values below and above the median 
# in order to check for U-shaped or inverse U-shaped relationships.

### Harvardx Probability Course ####
# preparation for analysis - see the dedicated script for that

# creating an imaginary 'hat' with 2 red balls and 3 blue balls
beads <- rep(c("red", "blue"), times = c(2,3))

# exploring the variable
beads
head(beads)
class(beads)
sapply(beads,class)

# table
tab<-table(beads)
tab

# histogram with percentages
barplot(prop.table(table(beads)))

# histogram with counts
ggplot(data.frame(beads), aes(x=beads)) +
  geom_bar()

# sampling 1 ball out of the hat
sample(beads,1)

B <- 10000
events <- replicate(B, sample(beads, 1))

head(events)
class(events)

# table with counts
tab<-table(events)
tab
# table with proportions
prop.table(tab)

# sampling 10,000 balls in a different way, without using replicate
events<-sample(beads,10000,replace=TRUE)
tab<-table(events)
prop.table(tab)

# Using the mean Function for Probability

# In R, applying the mean function to a logical vector returns the proportion of elements that are TRUE. 
# It is very common to use the mean function in this way to calculate probabilities 
# and we will do so throughout the course.

# Suppose you have the vector beads from a previous video:

beads <- rep(c("red", "blue"), times = c(2,3))
beads
[1] "red" "red" "blue" "blue" "blue"
# To find the probability of drawing a blue bead at random, you can run:
mean(beads == "blue")

# excercise 1 - probability of cyan - generalized
# One ball will be drawn at random from a box containing: 
# 3 cyan balls, 5 magenta balls, and 7 yellow balls.

# What is the probability that the ball will be cyan?

cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p<-cyan/(sum(cyan,magenta,yellow))

# Print the variable `p` to the console
p

# calculate the probability of choosing any ball that is not cyan from the box
1-p

# # The variable `p_1` is the probability 
# of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p_2` 
# as the probability of not choosing a cyan ball on the second draw without replacement.
# first scenario - we choose a cyan ball on our first draw
p_2<-((sum(magenta,yellow))/(sum(cyan,magenta,yellow)-1))
# second scenario - we choose a ball that is not cyan on our first draw
scenario2<-(1-p_1)*((sum(magenta,yellow)-1)/(sum(cyan,magenta,yellow)-1))
* overall probability of not choosing a cyan ball on the second draw without replacement, if the first draw was cyan.
p_1*p_2

# The variable 'p_1' is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the second draw with replacement.
p_2<-((sum(magenta,yellow))/(sum(cyan,magenta,yellow)))
p_1*p_2

# Say you've drawn 5 balls from the a box that has 3 cyan balls, 
# 5 magenta balls, and 7 yellow balls, 
# with replacement, and all have been yellow.

# What is the probability that the next one is yellow?
p_yellow<-yellow/(sum(cyan,magenta,yellow))
p_yellow

# If you roll a 6-sided die once, what is the probability of not seeing a 6?
p_no6<-5/6

# probability of the celtics winning one of the first 4 games 
# in a series, if the cavs have a 60% of winning each game
p_cavs_win4<-0.6^4
1-p_cavs_win4

# Create a Monte Carlo simulation 
# to confirm your answer to the previous problem 
# by estimating how frequently the Celtics win at least 1 of 4 games.
B<-10000
set.seed(1)
celtic_wins<-replicate(B,
                       any(simulated_games<-sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))=="win"))

mean(celtic_wins)

### Combinations and Permutations lesson

# The function expand.grid gives us all the combinations of 2 lists.
# So for example, if you have blue and black pants
# and white, gray, and plaid shirt, all your combinations
# can be computed using the expand.grid function like this.

expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")

numbers <- c("Ace", "Deuce", "Three", "Four", "Five", 
             "Six", "Seven", "Eight", "Nine", "Ten", 
             "Jack", "Queen", "King")

deck <- expand.grid(number=numbers, suit=suits)

deck <- paste(deck$number, deck$suit)

# Let's double-check that the probability of a king in the first card is 1 in 13.
kings <- paste("King", suits)
mean(deck %in% kings)

# The permutations function computes for any list of size n
# all the different ways we can select R items.
permutations(5,2)

# creating 7-digit phone numbers
# these four lines of code generate all phone numbers, picks 5 at random,
# and then shows them to you.
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

# To compute all possible ways that we can choose 2 cards when the order matters,
# we simply type the following piece of code.
hands <- permutations(52, 2, v = deck)

# This is a matrix with two columns and 2652 rows. 
# With a matrix we can get the first and second cards like this:

first_card <- hands[,1]
second_card <- hands[,2]

# Now the cases for which the first hand was a King can be computed like this:
kings <- paste("King", suits)
sum(first_card %in% kings)

# To get the conditional probability, 
# we compute what fraction of these have a King in the second card:
sum(first_card %in% kings & second_card %in% kings) /
  sum(first_card %in% kings)

# calculating using the mean instead of the sum gives the same answer:
mean(first_card %in% kings & second_card %in% kings) /
  mean(first_card %in% kings)

# the difference between the permutations and combinations functions is that 
# in combinations the order does not matter.
# observe the differences:
permutations(3,2)
combinations(3,2)

# So to compute the probability of a Natural 21 in Blackjack 
# (a sum of 21 in which the order does not matter, and one card is a 'picture' or a ten,
# and the other is an ace), we can do this:
aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# In the last line, we assume the Ace comes first. 
# This is only because we know the way combination 
# enumerates possibilities and it will list this case first. 
# But to be safe, we could have written this and produced the same answer:

mean((hands[,1] %in% aces & hands[,2] %in% facecard) |
       (hands[,2] %in% aces & hands[,1] %in% facecard))

# Instead of using combinations to deduce the exact probability of a Natural 21, 
# we can use a Monte Carlo to estimate this probability. 
# In this case, we draw two cards over and over and keep track 
# of how many 21s we get. We can use the function sample to draw two cards without replacements:

hand <- sample(deck, 2)
hand

# And then check if one card is an Ace and the other a face card or a 10. 
# Going forward, we include 10 when we say face card. 
# Now we need to check both possibilities:

(hands[1] %in% aces & hands[2] %in% facecard) | 
  (hands[2] %in% aces & hands[1] %in% facecard)

# If we repeat this 10,000 times, we get a very good approximation of the probability of a Natural 21.
# Let's start by writing a function that draws a hand 
# and returns TRUE if we get a 21. 
# The function does not need any arguments 
# because it uses objects defined in the global environment.

blackjack <- function(){
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | 
    (hand[2] %in% aces & hand[1] %in% facecard)}

# Here we do have to check both possibilities: 
# Ace first or Ace second because we are not using the combinations function. 
# The function returns TRUE if we get a 21 and FALSE otherwise:

blackjack()

# Now we can play this game, say, 10,000 times:

B <- 10000
results <- replicate(B, blackjack())
mean(results)

# The birthday problem
# Suppose you are in a classroom with 50 people. 
# If we assume this is a randomly selected group of 50 people, 
# what is the chance that at least two people have the same birthday? 

# Here we use a Monte Carlo simulation.
# For simplicity, we assume nobody was born on February 29.

# creating the 'class'
n <- 50
bdays <- sample(1:365, n, replace = TRUE)

# To check if in this particular set of 50 people 
# we have at least two with the same birthday, 
# we can use the function duplicated, 
# which returns TRUE whenever an element of a vector is a duplicate.

# example:
duplicated(c(1,2,3,1,4,3,5))

# to check if two birthdays were the same, we simply use the any and duplicated functions like this:
any(duplicated(bdays))

# To estimate the probability of a shared birthday in the group, 
# we repeat this experiment by sampling sets of 50 birthdays over and over:
same_birthday <- function(n){
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
}

B <- 10000
results <- replicate(B, same_birthday(50))
mean(results)

# Say we want to use this knowledge to bet with friends 
# about two people having the same birthday in a group of people. 
# When are the chances larger than 50%? Larger than 75%?
# Let's create a look-up table. 
# We can quickly create a function to compute this for any group size:
compute_prob <- function(n, B=10000){
  results <- replicate(B, same_birthday(n))
  mean(results)
}

# Using the function sapply, we can perform element-wise operations on any function:
n <- seq(1,60)
prob <- sapply(n, compute_prob)

# We can now make a plot of the estimated probabilities
# of two people having the same birthday in a group of size n:

library(tidyverse)
prob <- sapply(n, compute_prob)
qplot(n, prob)

# computing the exact probability
exact_prob <- function(n){
  prob_unique <- seq(365,365-n+1)/365 
  1 - prod( prob_unique)
}
eprob <- sapply(n, exact_prob)

qplot(n, prob) + 
  geom_line(aes(n, eprob), col = "red")

# how big do we need B to be?
# One practical approach we will describe here is to check for the stability of the estimate. 
# The following is an example with the birthday problem for a group of 22 people.

B <- 10^seq(1, 5, len = 100)
compute_prob <- function(B, n=25){
  same_day <- replicate(B, same_birthday(n))
  mean(same_day)
}
prob <- sapply(B, compute_prob)
qplot(log10(B), prob, geom = "line")

#In this plot, we can see that the values start to stabilize
# (that is, they vary less than .01) around 1000. 
# Note that the exact probability, which we know in this case, is 0.569.

# excercise 
# Two teams, say the Cavs and the Warriors, 
# are playing a seven game championship series. 
# The first to win four games wins the series. 
# The teams are equally good, 
# so they each have a 50-50 chance of winning each game.
# If the Cavs lose the first game, 
# what is the probability that they win the series?

# Assign a variable 'n' as the number of remaining games.
n<-6
n

# Assign a variable `outcomes` as a vector of possible game outcomes, 
# where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes<-c(0,1)
outcomes

# # Assign a variable `l` to a list of all possible outcomes in all remaining games. 
# Use the `rep` function on `list(outcomes)` to create list of length `n`.
l<-rep(list(outcomes),n)
l

# Create a data frame named 'possibilities' 
# that contains all combinations of possible outcomes 
# for the remaining games.
possibilities<-expand.grid(l)
possibilities

# Create a vector named 'results' 
# that indicates whether each row in the data frame 'possibilities' 
# contains enough wins for the Cavs to win the series.
cavswins<-rowSums(possibilities)
cavswins
results<-cavswins>=4
results

# Calculate the proportion of 'results' in which the Cavs win the series. 
# Print the outcome to the console.
mean(results)

# Monte Carlo excercise
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations 
# a simulated series and determines whether that series contains at least 
# four wins for the Cavs.
sixgames<-sample(c(0,1),6,replace=TRUE)
sixgames
results<-replicate(B, sum(sixgames<-sample(c(0,1),6,replace=TRUE))>=4)

# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.
mean(results)

# Exercise 3. A and B play a series - part 1
# Two teams, A and B, are playing a seven series game series. 
# Team A is better than team B and has a p>0.5 chance of winning each game

# Let's assign the variable 'p' as the vector of probabilities 
# that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series 
# for the underdog team B can be computed with the following 
# function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities 
# that team A will win to determine the probability that team B will win. 
# Call this object 'Pr'.
Pr<-sapply(p <- seq(0.5, 0.95, 0.025),prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)

# Exercise 4. A and B play a series - part 2
# Given a value 'p', the probability of winning the series 
# for the underdog team B can be computed with the following function
# based on a Monte Carlo simulation:

prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. 
# Use only odd numbers ranging from 1 to 25 games.
N<-seq(1,25,2)
N

# Apply the 'prob_win' function 
# across the vector of series lengths 
# to determine the probability that team B will win. Call this object `Pr`.
Pr<-sapply(N,prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N,Pr)

### The addition rule

# The rule is: P(A and B) = P(A)+P(B)-P(A and B)

# The Monty Hall Problem

# contestants were asked to pick one of three doors. 
# Behind one door there was a prize. 
# The other doors had a goat behind them 
# to show the contestant they had lost. 
# After the contestant picked a door, 
# before revealing whether the chosen door contained a prize, 
# Monty Hall would open one of the two remaining doors 
# and show the contestant there was no prize behind that door. 
# Then he would ask "Do you want to switch doors?"

# Monte Carlo simulation of The Monty Hall Problem

# estimating the chance of winning if we stick to our original door

B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

# estimating the chance of winning if we switch to the other door

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  switch <- doors[!doors%in%c(my_pick, show)]
  switch == prize_door
})
mean(switch)

# The Monte Carlo estimate confirms the 2/3 calculation. 

# Assessment

# 1. How many different ways can the 3 medals
# be distributed across 8 runners (order matters)?
medals<-3
runners<-8

perm<-permutations(runners,medals)
nrow(perm)

# How many different ways 
# can the three medals be distributed among the 3 runners from Jamaica?
permjam<-permutations(3,medals)
nrow(permjam)

# What is the probability that all 3 medals are won by Jamaica?
nrow(permjam)/nrow(perm)

# Run a Monte Carlo simulation 
# on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", 
             "USA", "Ecuador", "Netherlands", "France", "South Africa")

# For each iteration of the Monte Carlo simulation, 
# within a replicate loop, 
# select 3 runners representing the 3 medalists and check 
# whether they are all from Jamaica. 
# Repeat this simulation 10,000 times. 
# Set the seed to 1 before running the loop.

runners <- c("Jamaica", "Jamaica", "Jamaica", 
             "USA", "Ecuador", "Netherlands", "France", "South Africa")

winners<-sample(runners,3,replace=FALSE)
winners
winnersjam<-winners==c("Jamaica","Jamaica","Jamaica")
sum(winnersjam)==3

test<-c("Jamaica","Jamaica","Jamaica")
test

B<-10000
set.seed(1)
alljam<-replicate(B, {
  winners<-sample(runners,3,replace=FALSE)
  whojam<-winners==c("Jamaica","Jamaica","Jamaica")
  sum(whojam)==3
})
mean(alljam)

# question 2

# A meal at the restaurant includes 
# 1 entree, 2 sides, and 1 drink. 
# He currently offers a choice of 6 entrees, 
# a choice of 2 sides from a list of 6 options, and a choice of 2 drinks.

# How many meal combinations are possible with the current menu?
entree<-combinations(6,1)
sides<-combinations(6,2)
drink<-combinations(2,1)

currentcomb<-nrow(entree)*nrow(sides)*nrow(drink)
currentcomb

# How many combinations are possible 
# if he expands his original special to 3 drink options?
drink<-combinations(3,1)
newcomb<-nrow(entree)*nrow(sides)*nrow(drink)
newcomb

# How many meal combinations are there 
# if customers can choose from 6 entrees, 
# 3 drinks, and select 3 sides from the current 6 options?
newsides<-combinations(6,3)
newcomb<-nrow(entree)*nrow(newsides)*nrow(drink)
newcomb

# Write a function that takes a number of entree choices 
# and returns the number of meal combinations possible 
# given that number of entree options, 
# 3 drink choices, and a selection of 2 sides from 6 options.
comb_entree <- function(entree_num){
  entree<-combinations(entree_num,1)
  sides<-combinations(6,2)
  drink<-combinations(3,1)
  comb<-nrow(entree)*nrow(sides)*nrow(drink)
}

# Use sapply to apply the function to entree option counts ranging from 1 to 12.
entree_num<-c(1:12)

sapply(entree_num,comb_entree)

# Write a function that takes a number of side choices 
# and returns the number of meal combinations possible 
# given 6 entree choices, 3 drink choices, and a selection of 2 sides 
# from the specified number of side choices.

comb_sides <- function(sides_num){
  entree<-combinations(6,1)
  sides<-combinations(sides_num,2)
  drink<-combinations(3,1)
  comb<-nrow(entree)*nrow(sides)*nrow(drink)
}

# Use sapply to apply the function to side counts ranging from 2 to 12.
sides_num<-c(2:12)

sapply(sides_num,comb_sides)

# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1

head(esoph)
dim(esoph)

# How many groups are in the study?
nrow(esoph)

# How many cases are there?
all_cases<-sum(esoph$ncases)
all_cases

# How many controls are there?
all_controls<-sum(esoph$ncontrols)
all_controls

# What is the probability that a subject
# in the highest alcohol consumption group is a cancer case?
levels(esoph$alcgp)
sum(esoph$alcgp=="120+")

cases_by_alcgp<-esoph %>%
  group_by(alcgp) %>%
  summarize(nc=sum(ncases),n=sum(ncases)+sum(ncontrols),p=nc/n)
cases_by_alcgp

# their answer:
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# Given that a person is a case, 
# what is the probability that they smoke 10g or more a day?

tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

# Given that a person is a control, 
# what is the probability that they smoke 10g or more a day?

tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

tob_controls/all_controls

# For cases, what is the probability of being in the highest alcohol group?
levels(esoph$alcgp)
alc_highest <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

alc_highest/all_cases

# For cases, what is the probability of being in the highest tobacco group?
levels(esoph$tobgp)
tob_highest <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

tob_highest/all_cases

# For cases, what is the probability of being 
# in the highest alcohol group AND the highest tobacco group?
both_highest <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

both_highest/all_cases

# For cases, what is the probability of being 
# in the highest alcohol group OR the highest tobacco group?
either_highest <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

either_highest/all_cases

# For controls, what is the probability of being in the highest alcohol group?
alc_highest_controls <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

alc_highest_controls/all_controls

# How many times more likely are cases than controls 
# to be in the highest alcohol group?
p_cases<-alc_highest/all_cases
p_controls<-alc_highest_controls/all_controls
p_cases/p_controls

# For controls, what is the probability of being in the highest tobacco group?
# For cases, what is the probability of being in the highest tobacco group?
tob_highest_controls <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

tob_highest_controls/all_controls

# For controls, what is the probability of being 
# in the highest alcohol group AND the highest tobacco group?
both_highest_controls <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

both_highest_controls/all_controls

# For controls, what is the probability of being 
# in the highest alcohol group AND the highest tobacco group?
either_highest_controls <- esoph %>%
  filter(alcgp == "120+" | tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

either_highest_controls/all_controls

# How many times more likely are cases than controls 
# to be in the highest alcohol group or the highest tobacco group?
p_cases<-either_highest/all_cases
p_controls<-either_highest_controls/all_controls
p_cases/p_controls

# Section 2 - continuous probability

# We described empirical cumulative distribution function (eCDF) 
# in Section 9.4 as a basic summary of a list of numeric values. 
# As an example, we earlier defined the height distribution 
# for adult male students. Here, we define the vector x to contain these heights:

library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# We defined the empirical distribution function as:
F <- function(a) mean(x<=a)

# which, for any value a, gives the proportion of values 
# in the list x that are smaller or equal than a.

# what is the chance that he is taller than 70.5 inches?
1-F(70.5)

# The cumulative distribution for the normal distribution 
# is defined by a mathematical formula which in R can be 
# obtained with the function pnorm. 

# We say that a random quantity is normally distributed 
# with average m and standard deviation s if its probability distribution 
# is defined by:

F(a) = rnorm(a, m, s)

# Assuming that the 'heights' data has a normal distribution,
# what is the chance that a person is larger than 70.5 inches?

m <- mean(x)
s <- sd(x)
1 - pnorm(70.5, m, s)

# In R, we get the probability density for the normal distribution
# using the function dnorm.

# R provides functions to generate normally distributed outcomes. 
# Specifically, the rnorm function takes three arguments: 
# size, average (defaults to 0), and standard deviation (defaults to 1) 
# and produces random numbers. 
# Here is an example of how we could generate data 
#that looks like our reported heights:

x <- heights %>% filter(sex=="Male") %>% .$height
n <- length(x)
m <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, m, s)

#distribution of simulated heights:
ds_theme_set()
data.frame(simulated_heights=simulated_heights) %>% 
  ggplot(aes(simulated_heights))+geom_histogram(color="black",binwidth=2)

# This is one of the most useful functions in R
# as it will permit us to generate data that mimics 
# natural events and answers questions related to what 
# could happen by chance by running Monte Carlo simulations.

# If, for example, we pick 800 males at random, 
# what is the distribution of the tallest person? 
# How rare is a seven footer in a group of 800 males? 
# The following Monte Carlo simulation helps us answer that question:

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, m, s)
  max(simulated_data)
})

# Having a seven footer is quite rare:

mean(tallest >= 7*12)

# Other continuous distributions that we may encounter
# are the student-t, chi-squared, exponential, gamma, beta, 
# and beta-binomial.

# R provides functions to compute the density, 
# the quantiles, the cumulative distribution functions 
# and to generate Monte Carlo simulations. 
# R uses a convention that lets us remember the names, 
# namely using the letters 
# d - density
# q - quantile
# p - probability density function
# r - random

# The functions qnorm gives us the quantiles. 
# We can therefore draw a distribution like this:

x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>% 
  ggplot(aes(x, f)) + 
  geom_line()

# Exercise 1. Distribution of female heights
# Assume the distribution of female heights 
# is approximated by a normal distribution 
# with a mean of 64 inches and a standard deviation of 3 inches. 
# If we pick a female at random, what is the probability 
# that she is 5 feet or shorter?

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', 
# calculate the probability that a randomly selected female 
# is shorter than 5 feet.  
pnorm(60,female_avg,female_sd)

# Calculate the probability that a randomly selected female is 6 feet 
# or taller. Print this value to the console.
1-pnorm(72,female_avg,female_sd)

# If we pick a female at random, 
# what is the probability that she is between 61 and 67 inches?
pnorm(67,female_avg,female_sd)-pnorm(61,female_avg,female_sd)

# Repeat the previous exercise, but convert everything to centimeters. 
# That is, multiply every height, 
# including the standard deviation, by 2.54. What is the answer now?

# Assign a variable 'female_avg' as the average female height. 
# Convert this value to centimeters.
female_avg <- 64*2.54

# Assign a variable 'female_sd' as the standard deviation for female heights. 
# Convert this value to centimeters.
female_sd <- 3*2.54

# Calculate the probability that a randomly selected female 
# is between the desired height range. 
pnorm(67*2.54,female_avg,female_sd)-pnorm(61*2.54,female_avg,female_sd)

# Compute the probability that the height of a randomly chosen female 
# is within 1 SD from the average height.

# To a variable named 'taller', 
# assign the value of a height that is one SD taller than average.
taller<-pnorm(female_avg+female_sd,female_avg,female_sd)

# To a variable named 'shorter', 
# assign the value of a height that is one SD shorter than average.
shorter<-pnorm(female_avg-female_sd,female_avg,female_sd)

# # Calculate the probability that a randomly selected female 
# is between the desired height range. 
taller-shorter

# Imagine the distribution of male adults 
# is approximately normal with an average of 69 inches 
# and a standard deviation of 3 inches. 
# How tall is a male in the 99th percentile?

# Assign a variable 'male_avg' as the average male height.
male_avg <- 69

# Assign a variable 'male_sd' as the standard deviation for male heights.
male_sd <- 3

# Determine the height of a man in the 99th percentile of the distribution.
qnorm(0.99, mean=male_avg, sd=male_sd)

# The distribution of IQ scores is approximately normally distributed. 
# The average is 100 and the standard deviation is 15. 
# Suppose you want to know the distribution of the person 
# with the highest IQ in your school district, 
# where 10,000 people are born each year.

# Generate 10,000 IQ scores 1,000 times 
# using a Monte Carlo simulation. 
# Make a histogram of the highest IQ scores.

# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer 
# matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score 
# from each random distribution of 10,000 people.
highestIQ <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)
  max(simulated_data)
})

# Make a histogram of the highest IQ scores.
hist(highestIQ)

# For the three year period 2016-2018, 
# ACT standardized test scores were approximately normally distributed 
# with a mean of 20.9 and standard deviation of 5.7. 

# Set the seed to 16, 
# then use rnorm to generate a normal distribution of 10000 tests 
# with a mean of 20.9 and standard deviation of 5.7. 
# Save these values as act_scores.
set.seed(16, sample.kind = "Rounding")

act_scores<-rnorm(10000,20.9,5.7)

# What is the mean of act_scores?
mean(act_scores)

# What is the standard deviation of act_scores?
sd(act_scores)

# In act_scores, how many scores of 36 or greater are there 
# out of 10,000 simulated tests?
sum(act_scores>=36)

# In act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores>30)

# In act_scores, 
# what is the probability of an ACT score less than or equal to 10?
mean(act_scores<=10)

# Set x equal to the sequence of integers 1 to 36. 
# Use dnorm to determine the value of the probability density function over x 
# given a mean of 20.9 and standard deviation of 5.7; 
# save the result as f_x. 
# Plot x against f_x.

x<-c(1:36)
f_x<-dnorm(x,20.9,5.7)
plot(x,f_x)

# Convert act_scores to Z-scores.
zscores<-(act_scores-mean(act_scores))/sd(act_scores)

# What is the probability of a Z-score greater than 2 
# (2 standard deviations above the mean)?
mean(zscores>2)

# What score value corresponds to 2 standard deviations above the mean (Z = 2)?
score<-2*sd(act_scores)+mean(act_scores)
score

# Use qnorm to determine the 97.5th percentile 
# of normally distributed data 
# with the mean and standard deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?
qnorm(.975, mean(act_scores), sd(act_scores))

# Write a function that takes a value and produces the probability 
# of an ACT score less than or equal to that value (the CDF). 
# Apply this function to the range 1 to 36.

cdf<-function(score){
  prob<-pnorm(score,mean(act_scores), sd(act_scores))
}

score<-c(1:36)
allprob<-cdf(score)
plot(allprob)

# What is the minimum score such that the probability 
# of that score or lower is at least .95?
allprob  

# their answer:
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))

# Use qnorm to determine the expected 95th percentile, 
# the value for which the probability of receiving that score 
# or lower is 0.95, 
# given a mean score of 20.9 and standard deviation of 5.7.
qnorm(.95,20.9,5.7)

# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), 
# the 1st through 99th percentiles 
# of the act_scores data. Save these as sample_quantiles.
# In what percentile is a score of 26?

# their answer:
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))]))

# my answer: 
pnorm(26,mean(act_scores),sd(act_scores))

# Make a corresponding set of theoretical quantiles 
# using qnorm over the interval p <- seq(0.01, 0.99, 0.01) 
# with mean 20.9 and standard deviation 5.7. 
# Save these as theoretical_quantiles. 
# Make a QQ-plot graphing sample_quantiles on the y-axis 
# versus theoretical_quantiles on the x-axis.
p <- seq(0.01, 0.99, 0.01)

theoretical_quantiles<-qnorm(p,20.9,5.7)
plot(theoretical_quantiles,sample_quantiles)

# their answer:
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

### Random variables
# A random variable is a variable 
# whose possible values are numerical outcomes of a random phenomenon.

### Creating an 'urn' (glass) with 2 red beads and 3 blue beads
beads <- rep( c("red", "blue"), times = c(2,3))

# Defining X to be 1 if a bead is blue and red otherwise
X <- ifelse(sample(beads, 1) == "blue", 1, 0)

# X is a random variable. every time we select
# a new bead, the outcome changes randomly

# Sampling Models
# Constructing a roulette wheel that 
# has 18 red pockets, 18 black pockets and 2 green ones. 
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2))


# The 1,000 outcomes from 1,000 people playing are independent draws 
# from this urn. 
# If red comes up, the gambler wins and the casino loses a dollar, 
# so we draw a -$1. Otherwise, the casino wins a dollar and we draw a $1. 
# To construct our random variable, S, , we can use this code:

n <- 1000
X <- sample(ifelse(color == "Red", -1, 1),  n, replace = TRUE)
X[1:10]

# Because we know the proportions of 1s and -1s, 
# we can generate the draws with one line of code, without defining color:

X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))

# We call this a sampling model since we are modeling 
# the random behavior of roulette with the sampling of draws from an urn. 
# The total winnings, S, is simply the sum of these 1,000 independent draws:

X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
S <- sum(X)
S

# We can estimate the distribution function for the random variable S
# by using a Monte Carlo simulation to generate 
# many realizations of the random variable. 
# With this code, we run the experiment of having 1,000 people play roulette, 
# over and over, specifically B=10,000 times:

n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
  sum(X)
}
S <- replicate(B, roulette_winnings(n))

# Now we can ask the following: in our simulations, 
# how often did we get sums less than or equal to 0?

mean(S <= 0)

# This will be a very good approximation of F(a). 
# In fact, we can visualize the distribution 
# by creating a histogram showing the probability
# F(b)-F(a) for several intervals (a,b]:

# In the histogram above, we see that the distribution appears to be 
# approximately normal. 
# A qq-plot will confirm that the normal approximation is close to perfect.

# If, in fact, the distribution is normal, 
# then all we need to define the distribution 
# is the average and the standard deviation. 
# Because we have the original values 
# from which the distribution is created, we can easily compute these:

mean(S)
sd(S)

# If we add a normal density with this average and standard deviation 
# to the histogram above, we see that it matches very well:
s<-seq(min(S), max(S), length=100)
normal_density<-data.frame(s=s, f=dnorm(s, mean(S), sd(S)))
data.frame(S=S) %>% ggplot(aes(S, ..density..))+
  geom_histogram(color="black", binwidth=10)+
  ylab("Probability")+
  geom_line(data=normal_density, mapping=aes(s,f), color="Blue")

# In the visualization chapter, we described how any list of numbers x1,.,xn
# has a distribution. The definition is quite straightforward. We define  
# F(a) as the function that tells us 
# what proportion of the list is less than or equal to  a. 
# Because they are useful summaries when the distribution 
# is approximately normal, 
# we define the average and standard deviation. 
# These are defined with a straightforward operation of the vector 
# containing the list of numbers x:

library(dslabs)
x <- heights$height
m <- mean(x)
s <- sqrt(mean((x-m)^2))
m
s

# A random variable X
# has a distribution function. 
# To define this, we do not need a list of numbers. 
# It is a theoretical concept. 

# In statistical textbooks, upper case letters are used to denote 
# random variables and we follow this convention here. 
# Lower case letters are used for observed values.

# The Central Limit Theorem (CLT) tells us that when the number of draws, 
# also called the sample size, is large, the probability distribution 
# of the sum of the independent draws is approximately normal. 

# Monte Carlo simulation for playing the roulette one million times
B <- 10^6
x <- sample(c(-1,1), B, replace = TRUE, prob=c(9/19, 10/19))

# Calculating the mean in order to estimate the expected value of 
# the winnings of the casino per game:
mean(x)

# If a random variable has a probability distribution that is approximated with the normal distribution, then all we need to describe the probability distribution are the average and standard deviation, referred to as the expected value and standard error.

# We previously ran this Monte Carlo simulation:

n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
  sum(X)
}
S <- replicate(B, roulette_winnings(n))
# The Central Limit Theorem (CLT) tells us that the sum  S
# is approximated by a normal distribution. 
# Using the formulas above, 
# we know that the expected value and standard error are:

n * (20-18)/38 
sqrt(n) * 2 * sqrt(90)/19 

# The theoretical values above match those 
# obtained with the Monte Carlo simulation:

mean(S)
sd(S)

# Using the Central Limit Theorem, 
# we can skip the Monte Carlo simulation 
# and instead compute the probability of the casino 
# losing money using this approximation:

mu <- n * (20-18)/38
se <-  sqrt(n) * 2 * sqrt(90)/19 
pnorm(0, mu, se)

# which is also in very good agreement with our Monte Carlo result:
mean(S < 0) 

# Exercise 1. American Roulette probabilities
# An American roulette wheel has 18 red, 18 black, and 2 green pockets.
# What are the chances that the ball lands in a green pocket?

# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability 
# of the ball landing in a green pocket.
p_green<-green/(sum(green,black,red))
p_green

# In American roulette, the payout for winning on green is $17. 
# This means that if you bet $1 and it lands on green, you get $17 as a prize.
# Create a model to predict your winnings from betting on green one time.

# Use the `set.seed` function to make sure your answer 
# matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain 
# the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability
# of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` 
# as the probability of the ball not landing in a green pocket
p_not_green<-1-p_green

# Create a model to predict the random variable `X`, 
# your winnings from betting on green. Sample one time.
n<-1
X<-sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))

# Print the value of `X` to the console
X

# Now, compute the expected value of X, 
# the random variable you generated previously.
ev<-17*p_green+(-1)*p_not_green

# Now, compute the standard error of that random variable, 
# which represents a single outcome after one spin of the roulette wheel.
SE<-abs(17-(-1))*sqrt(p_green*p_not_green)
SE

# Now create a random variable S 
# that sums your winnings after betting on green 1,000 times.

# Use the `set.seed` function to make sure your answer 
# matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n<-1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X<-sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S<-sum(X)

# Print the value of 'S' to the console
S

# What is the expected value of S?
ev*n
(17*p_green+(-1)*p_not_green)*n


# What is the standard error of S?
SE*sqrt(n)
abs(17-(-1))*sqrt(p_green*p_not_green)*sqrt(n)

# Averages and proportions
# There are serveal useful mathematical results that we used above and often employ when working with data.

# What is the probability that you end up winning money 
# if you bet on green 100 times?

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', 
# compute the probability that you win money betting on green 100 times.
1-pnorm(0,avg,se)

# Create a Monte Carlo simulation 
# that generates 10,000 outcomes of S, the sum of 100 bets.

# Compute the average and standard deviation 
# of the resulting list and compare them to the expected 
# value (-5.263158) and standard error (40.19344) for S 
# that you calculated previously.

# Assign a variable `p_green` as the probability of the ball 
# landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability 
# of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times 
# we want the simulation to run. Let's run 
# the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure 
# your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates 
# the sample code for `B` iterations and sums the outcomes.
S<-replicate(B, {
  onehundredbets<-sample(c(17,-1),n,replace=TRUE,
                         prob=c(p_green,p_not_green))
  sum(onehundredbets)
})

# Compute the average value for 'S'
mean(S)

# Now create a random variable Y that contains 
# your average winnings per bet after betting on green 10,000 times. 

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
n<-10000
X<-sample(c(17,-1),n,replace=TRUE,
          prob=c(p_green,p_not_green))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y<-mean(X)


# Calculate the standard deviation of 'S'
sd(S)

# # Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)

# Now create a random variable Y 
# that contains your average winnings per bet 
# after betting on green 10,000 times.

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets
n<-10000
X<-sample(c(17,-1),n,replace=TRUE,
          prob=c(p_green,p_not_green))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y<-mean(X)
Y

# What is the expected value of Y, 
# the average outcome per bet after betting on green 10,000 times?
m<-17*p_green + -1*p_not_green

# What is the standard error of Y, the average result of 10,000 spins?
s<-abs(17 - (-1))*sqrt(p_green*p_not_green)/sqrt(n)

# What is the probability that your winnings are positive 
# after betting on green 10,000 times?
1-pnorm(0,m,s)

# Create a Monte Carlo simulation that generates 10,000 outcomes of S, 
# the average outcome from 10,000 bets on green.
n<-10000
B<-10000
S<-replicate(B, {
  onehundredbets<-sample(c(17,-1),n,replace=TRUE,
                         prob=c(p_green,p_not_green))
  mean(onehundredbets)
})

# Compute the proportion of outcomes in the vector 'S' 
# where you won more than $0
mean(S>0)

# An old version of the SAT college entrance exam had a -0.25 point penalty 
# for every incorrect answer and awarded 1 point for a correct answer. 
# The quantitative test consisted of 44 multiple-choice questions 
# each with 5 answer choices. 
# Suppose a student chooses answers by guessing for all questions on the test.

# What is the expected value of points for guessing on one question?
p_guessed_correct<-0.2
p_guessed_incorrect<-0.8

ev<-1*p_guessed_correct+ -.25*p_guessed_incorrect
ev

# What is the standard error of te final score when guessing on all 44 questions?
se<-abs(1- (-.25))*sqrt(p_guessed_correct*p_guessed_incorrect)*sqrt(44)
se

# Use the Central Limit Theorem to determine 
# the probability that a guessing student scores 8 points or higher on the test.
1-pnorm(8,ev,se)

# Set the seed to 21, then run a Monte Carlo simulation 
# of 10,000 students guessing on the test.
set.seed(21, sample.kind = "Rounding")

# What is the probability that a guessing student scores 8 points or higher?
n<-44
B<-10000
S<-replicate(B, {
  test_guessed<-sample(c(1,-.25),n,replace=TRUE,
                       prob=c(p_guessed_correct,p_guessed_incorrect))
  sum(test_guessed)
})

mean(S>=8)

# Suppose that the number of multiple choice options is 4 
# and that there is no penalty for guessing - that is, an incorrect 
# question gives a score of 0.

# What is the expected value of the score when guessing on this new test?
p_guessed_correct<-0.25
p_guessed_incorrect<-0.75

ev<-44*(1*p_guessed_correct+ 0*p_guessed_incorrect)

# What is the probability of scoring over 30 when guessing?
se<-abs(1- (0))*sqrt(p_guessed_correct*p_guessed_incorrect)*sqrt(44)

1-pnorm(30,ev,se)

# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) 
# representing a range of student skills.
# What is the lowest p such that the probability 
# of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)

chances<-function(p){  
  ev<-44*p
  se<-sqrt(p*(1-p))*sqrt(44)
  chance<-1-pnorm(35,ev,se)
  chance
}

chances_test<-sapply(p,chances)
chances_test

min(p[which(chances_test > 0.8)])

# Question 3A
p_guessed_correct<-5/38
p_guessed_incorrect<-33/38
n<-500

evonebet<-6*p_guessed_correct+(-1)*p_guessed_incorrect
seonebet<-abs(6-(-1))*sqrt(p_guessed_correct*p_guessed_incorrect)
seonebet

evfivehundred_sum<-n*(6*p_guessed_correct+(-1)*p_guessed_incorrect)
evfivehundred_sum

se_fivehundred_average<-seonebet/sqrt(n)
se_fivehundred_average

se_fivehundred_sum<-seonebet*sqrt(n)
se_fivehundred_sum

pnorm(0,evfivehundred_sum,se_fivehundred_sum)

# The Big Short

# Suppose your bank will give out 1,000 loans for $180,000 this year. 
# Also, after adding up all costs, suppose your bank loses $200,000 
# per foreclosure. 
# For simplicity, we assume this includes all operational costs. 
# A sampling model for this scenario can be coded like this:

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02 
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Note that the total loss defined by the final sum is a random variable.
# We can easily construct a Monte Carlo simulation 
# to get an idea of the distribution of this random variable.

B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

# Here is the distribution of this random variable:
data.frame(losses_in_millions=losses/10^6) %>% 
  ggplot(aes(losses_in_millions))+geom_histogram(binwidth=0.6,col="black")

# We don't really need a Monte Carlo simulation though. 
# The CLT tells us that because our losses are a sum of independent draws, 
# its distribution is approximately normal 
# with expected value and standard errors given by:

# expected value
n*(p*loss_per_foreclosure + (1-p)*0)

# standard error
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

# The profit per loan that is returned that guarantees that 
# on average we break even, is:
- loss_per_foreclosure*p/(1-p)

# So the interest rate is:
(- loss_per_foreclosure*p/(1-p))/180000

# Calculating the profit per loan
# for a chance of losing over 1,000 loans
# that is smaller than 0.01
# assuming the expected profit over 1,000
# loans is a random variable, with a 
# normal distribution (according to the Central Limit Theorem)

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# Our interest rate now goes up to 0.035. 
# This is still a very competitive interest rate. 
# By choosing this interest rate, 
# we now have an expected profit per loan of:

loss_per_foreclosure*p + x*(1-p)

# which is a total expected profit of about:

n*(loss_per_foreclosure*p + x*(1-p)) 

# We can run a Monte Carlo simulation 
# to double check our theoretical approximations:

B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})

mean(profit)

mean(profit<0)

# The financial crisis of 2007
# One of your employees points out that since the bank 
# is making 2124 dollars per loan, the bank should give out more loans! 
# He claims that even if the default rate is twice as high, say 4%,
#  if we set the rate just a bit higher than this value:
p <- 0.04
r <- (- loss_per_foreclosure*p/(1-p)) / 180000
r

# we will profit. At 5%, we are guaranteed a positive expected value of:
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x * (1-p)

# With x fixed, now we can ask what n 
# do we need for the probability to be 0.01? 
# In our example, if we give out:

z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n

# loans, the probability of losing is about 0.01 and 
# we are expected to earn a total of

n*(loss_per_foreclosure*p + x * (1-p))

# dollars! We can confirm this with a Monte Carlo simulation:

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

# But let's assume there is a global event that affects everybody 
# with high risk mortgages and changes their probability. 
# These draws are no longer independent.

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})

# Note that our expected profit is still large:
mean(profit)

# However, the probability of the bank having negative earnings shoots up to:
mean(profit<0)

# Even scarier is that the probability of losing more than 10 million dollars is:
mean(profit < -10000000)

# To understand how this happens look at the distribution:
data.frame(profit_in_millions=profit/10^6) %>% 
  ggplot(aes(profit_in_millions)) + 
  geom_histogram(color="black", binwidth = 5)

# Assessment
# Say you manage a bank that gives out 10,000 loans. 
# The default rate is 0.03 and you lose $200,000 in each foreclosure.
# Create a random variable \(S\) that contains the earnings of your bank. 
# Calculate the total amount of money lost in this scenario.

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches 
# the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` 
# that contains the default outcomes of `n` loans
n<-10000
defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE)

# Generate `S`, the total amount of money lost across all foreclosures. 
# Print the value to the console.
loss_per_foreclosure<- -200000
S<-sum(defaults * loss_per_foreclosure)
S

# Run a Monte Carlo simulation with 10,000 outcomes 
# for the sum of losses over 10,000 loans. Make a histogram of the results.

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise 
# over 'B' iterations to generate a list of summed losses for 'n' loans.  
# Ignore any warnings for now.

losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p_default, p_default), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(losses)

# What is the expected value of the sum of losses over 10,000 loans? 
# For now, assume a bank makes no money if the loan is paid.
loss_per_foreclosure*p_default*n

# What is the standard error of the sum of losses over 10,000 loans?
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p_default*(1-p_default))

# Assume we give out loans for $180,000. 
# How much money do we need to make when people 
# pay their loans so that our net loss is $0?

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Assign a variable `x` as the total amount necessary 
# to have an expected outcome of $0
# loss_per_foreclosure*p + x * (1-p)=0
# x=loss_per_foreclosure*p/(1-p)
x<- -loss_per_foreclosure*p_default/(1-p_default)
x

# Convert `x` to a rate, given that the loan amount is $180,000. 
loan_amount<-180000
r<-x/loan_amount
# Print this value to the console.
r

# What should the interest rate be so that the chance of losing money is 1 in 20?

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z<-qnorm(0.05,0,1)
z

# Generate a variable `x` using `z`, `p_default`, 
# `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*(n*p_default - z*sqrt(n*p_default*(1-p_default)))/ ( n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))
x


# Convert `x` to an interest rate, given that the loan amount is $180,000. 
# Print this value to the console.
r<-x/180000

# Final Assessment
data(death_prob)
head(death_prob)

# Question 1
# Use death_prob to determine the death probability of a 50 year old female, p.
death_prob_filtered <- death_prob %>% filter(age==50, sex=="Female")
p<-mean (death_prob_filtered$prob)

# Their answer:
p <- death_prob %>%
  filter(sex == "Female" & age == "50") %>%
  pull(prob)
p

# The loss in the event of the policy holder's death is -$150,000 
# and the gain if the policy holder remains alive is the premium $1,150.

# What is the expected value of the company's net profit 
# on one policy for a 50 year old female?

loss<- -150000
gain<- 1150

ev<-(1-p)*gain+p*loss
ev

# Their answer:
a <- -150000
b <- 1150

mu <- a*p + b*(1-p)
mu

# Calculate the standard error of the profit on one policy for a 50 year old female.
se<-(gain-loss)*sqrt(p*(1-p))
se

# Their answer:
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

# What is the expected value of the company's profit 
# over all 1,000 policies for 50 year old females?
n<-1000
ev_sum<-n*ev
ev_sum

# Their answer:
n <- 1000
n*mu

# What is the standard error of the sum of the expected value 
# over all 1,000 policies for 50 year old females?
se_sum<-se*sqrt(n)
se_sum

#Their answer:
sqrt(n) * sigma

# Use the Central Limit Theorem to calculate the probability 
# that the insurance company loses money on this set of 1,000 policies.
ploss<-pnorm(0,ev_sum,se_sum)
ploss

#Their answer:
pnorm(0, n*mu, sqrt(n)*sigma)

# Question 2
# Use death_prob to determine the probability 
# of death within one year for a 50 year old male.
head(death_prob)
names(death_prob)
death_prob %>% filter(sex=="Male" & age==50) %>% pull(prob)

# Their answer:
p<- death_prob %>%
  filter(sex == "Male" & age == "50") %>%
  pull(prob)
p 

# Suppose the company wants its expected profits from 1,000 50 year old males 
# with $150,000 life insurance policies to be $700,000. 
# Use the formula for expected value of the sum of draws 
# with the following values and solve for the premium b:

ev_sum<-700000
n<-1000
a<-150000

ev<-ev_sum/n
ev

nominator<-ev-p*(-a)
b<-nominator/(1-p)
b

# Their answer:
p <- p_male
mu_sum <- 700000
n <- 1000
a <- -150000

b <- (mu_sum/n-a*p)/(1-p)
b

# Using the new 50 year old male premium rate, 
# calculate the standard error of the sum of 1,000 premiums.

se_sum<-(b-a)*sqrt(p*(1-p))*sqrt(n)
se_sum

# Their answer:
sigma_sum <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sigma_sum

# What is the probability of losing money 
# on a series of 1,000 policies to 50 year old males?
# Use the Central Limit Theorem.

pnorm(0,ev_sum,se_sum)

# this 6-part question, we'll look at a scenario in which 
# a lethal pandemic disease increases the probability of death 
# within 1 year for a 50 year old to .015. 
# Unable to predict the outbreak, the company has 
# sold 1,000 $150,000 life insurance policies for $1,150.

# What is the expected value of the company's profits over 1,000 policies?
premium<-1150
compensation<- -150000
p<-.015
n<-1000

ev_one_loan<-premium*(1-p)+compensation*p
ev_one_loan
ev_sum<-ev_one_loan*n
ev_sum

# Their answer:
p <- .015    # probability of claim
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

exp_val <- n*(a*p + b*(1-p))
exp_val

# What is the standard error of the expected value 
# of the company's profits over 1,000 policies?
se_one_loan<-abs(compensation-premium)*sqrt(p*(1-p))
se_one_loan
se_sum<-se_one_loan*sqrt(n)
se_sum

# Their answer:
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
se

# What is the probability of the company losing money?
pnorm(0,ev_sum,se_sum)

# Their answer:
pnorm(0, exp_val, se)

# Suppose the company can afford to sustain one-time 
# losses of $1 million, but larger losses will force 
# it to go out of business.
# What is the probability of losing more than $1 million?
pnorm(-10^6,ev_sum,se_sum)

# Their answer:
pnorm(-1*10^6, exp_val, se)

# Investigate death probabilities p <- seq(.01, .03, .001).
# What is the lowest 
# death probability for which the chance of losing money exceeds 90%?

premium<-1150
compensation<- -150000
n<-1000
p<-seq(.01, .03, .001)

ev_one_loan<-premium*(1-p)+compensation*p # the expected valud of one loan
ev_sum<-ev_one_loan*n # the expected value of the sum of n loans
se_one_loan<-abs(compensation-premium)*sqrt(p*(1-p)) # the standard error of one loan
se_sum<-se_one_loan*sqrt(n) # the standard error of the sum of n loans

chance_of_losing<-function(p){
  ev_one_loan<-premium*(1-p)+compensation*p
  ev_sum<-ev_one_loan*n
  se_one_loan<-abs(compensation-premium)*sqrt(p*(1-p))
  se_sum<-se_one_loan*sqrt(n)
  pnorm(0,ev_sum,se_sum)
}

chances<-sapply(p,chance_of_losing)
chances
p

# Their answer:
p <- seq(.01, .03, .001)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

p_lose_money <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})

data.frame(p, p_lose_money) %>%
  filter(p_lose_money > 0.9) %>%
  pull(p) %>%
  min()

# Investigate death probabilities p <- seq(.01, .03, .0025).
# What is the lowest death probability for which the chance of losing 
# over $1 million exceeds 90%?
p<-seq(.01, .03, .0025)

ev_one_loan<-premium*(1-p)+compensation*p # the expected valud of one loan
ev_sum<-ev_one_loan*n # the expected value of the sum of n loans
se_one_loan<-abs(compensation-premium)*sqrt(p*(1-p)) # the standard error of one loan
se_sum<-se_one_loan*sqrt(n) # the standard error of the sum of n loans

chance_of_losing<-function(p){
  ev_one_loan<-premium*(1-p)+compensation*p
  ev_sum<-ev_one_loan*n
  se_one_loan<-abs(compensation-premium)*sqrt(p*(1-p))
  se_sum<-se_one_loan*sqrt(n)
  pnorm(-1*10^6,ev_sum,se_sum)
}

chances<-sapply(p,chance_of_losing)

data.frame(p,chances) %>%
  filter(chances>0.9) %>%
  pull(p) %>%
  min()

# Their answer
p_lose_million <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1*10^6, exp_val, se)
})

data.frame(p, p_lose_million) %>%
  filter(p_lose_million > 0.9) %>%
  pull(p) %>%
  min()

# Question 4
# This questions continues question 3.

# Define a sampling model for simulating the total profit over 1,000 
# loans with probability of claim p_loss = .015, loss of -$150,000 on a claim, 
# and profit of $1,150 when there is no claim. 
# Set the seed to 25, then run the model once.
# What is the reported profit (or loss) in millions (that is, divided by 10^6)?

n<-1000
p_loss<-.015
loss<- -150000
profit<- 1150

set.seed(25, sample.kind = "Rounding")
results<-sample(c(loss,profit),n,replace=TRUE,prob=c(p_loss,1-p_loss))
profit_sum_in_millions<-sum(results)/10^6
profit_sum_in_millions

# Their answer:

set.seed(25)

p <- .015
loss <- -150000
profit <- 1150
n <- 1000

outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
sum(outcomes)/10^6

# Set the seed to 27, then run a Monte Carlo simulation of your sampling model 
# with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
# What is the observed probability of losing $1 million or more?
set.seed(27, sample.kind = "Rounding")
B<-10000
outcomes_monte_carlo<-replicate(B,{
  results<-sample(c(loss,profit),n,replace=TRUE,prob=c(p_loss,1-p_loss))
  profit_sum_in_millions<-sum(results)/10^6
  profit_sum_in_millions<=-1})
mean(outcomes_monte_carlo)

# Their answer:
set.seed(27)
B <- 10000

profits <- replicate(B, {
  outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
  sum(outcomes)/10^6
})

mean(profits < -1)

# Suppose that there is a massive demand for life insurance due to the pandemic, 
# and the company wants to find a premium cost for which the probability 
# of losing money is under 5%, assuming the death rate stays stable at p=0.015.

# Calculate the premium required for a 5% chance of losing money 
# given n=1000 loans, probability of death p=0.015, and loss per claim l= ???150000. 
# Save this premium as x for use in further questions.

n<-1000 
p<-0.015
l<- ???150000

zscore<qnorm(0.05,0,1)
zscore

nominator<- zscore*l*sqrt(p*(1-p)*n)-n*p*l
denominator<-n-n*p+zscore*sqrt(p*(1-p)*n)
w<-nominator/denominator
w

# Their answer:
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# What is the expected profit per policy at this rate?
ev_one<-p*l+(1-p)*w
ev_one

# What is the expected profit over 1,000 policies?
ev_sum<-ev_one*n
ev_sum

# Run a Monte Carlo simulation with B=10000
# to determine the probability of losing money on 1,000 policies 
# given the new premium x, loss on a claim of $150,000, and probability 
# of claim p=.015. Set the seed to 28 before running your simulation.
n<-1000 
p<-0.015
l<- ???150000
w<-(zscore*l*sqrt(p*(1-p)*n)-n*p*l)/(n-n*p+zscore*sqrt(p*(1-p)*n))

set.seed(28, sample.kind = "Rounding")
B<-10000
outcomes_monte_carlo<-replicate(B,{
  results<-sample(c(l,w),n,replace=TRUE,prob=c(p,1-p))
  profit_sum<-sum(results)
  profit_sum<0})
mean(outcomes_monte_carlo)

# Their answer:
set.seed(27)
B <- 10000

profits <- replicate(B, {
  outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
  sum(outcomes)/10^6
})

mean(profits < -1)

# The company cannot predict whether the pandemic death rate 
# will stay stable. Set the seed to 29, then write a Monte Carlo 
# simulation that for each of B=10000 iterations:

# randomly changes p by adding a value between -0.01 and 0.01 
# with sample(seq(-0.01, 0.01, length = 100), 1)

# uses the new random p to generate a sample 
# of n=1,000 policies with premium x and loss per claim l=???150000

# returns the profit over n policies (sum of random variable)

# The outcome should be a vector of B total profits. 


set.seed(29, sample.kind = "Rounding")
B<-10000
outcomes_monte_carlo<-replicate(B,{
  random_addition_to_p<-sample(seq(-0.01, 0.01, length = 100), 1)
  p_new<-p+random_addition_to_p
  results<-sample(c(l,w),n,replace=TRUE,prob=c(p_new,1-p_new))
  profit_sum<-sum(results)
  profit_sum
})
head(outcomes_monte_carlo)
class(outcomes_monte_carlo)
length(outcomes_monte_carlo)

# What is the expected value over 1,000 policies?
mean(outcomes_monte_carlo)

# Their answer:
# n, p, l and x as defined in the problem information
set.seed(29)    # in R 3.6, set.seed(29, sample.kind="Rounding")

profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(x, l), n, 
                  prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})

mean(profit)

# What is the probability of losing money?
mean(outcomes_monte_carlo<0)

# Their answer:
mean(profit < 0)

# What is the probability of losing more than $1 million?
mean(outcomes_monte_carlo< -10^6)


# Code written in Facebook
# plot with colorblind palette
png("F6_Instagram.png", width = 600, height = 500)
add_new_friend_by_app %>%
  filter(question=="add_new_friend_on_instagram") %>%
  ggplot(aes(x = country, y = proportion, fill=country)) +
  geom_bar(stat = "identity", position=position_dodge(1)) +
  xlab("") +
  ylab("") +
  scale_fill_tableau(palette="Color Blind", type="regular")+
  labs(title = "% who would connect on Instagram")+
  scale_y_continuous(limits=c(0,.90), expand=c(0,0), 
                     labels=scales::percent_format(accuracy=1)) + 
  geom_text(aes(label=sprintf("%1.0f%%", 100*proportion)), vjust=-1, hjust=-0.2, size=5)+
  geom_label(aes(y=.2, label=n_label), fill = "snow2", label.padding = unit(0.6, "lines"), vjust=3, alpha=0.9)+
  geom_errorbar(aes(x=country,ymin=proportion_low,ymax=proportion_upp),
                width=0, colour="black", alpha=0.5, size=0.8)+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 0, vjust=1, hjust=0.6)) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA))+
  theme(axis.ticks.x=element_blank())+
  theme(plot.title = element_text(hjust = 0.5, vjust=-10, size=20))+
  theme(legend.position = "none")
dev.off()


# The Central Limit Theorem in Practice

install.packages("Lahman")

# Assessment 1.2
# 
library(tidyverse)
url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
test<-read.csv(url(url), header = F)
dim(test)

# 2.1 Tidy Data
# Code provided by course:
library(tidyverse)
library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# 2.2 Reshaping data
# original wide data
library(tidyverse) 
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)

# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

# 2.3 Separate and Unite
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

# Assessment 2.1
library(tidyverse)
library(dslabs)

head(co2)
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide,month,co2,-year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)

head(dat)

dat_tidy <- spread(dat, gender, admitted)

dat_tidy

tmp <- gather(admissions, key, value, admitted:applicants)
head(tmp)

tmp2 <- spread(tmp, column_name, c(key,gender))
tmp2 <- gather(tmp, column_name, c(gender,key))
tmp2 <- spread(tmp, column_name, key, gender)
tmp2 <- unite(tmp, column_name, c(gender, key))
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2

# 2.2. combining data
# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

head(tab1)
head(tab2)

tab1
tab2

df1<-cbind.data.frame(c("a","b"),c("a","a"))
names(df1)<-c("x","y")
df1

df2<-cbind.data.frame(c("a","a"),c("a","b"))
names(df2)<-c("x","y")
df2

final <- setdiff(df1, df2)
final

library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

head(top)
head(Master)

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

top_names

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

top_salary

head(AwardsPlayers)

Awards_2016<-AwardsPlayers %>% filter(yearID==2016)

Awards_2016_players<-unique(Awards_2016$playerID)

Awards_2016_players

top_players<-unique(top_names$playerID)
top_players

setdiff(Awards_2016_players,top_players)
length(setdiff(Awards_2016_players,top_players))

# web scraping
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

# Web scraping assessment
# Load the following web page, which contains information about Major League Baseball payrolls, 
# into R: https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm 

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

# We learned that tables in html are associated with the table node.  
# Use the html_nodes() function and the table node type to extract the first table. 
# Store it in an object nodes:

nodes <- html_nodes(h, "table")

# The html_nodes() function returns a list of objects of class xml_node. 
# We can see the content of each one using, for example, the html_text() function. 
# You can see the content for an arbitrarily picked component like this:
nodes
html_text(nodes[[8]])

# If the content of this object is an html table, we can use the html_table() function to convert it to a data frame:
html_table(nodes[[8]])

# Which of the first four nodes are tables of team payroll?
html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

length(nodes)

html_table(nodes[[19]])
html_table(nodes[[20]])
html_table(nodes[[21]])

tab1<-html_table(nodes[[10]])
tab2<-html_table(nodes[[19]])

names(tab1)
head(tab1)
head(tab2)

tab_test<-tab1[,2:ncol(tab1)]
tab_test
tab1<-tab_test

tab1<-tab1[2:nrow(tab1),]
tab2<-tab2[2:nrow(tab2),]

names(tab1)<-c("Team", "Payroll", "Average")
names(tab2)<-c("Team", "Payroll", "Average")

tab_together<-full_join(tab1,tab2, by="Team")
nrow(tab_together)

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)

tabs<-html_nodes(h, "table")
length(tabs)

nodes <- html_nodes(h, "table")
nodes

tab1<-html_table(nodes[[1]], fill=T)
tab1[2,1]
ncol(tab1)

tab2<-html_table(nodes[[2]], fill=T)
tab2[2,1]
ncol(tab2)

tab3<-html_table(nodes[[3]], fill=T)
tab3[2,1]
ncol(tab3)

tab4<-html_table(nodes[[4]], fill=T)
tab4[2,1]
ncol(tab4)

tab5<-html_table(nodes[[5]], fill=T)
tab5[2,1]
ncol(tab5)
head(tab5)

# 3.1 string parsing - basic

# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

s <- "10""    # error - unclosed quotes
s <- '10"'    # correct

# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape with \
s <- '5'10"'    # error
s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)

# murders_raw defined in web scraping video

# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

library(tidyverse)    # includes stringr

# murders_raw was defined in the web scraping section

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

# 3.2 string parsing - case studies
library(dslabs)
data(reported_heights)

# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)

# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems

# Excercise
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

library(stringr)
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

pattern <- "[A-Z]$"
str_detect(animals, pattern)

pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "moo*"
str_detect(animals, pattern)

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

converted

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
index

converted[!index]


yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
converted

# 3.3.1   
# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# 3.3.2
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

# Trimming
s <- "Hi "
cat(s)
identical(s, "Hi")

# To upper and to lower case
s <- c("Five feet eight inches")
str_to_lower(s)

# Putting it into a function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

# another example
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# checking for problematic entries
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# 3.3
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

new_heights %>% arrange(height) %>% head(n=7)

# 3.4 recode
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

# 3.5 case study
library(dslabs)
data("research_funding_rates")
research_funding_rates

library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]

data("raw_data_research_funding_rates")

raw_data_research_funding_rates %>% head

tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]
tab %>% head
the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

identical(research_funding_rates, new_research_funding_rates)

# string splitting
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

# Assessment

schedule<-data.frame(2,2)
class(schedule)
schedule[1,]<-c("Monday", "Mandy, Chris and Laura")
schedule
schedule[2,]<-c("Tuesday", "Steve, Ruth and Frank")
names(schedule)<-c("day", "staff")

library(stringr)
str_split(schedule$staff, ", | and ")
str_split(schedule$staff, ",\\s|\\sand\\s")

library(dplyr)
library(tidyr)
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()
tidy

install.packages("gapminder")
library(gapminder)

head(gapminder)

dat <- gapminder %>% #filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

head(polls)
names(polls)<-c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes") 
dim(polls)

index<-which(str_detect(polls$remain, "%")==TRUE)
index

polls2<-polls[index,]
dim(polls2)

as.numeric(str_replace(polls$remain, "%", ""))/100 
parse_number(polls$remain)/100 


polls$undecided
str_replace_all(polls$undecided, "N/A", "0")
polls$undecided

temp <- str_extract_all(polls$dates, "\\d?\\s[a-zA-Z]?")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

temp <- str_extract_all(polls$dates, "\\d+\\s[A-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

temp <- str_extract_all(polls$dates, "[0-9]+\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

temp <- str_extract_all(polls$dates,  "\\d{1,2}[a-zA-Z]+" )
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
end_date

# 4.1 dates
library(lubridate)

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

sys.time()
now()
now("GMT")

# 4.2 text mining
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")

head(trump_tweets)

names(trump_tweets)

trump_tweets %>% select(text) %>% head

trump_tweets %>% count(source) %>% arrange(desc(n))

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

install.packages("tidytext")
library(tidytext)

example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

stop_words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

sentiments 

install.packages("textdata")
library(textdata)

get_sentiments("bing")

get_sentiments("afinn")

get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Assessment
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)

names(brexit_polls)
sum(month(brexit_polls$startdate)==4)

sum(round_date(brexit_polls$enddate, unit="week")=="2016-06-12")

library(lubridate)
table(weekdays(brexit_polls$enddate))

data(movielens)

suppressWarnings(as_datetime())
table(year(as_datetime(movielens$timestamp)))
years<-as.data.frame(table(year(as_datetime(movielens$timestamp))))
names(years)
years[which(years$Freq==max(years$Freq)),1]

hours<-as.data.frame(table(hour(as_datetime(movielens$timestamp))))
hours[which(hours$Freq==max(hours$Freq)),1]

library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
library(tidytext)
options(digits = 3)

suppressWarnings()
gutenberg_metadata

head(gutenbergr)

dat<-gutenberg_metadata
names(dat)
sum(str_detect(dat$title, "Pride and Prejudice"), na.rm=T)
str_detect(dat$title, "Pride and Prejudice")
which(str_detect(dat$title, "Pride and Prejudice")==TRUE)

gutenberg_works()
gutenberg_works(title == "Pride and Prejudice")[1]

pp<-gutenberg_download(gutenberg_works(title == "Pride and Prejudice")[1])
class(pp)
head(pp)

library(tidytext)
library(dplyr)
library(tidyverse)
words<-pp %>% unnest_tokens(word, text)
dim(words)

cleaned_words <- words %>%
  anti_join(stop_words)

dim(cleaned_words)

test<-as.data.frame(c("a", "a2"))

library(dplyr)
library(stringr)
names(test)<-"entries"
test %>% 
  filter(str_detect(entries, "\\d"))

test %>% filter(str_detect(entries, "\\d"))
test %>% filter(str_detect(entries, "^[^0-9]+$"))

cleaned_words_no_digits<-cleaned_words %>% filter(str_detect(word, "^[^0-9]+$"))

dim(cleaned_words_no_digits)

table(cleaned_words_no_digits$word)

freq_table<-as.data.frame(cleaned_words_no_digits %>%
                            count(word, sort = TRUE))

freq_table[1:10,]

sum(freq_table$n>100)

# project
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
suppressWarnings()
system("cmd.exe", input = paste("start", fn))

