# Course 4 - Inference and Modeling

# To mimic the challenge real pollsters face in terms of competing with
# other pollsters for media attention, we will use an urn full of beads to represent voters and pretend we are
# competing for a $25 dollar prize. The challenge is to guess the spread between the proportion of blue and
# red beads in this urn.

# Before making a prediction, you can take a sample (with replacement) from the urn. To mimic the fact that
# running polls is expensive, it costs you $0.10 per each bead you sample. Therefore, if your sample size is
# 250, and you win, you will break even since you will pay $25 to collect your $25 prize. Your entry into the
# competition can be an interval. If the interval you submit contains the true proportion, you get half what
# you paid and pass to the second phase of the competition. In the second phase, the entry with the smallest
# interval is selected as the winner.
# The dslabs package includes a function that shows a random draw from this urn:

take_poll(25)

# Exercise 5. se versus p

# Write a line of code that calculates the standard error se of
# a sample average when you poll 25 people in the population. 
# Generate a sequence of 100 proportions of Democrats p 
# that vary from 0 (no Democrats) to 1 (all Democrats).

# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions 
# ranging from 0 to 1 using the `seq` function
p<-seq(0,1,length.out=100)
 
# Create a variable `se` that contains the standard error 
# of each sample average
se<-sqrt(p*(1-p)/sqrtN)
 
# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)

# Using the same code as in the previous exercise, 
# create a for-loop that generates three plots of p versus se 
# when the sample sizes equal N=25, N=100, and N=1000.

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for (N in sample_sizes) {
  se<-sqrt(p*(1-p)/N)
  plot(p,se, ylim=c(0,max(se)))
}

# Say the actual proportion of Democratic voters is p=0.45. 
# In this case, the Republican party is winning by a relatively large margin of d=-0.1, or a 10% margin of victory. 
# What is the standard error of the spread 2X¯-1 in this case?
# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
se<-2*sqrt(p*(1-p)/N)
se

# In our first sample we had 12 blue and 13 red so ¯X=0.48 
# and our estimate of standard error is:

x_hat <- 12/(12+13)
x_hat
se <- sqrt(x_hat*(1-x_hat)/25)
se

# Suppose we want to know what is the probability that we are within 1% from p. 
# Now we can answer the question:
pnorm(0.01/se) - pnorm(-0.01/se)

# The margin of error is simply 1.96 times the standard error.
# So assuming a normal distribution, 95% of the results
# will be within 1.96 standard errors from the real mean.

# Suppose we want to use a Monte Carlo simulation 
# to corroborate the tools we have built using probability theory. 
# To create the simulation, we would write code like this:

B <- 10000
N <- 1000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

# Let's set p=0.45. We can then simulate a poll:
p <- 0.45
N <- 1000

x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# In this particular sample, our estimate is x_hat. 
# We can use that code to do a Monte Carlo simulation:

B <- 10000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

mean(x_hat)
sd(x_hat)

# A histogram and qq-plot confirm 
# that the normal approximation is accurate as well.

# Excercise 1
`take_sample` that takes `p` and `N` as arguements 
# and returns the average value of a randomly sampled population.
take_sample<-function(p,N)
  {mean(sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p)))}


# Use the `set.seed` function to make sure your answer matches 
# the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` 
# randomly selected people from a population containing a proportion of Democrats 
# equal to `p`. Print this value to the console.
take_sample(p,N)

# excercise 2

  # Define `p` as the proportion of Democrats in the population being polled
  p <- 0.45
  
  # Define `N` as the number of people polled
  N <- 100
  
  # The variable `B` specifies the number of times we want the sample to be replicated
  B <- 10000
  
  # Use the `set.seed` function to make sure your answer 
  # matches the expected result after random sampling
  set.seed(1)
  
  # Create an objected called `errors` that replicates 
  # subtracting the result of the `take_sample` function 
  # from `p` for `B` replications
  errors<-replicate(B, {
    error<-p-take_sample(p,N)})
  
  
  # Calculate the mean of the errors. Print this value to the console.
  mean(errors)

# Use the hist function to plot a histogram of the values 
# contained in the vector errors

hist(errors)

# excercise 4
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your 
# answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual 
# proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value 
# of each simulated error. Print this value to the console.
mean(abs(errors))

# excercise 5
# Calculate the standard deviation of `errors`
errors_abs<-abs(errors)

errors_abs_squared<-errors_abs^2

errors_var<-mean(errors_abs_squared)

errors_sd<-sqrt(errors_var)

errors_sd

# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sd<-sqrt(p*(1-p)/N)
sd

# excercise 7
# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters 
# with a probability of picking a Democrat ('1') equal to `p`
X<-sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))

# Define `X_bar` as the average sampled proportion
  X_bar<-mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)

# Create a plot of the largest standard error 
# for N ranging from 100 to 5,000. 
# Based on this plot, how large does the sample size 
# have to be to have a standard error of about 1%?
install.packages("ggplot2")
library(ggplot2)
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)

dat<-as.data.frame(cbind(N,se))
head(dat)
ggplot(data=dat, aes(x=N, y=se)) + geom_line()

# excercise 11
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution
qqnorm(errors)
qqline(errors)

# excercise 12
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats
# in the population is greater than 0.5. Print this value to the console.
sd<-sqrt(p*(1-p)/N)

prob<-1-pnorm(0.5,p,sd)
prob

# Exercise 13. Estimating the probability of a specific error size
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat<-sqrt(X_hat*(1-X_hat)/N)
 
# Calculate the probability that the error is 0.01 or larger
1-(pnorm(0.01/se_hat)-pnorm(-0.01/se_hat))

########### Confidence intervals ##################
# Monte Carlo simulation for confidence intervals

# We use the same parameters as above:
p <- 0.45
N <- 1000

# Here is the code for computing the confidence interval

x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / N)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)

# Notice that the interval changes each time we run the code

# If we want to have a larger probability, say 99%, 
# we need to multiply by whatever z satisfies the following:
# Pr(???z ??? Z ??? z)=0.99

# Using:
z <- qnorm(0.995)

# will achieve this because by definition pnorm(qnorm(0.995)) is 0.995 
# and by symmetry pnorm(1-qnorm(0.995)) is 1 - 0.995. 
# As a consequence, we have that:
pnorm(z) - pnorm(-z)

# is 0.995 - 0.005 = 0.99

# We can use this approach for any proportion p: 
# we set z = qnorm(1 - (1 - p)/2) because 1???(1???p)/2+(1???p)/2=p

# So, for example, for p=0.95
# 1???(1???p)/2=0.975

# and we get the 1.96 we have been using:
  
qnorm(0.975)

# We can run a Monte Carlo simulation to confirm that, 
# in fact, a 95% confidence interval includes p 95% of the time. 

N <- 1000
B <- 10000
inside <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  between(p, x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
})
mean(inside) 

# A note about power
# In the context of polls, 
# power is the probability of detecting spreads different from 0.

# Exercise 1. Confidence interval for p 
# For the following exercises, we will use actual poll data 
# from the 2016 election. The exercises will contain 
# pre-loaded data from the dslabs package.
library(dslabs)
data("polls_us_election_2016")

# Assume there are only two candidates 
# and construct a 95% confidence interval for the election night proportion p.

# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered 
# for polls that ended on or after October 31, 2016 in the United States
names(polls_us_election_2016)
unique(polls_us_election_2016$state)
head(polls_us_election_2016$enddate)
dim(polls_us_election_2016)
class(polls_us_election_2016$enddate)

polls_us_election_2016$enddate_numeric<-as.POSIXct(polls_us_election_2016$enddate, 
                                                   format="%Y-%m-%d")
head(polls_us_election_2016$enddate_numeric)

polls_us_election_2016_after_oct312016<-
  polls_us_election_2016 %>% filter(as.numeric(polls_us_election_2016$enddate_numeric) >= 
                                      as.numeric(as.POSIXct("2016-10-31", format="%Y-%m-%d")),
                                      state=='U.S.')

head(polls_us_election_2016_after_oct312016)
# How many rows does `polls` contain? Print this value to the console.
nrow(polls_us_election_2016_after_oct312016)

# Assign the sample size of the first poll in `polls` to a variable called `N`. 
# Print this value to the console.
N<-polls_us_election_2016_after_oct312016$samplesize[1]
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters 
# to a variable called `X_hat`. Print this value to the console.
X_hat<-polls_us_election_2016_after_oct312016$rawpoll_clinton[1]/100
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. 
# Print this value to the console.
se_hat<-sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval 
# for the proportion of Clinton voters. 
# Save the lower and then the upper confidence interval 
# to a variable called `ci`.
lower<-X_hat-qnorm(0.975)*se_hat
upper<-X_hat+qnorm(0.975)*se_hat
ci<-c(lower,upper)
ci

# Create a new object called `pollster_results` 
# that contains columns for pollster name, end date, X_hat, se_hat, 
# lower confidence interval, and upper confidence interval for each poll.
names(polls_us_election_2016_after_oct312016)
polls_us_election_2016_after_oct312016_new<-polls_us_election_2016_after_oct312016 %>%
  mutate(N=samplesize,
          X_hat=rawpoll_clinton/100,
          se_hat=sqrt(X_hat*(1-X_hat)/N),
         lower=X_hat-qnorm(0.975)*se_hat,
         upper=X_hat+qnorm(0.975)*se_hat)
pollster_results<-polls_us_election_2016_after_oct312016_new %>% 
  select(pollster, enddate, X_hat, se_hat, lower, upper) 

# Exercise 3. Comparing to actual results - p
# The `pollster_results` object has already been loaded. 
# Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates 
# whether the actual value exists within the confidence interval 
# of each poll. 
# Summarize the average `hit` result to determine 
# the proportion of polls with confidence intervals 
# include the actual value. 
# Save the result as an object called `avg_hit`.
head(pollster_results)
pollster_results_with_hit <- pollster_results %>% 
  mutate(hit=(0.482>lower & 0.482<upper)) 

avg_hit<-pollster_results_with_hit %>% summarise(mean=mean(hit))
avg_hit

# Exercise 5. Confidence interval for d
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 

# Assign the sample size of the first poll in `polls` to a variable called `N`. 
# Print this value to the console.
N<-polls$samplesize[1]
N

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. 
# Print this value to the console.
polls_with_d<-polls %>%
  mutate(d_hat_all=rawpoll_clinton[1]/100 - rawpoll_trump[1]/100)
d_hat<-polls_with_d$d_hat_all[1]
d_hat

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2
X_hat

# Calculate the standard error of the spread and save it 
# to a variable called `se_hat`. Print this value to the console.
se_hat<-2*sqrt(X_hat*(1-X_hat)/N)


# Use `qnorm` to calculate the 95% confidence interval for the difference 
# in the proportions of voters. Save the lower and then the upper confidence interval
# to a variable called `ci`.
lower<-d_hat-qnorm(0.975)*se_hat
upper<-d_hat+qnorm(0.975)*se_hat
ci<-c(lower,upper)
ci

# Exercise 6. Pollster results for d
# The subset `polls` data with 'd_hat' already calculated has been loaded. 
# Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns 
# for pollster name, end date, d_hat, lower confidence interval of d_hat, 
# and upper confidence interval of d_hat for each poll.

pollster_results_prep<-polls %>%
  mutate(N=samplesize,
        d_hat=rawpoll_clinton/100 - rawpoll_trump/100,
         X_hat = (d_hat+1)/2,
         se_hat=2*sqrt(X_hat*(1-X_hat)/N),
         lower=d_hat-qnorm(0.975)*se_hat,
         upper=d_hat+qnorm(0.975)*se_hat)

pollster_results<- pollster_results_prep %>%
  select(pollster, enddate, d_hat, lower, upper)

# # The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether 
# the actual value (0.021) exists within the confidence interval 
# of each poll. Summarize the average `hit` result to determine 
# the proportion of polls with confidence intervals include the actual value. 
# Save the result as an object called `avg_hit`.
actual_value<-0.021
pollster_results_with_hit <- pollster_results %>% mutate(hit=(actual_value>=lower & actual_value<=upper))
avg_hit<-pollster_results_with_hit %>% summarise(mean=mean(hit))

# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains 
# the difference between d_hat and the actual difference on election day. 
# Then make a plot of the error stratified by pollster.
polls_with_error<-polls %>% mutate(error=d_hat-actual_value)

polls_with_error %>% ggplot(aes(x = pollster, y =error )) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Add variable called `error` to the object `polls` that contains 
# the difference between d_hat and the actual difference on election day. 
# Then make a plot of the error stratified by pollster, 
# but only for pollsters who took 5 or more polls.
actual_value<-0.021
polls_with_error<-polls %>% mutate(error=d_hat-actual_value)

polls_with_error_and_num5<-polls_with_error %>%
  group_by(pollster) %>%
  add_count(pollster) %>%
  filter(n>=5)

head(polls_with_error_and_num5)

polls_with_error_and_num5 %>% 
  ggplot(aes(x = pollster, y =error )) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#############################################
# Chapter 16. Statistical models ############
#############################################

# Generating a data frame that mimics the results of 12 polls for the 2012 presidential election in the US

library(tidyverse)
library(dslabs)
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

polls <- map_df(Ns, function(N) {
  x <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  list(estimate = 2 * x_hat - 1, 
       low = 2*(x_hat - 1.96*se_hat) - 1, 
       high = 2*(x_hat + 1.96*se_hat) - 1,
       sample_size = N)
}) %>% mutate(poll = seq_along(Ns))

# Constructing an estimate of the spread, d, with a weighted average of the estimate
sum(polls$sample_size)

d_hat <- polls %>% 
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% 
  pull(avg)

round (d_hat*100,1)

### section 4 - statistical models ###
library(dslabs)
data(polls_us_election_2016)
rm(list=())

# Excercise 1

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

# Excercise 2
# The vector of all male heights in our population `x` has already been loaded for you. 
# You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X<-sample(x,N,replace=TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

# Excercise 4
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
se<-sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. 
# Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)
ci

# Excercise 5 - Monte Carlo Simulation for heights
# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
se<-sd(X)/sqrt(N)
ci<-c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)

res<-replicate(B, {
          X <- sample(x, N, replace = TRUE)
          se<-sd(X)/sqrt(N)
          ci<-c(mean(X)-qnorm(0.975)*se, mean(X)+qnorm(0.975)*se)
          between(mu, min(ci), max(ci))
          })

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
length(res)
mean(res)

# Exercise 6 - Visualizing Polling Bias

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

head(polls)
names(polls)
# Make a boxplot with points of the spread for each pollster
ggplot(polls,aes(pollster,spread))+geom_boxplot() + geom_point()

# Exercise 13 - Compute the Estimates
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma<-polls %>% group_by(pollster) %>% summarise(s=sd(spread))

# Print the contents of sigma to the console
sigma

# Exercise 15 - Calculate the 95% Confidence Interval of the Spreads
# The `polls` data have already been loaded for you. 
# Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, 
# standard deviation, and number of polls for the two pollsters.
res<-polls %>% group_by(pollster) %>% summarise(average=mean(spread),
                                                sd=sd(spread),
                                                n_polls=length(unique(adjpoll_clinton)))
res
# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate<-abs(res$average[1]-res$average[2])
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat<-sqrt(res$sd[1]^2/res$n_polls[1]+res$sd[2]^2/res$n_polls[2])
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-c(estimate-qnorm(0.975)*se_hat, estimate+qnorm(0.975)*se_hat)
ci

# Exercise 16 - Calculate the P-value
# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# estimate/se_hat

# Calculate the p-value
pvalue<-1-(pnorm(estimate/se_hat)-pnorm(-estimate/se_hat))
estimate/se_hat
pnorm(estimate/se_hat)
pvalue

# Exercise 17 - Comparing Within-Poll and Between-Poll Variability
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. 
# Print the contents of this object to the console.
var<-polls %>% group_by(pollster) %>% summarise(avg=mean(spread), s=sd(spread))
var

# Lesson - Bayes' theorem simulation
# We start by randomly selecting 100,000 people from a population
# in which the disease in question has a 1 in 4,000 prevalence.
prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"), N, replace = TRUE, 
                  prob = c(prev, 1 - prev))
N_D <- sum(outcome == "Disease") # People with disease in the population
N_D
N_H <- sum(outcome == "Healthy") # Healthy people in the population
N_H

# Now each person gets the test, which is correct 99% of the time:
  
accuracy <- 0.99 # accuracy of the test (both false positive rate and false negative rate)
test <- vector("character", N) # creating a vector
test[outcome == "Disease"]  <- sample(c("+", "-"), N_D, replace = TRUE, 
                                      prob = c(accuracy, 1 - accuracy)) # the results of all the people with the disease taking the test
test[outcome == "Healthy"]  <- sample(c("-", "+"), N_H, replace = TRUE, 
                                      prob = c(accuracy, 1 - accuracy)) # the results of all the healthy people taking the test
table(outcome, test)

# Lesson - Bayes in practice
# Excercise 2 - Recalculating the SIDS Statistics
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.
Pr_1*Pr_2

# Exercise 4 - Calculate the Probability
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB<-Pr_BA*Pr_A/Pr_B
Pr_AB

# Exercise 6 - Back to Election Polls
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)
names(polls)
dim(polls)
polls
# Create an object called `results` that has two columns containing 
# the average spread (`avg`) and the standard error (`se`). 
# Print the results to the console.
results<-NA
results<-polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(nrow(polls)))
results
resul
sum(5)

# Exercise 8 - Estimate the Posterior Distribution
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma<-results$se

# Define a variable called `Y` that contains the average in the object `results`
Y<-results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B<-sigma^2/(sigma^2+tau^2)

# Calculate the expected value of the posterior distribution
B*mu + (1-B)*Y 

# Exercise 9 - Standard Error of the Posterior Distribution
# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console
1/(1/sigma^2 + 1/tau^2)
sqrt(1/(1/sigma^2 + 1/tau^2))

# Exercise 10- Constructing a Credible Interval
# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ev<-B*mu + (1-B)*Y 
ci<-c(ev-qnorm(0.975)*se, ev+qnorm(0.975)*se)
ci

# Exercise 11 - Odds of Winning Florida
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). 
# Print this value to the console.
pnorm(0, exp_value, se)

# Exercise 12 - Change the Priors

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc<-function(tau){
  B<-sigma^2 / (sigma^2 + tau^2)
  exp_value <- B*mu + (1-B)*Y 
  pnorm(0, exp_value, sqrt( 1/ (1/sigma^2 + 1/tau^2)))
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps<-p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(x=taus,y=ps)

# Lecture - Election Forecasting
# Now we can use the formulas for the posterior distribution for the parameter d: 
# the probability of d>0 given the observed poll data:
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# To make a probability statement, we use the fact that the posterior distribution is also normal.
# And we have a credible interval of:
posterior_mean + c(-1.96, 1.96)*posterior_se

# The posterior probability Pr(d>0???¯X) can be computed like this:
1 - pnorm(0, posterior_mean, posterior_se)

# Lesson: Predicting the electoral college
# The results_us_election_2016 object is defined in the dslabs package:
  
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

# Code: Computing the average and standard deviation for each state

results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

# Code: Calculating the posterior mean and posterior standard error

# Note there is a small error in the video code: B should be defined as sigma^2/(sigma^2 + tau^2).

mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

# Code: Monte Carlo simulation of Election Night results (no general bias)

mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clintonEV) %>%
  ggplot(aes(clintonEV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Code: Monte Carlo simulation including general bias

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# Code: Variability across one pollster

# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

# Code: Trend across time for several pollsters

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

# Code: Plotting raw percentages across time

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

# Exercise 1 - Confidence Intervals of Polling Data
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
polls<-polls %>% mutate(x_hat=(polls$spread+1)/2)
polls$se<-2*sqrt(polls$x_hat*(1-polls$x_hat)/polls$samplesize)
polls$lower<-polls$spread-qnorm(0.975)*polls$se
polls$upper<-polls$spread+qnorm(0.975)*polls$se
cis<-polls %>% select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
ci_data

ci_data <- ci_data %>% mutate(hit=(actual_spread>lower & actual_spread<upper))
mean(ci_data$hit)
p_hits<-as.data.frame(mean(ci_data$hit))

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
ci_data <- ci_data %>% mutate(hit=(actual_spread>lower & actual_spread<upper))
mean(ci_data$hit)
p_hits<-as.data.frame(mean(ci_data$hit))

ci_data <- ci_data %>% mutate(hit=(actual_spread>lower & actual_spread<upper))

dim(ci_data)
p_hits<- ci_data %>% group_by(pollster) %>% 
  add_count(pollster) %>%
  filter(n>=5) %>%
  summarize(
  proportion_hits = mean(hit), 
  n=n(), 
  grade=grade[1]) %>%
  arrange(desc(proportion_hits))


p_hits<- ci_data %>% group_by(state) %>% 
  add_count(state) %>%
  filter(n>=5) %>%
  summarize(
    proportion_hits = mean(hit), 
    n=n()) %>%
  arrange(desc(proportion_hits))

class(p_hits)

p_hits<-as.data.frame(p_hits)
p_hits %>% ggplot(aes(state,proportion_hits))+
        geom_bar(stat="identity")+
        coord_flip()

head(ci_data)

cis<-ci_data

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
cis <- cis %>% mutate(error=spread-actual_spread)
cis<- cis %>% mutate(hit=(sign(actual_spread) == sign(spread)))

# Examine the last 6 rows of `errors`
head(cis)
errors<-cis
tail(errors)

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits<- errors %>% group_by(state) %>% 
  add_count(state) %>%
  filter(n>=5) %>%
  summarize(
    proportion_hits = mean(hit), 
    n=n())


# Make a barplot of the proportion of hits for each state
p_hits<-as.data.frame(p_hits)
p_hits %>% ggplot(aes(state,proportion_hits))+
  geom_bar(stat="identity")+
  coord_flip()

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
head(errors)
errors_for_plot<-errors %>% filter(grade=="A+" | grade=="A" | grade=="A-" | grade=="B+")
p<-errors_reordered %>% ggplot(aes(x=reorder(state,error), y=error))
p<-p+geom_boxplot()+geom_point()
p


# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors_for_plot<-errors %>% filter(grade=="A+" | grade=="A" | grade=="A-" | grade=="B+")
errors_for_plot<-errors_for_plot %>% group_by(state) %>% 
  add_count(state) %>%
  filter(n>=5) %>%
  ungroup()

p<-errors_for_plot %>% ggplot(aes(x=reorder(state,error), y=error))
p<-p+geom_boxplot()+geom_point()
p

# Lesson: The t-Distribution

z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

# Excercises
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1-(pt(2,3)-pt(-2,3))

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df<-seq(3,50)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func<-function(df){
  pr<-1-(pt(2,df)-pt(-2,df))
  }

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs<-sapply(df,pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res<-replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se<-sd(X)/sqrt(N)
  ci<-c(mean(X)-qt(0.975, N-1)*se, mean(X)+qt(0.975, N-1)*se)
  between(mu, min(ci), max(ci))
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)


# Association Tests

# Code: Research funding rates example

# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

# Code: Two-by-two table and p-value for the Lady Tasting Tea problem

tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

# Chi-Squared Tests

# Code: Chi-squared test

# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value

# Code: Odds ratio

# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# Code: p-value and odds ratio responses to increasing sample size

# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()

# Exercise 1 - Comparing Proportions of Hits

# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
errors_filtered<-errors %>% filter(grade=="A-" | grade=="C-")
dim(errors_filtered)

errors_filtered_grouped<- errors_filtered %>% group_by(grade, hit)

totals<-errors_filtered_grouped %>% summarize(num_hits = sum(hit), num_misses=sum(1-hit))

two_by_two<-NA
two_by_two <- tibble(hit = c("miss", "hit"),
                     a_minus = c(totals$num_misses[3], totals$num_hits[4]),
                     c_minus = c(totals$num_misses[1], totals$num_hits[2]))
two_by_two


# Print the proportion of hits for grade A- polls to the console
two_by_two$a_minus[2]/(two_by_two$a_minus[1]+two_by_two$a_minus[2])  
  
# Print the proportion of hits for grade C- polls to the console
two_by_two$c_minus[2]/(two_by_two$c_minus[1]+two_by_two$c_minus[2])  

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
two_by_two
chisq_test<-two_by_two %>% select(-hit) %>% chisq.test()

# Print the p-value of the chi-squared test to the console
chisq_test$p.value

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (two_by_two$c_minus[2] / sum(two_by_two$c_minus)) /
  (two_by_two$c_minus[1] / sum(two_by_two$c_minus))

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (two_by_two$a_minus[2] / sum(two_by_two$a_minus)) /
  (two_by_two$a_minus[1] / sum(two_by_two$a_minus))

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C

# Final excercises
# Brexit poll analysis - Part 1

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

# Question 1: Expected value and standard error of a poll
# Define p=0.481 as the actual percent voting "Remain" on the Brexit referendum and d=2p???1=???0.038 as the actual spread of the Brexit referendum with "Remain" defined as the positive outcome:

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread = p-(1-p)

# The final proportion of voters choosing "Remain" was p=0.481. Consider a poll with a sample of N=1500 voters.
N<-1500

# What is the expected total number of voters in the sample choosing "Remain"?
X<-p*N
X

# What is the standard error of the total number of voters in the sample choosing "Remain"?
se<-sqrt(p*(1-p)*N)
se

# What is the expected value of X^, the proportion of "Remain" voters?
p

# What is the standard error of X^, the proportion of "Remain" voters?
se<-sqrt(p*(1-p)/N)
se

# What is the expected value of d, the spread between the proportion of "Remain" voters and "Leave" voters?
d

# What is the standard error of d, the spread between the proportion of "Remain" voters and "Leave" voters?
se_d<-2*se
se_d

# Question 2: Actual Brexit poll estimates
head(brexit_polls)

# Calculate x_hat for each poll, the estimate of the proportion of voters choosing "Remain" on the referendum day (p=0.481), given the observed spread and the relationship d^=2X^???1. Use mutate() to add a variable x_hat to the brexit_polls object by filling in the skeleton code below: 
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

# What is the average of the observed spreads (spread)?
mean(brexit_polls$spread)

# What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)

# What is the average of x_hat, the estimates of the parameter? 
mean(brexit_polls$x_hat)

# What is the standard deviation of x_hat?
sd(brexit_polls$x_hat)

# Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
brexit_polls[1,]

# Use qnorm() to compute the 95% confidence interval for X^.
se_hat<-sqrt(brexit_polls$x_hat[1]*(1-brexit_polls$x_hat[1])/brexit_polls$samplesize[1])
se_hat

brexit_polls$x_hat[1]
ci_lower<-brexit_polls$x_hat[1]-qnorm(0.975)*se_hat
ci_lower

ci_upper<-brexit_polls$x_hat[1]+qnorm(0.975)*se_hat
ci_upper

# Brexit poll analysis - Part 2
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

#  Question 4: Confidence intervals for polls in June 

# Create the data frame june_polls containing only Brexit polls ending in June 2016 (enddate of "2016-06-01" and later). 

june_polls <- brexit_polls %>% filter(enddate>="2016-06-01")

# First, use mutate() to calculate a plug-in estimate se_x_hat for the standard error of the estimate SE^[X] for each poll given its sample size and value of X^ (x_hat).
june_polls<-june_polls %>%
  mutate(x_hat = (spread + 1)/2, se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize))

head(june_polls)

# Second, use mutate() to calculate an estimate for the standard error of the spread for each poll given the value of se_x_hat.
june_polls<-june_polls %>%
  mutate(se_spread = 2*se_x_hat)

# Then, use mutate() to calculate upper and lower bounds for 95% confidence intervals of the spread. 
june_polls<-june_polls %>%
  mutate(ci_lower = spread - qnorm(0.975)*se_spread, ci_upper=spread+qnorm(0.975)*se_spread)

#  Last, add a column hit that indicates whether the confidence interval for each poll covers the correct spread d=???0.038.
correct_spread<-0.038*(-1)
june_polls<-june_polls %>%
  mutate(hit = (correct_spread>ci_lower & correct_spread<ci_upper))

head(june_polls)

# How many polls are in june_polls?
dim(june_polls)
head(june_polls)
# What proportion of polls have a confidence interval that covers the value 0?
june_polls<-june_polls %>%
  mutate(hit_zero = (0>ci_lower & 0<ci_upper))
mean(june_polls$hit_zero)

# What proportion of polls predict "Remain" (confidence interval entirely above 0)?
mean(june_polls$ci_lower>0)

# What proportion of polls have a confidence interval covering the true value of d?
mean(june_polls$hit)

# Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
june_polls_grouped<-june_polls %>% group_by(pollster) %>% summarize(hit_rate=mean(hit), n_per_pollster=n()) %>% arrange(hit_rate)
view(june_polls_grouped)

# Make a boxplot of the spread in june_polls by poll type.
p<-june_polls %>% ggplot(aes(x=poll_type, y=spread))
p<-p+geom_boxplot()+geom_point()
p

# Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type. 
# Recall that to determine the standard error of the spread, you will need to double the standard error of the estimate.

# Use this code (which determines the total sample size per poll type, 
# gives each spread estimate a weight based on the poll's sample size, 
# and adds an estimate of p from the combined spread) to begin your analysis:

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type <- combined_by_type %>% mutate(se_hat=2*sqrt(p_hat*(1-p_hat)/N), ci_lower=spread-qnorm(0.975)*se_hat, ci_upper=spread+qnorm(0.975)*se_hat)
combined_by_type

combined_by_type <- combined_by_type %>% mutate(difference=ci_upper-ci_lower)
combined_by_type

# Define brexit_hit, with the following code, 
# which computes the confidence intervals for all Brexit polls in 2016 and then calculates whether the confidence interval covers the actual value of the spread d=???0.038:

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

head(brexit_hit)

# Use brexit_hit to make a two-by-two table of poll type and hit status.
two_by_two_brexit<-
  brexit_hit %>%
  group_by(poll_type) %>%
  summarize(hits=sum(hit), misses=sum(1-hit))

# Then use the chisq.test() function to perform a chi-squared test to determine whether the difference in hit rate is significant.
chisq_test <- two_by_two_brexit %>%
  select(-poll_type) %>%
  chisq.test()
chisq_test$p.value

# Use the two-by-two table constructed in the previous exercise to calculate the odds ratio 
# between the hit rate of online and telephone polls to determine the magnitude of the difference in performance between the poll types.

# Odds Online survey hitting
two_by_two_brexit
odds_online<-(two_by_two_brexit[1,2] / sum(two_by_two_brexit[1,2:3])) / (two_by_two_brexit[1,3] / sum(two_by_two_brexit[1,2:3]))
odds_online

# Odds Telephone survey hitting
odds_telephone<-(two_by_two_brexit[2,2] / sum(two_by_two_brexit[2,2:3])) / (two_by_two_brexit[2,3] / sum(two_by_two_brexit[2,2:3]))
odds_telephone

# Odds ratio
odds_online/odds_telephone


# Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored by poll type (poll_type). 
p<-brexit_polls %>% ggplot(aes(x=enddate, y=spread))
p<-p+geom_point(aes(color=poll_type))
p

# Use geom_smooth() with method = "loess" to plot smooth curves with a span of 0.4. Include the individual data points colored by poll type. 
p<-p+geom_smooth(aes(color=poll_type), method="loess", span=0.4)
p

# Add a horizontal line indicating the final value of d=???.038.
p<-p+geom_hline(yintercept=-.038)
p

# Use the following code to create the object brexit_long, which has a column vote containing the
# three possible votes on a Brexit poll ("remain", "leave", "undecided") and a column proportion containing the raw proportion choosing that vote option on the given poll:

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

head(brexit_long)

# Make a graph of proportion over time colored by vote. 
p<-brexit_long %>% ggplot(aes(x=enddate, y=proportion))
p<-p+geom_point(aes(color=vote))
p

# Add a smooth trendline with geom_smooth() and method = "loess" with a span of 0.3.
p<-p+geom_smooth(aes(color=vote), method="loess", span=0.3)
p



