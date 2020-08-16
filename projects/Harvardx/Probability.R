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
