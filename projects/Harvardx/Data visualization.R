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

