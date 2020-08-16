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

 
 