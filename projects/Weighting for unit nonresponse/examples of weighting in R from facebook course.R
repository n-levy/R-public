library(dplyr)
library(survey)

#####################
## Example: Survey - (Subset) Count Trackula Facebook 
#####################

## Raw Count Trackula Data for Current Data (all engagement sample)
## 
survey_data<-fbr::presto("
                         SELECT 
                         ds,
                         user_id,
                         IF(CARDINALITY(answers_text[461127267674494]) >= 1, answers_text[461127267674494][1], NULL) city_type_piped_text,
                         IF(CARDINALITY(answers_numeric[461127267674494]) >= 1, answers_numeric[461127267674494][1], NULL) city_type_piped_numeric,
                         IF(CARDINALITY(answers_text[189198418584577]) >= 1, answers_text[189198418584577][1], NULL) city_type_piped_nos_text,
                         IF(CARDINALITY(answers_numeric[189198418584577]) >= 1, answers_numeric[189198418584577][1], NULL) city_type_piped_nos_numeric,
                         IF(CARDINALITY(answers_text[1979501122303786]) >= 1, answers_text[1979501122303786][1], NULL) country_live_in_2_tracker_text,
                         questions_context[1979501122303786]['translation'] country_live_in_2_tracker_translation,
                         IF(CARDINALITY(answers_text[1849082928736847]) >= 1, answers_text[1849082928736847][1], NULL) dob_day_text,
                         IF(CARDINALITY(answers_numeric[1849082928736847]) >= 1, answers_numeric[1849082928736847][1], NULL) dob_day_numeric,
                         IF(CARDINALITY(answers_text[139962333295941]) >= 1, answers_text[139962333295941][1], NULL) dob_month_text,
                         IF(CARDINALITY(answers_numeric[139962333295941]) >= 1, answers_numeric[139962333295941][1], NULL) dob_month_numeric,
                         IF(CARDINALITY(answers_text[1505820362835426]) >= 1, answers_text[1505820362835426][1], NULL) dob_year_text,
                         IF(CARDINALITY(answers_numeric[1505820362835426]) >= 1, answers_numeric[1505820362835426][1], NULL) dob_year_numeric,
                         IF(CARDINALITY(answers_text[415615515559272]) >= 1, answers_text[415615515559272][1], NULL) q3b_citylimits_openentrypiped_nos_text,
                         IF(CARDINALITY(answers_numeric[415615515559272]) >= 1, answers_numeric[415615515559272][1], NULL) q3b_citylimits_openentrypiped_nos_numeric,
                         IF(CARDINALITY(answers_text[129358344551198]) >= 1, answers_text[129358344551198][1], NULL) q3b_citylimits_pipedprediction_text,
                         IF(CARDINALITY(answers_text[134906823912807]) >= 1, answers_text[134906823912807][1], NULL) wa_l30_v2_text,
                         IF(CARDINALITY(answers_numeric[134906823912807]) >= 1, answers_numeric[134906823912807][1], NULL) wa_l30_v2_numeric,
                         IF(CARDINALITY(answers_text[1808220466156433]) >= 1, answers_text[1808220466156433][1], NULL) whatsapp_ever_use_text,
                         IF(CARDINALITY(answers_numeric[1808220466156433]) >= 1, answers_numeric[1808220466156433][1], NULL) whatsapp_ever_use_numeric
                         FROM simon_responses_formatted_non_bucketed
                         WHERE ds = '2019-03-15'
                         AND survey_id = 358818304522162
                         AND COALESCE(user_context['employee'], 'false') = 'false'
                         ", namespace="growth")

### Only run once
### It is much faster to do the join in SQL land than R
#fbr::upload.data.to.hive(survey_data,tablename="weighting_example_from_simon_data",namespace="growth")


surveyANDlog_data<-fbr::presto(
  "
  WITH survey AS
  (
  SELECT *
  FROM weighting_example_from_simon_data
  ),
  dim AS
  (
  SELECT 
  userid,
  case 
  when age between 13 and 18 then '13-18'
  when age between 19 and 25 then '19-25'
  when age between 26 and 34 then '26-34'
  when age> 34 then '35+'
  else 'unknown age'
  end as age_group,
  case 
  when cast(gender as bigint)=1 then 'female'
  when cast(gender as bigint)=2 then 'male'
  when cast(gender as bigint)=3 then 'female'
  when cast(gender as bigint)=4 then 'male'
  when cast(gender as bigint)=0 then 'unknown'
  end  as gender, 
  case
  WHEN l30 between 0 and 20  then 'fblow'
  WHEN l30 between 21 and 30 then 'fbhigh'
  end as fb30_strata
  FROM dim_all_users:bi
  WHERE ds = '2019-03-15'
  AND is30dayactive = '1'
  AND isactivated = '1'
  )
  SELECT survey.*,dim.*
  FROM survey
  LEFT JOIN dim
  ON survey.user_id = dim.userid
  ",namespace='growth'
)

#########################
### Look at data
#########################
dim(surveyANDlog_data)
names(surveyANDlog_data)
head(surveyANDlog_data)
tail(surveyANDlog_data)

## Delete extra userid
surveyANDlog_data<-surveyANDlog_data[,-which(names(surveyANDlog_data)=="")]


#####################
## Example: Population Aggregate Characteristics
#####################
pop_marginals<-fbr::presto(
  "
  SELECT 
  case 
  when age between 13 and 18 then '13-18'
  when age between 19 and 25 then '19-25'
  when age between 26 and 34 then '26-34'
  when age> 34 then '35+'
  else 'unknown age'
  end as age_group,
  case 
  when cast(gender as bigint)=1 then 'female'
  when cast(gender as bigint)=2 then 'male'
  when cast(gender as bigint)=3 then 'female'
  when cast(gender as bigint)=4 then 'male'
  when cast(gender as bigint)=0 then 'unknown'
  end  as gender, 
  case
  WHEN l30 between 0 and 20  then 'fblow'
  WHEN l30 between 21 and 30 then 'fbhigh'
  end as fb30_strata,
  COUNT(1) as pop_n
  FROM dim_all_users:bi
  WHERE ds = '2019-03-15'
  AND is30dayactive = '1'
  AND isactivated = '1'
  GROUP BY 1,2,3
  ORDER BY 1,2,3  
  ",namespace='growth'
)


region_pop_marginals<-fbr::presto(
  "
  WITH region AS (
  SELECT
  country_abbr as country,
  country_name,
  CASE

  WHEN geo_name LIKE '%Europe%' THEN 'Europe'
  WHEN geo_name IN ('United States', 'Canada') THEN 'US/CA'
  WHEN geo_name IN('Africa', 'Latin America', 'Middle East') THEN 'Rest of World'
  WHEN geo_name IS NULL THEN 'Rest of World'
  ELSE geo_name
  END AS region_primary,
  CASE
  WHEN geo_name LIKE '%Europe%' THEN 'Europe'
  WHEN geo_name IN ('United States', 'Canada') THEN 'US/CA'
  WHEN geo_name IN('Africa', 'Latin America', 'Middle East') THEN 'Rest of World - ' || geo_name
  WHEN geo_name IN('Rest of World') THEN 'Rest of World - Other'
  WHEN geo_name IS NULL THEN 'Rest of World - Other'
  ELSE geo_name
  END AS region_secondary
  FROM
  dim_geo_country_region:bi
  WHERE
  ds = '2019-03-15'
  ),
  dim AS
  (
  SELECT
  country,
  case 
  when age between 13 and 18 then '13-18'
  when age between 19 and 25 then '19-25'
  when age between 26 and 34 then '26-34'
  when age> 34 then '35+'
  else 'unknown age'
  end as age_group,
  case 
  when cast(gender as bigint)=1 then 'female'
  when cast(gender as bigint)=2 then 'male'
  when cast(gender as bigint)=3 then 'female'
  when cast(gender as bigint)=4 then 'male'
  when cast(gender as bigint)=0 then 'unknown'
  end  as gender, 
  case
  WHEN l30 between 0 and 20  then 'fblow'
  WHEN l30 between 21 and 30 then 'fbhigh'
  end as fb30_strata
  FROM dim_all_users:bi
  WHERE ds = '2019-03-15'
  AND is30dayactive = '1'
  AND isactivated = '1'
  ),
  dim_region AS
  (
  SELECT dim.*,region.*
  FROM dim
  LEFT JOIN region
  ON dim.country=region.country    
  )
  SELECT region_secondary, age_group, gender, fb30_strata, COUNT(*) as pop_n
  from dim_region
  GROUP BY 1,2,3,4
  ORDER BY 1,2,3,4 
  ",namespace='growth'
)


#####################
## Example: Post Stratafication Weights
#####################
weightFunction_postStrat<-function(survey,pop,variables){
  require(dplyr)
  ## Cannot have NAs on weighting variables
  var<-paste("!is.na(",variables,")",sep="")
  df<-dplyr::filter_(survey,var)
  
  ## Get sample sizd
  n.sample <-dplyr::summarise(df,n.sample = n())
  
  ## Summerize Marginals
  pop_marg<-lapply(variables,function(x){
    pop %>% 
      group_by_(x)%>%
      summarise(N = sum(as.numeric(pop_n))) %>%
      ungroup() %>%
      mutate(total = sum(N), proportion = N / total) %>%
      select_(x,"proportion")
  }
  )
  
  ## Based in HT sort of assumptions
  strat_expectedFreq<-lapply(pop_marg,function(x){
    strata<-merge(x,n.sample)
    strata<- strata %>%
      mutate(Freq = round(proportion * n.sample, 0)) %>%
      select(names(x)[1],"Freq")
    strata
  })
  
  ## Set up survey enviroment object
  suppressWarnings(df.unweighted <- survey::svydesign(ids = ~1, data = df))
  
  ### Variables
  sm_nam<-lapply(variables,function(x){as.formula(paste("~",x,sep=""))})
  
  ## PostStat
  df.ps<-vector("list",length(sm_nam))
  for(i in 1:length(sm_nam)){
    df.ps[[i]] <- weights(survey::postStratify(design = df.unweighted,strata = sm_nam[[i]],population = strat_expectedFreq[[i]]))
  }
  df.ps<-do.call("cbind",df.ps)
  colnames(df.ps)<-paste("ps_weights",variables,sep="_")
  #trimming weights at .1 and 12
  #overall_weight_trim <- survey::trimWeights(object, lower = 0.1, upper = 12, strict = TRUE)
  
  out<-data.frame(df,df.ps,stringsAsFactors=FALSE)
  out
}

#############
## Function for estimating proportions without weights
############
propEst<-function(var,d){
  tab<-table(d[[var]])
  out<-data.frame(prop.table(tab))
  out$se<-sqrt(out$Freq*(1-out$Freq)/sum(tab))
  out
}

##################
## Example
##################
survey_wts<-weightFunction_postStrat(survey=surveyANDlog_data,pop=pop_marginals,variables=c("gender","age_group","fb30_strata"))

head(survey_wts)
sum(survey_wts$ps_weights_gender)

dsn<-survey::svydesign(ids = ~1, weights=survey_wts$ps_weights_gender,data = survey_wts)

survey::svymean(~city_type_piped_text,dsn)
survey::svymean(~city_type_piped_text,dsn,na.rm=TRUE)
propEst("city_type_piped_text",survey_wts)
survey::svytotal(~city_type_piped_text,dsn,na.rm=TRUE)

survey_wts$dob_year<-as.numeric(survey_wts$dob_year_text)
dsn<-survey::svydesign(ids = ~1, weights=survey_wts$ps_weights_gender,data = survey_wts)
survey::svymean(~dob_year,dsn,na.rm=TRUE)
mean(survey_wts$dob_year,na.rm=TRUE)
se<-sd(survey_wts$dob_year,na.rm=TRUE)/sqrt(sum(!is.na(survey_wts$dob_year)))
se


#####################
## Example: Raking Weights
#####################
weightFunction<-function(survey,pop,variables){
  require(dplyr)
  ## Cannot have NAs on weighting variables
  var<-paste("!is.na(",variables,")",sep="")
  df<-dplyr::filter_(survey,var)
  
  ## Get sample sizd
  n.sample <-dplyr::summarise(df,n.sample = n())
  
  ## Summerize Marginals
  pop_marg<-lapply(variables,function(x){
    pop %>% 
      group_by_(x)%>%
      summarise(N = sum(as.numeric(pop_n))) %>%
      ungroup() %>%
      mutate(total = sum(N), proportion = N / total) %>%
      select_(x,"proportion")
  }
  )
  
  ## Based in HT sort of assumptions
  strat_expectedFreq<-lapply(pop_marg,function(x){
    strata<-merge(x,n.sample)
    strata<- strata %>%
      mutate(Freq = round(proportion * n.sample, 0)) %>%
      select(names(x)[1],"Freq")
    strata
  })
  
  ## Set up survey enviroment object
  suppressWarnings(df.unweighted <- survey::svydesign(ids = ~1, data = df))
  
  ### Variables
  sm_nam<-lapply(variables,function(x){as.formula(paste("~",x,sep=""))})
  
  ## Rake weights
  df.raked <- survey::rake(design = df.unweighted,  # this is our unweighted survey design object 
                           sample.margins = sm_nam, #this is the list of variable names from the data files under population.margins that the raking function will take
                           population.margins = strat_expectedFreq,#list of expected frequencies for the sample based on population frequencies
                           control = list(maxit = 100, epsilon = 1, verbose = FALSE))
  
  #trimming weights at .1 and 12
  overall_weight_trim <- survey::trimWeights(df.raked, lower = 0.1, upper = 12, strict = TRUE)
  
  out<-data.frame(df,weights_pop=weights(overall_weight_trim),stringsAsFactors=FALSE)
  out
}

##################
## Example
##################
survey_wts<-weightFunction(survey=surveyANDlog_data,pop=pop_marginals,variables=c("gender","age_group","fb30_strata"))
head(survey_wts)
sum(survey_wts$weights_pop)

dsn<-survey::svydesign(ids = ~1, weights=survey_wts$weights_pop,data = survey_wts)

survey::svymean(~city_type_piped_text,dsn)
survey::svymean(~city_type_piped_text,dsn,na.rm=TRUE)
propEst("city_type_piped_text",survey_wts)
survey::svytotal(~city_type_piped_text,dsn,na.rm=TRUE)


survey_wts$dob_year<-as.numeric(survey_wts$dob_year_text)
dsn<-survey::svydesign(ids = ~1, weights=survey_wts$weights_pop,data = survey_wts)
survey::svymean(~dob_year,dsn,na.rm=TRUE)
mean(survey_wts$dob_year,na.rm=TRUE)
se<-sd(survey_wts$dob_year,na.rm=TRUE)/sqrt(sum(!is.na(survey_wts$dob_year)))
se

#####################
## Example: Inverse Propensity Score Weights
#####################
response<-as.numeric(!is.na(surveyANDlog_data$dob_year_text))
surveyANDlog_data$response<-response
surveyANDlog_data$dob_year<-as.numeric(surveyANDlog_data$dob_year_text)
mod<-glm(response~age_group+gender+fb30_strata,family=binomial(link = "logit"),data=surveyANDlog_data)
x<-na.omit(surveyANDlog_data[,c("age_group","gender","fb30_strata")])
p<-predict(mod,x,type="response")
w<-ifelse(surveyANDlog_data$response==1,1/p,1/(1-p))

summary(lm(dob_year~q3b_citylimits_pipedprediction_text+city_type_piped_text,data=surveyANDlog_data,weights=w))
summary(lm(dob_year~q3b_citylimits_pipedprediction_text+city_type_piped_text,data=surveyANDlog_data))

dsn<-survey::svydesign(ids = ~1, weights=w,data = surveyANDlog_data)
survey::svymean(~dob_year,dsn,na.rm=TRUE)
survey::svymean(~dob_year_text,dsn,na.rm=TRUE)

help("svydesign")

dim(surveyANDlog_data)
dim(dim.data.frame(surveyANDlog_data))
view()
view()