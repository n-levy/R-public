#######################################
#### creating a summary table  ########
#######################################

# calculating counts and percentages without missing values
summary_table_func<-function(x){
  x_table_no_NA<-table(x) # table with counts
  x_percentages_table_no_NA<-prop.table(x_table_no_NA) # table with percentages
  x_counts_no_NA<-as.data.frame(x_table_no_NA) # counts without missing values
  x_percentages_no_NA<-as.data.frame(x_percentages_table_no_NA) # percentages without missing values
  x_percentages_no_NA$Freq<-round(x_percentages_no_NA$Freq,5) # rounding the percentages
  x_counts_no_NA['missing',]<-NA # adding an 'NA' value
  x_percentages_no_NA['missing',]<-NA # adding an 'NA' value
  
  # calculating count and percentages with missing variables
  x_table_with_NA<-table(x, exclude=FALSE) # table with counts
  x_percentages_table_with_NA<-prop.table(x_table_with_NA) # table with percentages
  x_counts_with_NA<-as.data.frame(x_table_with_NA) # counts with missing variables
  x_percentages_with_NA<-as.data.frame(x_percentages_table_with_NA) # percentages with missing variables
  x_percentages_with_NA$Freq<-round(x_percentages_with_NA$Freq,2) # rounding the percentages
  ifelse(nrow(x_counts_with_NA)==nrow(x_counts_no_NA),
         "",c(x_counts_with_NA['NA',]<-NA,x_percentages_with_NA['NA',]<-NA))
  x_df<-cbind.data.frame(x_counts_no_NA,x_percentages_no_NA$Freq,x_counts_with_NA$Freq,x_percentages_with_NA$Freq) # creating a data frame
  names(x_df)<-c(substitute(x),
                 'count_no_NA','p_no_NA','count_w_NA','p_w_NA') # renaming the columns
  x_df['Total',]<-c(NA, 
                    sum(x_table_no_NA),
                    sum(x_percentages_table_no_NA),
                    sum(x_table_with_NA),
                    sum(x_percentages_table_with_NA)) # adding a row of totals
  print(x_df)
  x_df<<-x_df
}

######################################################################################
#### plotting the distributions from the summary table without missing values ########
######################################################################################

summary_plot_func<-function(summary_table) {
  table_without_NAs<-subset(summary_table,!is.na(summary_table[,1])) # creating a table without missing variables
  distribution_plot<-ggplot (data=table_without_NAs, # defining the data
                             aes(x=table_without_NAs[,1], y=p_no_NA)) +  # setting the aesthetics
    geom_bar(fill = "#0073C2FF", stat="identity") + # creating a bar plot
    labs(title = paste("Distribution of '",names(summary_table)[1],"'", sep=""))+ # chart title
    xlab(names(summary_table)[1]) + # label of x axis
    ylab("Percentage") + # label of y axis
    scale_y_continuous(labels=scales::percent_format(), # scaling the y axis
                       limits=c(0,max(table_without_NAs$p_no_NA)*1.1), # setting the y axis limits
                       sec.axis = sec_axis(trans=~. * sum(as.numeric(table_without_NAs[,2])), # setting a second y axis with counts
                                           name="Count", labels=scales::comma)) + # setting the title of the second y axis and adding commas to the numbers
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, vjust=0, size=25), # designing the plot title
          axis.title.x = element_text(size=20, colour="grey30"), # designing the x axis title
          axis.title.y.right = element_text(size=20, colour="grey30", angle = 0, vjust=0.5), # designing the y axis right title
          axis.title.y.left = element_text(size=20, colour="grey30", angle = 0, vjust=0.5), # designing the y axis left title
          axis.text.x = element_text(size=20, colour="grey30", angle=0), # designing the x axis ticks
          axis.text.y.right = element_text(size=20, colour="grey30", angle=0), # designing the right y axis ticks
          axis.text.y.left = element_text(size=20, colour="grey30", angle=0), # designing the left y axis ticks
          panel.background = element_rect(fill = "transparent", colour = NA), # # creating blank background 1
          plot.background = element_rect(fill = "transparent", colour = NA)) # creating blank background 2
  
  #saving the plot   
  #png(paste(getwd(),'/plots/summary plots single vars/',names(summary_table)[1],'.png',sep=""), width = 800, height = 400)
  print(distribution_plot)
  #dev.off()
}

########################################################
##########  creating a summary table          ##########
##########  and plotting the distribution     ##########
##########  of a single variable              ##########
########################################################

summary_table_func_for_serial_plotting<-function(data, column_number){
  x_table_no_NA<-table(data[,column_number]) # table with counts
  x_percentages_table_no_NA<-prop.table(x_table_no_NA) # table with percentages
  x_counts_no_NA<-as.data.frame(x_table_no_NA) # counts without missing values
  x_percentages_no_NA<-as.data.frame(x_percentages_table_no_NA) # percentages without missing values
  x_percentages_no_NA$Freq<-round(x_percentages_no_NA$Freq,5) # rounding the percentages
  x_counts_no_NA['missing',]<-NA # adding an 'NA' value
  x_percentages_no_NA['missing',]<-NA # adding an 'NA' value
  
  # calculating count and percentages with missing variables
  x_table_with_NA<-table(data[,column_number], exclude=FALSE) # table with counts
  x_percentages_table_with_NA<-prop.table(x_table_with_NA) # table with percentages
  x_counts_with_NA<-as.data.frame(x_table_with_NA) # counts with missing variables
  x_percentages_with_NA<-as.data.frame(x_percentages_table_with_NA) # percentages with missing variables
  x_percentages_with_NA$Freq<-round(x_percentages_with_NA$Freq,2) # rounding the percentages
  ifelse(nrow(x_counts_with_NA)==nrow(x_counts_no_NA),
         "",c(x_counts_with_NA['NA',]<-NA,x_percentages_with_NA['NA',]<-NA))
  x_df<-cbind.data.frame(x_counts_no_NA,x_percentages_no_NA$Freq,x_counts_with_NA$Freq,x_percentages_with_NA$Freq) # creating a data frame
  names(x_df)<-c(names(data)[column_number],
                 'count_no_NA','p_no_NA','count_w_NA','p_w_NA') # renaming the columns
  x_df['Total',]<-c(NA, 
                    sum(x_table_no_NA),
                    sum(x_percentages_table_no_NA),
                    sum(x_table_with_NA),
                    sum(x_percentages_table_with_NA)) # adding a row of totals
  print(x_df)
  x_df<<-x_df
}

######################################################################################
#### plotting the distributions from the summary table without missing values ########
######################################################################################

summary_tables_and_plots_func<-function(dataset, column){
  table<-summary_table_func_for_serial_plotting(dataset,column)
  summary_plot_func(table)  
}

summary_tables_and_plots_func(pop,3)

##########################
#### survey stats ########
##########################

survey_stats_func<-function(survey_data, response_variables, response_labels, group_variables, group_names){
  
  # preparing the data frame
  # creating internal variables
  n_groups<-length(group_names)
  n_responses<-length(response_labels)
  
  survey_stats_df<-data.frame()
  
  # filling the data frame with values 
  for (j in 1: n_groups){
    i_lower_limit<-(j-1)*n_responses+1
    i_upper_limit<-j*n_responses
    for (i in (i_lower_limit:i_upper_limit)){
      index<-which(group_variables[,j]==1)
      survey_data$variables$groupvar<-NA
      survey_data$variables$groupvar[index]<-response_variables[i-(j-1)*n_responses][index,]
      survey_stats_df[i,1]<-group_names[j]
      survey_stats_df[i,2]<-response_labels[i-(j-1)*n_responses]
      survey_stats_df[i,3:4]<-as.data.frame(svymean(survey_data$variables$groupvar, survey_data, na.rm=TRUE))
      survey_stats_df[i,5]<-survey_stats_df[i,3]-qnorm(0.975)*survey_stats_df[i,4]
      survey_stats_df[i,6]<-survey_stats_df[i,3]+qnorm(0.975)*survey_stats_df[i,4]
      survey_stats_df[i,7]<-sum(group_variables[,j], na.rm=TRUE)
      n_group_with_comma<-format(survey_stats_df[i,7] ,big.mark=",",scientific=FALSE)
      survey_stats_df[i,8]<-paste(survey_stats_df[i,1], "\nn=", n_group_with_comma, sep="")
      
      names(survey_stats_df)<-c('group','response','mean','se','ci_lower_limit','ci_upper_limit','n_group', 'group_name_with_n')
    }
  }
  return(survey_stats_df)
}

#######################################################
### weighting and defining 'survey' datasets  #########
#######################################################

weighting_func<-function(pop_data,qname,indeps_equations){
  
  sink(paste(getwd(),'/log files/weighting_',qname,sep="")) # opening log file
  
  # creating a 'responded' variable
  questions<-pop_data[,grep(paste(qname,"_",sep=""), names(pop_data), value=TRUE)] # pulling the question variables
  place_numeric<-which(names(questions)==names(questions)[grepl("numeric", names(questions), fixed=TRUE)][1]) # finding the place of the first numeric question variable
  pop_data$responded<-0
  pop_data$responded[!is.na(questions[,place_numeric])]<-1 # creating a variable denoting whether the person responded to the question
  assign(paste("pop$responded_",qname,sep=""),pop_data$responded, envir= .GlobalEnv) # adding the 'responded' variable to the external population dataset
  
  # creating a variable denoting the number of regressions
  n_weighting_regressions<-length(indeps_equations)
  
  # running the models
  for(i in 1:n_weighting_regressions){
    assign(paste("equation_",i,sep=""),paste('"responded ~',indeps_equations[i],'"'))
    assign(paste("propensity_to_respond_",qname,"_",i,sep=""),glm(get(paste("equation_",i,sep="")), data=pop_data, na.action = "na.omit", family = "binomial" (link="logit")))
    assign(paste("summaries_",qname,"_",i,sep=""), summary(get(paste("propensity_to_respond_",qname,"_",i,sep=""))))
    assign(paste("AIC_",qname,"_",i,sep=""), AIC(get(paste("propensity_to_respond_",qname,"_",i,sep=""))), pos=1)
    print(get(paste("summaries_",qname,"_",i,sep="")))
  }
  
  
  # creating a vector with model names
  assign(paste("propensity_to_respond_",qname,"_models",sep=""),paste("propensity_to_respond_",qname,"_",seq(1:n_weighting_regressions), sep=""))
  
  # creating a vector with AICs
  assign(paste("AICs_",qname,sep=""),paste("AIC_",qname,"_",seq(1:n_weighting_regressions),sep=""))
  assign(paste("AICs_",qname,sep=""),sapply(get(paste("AICs_",qname,sep="")),get))
  
  # removing the AIC_ variables
  rm(list=((paste("AIC_",qname,"_",1:n_weighting_regressions,sep=""))), pos=1)
  
  # combining these vectors into a data frame
  assign(paste("models_and_AICs_",qname,sep=""),
         cbind.data.frame("model_number"=seq(1:n_weighting_regressions),
                          get(paste("propensity_to_respond_",qname,"_models",sep="")),
                          get(paste("AICs_",qname,sep=""))))
  
  get(paste("models_and_AICs_",qname,sep=""))
  # names(get(paste("models_and_AICs_",qname,sep="")))<-c("model_number","model_name","AIC")
  
  # choosing the model with the lowest AIC for weighting
  assign(paste("chosen_model_",qname,sep=""),as.character(get(paste("models_and_AICs_",qname,sep=""))[,2][which.min(get(paste("models_and_AICs_",qname,sep=""))[,3])]))
  assign(paste("chosen_model_number_",qname,sep=""),as.character(get(paste("models_and_AICs_",qname,sep=""))[,1][which.min(get(paste("models_and_AICs_",qname,sep=""))[,3])]))
  #get(paste("chosen_model_",qname,sep=""))
  #get(paste("chosen_model_number_",qname,sep=""))
  
  # obtaining the predicted variables for each respondent
  assign(paste("predicted_probability_to_respond_",qname,sep=""),predict(get(get(paste("chosen_model_",qname,sep=""))), type="response"))
  get(paste("predicted_probability_to_respond_",qname,sep=""))
  length(get(paste("predicted_probability_to_respond_",qname,sep="")))
  
  # creating a list of variables names for the analysis
  assign(paste("indeps_",qname,"_analysis",sep=""),get(paste("indeps_",get(paste("chosen_model_number_",qname,sep="")),sep=""))) # indeps
  get(paste("indeps_",qname,"_analysis",sep=""))
  
  # creating the preliminary dataset
  prelim_dat<-pop_data
  prelim_dat$number_missing_indeps <- rowSums(is.na(prelim_dat[,get(paste("indeps_",qname,"_analysis",sep=""))]))
  prelim_dat<-prelim_dat %>% filter(number_missing_indeps==0)
  dim(prelim_dat)
  
  # adding the predicted probability to respond
  prelim_dat$pptr<-get(paste("predicted_probability_to_respond_",qname,sep=""))
  
  # calculating the weight
  prelim_dat$weights_no_bins<-1/prelim_dat$pptr
  
  # dividing the weights variable into bins
  nbins<-10 # setting the number of bins
  prelim_dat$quantile_for_weighting<-ntile(prelim_dat$weights_no_bins, nbins)
  table(prelim_dat$quantile_for_weighting)
  
  prelim_dat<-prelim_dat %>%
    group_by(quantile_for_weighting) %>%
    mutate(weight_bin= mean(weights_no_bins))
  
  # View(Q1_df_for_weighting)[1:5,]
  table(prelim_dat$quantile_for_weighting,prelim_dat$weight_bin)
  
  survey_data<-prelim_dat %>% filter(responded==1 & !is.na(weight_bin))
  dim(survey_data)
  
  #no_weights<-rep(1,nrow(survey_data_Q1))
  # assigning the weights to the survey data
  N<-nrow(pop)
  # N
  survey_data<-svydesign( 
    id = ~0,
    data = survey_data,
    weights =~ survey_data$weight_bin)
  
  # survey_data_srvyr<- survey_data %>% as_survey_design(1, # using as_survey_design instead of svydesign so the srvry package will accept the survey object
  #   weight =~ survey_data$weight_bin)
  
  # exploring the data
  # class(survey_data)
  # names(survey_data)
  # dim(survey_data_Q1$variables) 
  # nrow(survey_data_Q1)
  # degf(survey_data_Q1) # degrees of freedom 
  
  # creating a weights variable in order to check that the sum of weights is about N
  # weights<-1/survey_data$prob
  # sum(weights)
  
  # creating a survey object outside of the function
  assign(paste("survey_data_",qname,sep=""),survey_data, envir= .GlobalEnv)
  
  # saving the survey object
  saveRDS(survey_data, paste(getwd(),"/backup/survey_data_",qname,".Rds",sep=""))
  
  sink() # closing the log file
  # while (!is.null(dev.list()))  dev.off() # fixing a bug
  # dev.set(dev.next()) # fixing a bug
  closeAllConnections() # fixing a bug
  
  print("finished, all is well")
}

#####################################################################################
####### plotting outcomes, one group at a time, based on the summary stats   ######## 
#####################################################################################

plot_bar_from_summary_stats_separate_groups_func<-function(question_name,full_question,survey_stats_df, group_variables, group_names){
  for(j in 1:length(group_names)){
    plot<-survey_stats_df %>%
      filter(group==group_names[j]) %>% 
      ggplot(aes(x = reorder(response, mean), y = mean)) + # setting x and y
      geom_bar(stat = "identity", fill="#054C70", position=position_dodge(1)) + # creating a bar plot
      coord_flip() + # rotating the plot 90 degrees
      xlab("") + # removing x label
      ylab(full_question) +  # setting the y label
      labs(title = survey_stats_df$group_name_with_n[length(unique(survey_stats_df$response))*(j-1)+1])+ # chart title 2
      scale_y_continuous(limits=c(0,0.75), breaks=seq(0, 0.7, by = 0.05), expand=c(0,0), 
                         labels=scales::percent_format(accuracy=1)) + # scaling the y axis
      geom_errorbar(aes(x=reorder(response, mean), ymin=ci_lower_limit, ymax=ci_upper_limit), # creating error bars 1
                    width=0.4, colour="black", alpha=0.5, size=2) + # creating error bars 2
      theme(plot.title = element_text(hjust = 0.5, vjust=0, size=35), # designing the plot title
            axis.title.y = element_text(size=25, colour="black"), # designing the y axis title
            axis.title.x = element_text(size=35, colour="black", margin = margin(t = 100, r = 20, b = 20, l = 20)), # designing the x axis title
            axis.text.x = element_text(size=25, colour="black"), # designing the x axis ticks
            axis.text.y = element_text(size=25, colour="black"), # designing the y axis tickss
            axis.ticks.y=element_blank(), # removing y axis ticks
            legend.position = NULL) # removing legend
    
    #saving the plot 
    png(paste(getwd(),'/plots/separate groups/',question_name,'_',group_names[j],'.png',sep=""), width = 2000, height = 1000)
    print(plot)
    dev.off()
  }
}


################################################################
#### bar plot within groups based on the summary stats ########
################################################################

plot_bar_from_summary_stats_within_groups_func<-function(question_name, full_question, survey_stats, response_labels, group_names, groups_for_comparison){
  # countiing the groups
  j<-length(unique(groups_for_comparison))
  
  # creating an index of rownumbers that will enter ggplot 
  index<-integer()
  for (i in 1:j){
    index<-c(index, which(survey_stats$group==groups_for_comparison[i]))
  }
  
  # preparing the variables for the plot
  survey_stats$group_name_with_n<-factor(survey_stats$group_name_with_n, levels=rev(unique(survey_stats$group_name_with_n))) # setting the order of the groups
  response_rank<-rank(survey_stats$mean[survey_stats$group=="All users"]) # setting the order of the responses
  response_rank<-rep(response_rank,j)
  
  plot<-survey_stats[index,] %>%
    ggplot(aes(x = group_name_with_n, y = mean, fill=reorder(response, response_rank))) + # setting x and y
    geom_bar(position="dodge", stat = "identity") + # creating a bar plot
    coord_flip() + # rotating the plot 90 degrees
    xlab("") + # removing x label
    ylab(full_question) + # creating y label
    labs(title = "") + # chart title
    guides(fill = guide_legend(reverse = TRUE, barheight = unit(1, "cm")))+ # reversing the order of the categories in the legend
    scale_y_continuous(limits=c(0,0.75), breaks=seq(0, 0.7, by = 0.1), expand=c(0,0),
                       labels=scales::percent_format(accuracy=1)) + # scaling the y axis
    geom_errorbar(aes(x=group_name_with_n, ymin=ci_lower_limit,ymax=ci_upper_limit), # creating error bars 1
                  width=0.4, colour="black", alpha=0.5, size=2, # creating error bars 2
                  position=position_dodge(.9))+ # creating error bars 3
    scale_fill_tableau(palette ="Color Blind")+ # setting the colors
    theme_minimal(base_size = 14) +
    theme (axis.title.y = element_text(size=25, colour="black"), # designing the y axis title
           axis.title.x = element_text(size=25, colour="black", margin=margin(t = 50, b = 4), vjust=2), # designing the x axis title
           axis.text.x = element_text(size=25, colour="black"), # designing the x axis ticks
           axis.text.y = element_text(size=25, colour="black"), # designing the y axis ticks
           legend.title = element_blank(), # removing the legend title
           legend.text=element_text(size=25, margin=margin(t = 5, r = 0, b = 0, l=0), vjust=0.5), # setting the font size in the legend
           axis.ticks.y=element_blank(), # removing y axis ticks
           legend.key = element_rect(size = unit(10, "lines")),
           legend.key.size = unit(2, "lines"),
           legend.key.height = unit(4, "lines"))
  
  #saving the plot 
  png(paste(getwd(),'/plots/group comparisons/within/',question_name,'_within','.png',sep=""), width = 1700, height = 1000)
  print(plot)
  dev.off()
}

################################################################
#### bar plot between groups based on the summary stats ########
################################################################

plot_bar_from_summary_stats_between_groups_func<-function(question_name, full_question, survey_stats, response_labels, group_names, groups_for_comparison){
  # countiing the groups
  j<-length(unique(groups_for_comparison))
  
  # creating an index of rownumbers that will enter ggplot 
  index<-integer()
  for (i in 1:j){
    index<-c(index, which(survey_stats$group==groups_for_comparison[i]))
  }
  
  # preparing the variables for the plot
  survey_stats$group_name_with_n<-factor(survey_stats$group_name_with_n, levels=rev(unique(survey_stats$group_name_with_n))) # setting the order of the groups
  response_rank<-rank(survey_stats$mean[survey_stats$group=="All users"]) # setting the order of the responses 1
  response_rank<-rep(response_rank,j) # setting the order of responses 2
  
  plot<-survey_stats[index,] %>%
    ggplot(aes(x = reorder(response, response_rank), y = mean, fill=group_name_with_n)) + # setting x and y
    geom_bar(position="dodge", stat = "identity") + # creating a bar plot
    coord_flip() + # rotating the plot 90 degrees
    xlab("") + # removing x label
    ylab(full_question) + # creating y label
    labs(title = "") + # chart title
    guides(fill = guide_legend(reverse = TRUE, barheight = unit(1, "cm")))+ # reversing the order of the categories in the legend
    scale_y_continuous(limits=c(0,0.8), breaks=seq(0, 0.8, by = 0.1), expand=c(0,0),
                       labels=scales::percent_format(accuracy=1)) + # scaling the y axis
    geom_errorbar(aes(x=reorder(response, response_rank), # creating error bars 1
                      ymin=ci_lower_limit, ymax=ci_upper_limit), # creating error bars 2
                  width=0.4, colour="black", alpha=0.5, size=2, # creating error bars 3
                  position=position_dodge(.9))+ # creating error bars 4
    scale_fill_tableau(palette ="Color Blind")+ # setting the colors
    theme_minimal(base_size = 14) +
    theme (
      axis.title.y = element_text(size=25, colour="black"), # designing the y axis title
      axis.title.x = element_text(size=25, colour="black", margin=margin(t = 50, b = 4), vjust=2), # designing the x axis title
      axis.text.x = element_text(size=25, colour="black"), # designing the x axis ticks
      axis.text.y = element_text(size=25, colour="black"), # designing the y axis ticks
      legend.title = element_blank(), # removing the legend title
      legend.text=element_text(size=25, margin=margin(t = 5, r = 0, b = 0, l=0), vjust=0.5), # setting the font size in the legend
      axis.ticks.y=element_blank(), # removing y axis ticks
      legend.key = element_rect(size = unit(10, "lines")),
      legend.key.size = unit(2, "lines"),
      legend.key.height = unit(6, "lines"))
  
  #saving the plot   
  png(paste(getwd(),'/plots/group comparisons/between/',question_name,'_between','.png',sep=""), width = 1700, height = 1000)
  print(plot)
  dev.off()
}

#################################################
###### regression equation function   ###########
#################################################

regression_equation_func<-function(depvar,indeps){
  indeps_with_plus_signs<-paste(unlist(indeps), collapse=' + ')
  equation<-paste(depvar, '~',indeps_with_plus_signs)
}

# a function for running multiple regressions, with multiple dependent variables and 
# multiple sets of indpendent variables 

multiple_regression_func<-function(qname,deps,indeps, survey_data){
  sink(paste(getwd(),'/log files/multiple_regressions_',qname,sep=""))# opening log file
  
  for (j in 1:length(deps)){
    for (i in 1:length(indeps)){
      print(paste("The dependent variable is",deps[j]))
      regression_equation<-regression_equation_func(deps[j],indeps[i])
      print(summary(svyglm(regression_equation, design=survey_data, na.action = "na.omit", family = "quasibinomial" (link="logit"))) )
    }
  }
  print ("finished, all is well")
  sink()
}

# saving variables separately 
saving_variables_func<-function(){
  
  # making a list of all variables
  variable_list_func <- function(env=.GlobalEnv){ unlist(lapply(ls(env=env),
                                                                function(x) if(!is.function(get(x)))x))}
  
  variable_list<<-variable_list_func()
  
  for (j in 1:(ceiling(length(variable_list)/5))){
    partial_variable_list<-NA
    partial_variable_list<-variable_list[((j-1)*5+1):(j*5)]
    partial_variable_list<-partial_variable_list[!is.na(partial_variable_list)]
    
    #partial_variable_list
    for(i in 1:length(partial_variable_list)){
      save(list = (partial_variable_list[i]),
           file = paste(paste(getwd(),"/backup_data/",sep=""),partial_variable_list[i], ".RData", sep = ""))
    }
  }
}
saving_variables_func()
