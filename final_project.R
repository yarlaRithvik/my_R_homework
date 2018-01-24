rm(list = objects(all=TRUE))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


#splitting my data frame so that there are 8 relevant columns  in the new dataframe
my.project.data<-read_excel("/Users/rithvikyarlagadda/Documents/R_GVPT622/PSYC798/my_R_homework/day3/event_data.xlsx")[,c("year","type_of_violence","dyad_name","where_coordinates", "adm_1", "adm_2","country", "best")] 

names(my.project.data)[names(my.project.data) == "best"]<-"fatalities" #renaming one of the variables

my.project.data$event.count<-rep(1, length(my.project.data$year)) # creating a new column to indicate the count of events

head(my.project.data)
summary(my.project.data)

names(my.project.data)

# type_of_violence is expected to be a factor variable, but it is coded as a numerical variable in my data. 
# So, it has to be transformed into factor type before conducting my further analyses. 

my.project.data$type_of_violence.factor<-factor(my.project.data$type_of_violence, levels = c(1,2,3), labels = c("state-based conflict", "non-state conflict", "one-sided violence")) 
#relabelling the names of the factor levels

unique(my.project.data$country) # to see how many individual countries are present in my data



#summarizing my dataset in order to conduct different sets of analyses

violence_type.summaries <- summarize(group_by(my.project.data, type_of_violence.factor, country), count.fatalities = sum(fatalities, na.rm=TRUE), mean.fatalities = mean(fatalities, na.rm = TRUE), median.fatalities = median(fatalities, na.rm = TRUE), maximum.fatalities = max(fatalities, na.rm = TRUE))

violence_type.summaries$violence.type.num<-as.numeric(violence_type.summaries$type_of_violence.factor) # creating a numeric version of the factor type variable



density.fatalities.transformed<-ggplot(violence_type.summaries, aes(x = count.fatalities)) + geom_density(adjust = .5, fill = "grey")

density.fatalities1.transformed<-ggplot(violence_type.summaries, aes(x = count.fatalities)) + geom_density(adjust = .25, fill = "grey")

# the above two density curves show that the variable is not normal, hence we need to make some transformations 

# The 'count.fatalities' variable is log-transformed so as to approximate it to a normal curve

density.transformed_fatalities<-ggplot(data = violence_type.summaries, aes(log(count.fatalities))) + geom_density(adjust = 0.5, fill = "grey")



# Visualizations: Plot it as a density curve, and mapping the plots to the factor variable using three different techniques


# 1. mapping color to type of violence factor variable 
fatalities.density.violencetype1<-ggplot(data = violence_type.summaries, aes(log(count.fatalities))) + geom_density(aes(color = type_of_violence.factor))

# 2. mapping fill to type of violence factor variable 
fatalities.density.violencetype2<-ggplot(data = violence_type.summaries, aes(log(count.fatalities))) + geom_density(aes(fill = type_of_violence.factor))

# 3. mapping line to type of violence factor variable 
fatalities.density.violencetype3<-ggplot(data = violence_type.summaries, aes(log(count.fatalities))) + geom_density(aes(linetype = type_of_violence.factor))

# 4. Analyzing the presence (or absence) of outliers using boxplots for each level of violence type
box.plot<-ggplot(violence_type.summaries, aes(type_of_violence.factor, log(count.fatalities))) + geom_boxplot()


# bar plots using grouping factor

mean.fatal_state_based<-mean(violence_type.summaries$count.fatalities[violence_type.summaries$type_of_violence.factor=="state-based conflict"])

mean.fatal_non_state<-mean(violence_type.summaries$count.fatalities[violence_type.summaries$type_of_violence.factor=="non-state conflict"])

mean.fatal_one_sided<-mean(violence_type.summaries$count.fatalities[violence_type.summaries$type_of_violence.factor=="one-sided violence"])

barplot.data<-data.frame(violence_type = c("state-based conflict", "non-state conflict", "one-sided violence"), mean.fatalities = c(mean.fatal_state_based, mean.fatal_non_state, mean.fatal_one_sided))

barplot_new.dataframe<-ggplot(data = barplot.data, aes(x = violence_type, y = mean.fatalities)) + geom_bar(stat = "identity", aes(fill = violence_type)) #stat = identity means that telling R to plot the numbers as they are, don't do anything else


# OLS or linear regression analysis to see if the type of violence influences the intensitiy of events?


regression.results.linear<-lm(fatalities ~ type_of_violence, data = my.project.data)

reg.summary<-summary(regression.results.linear)

str(regression.results.linear)

str(reg.summary)


#The regression analysis values are:

reg.estimate<-reg.summary$coefficients["type_of_violence", "Estimate"]
reg.se<-reg.summary$coefficients["type_of_violence", "Std. Error"]
reg.pval<-reg.summary$coefficients["type_of_violence", "Pr(>|t|)"]
reg.tval<-reg.summary$coefficients["type_of_violence", "t value"]
reg.adjR<-reg.summary$adj.r.squared


# the p-value is less than 0.05 thus indicating that the type of violence seen within each event has a 
# positive and significant impact on the intensity of violence per event measured in terms of event fatalities 


# Simulation part:


# Most excel and most point click statistical softwares have the sort function where we can 
# arrange the numerical values either in increasing or decreasing orders. 

# However, these sort options need to be applied through the entire numerical column. 
# But, these traditional sort functions may not be handy in a case where we have different sets of numerical values associated with different levels of a factor variable 
# and we wish to arrange numerical values within each level or category of the factor variable in an increasing or decreasing order. 

# Also,the 'min' and 'max' functions in R can only give us partial information about the data and we may wish to have our data arranged in a specific order of values (increasing or decreasing)
# This is true for numerical values that are either grouped using one variable or two variables. 
# This can be particularly useful in conflict event datasets (or even other large datasets) where event fatalities within each country or specific year can be sorted in an increasing or 
# In my analysis, I came up with a simulation-based analysis that uses 'for-loops' to carry out increasing or decreasing types of sorting where numerical values are grouped using one variable or two variables. 


# For example, I wish to have my dataset summarized and sorted in two ways:

# 1A. First, to summarize my dataset in a way that would output the fatalities within each year. 
# 1B: Next, sort the fatality values in such a way so we can have fatalities per year arranged separately in an increasing or decreasing manner. 
# This outut would loo

# 2A: Repeat the same process as above but here, the data is summarized using two variables. In other words, to summarize my data that would result in fatalities within each dyad_name for different years. 
# 2B: Next, sort that summarized data in a way so we can have different fatalities within each year arranged separately in an increasing or decreasing manner. 

# This process can be repeated for any combination of variables. In my analysis, I restricted my simulation to only one or two variable summaries. 


# 1A - Summarizing my dataset using one variable (for e.g., 'year')

summary.output <- as.data.frame(summarize(group_by_(my.project.data, "year"), 
                                           count.fatalities = sum(fatalities, na.rm=TRUE), count.event = sum(event.count, na.rm = TRUE)))

# 1B.1 - Sort fatalities for all years using increasing order 

for(q in 1:nrow(summary.output)){
  
  for(r in 1:(nrow(summary.output)-1)){
    
    fatalities.check<-summary.output$count.fatalities #the count fatalities can be replaced with any other numerical variable (e.g., event count)
    this.names<-summary.output$year # the 'year' variable can be replaced with any other variable in the dataset
    
    
    if(fatalities.check[r] >= fatalities.check[r+1]){
      
      num1<-as.numeric(rownames(as.data.frame(summary.output[summary.output$year==this.names[r],])))
      num2<-as.numeric(rownames(as.data.frame(summary.output[summary.output$year==this.names[r+1],])))
      
      storage.value<-summary.output[num1,]
      
      summary.output[num1,]<-summary.output[num2,]
      
      summary.output[num2,]<-storage.value
      
    } else{
      
      # do nothing
    }
    
  }
}


# 1B.2 - Sort fatalities for all years using decreasing order 

for(q in 1:nrow(summary.output)){
  
  for(r in 1:(nrow(summary.output)-1)){
    
    fatalities.check<-summary.output$count.fatalities
    this.names<-summary.output$year 
    
    
    if(fatalities.check[r] <= fatalities.check[r+1]){
      
      num1<-as.numeric(rownames(as.data.frame(summary.output[summary.output$year==this.names[r],])))
      num2<-as.numeric(rownames(as.data.frame(summary.output[summary.output$year==this.names[r+1],])))
      
      storage.value<-summary.output[num1,]
      
      summary.output[num1,]<-summary.output[num2,]
      
      summary.output[num2,]<-storage.value
      
    } else{
      
      # do nothing
    }
    
  }
}

# 2A - Summarizing dataset using two variables (e.g., 'year' and 'dyad_name')

summary.output1 <- as.data.frame(summarize(group_by_(my.project.data, "year", "dyad_name"), 
                                          count.fatalities = sum(fatalities, na.rm=TRUE), count.event = sum(event.count, na.rm = TRUE)))

# the two variables can be replaced with any other variables in my dataset like 'country' or/and 'type_of_violence'

# 2B.1 - Sort fatalities for different levels of 'dyad_names' that are arranged separately within each year in an increasing order

for(i in unique(summary.output1$year)){
  
  for(z in 1:nrow(summary.output1[summary.output1$year==i,])){
    
    
    for(k in 1:(nrow(summary.output1[summary.output1$year==i,])-1)){
      
      fatalities.check1<-summary.output1$count.fatalities[summary.output1$year==i]
      this.names1<-summary.output1$dyad_name[summary.output1$year==i]
      
      
      if((fatalities.check1[k]) >= (fatalities.check1[k+1])){
        
        num3<-as.numeric(rownames(as.data.frame(summary.output1[(summary.output1$dyad_name==this.names1[k])&(summary.output1$year==i),])))
        num4<-as.numeric(rownames(as.data.frame(summary.output1[(summary.output1$dyad_name==this.names1[k+1])&(summary.output1$year==i),])))
        
        storage.value1<-summary.output1[num3,]
        
        summary.output1[num3,]<-summary.output1[num4,]
        
        summary.output1[num4,]<-storage.value1
        
      } else{
        
        # do nothing
      }
      
    }
    
  }
  
}


# 2B.2 - Sort fatalities for different levels of 'dyad_names' that are arranged separately within each year in an decreasing order

for(i in unique(summary.output1$year)){
  
  for(z in 1:nrow(summary.output1[summary.output1$year==i,])){
    
    
    for(k in 1:(nrow(summary.output1[summary.output1$year==i,])-1)){
      
      fatalities.check1<-summary.output1$count.fatalities[summary.output1$year==i]
      this.names1<-summary.output1$dyad_name[summary.output1$year==i]
      
      
      if((fatalities.check1[k]) <= (fatalities.check1[k+1])){
        
        num3<-as.numeric(rownames(as.data.frame(summary.output1[(summary.output1$dyad_name==this.names1[k])&(summary.output1$year==i),])))
        num4<-as.numeric(rownames(as.data.frame(summary.output1[(summary.output1$dyad_name==this.names1[k+1])&(summary.output1$year==i),])))
        
        storage.value1<-summary.output1[num3,]
        
        summary.output1[num3,]<-summary.output1[num4,]
        
        summary.output1[num4,]<-storage.value1
        
      } else{
        
        #skip this
      }
      
    }
    
  }
  
}

# Note: I initially planned to implement a single function to do both increasing and decreasing functions for one/two variable summaries. 
# Although the function was implemented without any errors, the function did not sort the values completely as they were supposed to do.
# Hence, I dropped that idea and implemented a simulation using just for-loops (which I know may take time to run especially for large datasets)
# But, I defintely would like to work on that function at a later point. 

# I will get in touch with you to see what is going wrong with my function code
