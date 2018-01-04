# Complete all of the items below
# Use comments where you're having trouble or questions

# 1. Read your data set into R

rm(list=objects())
install.packages("readxl")
library(readxl) #to read excel file 
??read_excel
event_data <- read_excel("/Users/rithvikyarlagadda/Documents/R_GVPT622/PSYC798/my_class_stuff/day02/event_data.xlsx")[,c("year","type_of_violence","conflict_name","dyad_name","where_coordinates", "country","best")] #subsetting 7 out of 42 variables relevant to my analysis and creating a new dataframe 

# 2. Peek at the top few rows

head(event_data)

# 3. Peek at the top few rows for only a few columns

?head

head(event_data[,c(1,2,5,7)]) #first 6 rows of columns 1,2,5,7

# 4. How many rows does your data have?

nrow(event_data) #number of rows in the dataset 

print("the number of rows in the dataset are: 135181")

# 5. Get a summary for every column

summary(event_data) #all columns

# 6. Get a summary for one column

summary(event_data$best) #one column named 'best'

# 7. Are any of the columns giving you unexpected values?

#Yes, the variable in column 2 should actually have data that is of factor type, but its summary values indicate that the data is numeric. 

# 8. Select a few key columns, make a vector of the column names

key.colnames<-c("year", "type_of_violence", "where_coordinates","best") #creating an object with names of selected columns from the dataframe

# 9. Create a new data.frame with just that subset of columns

new.event_data<-event_data[,c(1,2,5,7)] #creating a new data frame with only a subset of columns from the original dataframe


# 10. Create a new data.frame that is just the first 10 rows
#     and the last 10 rows of the data from the previous step

first_last_ten<-new.event_data[c(1:10, 135172:135181),] #new dataframe with only the first and last 10 observations or rows obtained from the new.event_data 

# 11. Create a new data.frame that is a random sample of half of the rows.
# HINT: ?sample

?sample

x<-1:20

random_dataframe<-first_last_ten[sample(x,10),] #creating a new dataframe with 10 randomly selected rows from the first_last_ten dataframe

# 12. Find a comparison in your data that is interesting to make
#     (comparing two sets of numbers)
#     - run a t.test for that comparison

result1<-t.test(event_data$type_of_violence, event_data$best) #vector type t-test

#     - decide whether you need a non-default test
#       (e.g., Student's, paired)

#Student's t-test not required. 
#Since my sample size (event_data dataset) is much greater than 1000, I would not be required to use Student's t-test

result2<-t.test(event_data$type_of_violence, event_data$best, paired = T)

#Paired t-test may be required over here as it is different from the default test in outputting the type of sample estimates. 
#Here, the sample estimate is the mean of differences among each pair of sample observations while in default t-test, it gives the difference in means.  


#     - run the t.test with BOTH the formula and "vector"
#       formats, if possible

t.test(event_data$best ~ event_data$type_of_violence) #formula type t-test gives an error

#     - if one is NOT possible, say why you can't do it

#The 'formula' type t-test is not possible in this case because rhs is a factor variable with 4 levels, but this method only allows for 2 levels

# 13. Repeat #12 for TWO more comparisons
#     - ALTERNATIVELY, if correlations are more interesting,
#       do those instead of t-tests (and try both Spearman and
#       Pearson correlations)
#     - Tip: it's okay if the comparisons are kind of nonsensical, this is 
#       just a programming exercise

#correlation 1 - between 'type_of_violence' and 'best'
result3<-cor(event_data$type_of_violence, event_data$best, method="pearson") 
print("the pearson correlation coefficient is: 0.007574376")
result4<-cor(event_data$type_of_violence, event_data$best, method="spearman")
print("the spearman correlation coefficient is: -0.1019095")

#correlation 2 - between 'year' and 'best'
result5<-cor(event_data$year, event_data$best, method="pearson")
print("the pearson correlation coefficient is: -0.0122452")
result6<-cor(event_data$year, event_data$best, method="spearman")
print("the spearman correlation coefficient is: 0.007969258")

# 14. Save all results from #12 and #13 in an .RData file

save(result1, result2, result3, result4, result5, result6, file = "homework2.RData")

# 15. Email me your version of this script, PLUS the .RData
#     file from #14
#     - ALTERNATIVELY, push your version of this script and your .RData results
#       to a repo on GitHub, and send me the link


