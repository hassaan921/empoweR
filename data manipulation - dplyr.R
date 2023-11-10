# In preparation for research, it is important to have our data ready in a 
# tidy form. By definition, each column should be 1 variable, and each
# row should show one observation for each variable.

# install.packages('tidyverse')
library(tidyverse)


######## sample data for practice ######## 
# tibble is used to print data frames which can contain 
# more than one type of data (chr,dbl,int,fct etc.)
scores <- tibble(
  name = c("Ali","Amna","Ahmed","Umair","Seher","Hassaan","Aoun"), # strings go into ""
  class = c("IR","PS","IR","IR","PS","IR","PS"),
  grade = c("A","B","F","B","A","C","A"), 
  # c() is a one dimensional vector that can take only one type of data 
  
  # ^ comments are non-executable and can (read: should) be put in code to clarify for readers
  marks = c(95,79,52,78,91,67,89), # numbers need not go into ""
  percentage = c(95,79,52,78,91,67,89), 
  Status = c("PASS","PASS","FAIL","PASS","PASS","FAIL","PASS")
)

########  pipe operator '%>%' or '||>' (ctrl+shift+m) ######## 

scores %>%
  slice(1:3) %>% view()



# five verbs to manipulate data with tidyverse package

######## no. 1 = arrange ########  

#arrange in ascending order (by default, ascending) wrt marks
scores %>% arrange(marks)

#arrange in descending wrt to marks
scores %>% arrange(desc(marks))

#nominal/ordinal data is arranged wrt alphabetic order/ordinal order
scores %>% arrange(grade)

scores %>% arrange(Status)

scores %>% arrange(name)



########   no. 2 = select ########  

# used to load specific column(s)

scores %>% 
  select(name)

scores %>% 
  select(name,grade,marks)

# selecting all columns _except_ certain column(s)
scores %>% 
  select(-class, -Status)

scores %>% 
  select(-marks,-grade)
 
# preferably showing one variable column over other columns
scores %>% 
  select(Status,everything())

scores %>% 
  select(name,Status,everything())

# renaming a column in tibble 
scores_new <- scores %>% # give new name if original data is to be kept intact
  rename(status = Status) # new column = old column. Be mindful of capitalization, special characters

print(scores_new)

# if more than one columns have unpleasing names (e.g., capital beginnigs, 
# spaces between words etc.), use clean_names() from package "janitor" to fix e.g.,
library(janitor)
scores %>% 
  clean_names() # i did not give it a name so data intact


########  no. 3 = FILTER ########  

# used to select row(s) that meets certain conditions

# filter IR students
scores_new %>% filter(class=="IR")

# filter students who failed
scores_new %>% filter(status=="FAIL")

# filter students who got B
scores_new %>% filter(grade=="B")

# filter students who got >= 80
scores_new %>% filter(marks >= 80)

# filter students who are from IR and have a C grade
scores_new %>% filter(class == "IR" & grade == "C")

#filter students who have percentage > 90 %
scores_new %>% filter(percentage>=90)

# concatenate using filter function
scores_new %>% filter(marks %in% c(95,78,89))

# filter rows which are not equal to i.e., !(no_space)=
scores_new %>% filter(status != "PASS") 

# filtering in groups of specific characteristics in certain rows
scores_new %>% 
  group_by(class) %>% 
  summarize(max(marks) == 95)

###### no. 4 = mutate ###### 
# used to add, make changes in columns
scores_new %>% 
  mutate(attendance = c(81,89,59,75,83,72,85))

scores_new  %>% 
  mutate(final_marks = (marks + percentage) /5)

scores_new  %>% 
  mutate(high_achievers = (marks >= 85))

scores_new  %>% 
  mutate(scholarship = (marks) >= 85)

# glimpse to see all rows and columns 
scores_new  %>% glimpse()

###### no. 5 = summarize ######

# to get descriptive statistics

scores_new  %>% 
  summarize(mean_marks = mean(marks))

scores_new  %>% 
  summarize(min_marks = min(marks), max_marks = max(marks))

scores_new  %>% 
  group_by(class) %>% 
  summarize(mean_marks = mean(marks))

scores_new  %>% 
  group_by(grade) %>% 
  summarize(median_class = median(marks))
