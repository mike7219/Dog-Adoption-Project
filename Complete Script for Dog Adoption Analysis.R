#Loading Packages to Tidying, Analysis, plotting, and Modeling 
library(tidyr)
library(dbplyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(fastDummies)
library(DataExplorer)
library(janitor)
library(fastDummies)
library(corrplot)
library(readr)


df <- read_csv("G://Adopted Animal Data Set.csv")

### Data Wrangling 
##
#
summary(df)

df <- df %>% 
  clean_names()
names(df)



df <- as.tibble(df)

#Filtering data set to only look at dog adoption
dogs <- df %>%
  filter(type == "DOG")

#This changes the observation total from 18732 to 10500

introduce(dogs)

#Most are discrete variables, only a few continuous variables

plot_missing(dogs)

# date_of_birth has by far the most missing values

#There is only 1 observation that has a count that is different than 1,
#this is probably an error, I am removing this column, it does not porvide any useful information
dogs %>%
  filter(count != 1)


dogs$count <- NULL

dogs %>%
  count(dogs$outcome_zip_code)


#There are a very high number of distinct values. 
dogs %>%
  n_distinct("outcome_zip_code")

# Also removing location, impound number, kennel number, animal ID, Outcome Subtype, 
# ,name to work with a more concise data set. Got rid of Outcome Zip because it has very high distinct values


dogs <- dogs %>%
  select(-c(location, impound_number, kennel_number, animal_id, outcome_zip_code))




#There are that come from many different counties to adopt I am going to Change 
# Outcome.Jurisdiction to logical , True False 
dogs %>%
  count(outcome_jurisdiction == "OUT OF COUNTY")

dogs %>%
  count(n_distinct((outcome_jurisdiction)))

str(dogs$outcome_jurisdiction)

# Subbing NA in out_of_county to FALSE because 85% of outcome_jurisdictions are in county

dogs$out_of_county[is.na(dogs$out_of_county)] <- FALSE

dogs <- dogs %>%
  mutate(out_of_county = as.logical(outcome_jurisdiction == "OUT OF COUNTY"))



#Searching for Missing Data
str(dogs)
summary(dogs)

plot_missing(dogs)

# No missing data in breed , color, sex, intake_date, days_in_shelter, intake_type, intake_subtype, intake_condition, 
# intake_jurisdiction




#Converting these now to POSIX time objects
dogs$date_of_birth <- parse_date_time2(dogs$date_of_birth, order = "mdY")
dogs$intake_date <- parse_date_time2(dogs$intake_date, order = "mdY")
dogs$outcome_date <- parse_date_time2(dogs$outcome_date, order = "mdY")

summary(dogs$date_of_birth)

# Over 1700 Missing Entires from date_of_birth and max date shows in the future, Corrected for the "future dates" 

dogs$date_of_birth[dogs$date_of_birth > Sys.Date()] <- NA

dogs$date_of_birth[is.na(dogs$date_of_birth)] <- mean(dogs$date_of_birth, na.rm = TRUE)

summary(dogs$date_of_birth)

#Creating AGE variable for the Age of the dog when arriving at the Animal Shelter
dogs <- dogs %>%
  mutate(age = (intake_date - date_of_birth)/ (86400*365))

dogs$age <- as.character(dogs$age)
dogs$age <- as.numeric(dogs$age)

str(dogs)

dogs <- dogs %>%
  rename(age_years = age)


dogs$age_years[is.na(dogs$age_years)] <- mean(dogs$age_years, na.rm = TRUE)


plot_missing(dogs)


dogs <- dogs%>% 
  filter(!is.na(outcome_type))

dogs <- dogs%>% 
  filter(!is.na(placed))


# The highest missing values are now Name, outcome_jurisdiction, out_of_county which all are less than 10 percent


#Splitting sex into two  columns, sex and is.fixed
dogs$sex <- as.factor(dogs$sex)

dogs <- dogs %>%
  mutate(fixed.true = as.logical(sex == c("Spayed", "Neutered")))

#Fixing Sex to have only Sex and not Spayed or Neutered

dogs$sex[dogs$sex == "Spayed"] <- "Female"
dogs$sex[dogs$sex == "Neutered"] <- "Male"



#Detecting Outliers 
?boxplot
boxplot(dog_dataset_refined$Days.in.Shelter ~ dog_dataset_refined$Size)
 #1 Outlier in LArge Dogs, Spent a log time in shelter

boxplot(dog_dataset_refined$Days.in.Shelter ~ dog_dataset_refined$Age.Years)


#Fixing Dog Breed & COlor 
#Breed
# Created Dummy Variable on whether or not it is an intimidating Breed of dog 

dogs %>%
  group_by(breed) %>%
  count() %>%
  filter(breed == "PIT BULL")



breed_pattern <- "PIT BULL+|AKITA+|ROTTWEILER+|DOBERMAN+|GERM SHEPHERD+|PYRENEES+|GREAT DANE+" 

detection <- str_detect(dogs$breed, breed_pattern)


# Created logical type if it is an "Intimidating Breed" or not because there are too many breeds. 
dogs$intimidating_breed <- as.logical(detection)

dogs <- dogs %>%
  dummy_cols(select_columns = "intimidating_breed", remove_first_dummy = TRUE)

count(adoption_df, Intimidating_Breed_TRUE)

dogs$breed <- NULL

# Color 

n_distinct(adoption_df$Primary_Color)

dogs <- dogs %>%
  separate(color, into = c("Primary Color", "Secondary COlor"), sep = "\\/", remove = TRUE )

colnames(dogs)[colnames(dogs) == "Primary Color"] <- "primary_color"
colnames(dogs)[colnames(dogs) == "Secondary COlor"] <- "secondary_color"

n_distinct(dogs$primary_color)

# There are dozens of unique primary_colors 
dogs %>%
  group_by(primary_color) %>%
  count() %>%
  arrange(n) 

# Removing primary_color and secondary_color due to large amount of factors and this is highly related to breed

dogs$primary_color <- NULL
dogs$secondary_color <- NULL


#Removing columns name, type, breed because they do not provide any useful information
dogs$name <- NULL
dogs$type <- NULL
dogs$breed <- NULL

# removing intake_subtype & Outcome Sub type due to so many distinct factors
dogs$intake_subtype <- NULL
dogs$outcome_subtype <- NULL

dogs$out_of_county_FALSE <- NULL
dogs$outcome_jurisdiction <- NULL
dogs$outcome_condition <- NULL



#Creating the Dependent Variable placed. 
# This variable is to indicate whether or not a dog got to a home

dogs$placed <- ifelse((dogs$outcome_type == "ADOPTION" | dogs$outcome_type == "RETURN TO OWNER"), 1, 0)



###   Exploratory Analysis
##
#

plot_correlation(na.omit(dogs), maxcat = 2)

# Some notable correlations to the dependent variable are days_in_shelter, out_of_county, fixed.true

plot_bar(dogs, with = 'days_in_shelter', nrow = 2, ncol = 2)

# Most animals brought in are healthy, male, Medium - Large, Stray, and are not of an intimidating breed 


plot_histogram(dogs)
# The most common age looks to be between 4-5 years old 


# What does the outcome_type distribution look like? 
outcome <- dogs %>%
  group_by(outcome_type) %>%
  summarise(animal_count = n()) 
  

ggplot(outcome, aes(x=outcome_type, y=animal_count, fill=outcome_type)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y = "Percentage of Animals",
       x = "",
       title = "Outcomes for Dogs") +
  scale_fill_brewer(palette="Set3") 

#Most common outcomes are returned to owner, or adoption 


outcome_dog <- dogs %>%
  group_by(intake_type, outcome_type) %>%
  summarise(animal_count = n())

ggplot(outcome_dog, aes(x=intake_type, y=animal_count, fill=outcome_type)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  labs(y = "Percentage of Animals",
       x = "",
       title = "Intake Type by Outcomes") +
  scale_fill_brewer(palette="Set3") 

#Most confiscated dogs are returned to their owners. Most adoption returns are eventually
# adopted again. Strays are most commonly returned to their owner, or adopted. 

# Are healthy dogs most likely to be adopted? 

dogs %>%
  group_by(intake_condition, outcome_type) %>%
  summarise(animal_count = n()) %>%
  ggplot(aes(x = outcome_type, y = animal_count, fill = intake_condition)) +
  geom_bar(stat="identity") +
  labs(y = "Number of Animals",
       x = "",
       title = "Outcomes by Intake Condition") +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(size = 7, angle = 90))

#Adoption has the highest percentage of Healthy. 

dogs %>%
  filter(outcome_type=="EUTHANIZE") %>%
  ggplot(aes(x = factor(days_in_shelter), fill=intake_condition)) +
  geom_bar() +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  labs(y = "Number of Animals",
       x = "Days in Shelter",
       title = "Euthanized Dogs by Intake Condition") 
  
#As expected most euthanized dogs are brought in as "untreatable" 
# but there is still a large amount of dogs that are brought in healthy but are still euthanized


prop.table(table(dogs$outcome_type, dogs$fixed.true), margin = 2)

#Dogs that are fixed have a higher proportion of adoptions, but are less likely to be returned to owner.
# Dogs that are fixed are also less likely to die or be euthanized. 


#How long does it take for dogs to be returned to their owners?

dogs %>%
  filter(outcome_type=="RETURN TO OWNER") %>%
  ggplot(aes(factor(days_in_shelter))) +
  geom_bar(fill="#3399CC")



owner_returned <- dogs %>%
  filter(outcome_type == "RETURN TO OWNER" )


str(owner_returned)

#Vast Majority Spend 0-1 days in a shelter before they are Returned to the owner 

mean(owner_returned$days_in_shelter)
median(owner_returned$days_in_shelter)

# Due to some outliers the mean is 2.85 Days in Shelter for Dogs who were eventually returned
# but the median is 1 day 

# What is the mean days in shelter for adopted dogs 

dogs %>%
  filter(outcome_type=="ADOPTION") %>%
  summarise(avg_adoption_time = mean(days_in_shelter))

# 34.2 Days 

dogs %>%
  filter(outcome_type=="ADOPTION") %>%
  ggplot(aes(x = days_in_shelter)) +
  geom_histogram(binwidth = 5, fill="ORANGE")

#There are some outliers so I am going to narrow the scope 

dogs %>%
  filter(outcome_type=="ADOPTION" & days_in_shelter<=100) %>%
  ggplot(aes(x = days_in_shelter)) +
  geom_histogram(binwidth = 1, fill="ORANGE")

dogs %>%
  filter(outcome_type=="ADOPTION" & days_in_shelter<=100) %>%
  summarise(avg_adoption_time = mean(days_in_shelter))

dogs %>%
  filter(outcome_type=="ADOPTION" & days_in_shelter<=100) %>%
  summarise(avg_adoption_time = median(days_in_shelter))

# The median days in shelter for adopted dogs in 20 but the mean is 26.4

# Are intimidating breeds less adopted?

dogs %>% filter(outcome_type == 'ADOPTION') %>%
  count(intimidating_breed_TRUE) %>% arrange(desc(n))

prop.table(table(dogs$outcome_type, dogs$intimidating_breed_TRUE), margin = 2)

dogs %>%
  filter(intimidating_breed =="TRUE" ) %>%
  summarise(avg_adoption_time = mean(days_in_shelter))


dogs %>%
  filter(intimidating_breed == "FALSE" ) %>%
  summarise(avg_adoption_time = mean(days_in_shelter))

# Intimidating breeds are Adopted less and Returned to owner less than non-intimidating breeds
# Intimidating breeds are also more likely to be euthanized. The average adoption time 
# for Intimidating breeds is 21.5 days while non-intimidating breeds have an average adoption time 
# of 13.7 days


### Machine Learning
## 
#

# Logsitic Regression 
dogs_log_mod <-  glm(placed ~ ., data = dogs, family = binomial)

summary(dogs_log_mod)
ll.null <- dogs_log_mod$null.deviance/-2
ll.proposed <- dogs_log_mod$deviance/-2
(ll.null - ll.proposed)/ ll.null
#R2 is 1 which means there is an issue. It could be overfitted, or have multicolinearity 

dogs_log_md_simple <- glm(placed ~ days_in_shelter, data = dogs, family = binomial)
summary(dogs_log_md_simple)

#Large range of residuals

#Calculated Psuedo R2 
ll.null <- dogs_log_md_simple$null.deviance/-2
ll.proposed <- dogs_log_md_simple$deviance/-2
(ll.null - ll.proposed)/ ll.null

#Very low R2 =.02 Bad model 

dogs_log_md2 <- glm(placed ~ days_in_shelter + age_years + sex, data = dogs, family = binomial)
summary(dogs_log_md2)

ll.null <- dogs_log_md2$null.deviance/-2
ll.proposed <- dogs_log_md2$deviance/-2
(ll.null - ll.proposed)/ ll.null

#Still Very low R2 (.027) but all variables are statistically significant

dogs_log_mod3 <- glm(placed ~ days_in_shelter + age_years + sex + intake_type + fixed.true + intimidating_breed_TRUE, 
                     na.omit(NULL) ,data = dogs, family = "binomial")
summary(dogs_log_mod3)

ll.null <- dogs_log_mod3$null.deviance/-2
ll.proposed <- dogs_log_mod3$deviance/-2
(ll.null - ll.proposed)/ ll.null
#This made the r2 go higher but still only at .088, Not a good model 

predicted_data <- data.frame(
  probability.of.placed = dogs_log_mod3$fitted.values,
  placed=dogs$placed)

   predicted_data <- predicted_data[
order(predicted_data$probability.of.placed, decreasing = FALSE),]

predicted_data$rank <- 1:nrow(predicted_data)

ggplot(data= predicted_data, aes(x = rank, y = probability.of.placed)) +
  geom_point(aes(color = placed), alpha=1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("Predicted probability of being placed") 

# This Diagram shows it clearly isn't very accurate. 

# Going to use Random Forests to see if we can get better results 

set.seed(666)

#selecting the variables that would seem to best explain placed
dogs_forest <- dogs %>%
  select(age_years, days_in_shelter, intake_type, placed, intake_condition, out_of_county, fixed.true,
         intimidating_breed_TRUE) %>%
  mutate_if(is.character, as.factor)

#Splitting Data Set into Train and Test data sets 

split <- sample.split(dogs_forest$placed, SplitRatio = .7)
dogs_forest_train = subset(dogs_forest, split == TRUE)

dogs_forest_test = subset(dogs_forest, split == FALSE)

library(randomForest)

set.seed(120)
dog_model_rf <- randomForest(as.factor(placed) ~ .,
                             data = dogs_forsest_train,
                             ntree = 1000,
                             mtry = 4,
                             importance = TRUE,
                             replace = FALSE)

dog_model_rf


library(pROC)

roc(dog_model_rf$y, as.numeric(dog_model_rf$predicted))

#ROC value is at .801

(aucc <- roc.area(as.integer(dogs_forest_train$placed), dog_model_rf$votes[,2])$A)
# .912


ci.auc(dog_model_rf$y, as.numeric(dog_model_rf$predicted))


roc.plot(as.integer(dogs_forest_train$placed), dog_model_rf$votes[,2], main = "")


rn <- round(importance(dog_model_rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

varImpPlot(dog_model_rf, main = "") %>%
  title(main="Variable Importance Random Forest")

# It appears the 4 most important variables are intake_condition, days_in_shelter, age_years, and intake_type

library(verification)

outcome <- predict(dog_model_rf, dogs_forest_test)
table(dogs_forest_test$placed, outcome, dnn=c("Actual", "Predicted"))


(519+2241)/(519+233+135+2241)
# .88 Prediction accuracy
# The model performs reasonably well for predicting dogs that get placed. However, for dogs that are not
# placed, the accuracy is not that great.

 #Refining the variable to the 4 most important 

dogs_refined_rf <- dogs %>%
  select(age_years, days_in_shelter, intake_type, placed, intake_condition) %>%
  mutate_if(is.character, as.factor)

split <- sample.split(dogs_refined_rf$placed, SplitRatio = .7)
dogs_forest_train = subset(dogs_refined_rf, split == TRUE)

dogs_forest_test = subset(dogs_refined_rf, split == FALSE)

dog_model_rf <- randomForest(as.factor(placed) ~ .,
                             data = dogs_forest_train,
                             ntree = 1000,
                             mtry = 4,
                             importance = TRUE,
                             replace = FALSE)

dog_model_rf

library(pROC)

roc(dog_model_rf$y, as.numeric(dog_model_rf$predicted))

# ROC has slightly decreased to .7693

(aucc <- roc.area(as.integer(dogs_forest_train$placed), dog_model_rf$votes[,2])$A)
# Slightly decreased to .89


ci.auc(dog_model_rf$y, as.numeric(dog_model_rf$predicted))

roc.plot(as.integer(dogs_forest_train$placed), dog_model_rf$votes[,2], main = "")


rn <- round(importance(dog_model_rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

varImpPlot(dog_model_rf, main = "") %>%
  title(main="Variable Importance Random Forest")

outcome <- predict(dog_model_rf, dogs_forest_test)
table(dogs_forest_test$placed, outcome, dnn=c("Actual", "Predicted"))
(489+2171)/(489+263+205+2171)

# This is slightly lower to .85 Prediction accuracy but is a much more readable/ interpretable model 

write.csv(dogs, "dogs_data_set_final.csv")
 
