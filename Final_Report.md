Outcomes for Dogs in Animal Shelter
================
Michael Martin
01/06/2020

## Introduction

The purpose of this analysis was to see if there was any insight to be
gained from analyzing the dog adoption data to improve the adoptability
of dogs or to make them more likely to be adopted. There are dogs that
die in “Kill” shelters daily because many are too overcrowded.

The data can be found on
<https://data.sonomacounty.ca.gov/Government/Animal-Shelter-Intake-and-Outcome/924a-vesw/data>

Load the libraries needed.

``` r
library(tidyverse)
library(lubridate)
library(janitor) #For cleaning up names
library(DataExplorer)
library(rmarkdown)
library(dplyr)
library(dbplyr)
library(tidyverse)
library(ggplot2)
```

## Data Wrangling

# 

Read in the data.

``` r
df <- read_csv("G://Adopted Animal Data Set.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Days in Shelter` = col_double(),
    ##   `Outcome Zip Code` = col_double(),
    ##   Count = col_double()
    ## )

    ## See spec(...) for full column specifications.

We’ll now use the `clean_names()` function from the `janitor` package to
get clean column names.

``` r
df <- df %>% clean_names()
names(df)
```

    ##  [1] "name"                 "type"                 "breed"               
    ##  [4] "color"                "sex"                  "size"                
    ##  [7] "date_of_birth"        "impound_number"       "kennel_number"       
    ## [10] "animal_id"            "intake_date"          "outcome_date"        
    ## [13] "days_in_shelter"      "intake_type"          "intake_subtype"      
    ## [16] "outcome_type"         "outcome_subtype"      "intake_condition"    
    ## [19] "outcome_condition"    "intake_jurisdiction"  "outcome_jurisdiction"
    ## [22] "outcome_zip_code"     "location"             "count"

We are working with only dogs, so let’s subset the data for only dogs.

``` r
dogs <- df %>% filter(type == 'DOG')
```

The `DataExplorer` package has a number of useful functions for
automating data exploration and treatment.

To get introduced to our dataset:

``` r
introduce(dogs)
```

    ## # A tibble: 1 x 9
    ##    rows columns discrete_columns continuous_colu~ all_missing_col~
    ##   <int>   <int>            <int>            <int>            <int>
    ## 1 10500      24               21                3                0
    ## # ... with 4 more variables: total_missing_values <int>, complete_rows <int>,
    ## #   total_observations <int>, memory_usage <dbl>

The above table displays that we have mostly discrete columns as opposed
to continuous

``` r
plot_intro(dogs)
```

![](Final_Report_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

So we have about 26% of incomplete rows. Let’s have a look at the
missing values.

``` r
plot_missing(dogs)
```

![](Final_Report_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

The `date_of_birth` column seems to have the maximum number of missing
values. However none of the columns have so many missing values that
they need to be dropped.

We can also visualize frequency distributions for all discrete features.

``` r
dogs %>%
  filter(count != 1)
```

    ## # A tibble: 1 x 24
    ##   name  type  breed color sex   size  date_of_birth impound_number kennel_number
    ##   <chr> <chr> <chr> <chr> <chr> <chr> <chr>         <chr>          <chr>        
    ## 1 OGGY  DOG   LABR~ BROW~ Neut~ LARGE 3/29/2011     K19-034834     DS64         
    ## # ... with 15 more variables: animal_id <chr>, intake_date <chr>,
    ## #   outcome_date <chr>, days_in_shelter <dbl>, intake_type <chr>,
    ## #   intake_subtype <chr>, outcome_type <chr>, outcome_subtype <chr>,
    ## #   intake_condition <chr>, outcome_condition <chr>, intake_jurisdiction <chr>,
    ## #   outcome_jurisdiction <chr>, outcome_zip_code <dbl>, location <chr>,
    ## #   count <dbl>

``` r
dogs$count <- NULL 
```

There is only 1 observation that has a count that is different than 1,
This is probably an error, I am removing this column, it does not
provide any useful information

``` r
plot_bar(dogs,nrow = 2, ncol = 2)
```

    ## 11 columns ignored with more than 50 categories.
    ## name: 3734 categories
    ## breed: 745 categories
    ## color: 189 categories
    ## date_of_birth: 3797 categories
    ## impound_number: 10500 categories
    ## kennel_number: 145 categories
    ## animal_id: 9052 categories
    ## intake_date: 2061 categories
    ## outcome_date: 1750 categories
    ## outcome_subtype: 173 categories
    ## location: 448 categories

![](Final_Report_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](Final_Report_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->![](Final_Report_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

We are interested in the outcome type for these dogs and can immediately
see that most of the dogs do get returned to owners. The second highest
outcome seems to be adoption and then we have transfer of dogs and
unfortunatley some also need to be euthanized.

We also notice that the `sex` column has information on spayed (female)
and neutered(male) dogs. It might be worth it to make another variable
to indicate if the dog is “fixed” or not. AFter that I will fix the
boservations in the sex column.

``` r
dogs <- dogs %>%
  mutate(fixed.true = as.logical(sex == c("Spayed", "Neutered")))

dogs$sex[dogs$sex == "Spayed"] <- "Female"
dogs$sex[dogs$sex == "Neutered"] <- "Male"
table(dogs$sex)
```

    ## 
    ##  Female    Male Unknown 
    ##    4528    5900      72

It also has dates as a charactor class. We can deduce the animals age if
we convert this to a POSIX class

``` r
dogs$date_of_birth <- parse_date_time2(dogs$date_of_birth, order = "mdY")
dogs$intake_date <- parse_date_time2(dogs$intake_date, order = "mdY")
dogs$outcome_date <- parse_date_time2(dogs$outcome_date, order = "mdY")
```

A closer inspection of the `date_of_birth` column reveals that some
dates are beyond the current date, which is not possible.

``` r
summary(dogs$date_of_birth)
```

    ##                  Min.               1st Qu.                Median 
    ## "1995-04-24 00:00:00" "2010-06-18 00:00:00" "2013-07-28 00:00:00" 
    ##                  Mean               3rd Qu.                  Max. 
    ## "2012-09-17 07:18:53" "2015-08-22 00:00:00" "2020-09-22 00:00:00" 
    ##                  NA's 
    ##                "1707"

We can replace the same with NAs.

``` r
dogs$date_of_birth[dogs$date_of_birth > Sys.Date()] <- NA
```

Made a new variable for the dog’s age when they arrived at the animal
shelter

``` r
dogs <- dogs %>%
  mutate(age = (intake_date - date_of_birth)/ (86400*365))

dogs$age <- as.character(dogs$age)
dogs$age <- as.numeric(dogs$age)

str(dogs)
```

    ## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame': 10500 obs. of  25 variables:
    ##  $ name                : chr  "*BUTCH" "HUTCH" NA NA ...
    ##  $ type                : chr  "DOG" "DOG" "DOG" "DOG" ...
    ##  $ breed               : chr  "PUG/MIX" "LABRADOR RETR" "GREAT PYRENEES" "LABRADOR RETR/ROTTWEILER" ...
    ##  $ color               : chr  "BROWN" "YELLOW" "WHITE/GRAY" "BLACK" ...
    ##  $ sex                 : chr  "Male" "Male" "Unknown" "Female" ...
    ##  $ size                : chr  "SMALL" "MED" "LARGE" "LARGE" ...
    ##  $ date_of_birth       : POSIXct, format: "2014-07-02" NA ...
    ##  $ impound_number      : chr  "K15-014358" "K19-034353" "K19-034345" "K19-034321" ...
    ##  $ kennel_number       : chr  "DA01" "DS73" "DA36" "MOBILE" ...
    ##  $ animal_id           : chr  "A322747" "A393404" "A393396" "A393376" ...
    ##  $ intake_date         : POSIXct, format: "2015-06-06" "2019-10-30" ...
    ##  $ outcome_date        : POSIXct, format: "2015-07-28" "2019-10-31" ...
    ##  $ days_in_shelter     : num  52 1 2 2 5 2 6 1 3 0 ...
    ##  $ intake_type         : chr  "STRAY" "STRAY" "STRAY" "STRAY" ...
    ##  $ intake_subtype      : chr  "OVER THE COUNTER" "OVER THE COUNTER" "FIELD" "FIELD" ...
    ##  $ outcome_type        : chr  "ADOPTION" "RETURN TO OWNER" "RETURN TO OWNER" "RETURN TO OWNER" ...
    ##  $ outcome_subtype     : chr  "WALKIN" NA "OVER THE COUNTER_PRVS" "OVER THE COUNTER_CALL" ...
    ##  $ intake_condition    : chr  "HEALTHY" "HEALTHY" "UNKNOWN" "UNKNOWN" ...
    ##  $ outcome_condition   : chr  "HEALTHY" NA "PENDING" "HEALTHY" ...
    ##  $ intake_jurisdiction : chr  "SANTA ROSA" "COUNTY" "COUNTY" "WINDSOR" ...
    ##  $ outcome_jurisdiction: chr  "WINDSOR" "COUNTY" "WINDSOR" NA ...
    ##  $ outcome_zip_code    : num  95492 95476 95492 NA 95446 ...
    ##  $ location            : chr  "95492(38.541541, -122.809202)" "95476(38.280393, -122.464588)" "95492(38.541541, -122.809202)" NA ...
    ##  $ fixed.true          : logi  FALSE FALSE FALSE FALSE TRUE FALSE ...
    ##  $ age                 : num  0.929 NA NA NA NA ...

Change the name to make it more uniform and replace the NA with the mean
age\_years

``` r
dogs <- dogs %>%
  rename(age_years = age)


dogs$age_years[is.na(dogs$age_years)] <- mean(dogs$age_years, na.rm = TRUE)
```

We can also drop the `type` variable because it only contains a single
distinct value “DOG”

``` r
dogs$type <- NULL
```

As you can see below there are hundreds of distinct values for both
color and breed

``` r
dogs %>%
  group_by(breed) %>%
  count()
```

    ## # A tibble: 745 x 2
    ## # Groups:   breed [745]
    ##    breed                     n
    ##    <chr>                 <int>
    ##  1 AFFENPINSCHER             5
    ##  2 AFFENPINSCHER/MIX         1
    ##  3 AIREDALE TERR             1
    ##  4 AIREDALE TERR/MIX         2
    ##  5 AKBASH                    7
    ##  6 AKBASH/GREAT PYRENEES     2
    ##  7 AKBASH/MIX                4
    ##  8 AKITA                    16
    ##  9 AKITA/ALASKAN HUSKY       1
    ## 10 AKITA/ANATOL SHEPHERD     1
    ## # ... with 735 more rows

``` r
dogs %>%
  group_by(color) %>%
  count()
```

    ## # A tibble: 189 x 2
    ## # Groups:   color [189]
    ##    color                n
    ##    <chr>            <int>
    ##  1 AGOUTI              11
    ##  2 APRICOT             16
    ##  3 BL BRINDLE          37
    ##  4 BL BRINDLE/BLACK     1
    ##  5 BL BRINDLE/GRAY      2
    ##  6 BL BRINDLE/TAN       1
    ##  7 BL BRINDLE/WHITE    47
    ##  8 BLACK              813
    ##  9 BLACK/BL BRINDLE     3
    ## 10 BLACK/BLACK         18
    ## # ... with 179 more rows

Creating a few variables from breed and color to be able to extract
useful information from them

``` r
breed_pattern <- "PIT BULL+|AKITA+|ROTTWEILER+|DOBERMAN+|GERM SHEPHERD+|PYRENEES+|GREAT DANE+" 

detection <- str_detect(dogs$breed, breed_pattern)

dogs$intimidating_breed <- as.logical(detection)
```

Converting “intimidating\_breed” into a dummy variable and removing
breed since it has too many values distinct values to be useful.

``` r
library(fastDummies)
```

    ## Warning: package 'fastDummies' was built under R version 3.5.3

``` r
dogs <- dogs %>%
  dummy_cols(select_columns = "intimidating_breed", remove_first_dummy = TRUE)


dogs$breed <- NULL
```

There is other information in colors, so I am separating them to see if
this info would be more useful

``` r
dogs <- dogs %>%
  separate(color, into = c("Primary Color", "Secondary COlor"), sep = "\\/", remove = TRUE )
```

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 4390 rows [1, 2,
    ## 4, 5, 7, 8, 11, 13, 15, 19, 20, 21, 22, 25, 32, 35, 36, 37, 44, 45, ...].

``` r
colnames(dogs)[colnames(dogs) == "Primary Color"] <- "primary_color"
colnames(dogs)[colnames(dogs) == "Secondary COlor"] <- "secondary_color"

n_distinct(dogs$primary_color)
```

    ## [1] 35

There are still a lot of unique values after this. I am removing because
of the high distinct value as well as breed has a lot to do with the
color as well.

``` r
dogs$primary_color <- NULL
dogs$secondary_color <- NULL
```

Made out\_of\_county variable to see if county that they are adopted in
has any affect.

``` r
dogs <- dogs %>%
  mutate(out_of_county = as.logical(outcome_jurisdiction == "OUT OF COUNTY"))
```

Also removing name, intake\_subtype, outcome\_subtype,
outcome\_jurisdiction, outcome\_condition,

``` r
dogs$name <- NULL
dogs$intake_subtype <- NULL
dogs$outcome_subtype <- NULL
dogs$outcome_jurisdiction <- NULL
dogs$outcome_condition <- NULL
dogs$location <- NULL
dogs$outcome_zip_code <- NULL
dogs$kennel_number <- NULL
dogs$impound_number <- NULL
```

Have a look at the discrete features by the number of days spent in the
shelter.

``` r
plot_bar(dogs, with = 'days_in_shelter', nrow = 2, ncol = 2)
```

    ## 4 columns ignored with more than 50 categories.
    ## date_of_birth: 3796 categories
    ## animal_id: 9052 categories
    ## intake_date: 2061 categories
    ## outcome_date: 1750 categories

![](Final_Report_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->![](Final_Report_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->![](Final_Report_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->

``` r
plot_histogram(dogs) 
```

![](Final_Report_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Creating “placed” variable to contain all dogs that have either been
adopted or have been returned to their owners.

``` r
dogs$placed <- ifelse((dogs$outcome_type == "ADOPTION" | dogs$outcome_type == "RETURN TO OWNER"), 1, 0)
```

### Correlation Analysis

We can visualize correlation heatmap for all non-missing features.

``` r
plot_correlation(na.omit(dogs), maxcat = 2)
```

    ## 10 features with more than 2 categories ignored!
    ## sex: 3 categories
    ## size: 6 categories
    ## date_of_birth: 3778 categories
    ## animal_id: 7414 categories
    ## intake_date: 1997 categories
    ## outcome_date: 1677 categories
    ## intake_type: 6 categories
    ## outcome_type: 7 categories
    ## intake_condition: 5 categories
    ## intake_jurisdiction: 13 categories

![](Final_Report_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

Limited maxcat=2 so the correlation plot is understandable.

It looks like placed is correlated to “out\_of\_county”,
“days\_in\_shelter”, “fixed.true”, and “intimidating\_breed”

## Statistical Analysis

### Possible outcomes for dogs adn the distribution

``` r
outcome <- dogs %>% 
  group_by(outcome_type) %>% 
  summarise(animal_count = n())

ggplot(outcome, aes(x=outcome_type, y=animal_count, fill=outcome_type)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(y = "Animals", 
       x = "",
       title = "Outcomes for Dogs") +
  scale_fill_brewer(palette="Set3") +
  theme(legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        title = element_text(size=10),
        axis.title.x = element_text(size=8),
        axis.text.y = element_text(size=8),
        axis.text.x = element_text(size=8))
```

![](Final_Report_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

This indicates that return to owner is the most common, and adoption is
second after that.

### Intake type by outcomes

For all dogs that go through the shelter-adoption process, intake and
outcome are two crucial steps. The data offers information on both the
intake type and outcome type, and I’m curious to know if there’s any
relationship between the two activities.

``` r
outcome_dog <- dogs %>% 
  group_by(intake_type, outcome_type) %>% 
  summarise(animal_count = n())
ggplot(outcome_dog, aes(x=intake_type, y=animal_count, fill=outcome_type)) +
  geom_bar(stat="identity", position = "fill") +
  coord_flip() +
  labs(y = "Percentage of Animals", 
       x = "",
       title = "Intake Type by Outcomes") +
  scale_fill_brewer(palette="Set3") +
  theme(legend.text = element_text(size=7))
```

![](Final_Report_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

The 2 most surprising things to me was that confiscated dogs were mostly
returned to owners and how many Owner Surrender dogs are eventually
euthanized.

It is also notable that adoption returns have a high rate of being
adopted again

### Are healthy dogs more likely to be adopted?

``` r
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
```

![](Final_Report_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

There is a number of healthy or treatable dogs that are euthanized. It
is also clear that the majority of adoptions are healthy dogs.

What proportion of Euthanized dogs are healthy when entering?

``` r
prop.table(table(dogs$outcome_type, dogs$fixed.true), margin = 2)
```

    ##                  
    ##                          FALSE         TRUE
    ##   ADOPTION        0.1977335464 0.3947516930
    ##   DIED            0.0024698533 0.0014108352
    ##   DISPOSAL        0.0015981403 0.0002821670
    ##   ESCAPED/STOLEN  0.0005811419 0.0002821670
    ##   EUTHANIZE       0.1388929246 0.0632054176
    ##   RETURN TO OWNER 0.5240447479 0.4382054176
    ##   TRANSFER        0.1346796455 0.1018623025

``` r
dogs %>%
  filter(outcome_type=="EUTHANIZE") %>% 
  ggplot(aes(x = factor(days_in_shelter), fill=intake_condition)) +
  geom_bar() +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(size = 7, angle = 90)) +
  labs(y = "Number of Animals", 
       x = "Days in Shelter",
       title = "Euthanized Dogs by Intake Condition")
```

![](Final_Report_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

There is a a general trend that the proportion of Euthanized dogs
increases as the days in shelter increases. This is probably due to; 1.)
The longer an animal stays in a “kill” shelter the more likely tehy are
to be euthanized to make room for more animals. 2.) They dog gets sick
from other dogs or the conditions leading to them being put down.

### How long does it take for dogs to be returned to their owners?

``` r
dogs %>% 
  filter(outcome_type=="RETURN TO OWNER") %>% 
  ggplot(aes(factor(days_in_shelter))) +
  geom_bar(fill="#3399CC")
```

![](Final_Report_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

Vast Majority Spend 0-1 days in a shelter before they are Returned to
the owner

``` r
owner_returned <- dogs %>%
  filter(outcome_type == "RETURN TO OWNER" )

mean(owner_returned$days_in_shelter)
```

    ## [1] 2.853682

``` r
median(owner_returned$days_in_shelter)
```

    ## [1] 1

Due to some outliers the mean is 2.85 Days in Shelter for Dogs who were
eventually returned but the median is 1 day

### Among the dogs that are adopted, what is the average adoption time?

``` r
dogs %>% 
  filter(outcome_type=="ADOPTION") %>% 
  summarise(avg_adoption_time = mean(days_in_shelter))
```

    ## # A tibble: 1 x 1
    ##   avg_adoption_time
    ##               <dbl>
    ## 1              34.2

``` r
dogs %>% 
  filter(outcome_type=="ADOPTION" & days_in_shelter<=100) %>% 
  ggplot(aes(x = days_in_shelter)) +
  geom_histogram(binwidth = 1, fill="Orange")
```

![](Final_Report_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
dogs %>%
  filter(outcome_type=="ADOPTION" & days_in_shelter<=100) %>%
  summarise(avg_adoption_time = mean(days_in_shelter))
```

    ## # A tibble: 1 x 1
    ##   avg_adoption_time
    ##               <dbl>
    ## 1              26.4

``` r
dogs %>%
  filter(outcome_type=="ADOPTION" & days_in_shelter<=100) %>%
  summarise(avg_adoption_time = median(days_in_shelter))
```

    ## # A tibble: 1 x 1
    ##   avg_adoption_time
    ##               <dbl>
    ## 1                20

The median days in shelter for adopted dogs in 20 but the mean is 26.4

### Are “intimidating breeds” adopted less?

``` r
dogs %>% filter(outcome_type == 'ADOPTION') %>%
  count(intimidating_breed_TRUE) %>% arrange(desc(n))
```

    ## # A tibble: 2 x 2
    ##   intimidating_breed_TRUE     n
    ##                     <int> <int>
    ## 1                       0  1865
    ## 2                       1   895

``` r
prop.table(table(dogs$outcome_type, dogs$intimidating_breed_TRUE), margin = 2)
```

    ##                  
    ##                              0            1
    ##   ADOPTION        0.2806199218 0.2367098651
    ##   DIED            0.0027083960 0.0010579212
    ##   DISPOSAL        0.0010532651 0.0013224015
    ##   ESCAPED/STOLEN  0.0004513993 0.0005289606
    ##   EUTHANIZE       0.0704182967 0.1883099709
    ##   RETURN TO OWNER 0.5093289197 0.4694525258
    ##   TRANSFER        0.1354198014 0.1026183549

``` r
dogs %>%
  filter(intimidating_breed =="TRUE" ) %>%
  summarise(avg_adoption_time = mean(days_in_shelter))
```

    ## # A tibble: 1 x 1
    ##   avg_adoption_time
    ##               <dbl>
    ## 1              21.5

``` r
dogs %>%
  filter(intimidating_breed == "FALSE" ) %>%
  summarise(avg_adoption_time = mean(days_in_shelter))
```

    ## # A tibble: 1 x 1
    ##   avg_adoption_time
    ##               <dbl>
    ## 1              13.7

Intimidating breeds are Adopted less and Returned to owner less than
non-intimidating breeds intimidating breeds are also more likely to be
euthanized. The average adoption time for intimidating breeds is 21.5
days while non-intimidating breeds have an average adoption time of 13.7
days

## Machine Learning

We would like to predict the outcome for each dog. In this case it will
be a binary classification problem which will classify whether a dog is
placed in a home or not. “Adoption” and “Return to owner” will be
regarded as “placed in a home”, and all others will be categorized into
“not placed in a home”. We have made the variable “placed” for this
purpose.

### Prediction using Logistic Regression

###### Overly Simple Log Model

``` r
dogs_log_md_simple <- glm(placed ~ days_in_shelter, data = dogs, family = binomial)
summary(dogs_log_md_simple)
```

    ## 
    ## Call:
    ## glm(formula = placed ~ days_in_shelter, family = binomial, data = dogs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7824   0.6760   0.6795   0.7119   3.9635  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      1.3599905  0.0275073   49.44   <2e-16 ***
    ## days_in_shelter -0.0116490  0.0007991  -14.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11503  on 10426  degrees of freedom
    ## Residual deviance: 11257  on 10425  degrees of freedom
    ## AIC: 11261
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
ll.null <- dogs_log_md_simple$null.deviance/-2
ll.proposed <- dogs_log_md_simple$deviance/-2
(ll.null - ll.proposed)/ ll.null
```

    ## [1] 0.02132099

This has a large range of residuals and a very low psuedo r2 (.02),
meaning this is not a good model.

``` r
dogs_log_md2 <- glm(placed ~ days_in_shelter + age_years + sex, data = dogs, family = binomial)
summary(dogs_log_md2)
```

    ## 
    ## Call:
    ## glm(formula = placed ~ days_in_shelter + age_years + sex, family = binomial, 
    ##     data = dogs)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8441   0.6395   0.6808   0.7113   4.0614  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      1.4988146  0.0492494  30.433  < 2e-16 ***
    ## days_in_shelter -0.0121112  0.0008139 -14.881  < 2e-16 ***
    ## age_years       -0.0138804  0.0068049  -2.040   0.0414 *  
    ## sexMale         -0.1028112  0.0474399  -2.167   0.0302 *  
    ## sexUnknown      -1.8506282  0.2451605  -7.549  4.4e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11503  on 10426  degrees of freedom
    ## Residual deviance: 11195  on 10422  degrees of freedom
    ## AIC: 11205
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
ll.null <- dogs_log_md2$null.deviance/-2
ll.proposed <- dogs_log_md2$deviance/-2
(ll.null - ll.proposed)/ ll.null
```

    ## [1] 0.02673437

Still Very low R2 (.027) but all variables are statistically significant

``` r
dogs_log_mod3 <- glm(placed ~ days_in_shelter + age_years + sex + intake_type + fixed.true + intimidating_breed_TRUE, 
                     na.omit(NULL) ,data = dogs, family = "binomial")
summary(dogs_log_mod3)
```

    ## 
    ## Call:
    ## glm(formula = placed ~ days_in_shelter + age_years + sex + intake_type + 
    ##     fixed.true + intimidating_breed_TRUE, family = "binomial", 
    ##     data = dogs, weights = na.omit(NULL))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4906   0.3429   0.5792   0.7376   4.1397  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                 1.2790839  0.1631774   7.839 4.56e-15 ***
    ## days_in_shelter            -0.0117462  0.0008458 -13.888  < 2e-16 ***
    ## age_years                  -0.0367860  0.0071907  -5.116 3.12e-07 ***
    ## sexMale                    -0.1154892  0.0493219  -2.342   0.0192 *  
    ## sexUnknown                 -1.7463878  0.2524947  -6.917 4.63e-12 ***
    ## intake_typeCONFISCATE       1.1024103  0.1850697   5.957 2.57e-09 ***
    ## intake_typeOWNER SURRENDER -0.9437300  0.1686301  -5.596 2.19e-08 ***
    ## intake_typeQUARANTINE      -0.2864626  0.2083258  -1.375   0.1691    
    ## intake_typeSTRAY            0.3369795  0.1590918   2.118   0.0342 *  
    ## intake_typeTRANSFER         0.2466881  0.2189369   1.127   0.2598    
    ## fixed.trueTRUE              0.8225612  0.0561234  14.656  < 2e-16 ***
    ## intimidating_breed_TRUE    -0.3621120  0.0506602  -7.148 8.81e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 11503  on 10426  degrees of freedom
    ## Residual deviance: 10503  on 10415  degrees of freedom
    ## AIC: 10527
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
ll.null <- dogs_log_mod3$null.deviance/-2
ll.proposed <- dogs_log_mod3$deviance/-2
(ll.null - ll.proposed)/ ll.null
```

    ## [1] 0.08687725

This made the r2 go higher but still only at .087, Not a good model

##### Plotting this Log Model

``` r
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
```

![](Final_Report_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

This shows clearly that this model is not very accurate

### Prediction Using Random Forest

Selecting the variables that would seem to best explain “placed”

``` r
dogs_forest <- dogs %>%
  select(age_years, days_in_shelter, intake_type, placed, intake_condition, out_of_county, fixed.true,
         intimidating_breed_TRUE) %>%
  mutate_if(is.character, as.factor)
```

``` r
library(rpart.plot)
```

    ## Warning: package 'rpart.plot' was built under R version 3.5.3

    ## Loading required package: rpart

    ## Warning: package 'rpart' was built under R version 3.5.3

``` r
library(caTools)
```

    ## Warning: package 'caTools' was built under R version 3.5.3

``` r
library(rpart)
```

Separate the dataset into training and testing dataframes.

``` r
set.seed(666)
split <- sample.split(dogs_forest$placed, SplitRatio = .7)

dogs_forest_train = subset(dogs_forest, split == TRUE)

dogs_forest_test = subset(dogs_forest, split == FALSE)
```

Create the random forest model.

``` r
library(randomForest)
```

    ## Warning: package 'randomForest' was built under R version 3.5.3

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
set.seed(120)
dog_model_rf <- randomForest(formula = as.factor(placed) ~ .,
                             data = dogs_forest_train,
                             ntree=1000,
                             mtry=4,
                             importance=TRUE,
                             replace=FALSE)
dog_model_rf
```

    ## 
    ## Call:
    ##  randomForest(formula = as.factor(placed) ~ ., data = dogs_forest_train,      ntree = 1000, mtry = 4, importance = TRUE, replace = FALSE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 1000
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 12.62%
    ## Confusion matrix:
    ##      0    1 class.error
    ## 0 1174  581  0.33105413
    ## 1  340 5204  0.06132756

``` r
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 3.5.3

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
roc(dog_model_rf$y, as.numeric(dog_model_rf$predicted))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## 
    ## Call:
    ## roc.default(response = dog_model_rf$y, predictor = as.numeric(dog_model_rf$predicted))
    ## 
    ## Data: as.numeric(dog_model_rf$predicted) in 1755 controls (dog_model_rf$y 0) < 5544 cases (dog_model_rf$y 1).
    ## Area under the curve: 0.8038

ROC value is at .803

Random forest also gives us the importance of each variable.

``` r
rn <- round(importance(dog_model_rf), 2)
rn[order(rn[,3], decreasing=TRUE),]
```

    ##                              0      1 MeanDecreaseAccuracy MeanDecreaseGini
    ## intake_condition        216.71 260.60               321.64           469.02
    ## days_in_shelter         181.61 237.62               293.48           461.86
    ## intake_type              55.92 135.27               147.44           100.00
    ## age_years                62.89 115.33               130.32           394.71
    ## out_of_county            95.13  14.18                72.57            62.88
    ## fixed.true               47.10  21.49                44.84            43.59
    ## intimidating_breed_TRUE  16.17   8.47                16.56            38.14

``` r
varImpPlot(dog_model_rf, main = "") %>% 
title(main="Variable Importance Random Forest")
```

![](Final_Report_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

It appears the 4 most important variables are intake\_condition,
days\_in\_shelter, age\_years, and intake\_type

``` r
(aucc <- roc.area(as.integer(dogs_forest_train$placed), dog_model_rf$votes[,2])$A)
```

    ## [1] 0.9125461

``` r
roc.plot(as.integer(dogs_forest_train$placed), dog_model_rf$votes[,2], main = "")
```

    ## Warning in roc.plot.default(as.integer(dogs_forest_train$placed),
    ## dog_model_rf$votes[, : Large amount of unique predictions used as thresholds.
    ## Consider specifying thresholds.

![](Final_Report_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

Area under the curve is .912

### Testing

``` r
outcome <- predict(dog_model_rf, dogs_forest_test)
table(dogs_forest_test$placed, outcome, dnn=c("Actual", "Predicted"))
```

    ##       Predicted
    ## Actual    0    1
    ##      0  515  237
    ##      1  137 2239

The model performs reasonably well for predicting dogs that get placed.
However, for dogs that are not placed, the accuracy is not that great.

``` r
(515+2239)/(515+237+137+2239)
```

    ## [1] 0.8804348

It has a prediction accuracy of .88

#### Refining the model to the 4 most important variables

``` r
dogs_refined_rf <- dogs %>%
  dplyr::select(age_years, days_in_shelter, intake_type, placed, intake_condition) %>%
  mutate_if(is.character, as.factor)
```

Including all 4 dependent variables in the data set to predict “placed”

``` r
split <- sample.split(dogs_refined_rf$placed, SplitRatio = .7)

dogs_refined_train = subset(dogs_refined_rf, split == TRUE)
dogs_refined_test = subset(dogs_refined_rf, split == FALSE)
```

``` r
dog_model_rf <- randomForest(as.factor(placed) ~ .,
                             data = dogs_refined_train,
                             ntree = 1000,
                             mtry = 4,
                             importance = TRUE,
                             replace = FALSE)

dog_model_rf
```

    ## 
    ## Call:
    ##  randomForest(formula = as.factor(placed) ~ ., data = dogs_refined_train,      ntree = 1000, mtry = 4, importance = TRUE, replace = FALSE) 
    ##                Type of random forest: classification
    ##                      Number of trees: 1000
    ## No. of variables tried at each split: 4
    ## 
    ##         OOB estimate of  error rate: 14.97%
    ## Confusion matrix:
    ##      0    1 class.error
    ## 0 1144  611  0.34814815
    ## 1  482 5062  0.08694084

``` r
roc(dog_model_rf$y, as.numeric(dog_model_rf$predicted))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## 
    ## Call:
    ## roc.default(response = dog_model_rf$y, predictor = as.numeric(dog_model_rf$predicted))
    ## 
    ## Data: as.numeric(dog_model_rf$predicted) in 1755 controls (dog_model_rf$y 0) < 5544 cases (dog_model_rf$y 1).
    ## Area under the curve: 0.7825

ROC has decreased slightly to

``` r
rn <- round(importance(dog_model_rf), 2)

rn[order(rn[,3], decreasing=TRUE),]
```

    ##                       0      1 MeanDecreaseAccuracy MeanDecreaseGini
    ## intake_condition 228.90 307.26               357.14           534.04
    ## days_in_shelter  201.04 267.16               338.55           479.44
    ## age_years         68.35 157.59               173.94           494.97
    ## intake_type       58.13 103.77               115.97            98.26

``` r
varImpPlot(dog_model_rf, main = "") %>%

  title(main="Variable Importance Random Forest")
```

![](Final_Report_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
outcome <- predict(dog_model_rf, dogs_refined_test)

table(dogs_refined_test$placed, outcome, dnn=c("Actual", "Predicted"))
```

    ##       Predicted
    ## Actual    0    1
    ##      0  478  274
    ##      1  192 2184

``` r
(478+2184)/(478+274+192+2184)
```

    ## [1] 0.851023

The prediction accuracy has decreased to .85 but it is a more readable/
interpretable model

### Findings & Recommendations

The model that we used was able to predict with 85% accuracy whether or
not a dog would be placed in a home via adoption or returned to owner.
The model used the 4 variables age\_years, days\_in\_shelter,
intake\_type, intake\_condition to predict the result. Through the
findings there a few things that might be able to be done to make
getting an animal a home or back to home more likely.

1.) The Intake Condition of animal is so important for how long they
will be in a shelter, and their likelihood of return or adoption. If
people were more incentivised to bring in strays or lost animals then it
is entirely possible that a dog may be brought in when he/she is lost or
hungry as opposed to sickly or dying.

2.) It is entirely clear that the longer a dog spends in a shelter the
less likely he/ she is to be “placed”. If there were more behavior
training that the shelters offered then the dogs that have been in the
shelter longer would actually be more well behaved and more ready for
home life. This would ideally lessen adoption returns and would get more
dogs that have been there for a while a new home.

3.) The more exposure older dogs can get the more likely they are to be
adopted or returned. I recommend prioritizing older dogs by posting on
social media their availability as well as have “Elder Adoption Days”.
Encourage people to come in and place the older dogs in the rooms with
more exposure/ traffic.

### Further Research

There were some limitations with the data. One would be research to see
if people who voluntarily give up animals are repeat offenders or if it
is usually a one time occurance. It would also be interesting to see if
the owners that had their animals confiscated were also people who have
voluntarily given up their animals. I did not dive much into breed
because of the amount of breeds there are but if there are huge
differences in the average time spent at a shelter per breed. I imagine
that how people value their and respect their animals changes per
county, and state dramatically. I would be curious to see what states or
cities adopted the most animals.
