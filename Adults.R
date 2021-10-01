pacman::p_load(tidyverse, olsrr, forecast, corrr, caret, GGally,
               lmtest, car, rsample, class, lime, reshape2, ggpubr)

setwd('~/R/DANA-4820/Project/Predictive-adults')
### Further dataset description found at: https://www.rdocumentation.org/packages/arules/versions/1.6-8/topics/Adult ###

### Loading adults-all dataset
col_names <- c('age', 'workclass', 'fnlwgt', 'education', 'education-num', 'marital-status', 'ocupation',
               'relationship', 'race', 'sex', 'capital-gain', 'capital-loss', 'hours-per-week', 'native-country', 'income')
adults <- read.csv('data/adult-all.csv', header = FALSE, col.names = col_names)

# Removing redundant variables
adults <- select(adults, -c('education.num')) # Numeric variable of education

#### Data Screening ####
summary(adults)
str(adults)

# Defining categorical and numerical data
categorical <- names(adults[ , sapply(adults, is.character)])
numeric <- names(adults[ , sapply(adults, is.numeric)])

## Screening categorical variables
sapply(adults[ , categorical], table)
adults$workclass <- factor(adults$workclass,
                           levels = c(names(table(adults$workclass))[-1])) # Removing "?" values and assigning as NA

adults$education <- factor(adults$education,
                           levels = c('Preschool', '1st-4th', '5th-6th', '7th-8th',
                                      '9th', '10th', '11th', '12th', 'HS-grad', 'Prof-school',
                                      'Assoc-acdm', 'Assoc-voc', 'Some-college', 'Bachelors',
                                      'Masters', 'Doctorate'),
                           ordered = TRUE) # Assigning ordered values according to dataset description

adults$ocupation <- factor(adults$ocupation,
                              levels = c(names(table(adults$ocupation))[-1])) # Removing "?" values and assigning as NA

adults$native.country <- factor(adults$native.country,
                                levels = c(names(table(adults$native.country))[-1])) # Removing "?" values and assigning as NA

adults[adults$income == '<=50K', 'income'] <- 0
adults[adults$income == '>50K', 'income'] <- 1
adults$income <- as.integer(adults$income)

adults$marital.status <- as.factor(adults$marital.status)
adults$relationship <- as.factor(adults$relationship)
adults$race <- as.factor(adults$race)
adults$sex <- as.factor(adults$sex)

# Frequencies of categorical variables
sapply(adults[ , categorical], table)

# Missing values
library(naniar)
vis_miss(adults[ , categorical])
gg_miss_upset(adults[c('workclass', 'ocupation', 'native.country')]) # Missing values for workclass and ocupation variables are (mostly) from the same individuals

## Screening numeric variables
summary(adults[ , numeric])
