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

# Converting income to binary variable
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
summary(adults[ , numeric]) # capital.gain probably has missing values as '99,999'

ggplot(adults, aes(age)) + geom_histogram()
ggplot(adults, aes(race, age)) + geom_boxplot()

ggplot(adults, aes(fnlwgt)) + geom_histogram()
ggplot(adults, aes(race, fnlwgt)) + geom_boxplot()

ggplot(adults, aes(capital.gain)) + geom_histogram()
ggplot(adults, aes(race, capital.gain)) + geom_boxplot()
table(adults[adults$capital.gain > 90000, 'capital.gain']) # 244 observations with 99,999 values
adults[adults$capital.gain == 99999, 'capital.gain'] <- NA # Assigned NA to rows with 99,999 values
table(adults[adults$capital.gain == 0, 'capital.gain']) # 44,807 observations with capital.gain = 0, or 91.74% of the dataset

ggplot(adults, aes(capital.loss)) + geom_histogram()
ggplot(adults, aes(race, capital.loss)) + geom_boxplot()
table(adults[adults$capital.loss == 0, 'capital.loss']) # 46,560 observations with capital.loss = 0, or 95.33% of the dataset

ggplot(adults, aes(hours.per.week)) + geom_histogram()
ggplot(adults, aes(race, hours.per.week)) + geom_boxplot()

# Missing values
vis_miss(adults[ , numeric])

# Tests
library(dplyr)
group_by(adults, sex) %>%
  summarise(
    count = n(),
    mean = mean(hours.per.week, na.rm = TRUE),
    sd = sd(hours.per.week, na.rm = TRUE)
  )

ggboxplot(adults, x = "sex", y = "hours.per.week", 
          color = "sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "hours.per.week", xlab = "sex")
str(adults)

<<<<<<< HEAD
# Compute t test
var.test(hours.per.week ~ sex, data = adults) # Test for Homescedasticity - Variances ratio = 0.97
t.test(hours.per.week ~ sex, data = adults, var.equal = FALSE)
var.test(as.numeric(education) ~ sex, data = adults) # Test for Homescedasticity - Variances ratio = 0.89
t.test(as.numeric(education) ~ sex, data = adults, var.equal = FALSE)

# chisquare test for independence
chisq.test(adults$sex, adults$native.country)
chisq.test(adults$sex, adults$race)
chisq.test(adults$ocupation, adults$sex)
chisq.test(adults$relationship, adults$sex)
chisq.test(adults$workclass, adults$native.country)
=======
# Compute t test for hours per week and income
var.test(adults$hours.per.week ~ adults$income) # there is no significant difference between the two 

res <- t.test(hours.per.week ~ income, data = adults, var.equal = TRUE)
res

# chisquare test for independence
# chisq.test(adults$sex, adults$native.country)
# chisq.test(adults$sex, adults$race)
# chisq.test(adults$ocupation, adults$sex)
# chisq.test(adults$relationship, adults$sex)
# chisq.test(adults$workclass, adults$native.country)

chisq.test(adults$income, adults$sex)
chisq.test(adults$income, adults$native.country)
chisq.test(adults$income, adults$race)
chisq.test(adults$income, adults$relationship)
chisq.test(adults$income, adults$workclass)
chisq.test(adults$income, adults$ocupation)
chisq.test(adults$income, adults$marital.status)

>>>>>>> 986b8b6355aba8d6eb77746e7731cdf438b6d59e
