pacman::p_load(tidyverse, olsrr, forecast, corrr, caret, GGally,
               lmtest, car, rsample, class, lime, reshape2, ggpubr, dplyr)

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

char <- sapply(adults, is.factor)
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

## Grouping Armed Forces with Other-services
adults$ocupation[adults$ocupation == 'Armed-Forces'] <- 'Other-service'
adults$ocupation[adults$ocupation == 'Priv-house-serv'] <- 'Other-service'
adults <- adults[adults$workclass != 'Never-worked', ] 
adults <- adults[adults$workclass != 'Without-pay', ] 

adults$ocupation <- droplevels(adults$ocupation) 
adults$workclass <- droplevels(adults$workclass)
levels(adults$ocupation)
levels(adults$workclass)

# Grouping Native Country by United States and Outside United states 
adults <- adults %>% mutate(native.country = factor(ifelse(native.country == "United-States", "US", "Other countries")))

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

# Initial Statistical Tests  ----------------------------------------------
ggboxplot(adults, x = "sex", y = "hours.per.week", 
          color = "sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "hours.per.week", xlab = "sex")
str(adults)

head(adults)
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

# Compute t test for hours per week and income
var.test(adults$age ~ adults$income)

res1 <- t.test(adults$age ~ adults$income)
res1 

var.test(adults$hours.per.week ~ adults$income) # there is no significant difference between the two 

res2 <- t.test(hours.per.week ~ income, data = adults, var.equal = TRUE)
res2

# Chisquare test for independence
chisq1 <- chisq.test(adults$income, adults$sex)
chisq1
chisq1$observed
chisq1$residuals

chisq2 <- chisq.test(adults$income, adults$native.country)
chisq2
chisq2$observed
round(chisq2$residuals, 3)

chisq3 <- chisq.test(adults$income, adults$race)
chisq3
chisq3$observed
chisq3$residuals

chisq4 <- chisq.test(adults$income, adults$relationship)
chisq4
chisq4$observed
chisq4$residuals

chisq5 <- chisq.test(adults$income, adults$workclass)
chisq5
chisq5$observed
chisq5$residuals

chisq6 <- chisq.test(adults$income, adults$ocupation)
chisq6
chisq6$observed
chisq6$residuals

chisq7 <- chisq.test(adults$income, adults$marital.status)
chisq7
chisq7$observed
chisq7$residuals

adults$income <- factor(adults$income)

# Divide data based on income greater or less than 50G
more.than.50 <- adults %>% 
  filter(income == '1')

more.than.50$income <- factor(more.than.50$income)
  
  less.than.50 <- adults %>%
    filter(income == '0')
  
less.than.50$income <- factor(less.than.50$income)
  
more.than.50 %>% select(income, sex) %>% summary()
less.than.50 %>% select(income, sex) %>% summary()


# Comparative Statistical Tests -------------------------------------------
# Variance tests conducted on both samples
var.test(more.than.50$hours.per.week ~ more.than.50$sex) # variance test for hours.per.week vs. sex for adults who earned more than 50k
var.test(less.than.50$age ~ less.than.50$sex) # variance test for age vs. sex for adults who earned less than 50k  

t.test.50.more.hpw.sex<- t.test(hours.per.week ~ sex, data = more.than.50, var.equal = TRUE)
t.test.50.less.hpw.sex<- t.test(hours.per.week ~ sex, data = less.than.50, var.equal = TRUE)

t.test.50.more.age.sex<- t.test(age ~ sex, data = more.than.50, var.equal = TRUE)
t.test.50.less.age.sex<- t.test(age ~ sex, data = less.than.50, var.equal = TRUE)

# Conducted t-tests on both samples
t.test.50.less.hpw.sex #t-test for hours.per.week and sex for adults who earned less than 50k
t.test.50.more.hpw.sex #t-test for hours.per.week and sex for adults who earned more than 50k

t.test.50.less.age.sex #t-test for age and sex for adults who earned less than 50k
t.test.50.more.age.sex #t-test for age and sex for adults who earned more than 50k

# Chi-sqaured test 
chisq8 <- chisq.test(more.than.50$sex, more.than.50$hours.per.week)
chisq8$observed
chisq8$residuals

chisq9 <- chisq.test(less.than.50$sex, less.than.50$hours.per.week)
chisq9$observed
chisq9$residuals

# Multicolinearity check 
library(corrr)

model_vif <- glm(income ~., family = binomial('logit'), data = adults)

summary(model_vif)
vif(model_vif)

# Marital status and relationship status VIF >10 so we are removing marital
# status

model_vif2 <- glm(income ~. -relationship, family = binomial('logit'), data = adults)
summary(model_vif2)
vif(model_vif2)
# after removing marital status no other variable presented with 
# VIF >10 

# Interaction terms 
library(interactions)
library(ggplot2)

str(adults$income)
str(as.factor(adults$income))

model_cat_plot <- glm(income ~. -relationship, family = binomial(link = 'logit'), data = adults)

interaction.plot(adults$relationship, adults$sex, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = ocupation, modx = sex, interval = TRUE, geom = 'line')

interaction.plot(adults$education, adults$sex, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = education, modx = sex, interval = TRUE, geom = 'line')

interaction.plot(adults$education, adults$race, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = education, modx = race, interval = TRUE, geom = 'line')

interaction.plot(adults$ocupation, adults$sex, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = ocupation, modx = sex, interval = TRUE, geom = 'line')

interaction.plot(adults$ocupation, adults$race, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = ocupation, modx = race, interval = TRUE, geom = 'line')

interaction.plot(adults$workclass, adults$sex, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = workclass, modx = sex, interval = TRUE, geom = 'line')

interaction.plot(adults$workclass, adults$race, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = workplace, modx = race, interval = TRUE, geom = 'line')

interaction.plot(adults$marital.status, adults$sex, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = marital.status, modx = sex, interval = TRUE, geom = 'line')

interaction.plot(adults$marital.status, adults$race, as.numeric(adults$income))
cat_plot(model_cat_plot, pred = marital.status, modx = race, interval = TRUE, geom = 'line')


# Model chosen for best interaction
cat_plot(model_cat_plot, pred = ocupation, modx = sex, interval = TRUE, geom = 'line')
interaction.plot(adults$ocupation, adults$sex, as.numeric(adults$income), plot.points = TRUE)
# Observable interaction between the occupation and income among genders

# Dataset split 
# 70/30
library(caTools)
set.seed(1000) # is used so that each time we get the same data set after splitting 
sample_size<- sample.split(adults, SplitRatio = 7/10) #Splitting the dataset into 70/30 ratio  
  
train<-subset(adults, sample_size==T)  
  
test<-subset(adults, sample_size==F)  
  
nrow(train)  
  
nrow(test)



# Select explanatory variables
# Backwards elimination without interaction
stepwise_sel <- glm(income ~ .-relationship, family = binomial(link = 'logit'), data = adults)
summary(stepwise_sel)

stepwise_sel2 <- glm(income ~ .-relationship -native.country -education  + as.numeric(education), family = binomial(link='logit'), data = adults)
summary(stepwise_sel2)

library(MASS)
step.model <- stepAIC(stepwise_sel, direction = "backward")
summary(step.model)

##### Purposeful Selection Process #####

# # Step 1 - Defining individual explanatory variables
# attach(adults)
y <- income
# model1 <- glm(y ~ age, family = binomial(link = 'logit'))
# summary(model1)
# pchisq(2537, 1, lower.tail = FALSE)
# model2 <- glm(y ~ sex, family = binomial(link = 'logit'))
# summary(model2)
# pchisq(2485, 1, lower.tail = FALSE)
# model3 <- glm(y ~ hours.per.week, family = binomial(link = 'logit'))
# summary(model3)
# pchisq(2591, 1, lower.tail = FALSE)
#
# Step 2 - Backwards Elimination
model4 <- glm(y ~ age + sex, family = binomial(link = 'logit'))
summary(model4)
pchisq(2248, 1, lower.tail = FALSE)
model5 <- glm(y ~ age + hours.per.week, family = binomial(link = 'logit'))
summary(model5)
pchisq(2595, 1, lower.tail = FALSE)
model6 <- glm(y ~ sex + hours.per.week, family = binomial(link = 'logit'))
summary(model6)
pchisq(1744, 1, lower.tail = FALSE)
model7 <- glm(y ~ age + sex + hours.per.week, family = binomial(link = 'logit'))
summary(model7)
pchisq(1811, 1, lower.tail = FALSE)
pchisq(1464, 1, lower.tail = FALSE)
pchisq(2367, 1, lower.tail = FALSE)
# 
# Step 4 - Checking for Interactions
model8 <- glm(y ~ age + sex + hours.per.week + age*sex, family = binomial(link = 'logit'))
summary(model8)
pchisq(64, 1, lower.tail = FALSE)
model9 <- glm(y ~ age + sex + hours.per.week + age*hours.per.week, family = binomial(link = 'logit'))
summary(model9)
pchisq(7, 1, lower.tail = FALSE)
model10 <- model10 <- glm(y ~ age + sex + hours.per.week + sex*hours.per.week, family = binomial(link = 'logit'))
summary(model10)
pchisq(13, 1, lower.tail = FALSE)
model11 <- glm(y ~ age + sex + hours.per.week +
                 age*sex +
                 age*hours.per.week +
                 sex*hours.per.week, family = binomial(link = 'logit'))
summary(model11)
pchisq(22, 2, lower.tail = FALSE)
pchisq(79, 2, lower.tail = FALSE)
pchisq(73, 2, lower.tail = FALSE)

y <- adults$income

# With interaction

model.int <- glm(income ~ . -relationship -native.country -education  + as.numeric(education) 
                 + ocupation + sex + ocupation*sex, family = binomial(link='logit'), data = adults)
summary(model.int)

# without interaction
model.int2 <- glm(income ~ . -relationship -native.country -education  + as.numeric(education) 
                  + ocupation + sex, family = binomial(link='logit'), data = adults)
summary(model.int2)


# Likelihood ratio test 
require(lmtest)
lrtest(model.int2, model.int)

# Anova
# H0 = interaction betas = 0
# Ha = atleast one interaction beta not equals to 0
anova(model.int2, model.int, test = 'Chisq')
# Reject Ho p-value < 0.05

# Wald test 
waldtest(model.int2, model.int)
# p-value = 0.077, is slight evidence 
# of being statistically meaningful for the model

# Deviance Goodness of Fit
# H0 = Model correctly specified 
# Ha = Saturated model is correctly specified
pchisq(30240, 45174, lower.tail = FALSE)
# Fail to reject null hypothesis because p-value > 0.05

# Hosmer-Lemeshow Test
library(ResourceSelection)
# H0 = interaction betas = 0
# Ha = atleast one interaction beta not equals to 0
hoslem.test(model.int$y, fitted(model.int))
# Reject H0

# Classification Report on Train  -----------------------------------------
library(caret)
model.train <- glm(income ~ . -relationship -native.country -education  + as.numeric(education)
                   + ocupation*sex, family = binomial(link = 'logit'), data = train)
summary(model.train)


model.int <- glm(income ~ . -relationship -native.country -education  + as.numeric(education) 
                 + ocupation + sex + ocupation*sex, family = binomial(link='logit'), data = adults)
summary(model.int)


predictions <- predict(model.train, test, type = 'response')

# confusion matrix
table_mat <- table(test$income, predictions > 0.5)
table_mat

# Accuracy test 
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

# ROC curve
library(ROCR)
ROCRpred <- prediction(predictions, test$income)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7)) 




# Baseline Model ----------------------------------------------------------
##### Income based on sex and race 
# library(VGAM)
# model_1 <- vglm(race ~ sex + income, family = multinomial, data = adults)
# model_1
# 
# model_2 <- vglm(race ~ sex, family = multinomial, data = adults)
# model_2
# 
# pchisq(366.74, 8, lower.tail = FALSE)
# 
# str(adults)
# cor(adults)
# package - interaction: cat_plot

