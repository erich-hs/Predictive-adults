library(dplyr)
library(mice)
library(naniar)
library(VIM)

adult.all <- read.csv("C:/Users/aran_/Desktop/Langara/1. Courses/Term 03/5. DANA 482 - Predictive Analysis/2. Coursework/Group Project/Data/Lucas/adult-all.csv", na.strings="?", stringsAsFactors=TRUE)
View(adult.all)

dim(adult.all)
str(adult.all)

adult.all %>% 
  sapply(levels)

adult.all %>% 
  sapply(summary)

rename(adult.all, col)
names(adult.all) <- c('Age', 'Work.Class', 'financial.weight', 'education', 'education-num', 'marital-status', 'work.role', 'familial.member', 'ethnicity', 'gender', 'capital.gain', 'capital.loss', 'hours.per.week', 'native.country', 'earnings')

# Missing values visual - VIM
aggr_plot <- aggr(adult.all, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(adult.all), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Missing values visual and summary- narniar
gg_miss_upset(adult.all)

gg_miss_upset(adult.all, 
              nsets = 15,
              nintersects = 100)

adult.all %>%
  miss_var_summary()

  # Row wise
# gg_miss_case_cumsum(adult.all,
#                     breaks = 1500)

