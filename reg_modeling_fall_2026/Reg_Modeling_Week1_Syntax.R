# Regression Modeling in R 
# Week 1 | Fall 2026

# load tidy
library(tidyverse)

# change the working directory to the fall 2026 folder
setwd("reg_modeling_fall_2026")
getwd()

# read in the cassady data (students will need to download and add to their directory)
students <- read_csv("data/cassady_finch_bolin.csv")
head(students, 4)

summary(students)

# model the outcome variable
ggplot(students, aes(x = GPA)) + geom_histogram() + xlim(1, 5) 

# correlations
cor(students$GPA, students$CTA.tot, use = "pairwise.complete.obs") # -0.303901

test <- students |> 
  filter(GPA <= 5)

cor(test$GPA, test$CTA.tot, use = "pairwise.complete.obs") # same b/c there's NA on CTA.tot for the person with the 300 GPA

cor(students$GPA, students$BStotal, use = "pairwise.complete.obs") # -0.04030298
cor(test$GPA, test$BStotal, use = "pairwise.complete.obs") # -0.136426, stronger correlation without the OUTLIER

cor(students$CTA.tot, students$BStotal, use = "pairwise.complete.obs") # the IVS are actually strongly correlated #0.709 

## scatterplot with geom_smooth
# default is loess smooth if n < 1000
ggplot(students, aes(x = CTA.tot, y = GPA)) + geom_point() + 
  geom_smooth() + ylim(0,4)

# with OLS lm method
ggplot(students, aes(x = CTA.tot, y = GPA)) + geom_point() + 
  geom_smooth(method = "lm") + ylim(0,4)

# with 95% CI off
ggplot(students, aes(x = CTA.tot, y = GPA)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + ylim(0,4)

# building an linear model
model_gpa_anxiety <- lm(GPA ~ CTA.tot + BStotal, data = students)
class(model_gpa_anxiety)

summary(model_gpa_anxiety) 
