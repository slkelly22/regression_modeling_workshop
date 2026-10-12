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

sum(complete.cases(students))

students |> 
  drop_na(GPA, CTA.tot, BStotal) |> 
  distinct(GPA, CTA.tot, BStotal) |> 
  nrow()

# model the outcome variable
ggplot(students, aes(x = GPA)) + geom_histogram() + xlim(1, 5) # -0.303901


cor(students$GPA, students$CTA.tot, use = "pairwise.complete.obs")

test <- students |> 
  filter(GPA <= 5)

cor(test$GPA, test$CTA.tot, use = "pairwise.complete.obs") # same b/c there's NA on CTA.tot for the person with the 300 GPA

cor(students$GPA, students$BStotal, use = "pairwise.complete.obs") # -0.04030298
cor(test$GPA, test$BStotal, use = "pairwise.complete.obs") # -0.136426, stronger correlation without the OUTLIER