# Regression Modeling
# Session 1

# this will create a new folder
dir.create()

library(tidyverse)
install.packages("car")
library(car)
search()

students <- read_csv("data/cassady_finch_bolin.csv")

model_gpa_anxiety <- lm(GPA ~ CTA.tot + BStotal, data = students)
