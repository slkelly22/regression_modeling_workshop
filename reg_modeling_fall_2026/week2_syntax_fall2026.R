library(tidyverse)

getwd()
students <- read_csv("reg_modeling_fall_2026/data/cassady_finch_bolin.csv")

students_clean <- students |> 
  mutate(GPA = if_else(GPA > 5, 3.02, GPA))

students_clean |> 
  filter(GPA > 5) 

students_clean |> 
  ggplot(aes(x = GPA)) + geom_histogram() 

students_clean <- students_clean |> 
  filter(GPA > 0)

students_clean |> 
  ggplot(aes(x = GPA)) + geom_histogram() 

model_gpa_anxiety <- lm(GPA ~ CTA.tot + BStotal, data = students_clean)
summary(model_gpa_anxiety)

options(scipen = 999)
summary(model_gpa_anxiety)

model_gpa_anxiety2 <- lm(GPA ~ CTA.tot + BStotal + CTA.tot:BStotal, 
                         data = students_clean)
summary(model_gpa_anxiety2)

anova(model_gpa_anxiety, model_gpa_anxiety2)

students_clean |> 
  ggplot(aes(x = GPA)) + geom_histogram() + 
  facet_wrap(~ Gender)

students_clean$gender_factor <- factor(students_clean$Gender, 
                                       levels = c(1, 2), 
                                       labels = c("Female", "Male"))

students_clean |> 
  ggplot(aes(x = GPA)) + geom_histogram() + 
  facet_wrap(~ gender_factor)


model_gpa_anxiety_gender2 <- lm(GPA ~ CTA.tot + BStotal + gender_factor, 
                                data = students_clean)
summary(model_gpa_anxiety_gender2)

students_clean <- students_clean |> 
  mutate(adult_group = case_when(
    Age <= 19 ~ "young",
    Age > 19 & Age <= 21 ~ "mid",
    Age > 21 ~ "older"))

GPA_adult <- lm(GPA ~ adult_group, data = students_clean)
summary(GPA_adult)

GPA_adult$coefficients

students_clean$adult_fac <- factor(students_clean$adult_group, 
                                   levels = c("young", "mid", "older"))

GPA_adult2 <- lm(GPA ~ adult_fac, data = students_clean)
summary(GPA_adult2)

library(emmeans)
emmeans(GPA_adult2, pairwise ~ adult_fac)

