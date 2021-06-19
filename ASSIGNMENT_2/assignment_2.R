library(tidyverse)

HR_Employee_Attrition <- read.csv("HR-Employee-Attrition.csv")
summary(HR_Employee_Attrition)

data <- HR_Employee_Attrition %>%
  group_by(JobRole) %>%
  summarise(n = n()) %>%
  arrange(n)

data %>%
  ggplot(mapping = aes(x=JobRole, y = n)) + 
  geom_col()

summary(data)

ggplot(HR_Employee_Attrition, aes(MonthlyIncome)) +
  geom_histogram(bins = 40)

ggplot(HR_Employee_Attrition, aes(MonthlyIncome)) +
  geom_boxplot()

ggplot(HR_Employee_Attrition,aes(x = EducationField, y = MonthlyIncome, fill = EducationField)) + 
  geom_boxplot()
