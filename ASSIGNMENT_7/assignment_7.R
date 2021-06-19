library(tidyverse)
library(caret)

# Using the credit approval data set and prepare the data from modeling
data <- read.csv("credit-approval.csv", stringsAsFactors=T, na.strings= '?')
names(data) <- c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10", "A11","A12","A13","A14","A15","A16")
data <- na.omit(data)

#build a logistic regression model to predict the class A16
data %>% sample_frac(0.1) -> data_train
model <- glm(A16 ~ ., data_train, family = binomial)
res <- predict(model, data)
res_c <- factor(ifelse(res > 0.2, "+", "-"))
confusionMatrix(res_c, data$A16, mode = "prec_recall", positive= "+")


