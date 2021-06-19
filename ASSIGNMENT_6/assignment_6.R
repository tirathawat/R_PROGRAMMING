library(tidyverse)

data <- read.csv('L8-demand.csv', sep = ';')


cor(data$Target..Total.orders., data$Urgent.order) 
model1 <- lm(Target..Total.orders. ~ Urgent.order, data)
summary(model1) 

cor(data$Target..Total.orders., data$Order.type.C)
model2 <- lm(Target..Total.orders. ~ Order.type.C, data)
summary(model2)

cor(data$Target..Total.orders., data$Non.urgent.order)
model3 <- lm(Target..Total.orders. ~ Non.urgent.order, data)
summary(model3) 

model4 <- lm(Target..Total.orders. ~ Non.urgent.order + Order.type.B, data)
summary(model4)

model5 <- lm(Target..Total.orders. ~ Non.urgent.order + Orders.from.the.traffic.controller.sector, data)
summary(model5)

model6 <- lm(Target..Total.orders. ~ Non.urgent.order + Order.type.B + Banking.orders..3., data)
summary(model6) 

model7 <- lm(Target..Total.orders. ~ Non.urgent.order + Order.type.B + Fiscal.sector.orders, data)
summary(model7) 

model8 <- lm(Target..Total.orders. ~ Non.urgent.order + Order.type.B + Banking.orders..1., data)
summary(model8) 


