library(readxl)
library(tidyverse)

data <- read_delim("superstore.csv", 
                   "|", escape_double = FALSE, trim_ws = TRUE)

# 1


data %>%
  group_by(`Order Date`, `Order ID`) %>% 
  summarise(n = 1) %>%
  summarise(n = sum(n)) -> orderByDate

ggplot(orderByDate) + geom_histogram(aes(x = n), bins = 18)

# 2

data %>% 
  mutate(ProcessingTime = as.Date(`Ship Date` ) - as.Date(`Order Date`)) %>%
  group_by(`Category`) %>% 
  summarise(TimeBetweenOrder = mean(ProcessingTime)) %>% 
  ggplot() + geom_col(aes(x = Category, y = TimeBetweenOrder))

data %>% 
  mutate(ProcessingTime = as.Date(`Ship Date` ) - as.Date(`Order Date`)) %>%
  group_by(`Category`, `Order ID`) %>% 
  summarise(n = 1, ProcessingTime = ProcessingTime, Category = Category) -> a

a %>% ggplot() + geom_bar(aes(ProcessingTime, fill = Category), position = "dodge")

# 3
data %>% 
  mutate(ProcessingTime = as.Date(`Ship Date` ) - as.Date(`Order Date`)) %>%
  select(ProcessingTime) %>% 
  ggplot() + geom_density(aes(x = ProcessingTime))

data %>% 
  mutate(ProcessingTime = as.Date(`Ship Date` ) - as.Date(`Order Date`)) %>%
  select(ProcessingTime) %>% 
  ggplot() + geom_histogram(aes(x = ProcessingTime), bins = 5)

# 4
data %>% 
  mutate(ProcessingTime = as.Date(`Ship Date` ) - as.Date(`Order Date`)) %>%
  select(ProcessingTime) -> OrderTime
# Sampling
ceiling(quantile(OrderByDate$n, runif(10))) -> NumberOfOrder
quantile(OrderTime$ProcessingTime, runif(10)) -> ProcessingTime
d <- data.frame(
  NumberOfOrder,
  ProcessingTime
)
mean(NumberOfOrder)
mean(ProcessingTime)


# 5
data %>% 
  group_by(`Order ID`) %>% 
  mutate(ProcessingTime = as.Date(`Ship Date` ) - as.Date(`Order Date`)) %>% 
  summarise(OrderSize = n(), ProcessingTime = sum(ProcessingTime)) %>% 
  group_by(OrderSize) %>% 
  summarise(AvgTime = mean(ProcessingTime)) %>% 
  ggplot() + geom_smooth(aes(x = OrderSize, y = AvgTime), method='lm')

# 6 
data %>%
  mutate(Day = format(as.Date(`Order Date`), format="%a")) %>% 
  group_by(Day) %>% 
  summarise(n = n(), Profit = sum(Profit)) %>% 
  filter(Day == 'Fri') -> rawFriday

data %>%
  mutate(Day = format(as.Date(`Order Date`), format="%a")) %>% 
  group_by(Day) %>%
  summarise(n = n(), Profit = sum(Profit)) %>% 
  mutate(n = ifelse(Day == 'Fri', n * 2, n)) %>% 
  mutate(Profit = ifelse(Day == 'Fri', Profit * 2, Profit)) %>% 
  filter(Day == 'Fri') %>% 
  mutate(Day = 'Boosted') -> boosted

rbind(rawFriday, boosted) %>% 
  ggplot() + geom_col(aes(x = Day, y = Profit))