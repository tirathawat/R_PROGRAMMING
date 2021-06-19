library(tidyverse)
library(readr)

# Load superstore data set
superstore <- read_delim("superstore.csv", 
                         "|", escape_double = FALSE, trim_ws = TRUE)
summary(superstore)

# In which region, products in each subcategory were the most profitable to sell?
question1 <- superstore %>% 
  group_by(`Sub-Category`, `Region`) %>% 
  summarise(total_profit = sum(Profit)) %>% 
  arrange(desc(total_profit)) %>%
  ggplot() +
  geom_col(mapping = aes(x = `Sub-Category`, y = total_profit, fill = `Region`), position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total profit per sub category", 
          subtitle = "In which region, products in each subcategory were the most profitable to sell?") + 
  xlab("Sub Category") + ylab("Total Profit")
question1

# What is the proportion of total sales in each region?
question2 <- superstore %>% 
  group_by(`Sub-Category` ,`Region`) %>% 
  summarise(total_Sales = sum(Sales)) %>% 
  arrange(desc(total_Sales)) %>%
  ggplot() +
  geom_col(mapping = aes(x = `Sub-Category`, y = total_Sales, fill = `Region`), position = 'fill') +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Percentage of total sales in each region", 
          subtitle = "What is the proportion of total sales in each region?") + 
  xlab("Sub Category") + ylab("Percentage")
question2

# What is the average number of orders in each product sub category?
question3 <- superstore %>% 
  group_by(`Sub-Category`) %>% 
  summarise(mean_quantity = mean(`Quantity`)) %>%
  ggplot() + 
  geom_point(aes(x = `Sub-Category`, y = mean_quantity)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Mean quantity per sub cetagory", 
          subtitle = "What is the average number of orders in each product sub category?") + 
  xlab("Sub Category") + ylab("Mean Quantity")
question3



  





