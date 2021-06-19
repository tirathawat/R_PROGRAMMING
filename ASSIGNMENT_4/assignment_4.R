library(tidyverse)
library(readr)
# Load superstore data set
market <- read_delim("superstore.csv", 
                         "|", escape_double = FALSE, trim_ws = TRUE)


market  %>% filter(Sales < 600)%>%
  ggplot() + 
  geom_histogram(aes(x = Sales), bins = 13) 

market %>% filter(Sales < 600)%>%
  group_by(`Sales`) %>%
  summarise(count = n()) -> gan
cdf <- ecdf(gan$count)
plot(cdf)

market %>% 
  ggplot(aes(x = Sales)) +
  stat_ecdf()


quantile(market$Sales, runif(10))
