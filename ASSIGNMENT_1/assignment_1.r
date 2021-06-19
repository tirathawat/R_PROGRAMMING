library(tidyverse)
options(pillar.sigfig = 5)

# Load two data frames "flights.csv and airlines.csv"
flights <- read.csv('flights.csv')
airlines <- read.csv('airlines.csv')

# Select two columns (carrier, dep_delay)
flights <- select(flights, carrier, dep_delay)

# Filter NA out of dep_delay
flights <- drop_na(flights, dep_delay)

# Use carrier as a group and calculate mean of dep_delay
flights <- group_by(flights, carrier) %>% 
  summarise(mean_delay = mean(dep_delay))

# Sort the worst carrier by departure delay (dep_delay)
flights <- arrange(flights, desc(mean_delay))

# Join airlines data to flight data using 'carrier' as a key
result_data <- left_join(flights, airlines, by = 'carrier')

# Show the result
print.data.frame(result_data)


