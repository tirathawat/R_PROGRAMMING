library(tidyverse)

err <- c()

for (i in 1:7) {
  N <- 10 ** i
  x <- runif(N)
  y <- runif(N)
  res <- mean(x ** 2 + y ** 2 < 1) * 4
  print(res)
  err <- c(err, abs(pi - res))
}

graph <- ggplot() +
  geom_point(aes(1:7, err)) + 
  geom_smooth(aes(1:7, err),method = 'lm') +
  scale_x_log10() + 
  scale_y_log10()




