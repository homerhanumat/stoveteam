library(tidyverse)

mu <- 5
sb <- 1
sw <- 1
J <- 4
n <- 16

get_sample_df <- function(mu, sb, sw, J, n) {
  means <- rnorm(n, mean = mu, sd = sb)
  group_xbars <- numeric(n)
  for (i in 1:n) {
    group_means[i] <- mean(rnorm(J, mean = means[i], sd = sw))
  }
  
}

get_estimate <- function() {
  
}

sims <- numeric()