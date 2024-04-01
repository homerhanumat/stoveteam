library(tidyverse)
library(readxl)

## imnport summary data
## (this script assumes ctive directory is root dir of project):
summary_data <- read_excel(
  "st_files/For Review_Ret_Justa_July KPT Complete_KPT_4day (1).xlsx", 
  range = "J22:Q38"
)

names(summary_data) <-
  c(
    "per_cap_mean",
    "weighted_per_cap_mean",
    "sd",
    "var",
    "weighted_var",
    "days_measured",
    "df",
    "cov"
  )



## function to compute confidence interval from summary data
## as per:
## https://stats.stackexchange.com/questions/12002/how-to-calculate-
## the-confidence-interval-of-the-mean-of-means

find_replicates <- function(fn, n) {
  replicates <- vector(mode = "list", length = n)
  for (i in 1:length(lst)) {
    vals <- read_excel(
      path = fn,
      range = "I15:L15",
      sheet = paste0("HH", i, " Data"),
      col_names = FALSE
    ) %>% 
      as.matrix() %>% 
      t() %>% 
      .[,1]
    replicates[[i]] <- vals[!is.na(vals)]
  }
  replicates
}

measures <- find_replicates(
  fn = "st_files/For Review_Ret_Justa_July KPT Complete_KPT_4day (1).xlsx",
  n = nrow(summary_data)
)

compute_ci_1 <- function(data, level, use = NULL, r) {
  n <- nrow(data)
  sample_mean <- sum(data$per_cap_mean) / n
  multiplier <- qnorm((1 + level) / 2)
  if (is.null(use)) {
    ss_between <- sum((sample_mean - data$per_cap_mean)^2)
  } else {
    avgs <- numeric(length(r))
    for (i in 1:length(r)) {
      avgs[i] <- mean(r[[i]][1:use])
    }
    ss_between <- sum((sample_mean - avgs)^2)
  }
  margin <- multiplier * sqrt(ss_between / (n * (n - 1)))
  list(
    point_estimate = sample_mean, 
    margin = margin,
    interval = c(
      sample_mean - margin,
      sample_mean + margin
    )
  )
}

## try it out:
res <- compute_ci_1(
  data = summary_data, 
  level = 0.90,
  use = 3,
  r = measures
)
res

## might want to plot those means:
ggplot(summary_data, aes(x = per_cap_mean)) +
  geom_density(fill = "skyblue") +
  geom_rug()

## Simulate to see size of ss_b as replications incfease
sim_ssb <- function(sb, sw, mu, n, repeats, reps = 1000) {
  sims <- numeric(reps)
  for (i in 1:reps) {
    household <- rnorm(n, mean = mu, sd = sb)
    avgs <- numeric(n)
    for (j in 1:n) {
      avgs[j] <- mean(rnorm(repeats[j], mean = household[j], sd = sw))
    }
    xbb <- mean(avgs)
    sims[i] <- sum((xbb - avgs)^2)
    tausq <- sb^2 + sw^2 / repeats
  }
  df <- data.frame(x = sims / tausq)
  p <- ggplot(df, aes(x = x)) +
    geom_density(fill = "burlywood")
  print(p)
  invisible(list(sims = sims, mean = mean(df$x)))
}

res <- sim_ssb(
  sb = 2,
  sw = 5,
  mu = 5,
  n = 10,
  repeats = c(rep(4, 9), rep(3, 7)),
  reps = 10000
)
res$mean



## hmm, pretty strong evidence of skewness here
#3 and n = 16 sample size is small
#3 perhaps bootstrap instead?

lst <- vector(mode = "list", length = nrow(summary_data))
for (i in 1:length(counts)) {
  vals <- read_excel(
    "st_files/For Review_Ret_Justa_July KPT Complete_KPT_4day (1).xlsx", 
    range = "I15:L15",
    sheet = paste0("HH", i, " Data"),
    col_names = FALSE
  ) %>% 
    as.matrix() %>% 
    t() %>% 
    .[,1]
  vals <- vals[!is.na(vals)]
  lst[[i]] <- vals
}

resampled_means <- function(data) {
  n <- length(data)
  means <- numeric(n)
  for (i in 1:n) {
    grp <- data[[sample(1:n, size = 1)]]
    means[i] <-
      sample(grp, size = length(grp), replace = TRUE) %>% 
      mean()
  }
  means
}

## try it:
resampled_means(data = lst)

bootstrap_resamples <- function(m, data) {
  resamps <- numeric(m)
  for (i in 1:m) {
    resamps[i] <- mean(resampled_means(data = data))
  }
  resamps
}

resamps <- bootstrap_resamples(2000, data = lst)

level <- 0.90
interval <- quantile(
  resamps, 
  probs = c((1-level) / 2, (1 + level) / 2)
)

interval

m <- mean(resamps)
se <- sqrt(sum((m - resamps)^2) / length(resamps))
se

ggplot(data.frame(resamps), aes(x = resamps)) +
  geom_density()
