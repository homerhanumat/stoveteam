library(tidyverse)
library(readxl)

## DATA IMPORT ----

## imnport summary data
## (this script assumes active directory is root dir of project):
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

## import all measurements on each household:

get_house <- function(i) {
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
  data.frame(
    house = rep(i, length(vals)),
    wood = vals
  )
}

all_meas <- 
  map_dfr(factor(1:16), get_house) %>% 
  group_by(house) %>% 
  mutate(mean_percap = mean(wood)) %>% 
  mutate(deviation = wood - mean_percap)

## Need Hierarchical Model ----


## should we make household a random effect?
## (someone could claim that it's reasonble to assume
## the per capita mean is the same for all households)

library(nlme)
mod_null <- lm(wood ~ 1, data = all_meas)
mod_re <- nlme::lme(wood ~ 1, random = ~ 1 | house, data = all_meas)
res <- anova.lme(mod_re, mod_null)
res
res$`p-value`[2]

## clearly, we need to think of household as a random effect!


## Confidence Interval (Parametric) ----

## function to compute confidence interval from summary data
## as per:
## https://stats.stackexchange.com/questions/12002/how-to-calculate-
## the-confidence-interval-of-the-mean-of-means
## (Nut fixing their error)

group_measures <- function() {
  grps <- unique(all_meas$house)
  map(
    grps, 
    .f = function(grp) all_meas %>% filter(house == grp) %>% pull(wood))
}

compute_ci_1 <- function(data, level, use = NULL) {
  n <- nrow(data)
  multiplier <- qnorm((1 + level) / 2)
  if (is.null(use)) {
    sample_mean <- sum(data$per_cap_mean) / n
    ss_between <- sum((sample_mean - data$per_cap_mean)^2)
  } else {
    grps_list <- group_measures()
    avgs <- numeric(length(grps_list))
    for (i in 1:length(grps_list)) {
      avgs[i] <- mean(grps_list[[i]][1:use])
    }
    sample_mean <- sum(avgs) / n
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
  use = 3
)
res

res_all <- compute_ci_1(
  data = summary_data, 
  level = 0.90
)
res_all

## Checking some assumptions ----

## might want to plot those means:
ggplot(summary_data, aes(x = per_cap_mean)) +
  geom_density(fill = "skyblue") +
  geom_rug()
## evidence for strong skewness, a problem


## are the deviations about the household means normal?
ggplot(all_meas, aes(x = deviation)) +
  geom_density(fill = "skyblue") +
  geom_rug()



## Simulate to see effect of departure from
## same number of measures per household
## on the t-statistic:
sim_t <- function(sb, sw, mu, repeats, reps = 1000) {
  n <- length(repeats)
  sims <- numeric(reps)
  for (i in 1:reps) {
    household <- rnorm(n, mean = mu, sd = sb)
    avgs <- numeric(n)
    for (j in 1:n) {
      avgs[j] <- mean(rnorm(repeats[j], mean = household[j], sd = sw))
    }
    xbb <- mean(avgs)
    ssb <- sum((xbb - avgs)^2)
    t <- (xbb - mu)/sqrt(ssb / (n*(n-1)))
    sims[i] <- t
  }
  df <- data.frame(x = sims)
  p <- ggplot(df, aes(x = x)) +
    geom_density(fill = "burlywood") +
    stat_function(
      fun = dt, n = 101, 
      args = list(df = n - 1), 
      color = "red"
    )
  print(p)
  invisible(list(sims = sims, mean = mean(df$x)))
}

res <- sim_t(
  sb = 2,
  sw = 2,
  mu = 3.3,
  repeats = c(rep(4, 8), rep(300, 12)),
  reps = 10000
)
res$mean


## what about this possibly-skewed pop of household means?
dev_sq <- numeric(nrow(summary_data))
js <- numeric(nrow(summary_data))
for (i in 1:nrow(summary_data)) {
  vals <- read_excel(
    "st_files/For Review_Ret_Justa_July KPT Complete_KPT_4day (1).xlsx", 
    range = "I15:L15",
    sheet = paste0("HH", i, " Data"),
    col_names = FALSE
  ) %>% 
    as.matrix() %>% 
    t() %>% 
    .[, 1]
  vals <- vals[!is.na(vals)]
  m <- mean(vals)
  dev_sq[i] <- sum((vals - m)^2)
  js[i] <- length(vals)
}

sw <- sqrt(sum(dev_sq) / sum(js - 1))

sim_t2 <- function(sw, repeats, level, reps = 1000) {
  mu <- mean(summary_data$per_cap_mean)
  n <- length(repeats)
  sims <- numeric(reps)
  good <- numeric(reps)
  for (i in 1:reps) {
    household <- sample(
      summary_data$per_cap_mean,
      size = n,
      replace = TRUE)
    avgs <- numeric(n)
    for (j in 1:n) {
      avgs[j] <- mean(
        rnorm(repeats[j], 
              mean = household[j] + rnorm(1, sd = 0.5), 
              sd = sw)
      )
    }
    xbb <- mean(avgs)
    ssb <- sum((xbb - avgs)^2)
    t <- (xbb - mu)/sqrt(ssb / (n*(n-1)))
    sims[i] <- t
    crit <- qt((1 + level) / 2, df = n - 1)
    margin <- crit * sqrt(ssb / (n*(n-1)))
    good[i] <- mu > xbb - margin & mu < xbb + margin
  }
  df <- data.frame(x = sims)
  p <- ggplot(df, aes(x = x)) +
    geom_density(fill = "burlywood") +
    stat_function(
      fun = dt, n = 101, 
      args = list(df = n - 1), 
      color = "red"
    )
  print(p)
  print(mean(good))
  invisible(list(sims = sims, mean = mean(df$x)))
}

sim_t2(
  sw = sw,
  repeats = c(rep(4, 9), rep(3, 7)),
  level = 0.90,
  reps = 10000
)

## bootstrap ----

## get all of the data:
library(tidyverse)
library(readxl)
get_house <- function(i) {
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
  data.frame(
    house = rep(i, length(vals)),
    wood = vals
  )
}

all_meas <- 
  map_dfr(factor(1:16), get_house) %>% 
  group_by(house) %>% 
  mutate(mean_percap = mean(wood)) %>% 
  mutate(deviation = wood - mean_percap)

## estimate within-household variance:
sd_w <-
  all_meas %>% 
  group_by(house) %>% 
  mutate(house_mean = mean(wood)) %>% 
  summarize(n = n(), sum_sq = sum((wood - house_mean)^2)) %>% 
  mutate(deg_freedom = n - 1) %>% 
  summarize(var = sum(sum_sq * deg_freedom) / sum(deg_freedom)) %>% 
  pull(var) %>% 
  sqrt()

## get household means and measurement-numbers:
house_info <-
  all_meas %>% 
  group_by(house) %>% 
  summarize(mean = mean(wood), J = n())

## get resamples
B <- 4999
resamples <- numeric(B)
n <- nrow(house_info)
houses <- numeric(n)
for (i in 1:B) {
  resampled_houses <-
    house_info[sample(1:n), size = n]
  resampled_mean <-
    resampled_houses %>% 
    mutate(mean_rs = mean(rnorm(J, mean = mean, sd = sd_w))) %>% 
    summarize(xbar = mean(mean_rs)) %>% 
    pull(xbar)
  resamples[i] <- resampled_mean
}

## compute a percentile bootstrap interval:
level <- 0.90
interval <- quantile(resamples, probs = c(0.05, 0.95))
