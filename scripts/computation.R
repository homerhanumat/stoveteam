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

find_replicate_counts <- function(fn, n) {
  counts <- numeric(n)
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
    vals <- vals[!is.na(vals)]
    counts[i] <- length(vals)
  }
  counts
}

counts <- find_replicate_counts(
  fn = "st_files/For Review_Ret_Justa_July KPT Complete_KPT_4day (1).xlsx",
  n = nrow(summary_data)
)

compute_ci_1 <- function(data, level, replications) {
  n <- nrow(data)
  sample_mean <- sum(data$per_cap_mean) / n
  ss_between <- sum((sample_mean - data$per_cap_mean)^2)
  multiplier <- qnorm((1 + level) / 2)
  compute_margin <- function(j) {
    multiplier * sqrt(ss_between / (n * (n - 1) * j))
  }
  list(
    point_estimate = sample_mean, 
    margins = c(
      compute_margin(max(replications)),
      compute_margin(min(replications))
    ),
    interval = c(
      sample_mean - compute_margin(min(replications)),
      sample_mean + compute_margin(min(replications))
    )
  )
}

## try it out:
res <- compute_ci_1(
  data = summary_data, 
  level = 0.90,
  replications = counts
)
res

## might want to plot those means:
ggplot(summary_data, aes(x = per_cap_mean)) +
  geom_density(fill = "skyblue") +
  geom_rug()

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
