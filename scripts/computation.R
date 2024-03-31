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

## function to compute confidence interval from summary data:
compute_ci <- function(data, level) {
  pooled_var <- with(
    data,
    sum(var * df) / sum(df)
  )
  
  sample_mean <- with(
    data,
    sum(per_cap_mean * days_measured) / sum(days_measured)
  )
  
  n <- sum(data$days_measured)
  df <- n - nrow(data)
  multiplier <- qt((1 + level) / 2, df = df)
  margin <- multiplier * sqrt(pooled_var / n)
  interval <-
    c(
      lower = sample_mean - margin, 
      upper = sample_mean + margin
  )
  list(point_estimate = sample_mean, interval = interval)
}


## try it out:
compute_ci(data = summary_data, level = 0.90)

## this one follows a better probability model:
compute_ci_2 <- function(data, level) {
  n <- nrow(data)
  sample_mean <- sum(data$per_cap_mean) / n
  sd_sample_mean <- sqrt(sum(data$var) / n)
  multiplier <- qnorm((1 + level) / 2)
  margin <- multiplier * sd_sample_mean
  interval <-
    c(
      lower = sample_mean - margin, 
      upper = sample_mean + margin
    )
  list(
    point_estimate = sample_mean, 
    sd = sd_sample_mean,
    interval = interval
  )
}

## try it out:
compute_ci_2(data = summary_data, level = 0.90)

## might want to plot those means:
ggplot(summary_data, aes(x = per_cap_mean)) +
  geom_density(fill = "skyblue") +
  geom_rug()

## hmm, pretty strong evidence of skewness here
#3 and n = 16 sample size is small
#3 perhaps bootstrap instead?

lst <- vector(mode = "list", length = nrow(summary_data))
for (i in 1:length(lst)) {
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
  probs = c(level / 2, (1 + level) / 2)
)

interval

m <- mean(resamps)
sd <- sqrt(sum((m - resamps)^2 / length(resamps)))
sd
