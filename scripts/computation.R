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






