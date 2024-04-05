library(tidyverse)
library(readxl)

## DAata Import ----

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

## Eivory's App ----
## material for an app to demonstrate that
## the eman per-capita daily usage varies from
#3 household to household

## this function returns a data frame
#3 where the usages have been randomly
## scrambled:
resample_data <- function() {
  rs <-data.frame(
    house = all_meas$house,
    wood = sample(
      all_meas$wood,
      size = length(all_meas$wood),
      replace = FALSE
    )
  )
}

density_with_rug <- function(df) {
  ggplot(df, aes(x = household_mean)) +
    geom_density(fill = "skyblue") +
    geom_rug() +
    xlim(c(0,8)) +
    labs(x = "household means of daily per-capita usage")
}

rs_means <- 
  resample_data() %>% 
  group_by(house) %>% 
  summarise(household_mean = mean(wood))
density_with_rug(df = rs_means)

## note how they are much ;ess spread out that
## the original household means:
sample_means <- 
  all_meas %>% 
  group_by(house) %>% 
  summarise(household_mean = mean(wood))
density_with_rug(df = sample_means)

## see this numerically, too:
summary_stats <- function(vals) {
  c(
    mean = mean(vals),
    stdev = sd(vals)
  )
}

rs_means <- 
  resample_data() %>% 
  group_by(house) %>% 
  summarise(household_mean = mean(wood))
summary_stats(vals = rs_means$household_mean)

## as compared with:
summary_stats(vals = sample_means$household_mean)

## Simple App to Illustrate Variability ----
## Dr. White should write this as an app in the 
## section on sampling

mean <- 70
sd <- 3
n <- 20

random_sample <- rnorm(n, mean = mean, sd = sd)

density_plots <- function(mean, sd, sample) {
  
  sample_mean <- mean(sample)
  
  ## set uop data frames for plot:
  sample_df <- data.frame(
    x = sample
  )
  curve_df <- data.frame(
    x = c(mean - 3 * sd, mean + 3 * sd)
  )
  vline_df <- data.frame(
    x = c(mean, sample_mean),
    mean = c("population", "sample")
  )
  
  ## construct plot object
  p <-
    curve_df %>% 
    ggplot(aes(x = x)) +
    stat_function(
      fun = dnorm, 
      n = 101, 
      args = list(mean = mean, sd = sd),
      lwd = 2
    ) + 
    ylab("") +
    xlab("sample values") +
    scale_x_continuous(breaks = c(mean - sd, mean, mean + sd)) +
    scale_y_continuous(breaks = NULL) +
    ## switch data to sample to make denisty plot:
    geom_density(
      data = sample_df, 
      aes(x = x), 
      fill = "skyblue",
      alpha = 0.5
    ) +
    ## now the rug, again using sample data:
    geom_rug(data = sample_df) +
    ## verticla lines show population and sample means:
    geom_vline(data = vline_df, aes(xintercept = x, color = mean)) +
    ## strip away distracting background and grids:
    theme_bw() +
    theme(axis.line = element_line(color='black'),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())
  ## return plot for printing:
  p
}

density_plots(mean, sd, random_sample)
