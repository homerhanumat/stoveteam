library(dplyr)
library(ggplot2)

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
