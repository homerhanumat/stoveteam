###################################################
#  Utility Functions for Generating Pareto Values
###################################################

rpareto <- function(n,alpha,theta) {#random values for Pareto(alpha,theta) distribution
  theta*((1-runif(n))^(-1/alpha)-1)
}

dpareto <- function(x,alpha,theta) {  #pdf for Pareto(alpha,theta) distribution
  alpha*theta^alpha/(x+theta)^(alpha+1)
}


############################
# Generate the populations
############################
mu_norm <- 70
sigma_norm <- 5
shape_gamma <- 2
scale_gamma <- 50

# for pareto:
alpha_pareto <- 5
theta_pareto <- 100
tail_prob <- 0.02  #want to find a Value at risk of 1 - this
val_risk <- theta_pareto*(tail_prob^(-.5)-1)

# for pop with group of outliers
prop_outliers <- 0.10
mean_outliers <- 200
sd_outliers <- 5
mean_regulars <- 50
sd_regulars <- 5

r_outlier<- function(n) {
  prop_normals <- 1- prop_outliers
  which_hump <- rbinom(n, size = 1, prob = prop_normals)
  outlier_samp <- 
    ifelse(
      which_hump,
      rnorm(n,mean=mean_regulars,sd=sd_regulars),
      rnorm(n,mean=mean_outliers,sd=sd_outliers)
    )
  outlier_samp
}

d_outlier <- function(x) {
  reg <- dnorm(x,mean=mean_regulars,sd=sd_regulars)
  out <- dnorm(x,mean=mean_outliers,sd=sd_outliers)
  (1-prop_outliers) * reg + prop_outliers * out
}

############################
# Get the population means
############################

normal_mean <- mu_norm
skew_mean <- shape_gamma*scale_gamma
super_skew_mean <- theta_pareto/(alpha_pareto - 1)
outlier_mean <- (1-prop_outliers)*mean_regulars+prop_outliers*mean_outliers

###################
# plot population
###################

draw_pop <- function(pop = c("normal", "skew", "super_skew", "outlier")) {
  if (pop == "normal") {
    df <- data.frame(
      x = c(mu_norm - 3 * sigma_norm, mu_norm + 3 * sigma_norm)
    )
    p <-
      ggplot(df, aes(x = x)) +
      stat_function(
        fun = dnorm,
        n = 101,
        args = list(
          mean = mu_norm,
          sd = sigma_norm
        )
      ) + 
      labs(
        x = "population value",
        y = NULL
      ) +
      geom_vline(aes(xintercept = normal_mean))
  }
  if (pop == "skew") {
    df <- data.frame(
      x = c(0, shape_gamma*scale_gamma+7.5*sqrt(shape_gamma)*scale_gamma)
    )
    p <-
      ggplot(df, aes(x = x)) +
      stat_function(
        fun = dgamma,
        n = 101,
        args = list(
          shape = shape_gamma,
          scale = scale_gamma
        )
      ) + 
      labs(
        x = "population value",
        y = NULL
      ) +
      geom_vline(aes(xintercept = skew_mean))
  }
  if (pop == "super_skew") {
    df <- data.frame(
      x = c(0, val_risk)
    )
    p <-
      ggplot(df, aes(x = x)) +
      stat_function(
        fun = dpareto,
        n = 101,
        args = list(
          alpha = alpha_pareto,
          theta = theta_pareto
        )
      ) + 
      labs(
        x = "population value",
        y = NULL
      ) +
      geom_vline(aes(xintercept = super_skew_mean))
  }
  if (pop == "outlier") {
    df <- data.frame(
      x = c(0, mean_outliers +5 * sd_outliers)
    )
    p <-
      ggplot(df, aes(x = x)) +
      stat_function(
        fun = d_outlier,
        n = 201
      ) + 
      labs(
        x = "population value",
        y = NULL
      ) +
      geom_vline(aes(xintercept = outlier_mean))
  }
  p
}

#######################################
# Function to get m samples of size n
#######################################

get_samples <- function(m, n, pop = c("normal", "skew", "super_skew", "outlier")) {
  samps <- vector(mode = "list", length = m)
  for (i in 1:m) {
    if (pop == "normal") {
      samps[[i]] <- rnorm(n, mean = normal_mean, sd = sigma_norm)
    }
    if (pop == "skew") {
      samps[[i]] <- rgamma(n, shape = shape_gamma, scale = scale_gamma)
    }
    if (pop == "super_skew") {
      samps[[i]] <- rpareto(n, alpha = alpha_pareto, theta = theta_pareto)
    }
    if (pop == "outlier") {
      samps[[i]] <- r_outlier(n)
    }
  }
  samps
}

##########################################
# function to get intervals from samples
##########################################

get_intervals <- function(samps, pop = c("normal", "skew", "super_skew", "outlier"), level) {
  if (pop == "normal") mu <- normal_mean
  if (pop == "skew") mu <- skew_mean
  if (pop == "super_skew") mu <- super_skew_mean
  if (pop == "outlier") mu <- outlier_mean
  m <- length(samps)
  good <- logical(m)
  xbar <- numeric(m)
  lower <- numeric(m)
  upper <- numeric(m)
  number = 1:m
  for (i in 1:m) {
    xs <- samps[[i]]
    n <- length(xs)
    xbar[i] <- mean(xs)
    crit <- qt((1 + level) / 2, df = n-1)
    margin <- crit * sd(xs) / sqrt(n)
    lower[i] <- xbar[i] - margin
    upper[i] <- xbar[i] + margin
    good[i] <- mu >= lower[i] & mu <= upper[i]
  }
  df <- data.frame(
    number = number,
    xbar = xbar,
    lower = lower,
    upper = upper,
    good = good
  )
  df
}


##############################
# function to plot intervals
##############################

## HSW:  revise this as needed so that it gives a pretty graph
## for both 50 and 100 intervals:

interval_plot <- function(data, pop = c("normal", "skew", "super_skew", "outlier")) {
  if (pop == "normal") mu <- normal_mean
  if (pop == "skew") mu <- skew_mean
  if (pop == "super_skew") mu <- super_skew_mean
  if (pop == "outlier") mu <- outlier_mean
  plot <- ggplot(data) +
    geom_vline(aes(xintercept = mu)) +
    geom_segment(
      aes(
        x = lower,
        xend = upper,
        y = number,
        yend = number,
        color = good
      )
    ) +
    geom_point(aes(x = xbar, y = number), size = 0.5) +
    labs( x = NULL, y = NULL) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  plot
}



