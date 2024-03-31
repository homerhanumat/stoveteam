## for confidence intervals, we might use
## the agresti-coull method
## (see, e.g.,
## https://towardsdatascience.com/five-confidence-intervals-for-proportions-that-you-should-know-about-7ff5484c024f)
## original citation:
## Agresti A. and Coull B.A. (1998) Approximate is better than "exact" for interval estimation of binomial proportions. American Statistician, 52, pp. 119-126. 

library(DescTools)
successes <- 37
trials <- 65
level <- 0.90

res <- BinomCI(
  x = successes,
  n = trials,
  conf.level = level,
  method = "agresti-coull"
)

lower <- round(res[1, "lwr.ci"], 3)
upper <- round(res[1, "upr.ci"], 3)
margin <- (upper - lower) / 2

msg <- glue::glue(
  'When there are {successes} successes in {trials} trials,
  then our point estimate for the population proportion is:
  {successes} / {trials}, which is about {round(res[1, "est"], 3)},
  and the {100 * level}%-confidence interval for the
  population proportion is:
  
  ({lower}, {upper}).
  
  The margin of error is {round(margin * 100, 3)}%.'
)

msg
