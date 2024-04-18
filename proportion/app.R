library(shiny)
library(tidyverse)
library(DescTools)

interval_df <- function(N, n, level, prob, method) {
  xs <- rbinom(N, size = n, prob = prob)
  ints <- xs %>% 
    map_dfr(
      make_interval, 
      n = n, 
      level = level,
      method = method,
      report = FALSE
    ) %>% 
    mutate(number = 1:N) %>% 
    mutate(covers = prob >= lwr.ci & prob <= upr.ci)
}

make_interval <- function(x, n, level, method, report) {
  res <- BinomCI(
    x = x,
    n = n,
    conf.level = level,
    method = method
  )
  if (report) {
    lower <- round(res[1, "lwr.ci"], 3)
    upper <- round(res[1, "upr.ci"], 3)
    margin <- (upper - lower) / 2
    
    msg <- glue::glue(
      'When there are {successes} successes in {n} trials,
      then our point estimate for the population proportion is:
      {successes} / {n}, which is about {round(res[1, "est"], 3)},
      and the {100 * level}%-confidence interval for the
      population proportion is:
  
      ({lower}, {upper}).
  
      The margin of error is {round(margin * 100, 3)}%.'
    )
    cat(msg)
  }
  invisible(
    data.frame(
      est = res[1, "est"],
      lwr.ci = res[1, "lwr.ci"],
      upr.ci = res[1, "upr.ci"]
    )
  )
}

ui <- pageWithSidebar(
  headerPanel = headerPanel(
    "Proportion App"
  ),
  sidebarPanel = sidebarPanel(
    sliderInput("proportion", "Proportion", min=0, max=1, value=0.5),
    numericInput("sample_size", "Sample Size", value=100),
    sliderInput("level", "Level", min=0, max=1, value=0.9),
    selectInput("interval_type", "Interval Type", choices=c("wilson", "wald", "waldcc", "agresti-coull", "jeffreys",
                                                            "modified wilson", "wilsoncc","modified jeffreys",
                                                            "clopper-pearson", "arcsine", "logit", "witting", "pratt", 
                                                            "midp", "lik", "blaker")),
    actionButton("make_intervals", "Make Intervals"),
    actionButton("start_over", "Start Over")
  ),
  mainPanel = mainPanel(
    column( width=12,
      plotOutput("plot"),
      textOutput("text")
    )
  )
)

server <- function(input, output, session) {
    
}
  
 
  

shinyApp(ui, server)
