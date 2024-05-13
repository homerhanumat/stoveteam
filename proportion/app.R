library(shiny)
library(tidyverse)
library(DescTools)
library(shinyjs)

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

interval_plot <- function(data, prob) {
  plot <- ggplot(data) +
    geom_vline(aes(xintercept = prob)) +
    geom_segment(
      aes(
        x = lwr.ci,
        xend = upr.ci,
        y = number,
        yend = number,
        color = covers
      )
    ) +
    geom_point(aes(x = est, y = number), size = 0.5) +
    labs(x = NULL, y = NULL) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  plot
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

ui <- fluidPage(
  sidebarPanel(
    useShinyjs(),
    div(
      id = "inputs",
      sliderInput("proportion", "Proportion", min = 0, max = 1, value = 0.5),
      numericInput("sample_size", "Sample Size", value = 100),
      sliderInput("level", "Level", min = 0.10, max = 0.99, step = 0.01, value = 0.9),
      selectInput("interval_type", "Interval Type", choices = c(
        "wilson", "wald", "waldcc", "agresti-coull", "jeffreys",
        "modified wilson", "wilsoncc", "modified jeffreys",
        "clopper-pearson", "arcsine", "logit", "witting", "pratt",
        "midp", "lik", "blaker"
      ))
    ),
    actionButton("make_intervals", "Make Intervals"),
    hidden(actionButton("start_over", "Start Over"))
  ),
  mainPanel(
    column(
      width = 12,
      hidden(plotOutput("plot")),
      hidden(htmlOutput("results_table"))
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    total_intervals = 0,
    good_intervals = 0,
    last_intervals = NULL
  )
  
  observeEvent(input$make_intervals, {
    intervals <- interval_df(N = 50, n = input$sample_size, level = input$level, prob = input$proportion, method = input$interval_type)
    rv$total_intervals <- rv$total_intervals + 50
    rv$good_intervals <- rv$good_intervals + sum(intervals$covers)
    rv$last_intervals <- intervals
    
    show("start_over")
    show("plot")
    show("results_table")
    hide("inputs")
  })
  observeEvent(input$start_over, {
    rv$total_intervals <- 0
    rv$good_intervals <- 0
    rv$last_intervals <- NULL
    
    show("inputs")
    hide("plot")
    hide("results_table")
  })
  output$plot <- renderPlot({
    interval_plot(
      data = rv$last_intervals,
      prob = isolate(input$proportion)
    )
  })
  
  output$results_table <- renderText({
    
    perc <- round(rv$good_intervals / rv$total_intervals * 100, 2)
    
    glue::glue(
      '<table class="table table-bordered">
        <thead>
          <tr>
            <th scope="col">Total Intervals</th>
            <th scope="col">Intervals Covering the Proportion</th>
            <th scope="col">Coverage Rate</th>
          </tr>
        <tbody>
          <tr>
            <td>{rv$total_intervals}</td>
            <td>{rv$good_intervals}</td>
            <td>{perc}%</td>
          </tr>
        </tbody>
      '
    )
    
  })
}




shinyApp(ui, server)