library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)

## Load utility functions
source("Utilities.R")

ui <- fluidPage(useShinyjs(),
                theme = bslib::bs_theme(primary = "orange"),
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      "pop",
                      "Population Type",
                      c("normal", "skew", "super_skew", "outlier")
                    ),
                    numericInput("n", "Sample Size", 1),
                    sliderInput(
                      "level",
                      "Confidence Level",
                      min = 0,
                      max = 100,
                      value = 50,
                      step = 5,
                      post = "%"
                    ),
                    radioButtons("m", "Intervals to Draw", c(50, 100)),
                    actionButton("make_intervals", "Make Intervals"),
                    hidden(actionButton("start_over", "Start Over"))
                  ),
                  column(width = 9, tabsetPanel(
                    tabPanel("Coverage Properties",
                             fluidPage(
                               fluidRow(plotOutput("cov_prop_plot")),
                               fluidRow(
                                 value_box(
                                   "Population Mean",
                                   textOutput("popMean"),
                                   showcase = bsicons::bs_icon("emoji-heart-eyes"),
                                   theme = "bg-gradient-green-teal",
                                   height = "100%"
                                   #showcase_layout = "bottom"
                                 )
                               )
                             )),
                    tabPanel("More Intervals at a Time",
                             fluidPage(
                               fluidRow(plotOutput("more_ints_plot")),
                               fluidRow(
                                 column(
                                   width = 4,
                                   value_box(
                                     "Intervals So Far",
                                     textOutput("simCount"),
                                     showcase = bsicons::bs_icon("emoji-heart-eyes"),
                                     theme = "bg-gradient-green-teal",
                                     height = "100%"
                                     #showcase_layout = "bottom"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   value_box(
                                     "Number Containing Proportion",
                                     textOutput("GI"),
                                     showcase = bsicons::bs_icon("emoji-heart-eyes"),
                                     theme = "bg-gradient-green-teal",
                                     height = "100%"
                                     #showcase_layout = "bottom"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   value_box(
                                     "Percentage Containing Proportion",
                                     textOutput("percGI"),
                                     showcase = bsicons::bs_icon("emoji-frown"),
                                     theme = "bg-gradient-red-orange",
                                     height = "100%"
                                     #showcase_layout = "bottom"
                                   )
                                 )
                               )
                             ))
                  ))
                ))

server <- function(input, output, session) {
  
  ###########################################################
  # Function to keep track of intervals, means and coverage
  ###########################################################
  
  rv <- reactiveValues(
    number = NULL,
    xbar = NULL,
    lower = NULL,
    upper = NULL,
    good = NULL
  )
  
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
    data.frame(
      number = number,
      xbar = xbar,
      lower = lower,
      upper = upper,
      good = good
    )
  }
  
  ######################################
  # Code to Control Element Visibility
  ######################################
  
  observeEvent(input$make_intervals, {
    hide("pop")
    hide("n")
    hide("level")
    show("start_over")
  })
  observeEvent(input$start_over, {
    show("pop")
    show("n")
    show("level")
    hide("start_over")
  })
  
  some_samps <- reactive(get_samples(input$m, input$n, input$pop))
  
  ints_data <-
    reactive(get_intervals(
      some_samps(),
      input$pop,
      input$level
    ))
  
  output$cov_prop_plot <- renderPlot({
    draw_pop(input$pop)
  })
  
  output$simCount <- renderText(sum(ints_data()$number))
  output$GI <- renderText(ints_data()$good)
  output$percGI <- renderText(ints_data()$good/sum(ints_data()$number))
  output$popMean <- renderText(tail(ints_data()$number, n=1))
  
  output$more_ints_plot <-
    renderPlot({
      interval_plot(ints_data(), input$pop)
    })
}
shinyApp(ui, server)
