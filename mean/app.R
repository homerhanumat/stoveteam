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
                    ## HSW: need realistic sample size:
                    numericInput("n", "Sample Size", min = 2, value = 10),
                    sliderInput(
                      "level",
                      "Confidence Level",
                      ## HSW:  make realistic min and max:
                      min = 10,
                      max = 99,
                      value = 50,
                      step = 1,
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
    ## HSW:  you don't need these:
    # xbar = NULL,
    # lower = NULL,
    # upper = NULL,
    good = NULL,
    ## HSW:  but you could store the latest intervals here:
    intervals = NULL
  )
  
  ######################################
  # Code to Control Element Visibility
  ######################################
  
  observeEvent(input$make_intervals, {
    hide("pop")
    hide("n")
    hide("level")
    show("start_over")
    ## HSW:  you need to find your samples and interval right now,
    ## so you can update the rv object:
    some_samps <- get_samples(input$m, input$n, input$pop)
    print(some_samps)
    ints_data <-
      get_intervals(
        some_samps,
        input$pop,
        ## HSW;  you bring in confidence level as a percentage, but for
        ## our computations it needs to be  number between 0 and 1, so:
        input$level / 100
      )
    
    if (is.null(rv$number)) {
      rv$number <- input$m
    } else {
      rv$number <- rv$number + input$m
    }
    
    if (is.null(rv$good)) {
      rv$good <- sum(ints_data$good)
    } else {
      rv$number <- rv$number + sum(ints_data$good)
    }
    
    rv$intervals <- ints_data
    
  })
  
  observeEvent(input$start_over, {
    show("pop")
    show("n")
    show("level")
    hide("start_over")
    ## HSW:  you need to set the rv object back to
    ## its original values:
    rv$number <- NULL
    rv$good <- NULL
    rv$intervals<- NULL
  })
  
  #some_samps <- reactive(get_samples(input$m, input$n, input$pop))
  
  # ints_data <-
  #   reactive(get_intervals(
  #     some_samps(),
  #     input$pop,
  #     input$level
  #   ))
  
  output$cov_prop_plot <- renderPlot({
    draw_pop(input$pop)
  })
  
  ## HSW:  these needed help:
  output$simCount <- renderText(rv$number)
  output$GI <- renderText(rv$good)
  output$percGI <- renderText(rv$good / rv$number)
  
  ## HSW:  see if you can fix this one:
  ## output$popMean <- renderText(tail(ints_data()$number, n=1))
  
  output$more_ints_plot <-
    renderPlot({
      interval_plot(rv$intervals, input$pop)
    })
}
shinyApp(ui, server)
