library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)

## Load utility functions
source("Utilities.R")

## HSW:  I suggest leaving out the emojis, and 
## changing the color to something less "loud" (please, not orange!)

ui <- fluidPage(useShinyjs(),
                theme = bslib::bs_theme(primary = "forestgreen"),
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      "pop",
                      "Population Type",
                      c("normal", "skew", "super_skew", "outlier")
                    ),
                    numericInput("n", "Sample Size", min = 2, value = 10),
                    sliderInput(
                      "level",
                      "Confidence Level",
                      min = 10,
                      max = 99,
                      value = 50,
                      step = 1,
                      post = "%"
                    ),
                    radioButtons("m", "Intervals to Draw", c(50,100)),
                    actionButton("make_intervals", "Make Intervals"),
                    hidden(actionButton("start_over", "Start Over"))
                  ),
                  column(width = 9, tabsetPanel(id="inTabset",
                    tabPanel("Population Graph",
                             fluidPage(
                               fluidRow(plotOutput("cov_prop_plot")),
                               fluidRow(
                                 value_box(
                                   "Population Mean",
                                   textOutput("popMean"),
                                   theme = "bg-gradient-green-teal",
                                   height = "100%"
                                 )
                               )
                             )),
                    tabPanel("Confidence Intervals",
                             fluidPage(
                               fluidRow(plotOutput("more_ints_plot")),
                               fluidRow(
                                 column(
                                   width = 4,
                                   value_box(
                                     "Intervals So Far",
                                     textOutput("simCount"),
                                     theme = "primary",
                                     height = "100%"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   value_box(
                                     "Number Containing Proportion",
                                     textOutput("GI"),
                                     theme = "primary",
                                     height = "100%"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   value_box(
                                     "Percentage Containing Proportion",
                                     textOutput("percGI"),
                                     theme = "primary",
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
    ## HSW:  let's start off with zeros:
    number = 0,
    good = 0,
    intervals = NULL,
    percs = NULL
  )
  
  ######################################
  # Code to Control Element Visibility
  ######################################
  
  observeEvent(input$make_intervals, {
    hide("pop")
    hide("n")
    hide("level")
    show("start_over")
    some_samps <- get_samples(input$m, input$n, input$pop)
    ints_data <-
      get_intervals(
        some_samps,
        input$pop,
        input$level / 100
      )
    
    rv$number <- rv$number + input$m
    rv$good <- rv$good + sum(ints_data$good)
    rv$intervals <- ints_data

    updateTabsetPanel(inputId = "inTabset",
                      selected = "Confidence Intervals")
    
  })
  
  observeEvent(input$start_over, {
    show("pop")
    show("n")
    show("level")
    hide("start_over")
    rv$number <- 0
    rv$good <- 0
    rv$intervals<- NULL
    rv$percs <- NULL
    updateTabsetPanel(inputId = "inTabset",
                      selected = "Population Graph")
  })
  
  output$cov_prop_plot <- renderPlot({
    draw_pop(input$pop)
  })
  
  output$simCount <- renderText(rv$number)
  output$GI <- renderText(sum(rv$good))
  output$percGI <- renderText(round(sum(rv$good) / rv$number, 5) * 100)
  
  ## HSW: make this reactive on input$pop
  ## use a switch statment (see R help on "switch") to set the mean to whatever
  ## it should be
  output$popMean <- renderText(switch(input$pop,
                                      normal = normal_mean,
                                      skew = skew_mean,
                                      super_skew = super_skew_mean,
                                      outlier = outlier_mean))
  
  # output$percs_plot <- 
  #   renderPlot({
  #     perc_plotr()
  #   })
  
  output$more_ints_plot <-
    renderPlot({
      interval_plot(rv$intervals, input$pop)
    })
}
shinyApp(ui, server)
