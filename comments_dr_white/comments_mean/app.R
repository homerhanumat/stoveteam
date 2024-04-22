library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)

## Load utility functions
source("Utilities.R")

## HSW:  I suggest leaving out the emojis, and 
## changing the color to something less "loud" (please, not orange!)
## Also, I looked into the showcasing, and there appears to be no way to
## place the smiley-face icons so that they don't squeeze the text.
## I'm afraid we will have to let them go!

## RCT: No worries, I was already thinking of the emojis as a temporary placeholder.
## In the tile for the percentage of intervals that were good, I was thinking of making
## inputting a graph that would update to show how the percentage gets closer
## to the percentage error as we consider more intervals. Would this be useful?


ui <- fluidPage(useShinyjs(),
                theme = bslib::bs_theme(primary = "forestgreen"), ## RCT: What do you think of forest green?
                                                                  ## I'm open to any suggestions.
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
                                     theme = "bg-gradient-green-teal",
                                     height = "100%"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   value_box(
                                     "Number Containing Proportion",
                                     textOutput("GI"),
                                     theme = "bg-gradient-green-teal",
                                     height = "100%"
                                   )
                                 ),
                                 column(
                                   width = 4,
                                   value_box(
                                     "Percentage Containing Proportion",
                                     textOutput("percGI"),
                                     theme = "bg-gradient-red-orange",
                                     height = "100%"
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
    ## HSW:  what's your plan with rv$percs?
    ## RCT: this is the storage space for the percentages which would be used
    ##      in the graph i mentioned on lines 17-19.
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
    ## HSW:  inputs form a radio button are of type
    ## character, not numeric, so:
    m <- as.numeric(input$m)
    some_samps <- get_samples(m, input$n, input$pop)
    ints_data <-
      get_intervals(
        some_samps,
        input$pop,
        input$level / 100
      )
    ## HSW:  change here too:
    rv$number <- rv$number + m
    rv$good <- rv$good + sum(ints_data$good)
    rv$intervals <- ints_data
## HSW:  you must always remove material like this when
## resolving a merge conflict:
## <<<<<<< HEAD
 

    updateTabsetPanel(inputId = "inTabset",
                      selected = "Confidence Intervals")
## =======
    
## HSW:  this should have been removed:
    # updateTabsetPanel(inputId = "inTabset",
    #                   selected = "More Intervals at a Time")
## >>>>>>> a05456eae6df945da34263df9deda8313d3685e6
    
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
  ## RCT: I added the as.numeric() because it said that these variables weren't numbers:
  ## HSW:   That wasn't the issue:  we need sum of rv$good:
  output$percGI <- renderText(round(sum(rv$good) / rv$number, 5) * 100)
  
  ## HSW:  see if you can fix this one:
  ## I have a mean now but because it isn't reactive I believe it is the wrong one:
  ## HSW: make this reactive on input$pop
  ## use a switch statement (see R help on "switch") to set the mean to whatever
  ## it should be
  output$popMean <- renderText(mu_norm)
  
  # output$percs_plot <- 
  #   renderPlot({
  #     perc_plotr()
  #   })
  
  output$more_ints_plot <-
    renderPlot({
      ## HSW:  i notice that when we start over and select a
      ## different population and then make intervals, there
      ## is a brief error message in the intervals tab.
      ## We mgiht be able to preven this with req(), maybe
      ## req(rv$intervals), let's see:
      ## RCT: Under the hood, is req() using the promises from JS?
      req(rv$intervals)
      interval_plot(rv$intervals, input$pop)
    })
}
shinyApp(ui, server)
