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
                theme = bslib::bs_theme(primary = "orange"),
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
                             ## HSW:  the above is not a good title.
                             ## Why not:  "Population Graph"?
                             ## make sure to revise your 
                             ## updateTabsetPanel call accordingly
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
                    tabPanel("Confidence Intervals",
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
  ## RCT: I added the as.numeric() because it said that these variables weren't numbers:
  ## HSW:   That wasn't the issue:  we need sum of rv$good:
  output$percGI <- renderText(round(sum(rv$good) / rv$number, 5) * 100)
  
  ## HSW:  see if you can fix this one:
  ## I have a mean now but because it isn't reactive I believe it is the wrong one:
  ## HSW: make this reactive on input$pop
  ## use a switch statment (see R help on "switch") to set the mean to whatever
  ## it should be
  output$popMean <- renderText(mu_norm)
  
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
