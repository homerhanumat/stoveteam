


library(tidyverse)
library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)

## Load utility functions
source("Utilities.R")

ui <- fluidPage(useShinyjs(),
                theme = bslib::bs_theme(primary = "grey"),
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
                      value = 90,
                      step = 1,
                      post = "%"
                    ),
                    uiOutput("m"),
                    actionButton("make_intervals", "Make Intervals"),
                    hidden(actionButton("start_over", "Start Over"))
                  ),
                  column(width = 9, tabsetPanel(id="inTabset",
                                                tabPanel("Population Graph",
                                                         fluidPage(
                                                           fluidRow(plotOutput("cov_prop_plot")),
                                                           fluidRow(
                                                             value_box(
                                                               "Population Mean:",
                                                               textOutput("popMean"),
                                                               theme = "primary",
                                                               height = "100%"
                                                             )
                                                           )
                                                         )),
                                                tabPanel("Confidence Intervals",
                                                         fluidPage(
                                                           fluidRow(plotOutput("more_ints_plot")),
                                                           fluidRow(
                                                             value_box(
                                                               title = "Coverage:",
                                                               value = textOutput("percGI"),
                                                               p(textOutput("simCount")),
                                                               p(textOutput("GI")),
                                                               theme = "primary",
                                                               height = "100%"
                                                             )
                                                           )
                                                         ))
                  ))
                ))

server <- function(input, output, session) {
  
  ###########################################################
  # Reactive values to keep track of intervals, means and coverage
  ###########################################################
  
  rv <- reactiveValues(
    number = 0,
    good = 0,
    intervals = NULL,
    m = 50
  )
  
  ######################################
  # Code to Control Element Visibility
  ######################################
  
  observeEvent(input$make_intervals, {
    hide("pop")
    hide("n")
    hide("level")
    show("start_over")
    some_samps <- get_samples(
      as.numeric(input$m), 
      input$n, 
      input$pop
    )
    ints_data <-
      get_intervals(
        some_samps,
        input$pop,
        input$level / 100
      )
    rv$number <- rv$number + as.numeric(input$m)
    rv$good <- rv$good + sum(ints_data$good)
    print("hello")
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
    updateTabsetPanel(inputId = "inTabset",
                      selected = "Population Graph")
  })
  
  observeEvent(input$m, {
    rv$m <- input$m
  })

  output$m <- renderUI({
    label <- ifelse(
      rv$number == 0,
      "Number of samples to take:",
      "Number of new samples to take:"
    )
    radioButtons(
      inputId = "m",
      label = label,
      choices = c(50, 100),
      selected = isolate(rv$m)
    )
  })
  
  output$cov_prop_plot <- renderPlot({
    draw_pop(input$pop)
  })
  
  output$simCount <- renderText({
    paste0("So far, ", rv$number, " simulated samples have been taken.")
  })
  
  output$GI <- renderText({
    paste0(
      sum(rv$good),
      " of the intervals built from these samples covered the mean."
    )
  })
  
  output$percGI <- renderText({
    perc <- round(sum(rv$good) / rv$number, 5) * 100
    paste0(perc, "%")
  })
  
  output$popMean <- renderText(
    switch(
      input$pop,
      normal = normal_mean,
      skew = skew_mean,
      super_skew = super_skew_mean,
      outlier = outlier_mean)
  )
  
  output$more_ints_plot <-
    renderPlot({
      interval_plot(rv$intervals, input$pop)
  })
}

shinyApp(ui, server)
