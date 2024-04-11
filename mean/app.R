library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)

## Load utility functions
source("Utilities.R")

ui <- fluidPage(theme = bslib::bs_theme(primary = "orange"),
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
                    actionButton("start_over", "Start Over")
                  ),
                  column(width = 9, tabsetPanel(
                    tabPanel("Coverage Properties",
                             fluidPage(
                               fluidRow(renderPlot("cov_prop_plot")),
                               fluidRow(
                                 value_box(
                                   "Population Mean",
                                   "[insert output here]",
                                   showcase = bsicons::bs_icon("emoji-heart-eyes"),
                                   theme = "bg-gradient-green-teal",
                                   height = "100%"
                                   #showcase_layout = "bottom"
                                 )
                               )
                             )),
                    tabPanel("More Intervals at a Time",
                             fluidPage(
                               fluidRow(renderPlot("more_ints_plot")),
                               fluidRow(
                                 column(
                                   width = 4,
                                   value_box(
                                     "Intervals So Far",
                                     "[insert output here]",
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
                                     "[insert output here]",
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
                                     "[insert output here]",
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
  
  samps_data <- reactive(get_samples(input$m, input$n, input$pop))
  ints_data <- reactive(get_intervals(samps_data(), input$pop, input$level))
  
  
  
  
  output$cov_prop_plot <- renderPlot(draw_pop(input$pop))
  output$more_ints_plot <- renderPlot(interval_plot(ints_data, input$pop))
}
shinyApp(ui, server)
