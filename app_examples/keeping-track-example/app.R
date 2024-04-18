library(shiny)
library(ggplot2)
library(glue)
library(shinyjs)

ui <- fluidPage(

    # Application title
    titlePanel("Sampling"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
          useShinyjs(),
          actionButton(inputId = "get", label = "Gimme 10 Values!"),
          hidden(
            actionButton(inputId = "revert", label = "Start Over")
          )
        ),
        mainPanel(
           plotOutput("plot"),
           hidden(
             verbatimTextOutput("report")
           )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues(
    values = numeric(),
    recent = NULL
  )
  
  observeEvent(input$get, {
    new_vals <- rnorm(10)
    rv$recent <- new_vals
    rv$values <- c(rv$values, new_vals)
    show("plot")
    show("report")
  })
  
  observeEvent(input$revert, {
    rv$recent <- NULL
    rv$values <- numeric()
    hide("plot")
    hide("report")
  })

    output$plot <- renderPlot({
       req(!is.null(rv$recent))
       df <- data.frame(x = rv$recent)
       ggplot(df, aes(x = x)) +
         geom_density(fill = "skyblue") +
         geom_rug() +
         labs(title = "The Last 10 Values")
    })

  output$report <- renderPrint({
    req(!is.null(rv$recent))
    n <- length(rv$values)
    xbar <- mean(rv$values)
    glue::glue(
      "So far you have got {n} values, and their mean is {xbar}."
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
