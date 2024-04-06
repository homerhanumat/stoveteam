library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            ## some ui to hide or show:
            div(
              id = "some_widgets",
              textInput(inputId = "text", label = "Enter some text:"),
              numericInput((inputId = "number"), value = 10, label = "Give me a number:")
            ),
            ## action buttons that control hide/show:
            actionButton(inputId = "start", label = "Begin"),
            hidden(actionButton(inputId = "revert", label = "Start Over"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$start, {
    hide("some_widgets")
    show("revert")
    hide("start")
  })
  
  observeEvent(input$revert, {
    show("some_widgets")
    show("start")
    hide("revert")
  })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
