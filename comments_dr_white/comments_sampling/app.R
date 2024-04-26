library(shiny)
library(tidyverse)
library(readxl)


# Globals


get_house <- function(i) {
  vals <- read_excel(
    "For Review_Ret_Justa_July KPT Complete_KPT_4day (1).xlsx",
    range = "I15:L15",
    sheet = paste0("HH", i, " Data"),
    col_names = FALSE
  ) %>%
    as.matrix() %>%
    t() %>%
    .[, 1]
  vals <- vals[!is.na(vals)]
  data.frame(
    house = rep(i, length(vals)),
    wood = vals
  )
}

density_with_rug <- function(df, title) {
  ggplot(df, aes(x = household_mean)) +
    geom_density(fill = "skyblue") +
    geom_rug() +
    xlim(c(0, 8)) +
    ylim(c(0, 1)) +
    labs(
      x = "household means of daily per-capita usage",
      title = title
    )
}


all_meas <-
  map_dfr(factor(1:16), get_house) %>%
  group_by(house) %>%
  mutate(mean_percap = mean(wood)) %>%
  mutate(deviation = wood - mean_percap)


summary_stats <- function(vals) {
  c(
    mean = mean(vals),
    stdev = sd(vals)
  )
}

## UI

ui <- fluidPage(
  titlePanel("Sampling Measurement App"),
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = "resample", "Resample")
    ),
    mainPanel(
      fluidRow(
        column(
          width = 6,
          plotOutput("the_actual_plot"),
          textOutput("the_actual_stats")
        ),
        column(
          width = 6,
          plotOutput("resampling_plot"),
          verbatimTextOutput("the_resampling_stats")
        )
      )
    )
  )
)


## server logic

server <- function(input, output) {
  resample_data <- eventReactive(input$resample, {
    resampled_wood <- sample(all_meas$wood)
    data.frame(house = all_meas$house, wood = resampled_wood)
  })

  output$the_actual_plot <- renderPlot({
    # ggplot(sample_means, aes(x = household_mean)) +
    #   geom_density(fill = "skyblue") +
    #   geom_rug() +
    #   xlim(c(0, 8)) +
    #   ylim(c(0, 1)) +
    #   labs(x = "Household Means of Daily Per-Capita Usage")
  })

  output$the_actual_stats <- renderPrint({
    # summary_stats(sample_means$household_mean)
  })


  output$resampling_plot <- renderPlot({
    rs_means <-
      resample_data() %>%
      group_by(house) %>%
      summarise(household_mean = mean(wood))
    plot <- density_with_rug(
      df = rs_means,
      title = "Household means for the resampled data"
    )
    plot
  })

  output$the_resampling_stats <- renderPrint({
    rs_means <-
      resample_data() %>%
      group_by(house) %>%
      summarise(household_mean = mean(wood))
    summary_stats(rs_means$household_mean)
  })
}


## run the app

shinyApp(ui, server)
