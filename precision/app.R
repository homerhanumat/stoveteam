library(shiny)
library(bslib)
library(tidyverse)
library(nlme)
library(glue)

all_meas <- read_csv(file = "all_meas.csv")

mod <- nlme::lme(wood ~ 1, random = ~ 1 | house, data = all_meas)

precision_sim <- function(model, J, level, reps) {
  js <- rep(J$j, times = J$n)
  sds <- as.numeric(VarCorr(model)[, 2])
  #sb <- sds[1]
  sw <- sds[2]
  house_means <- 
    all_meas %>% 
    group_by(house) %>% 
    summarize(mean = mean(wood)) %>% 
    pull(mean)
  n <- sum(J$n)
  precisions <- numeric(reps)
  progress_steps <-
    ((1:reps) / floor(reps / 10)) %>% 
    ceiling() %>% 
    unique()
  progress_steps <- progress_steps * floor(reps / 10)
  progress_steps <- progress_steps[1:(length(progress_steps) - 1)]
  progress_df <- data.frame(
    n = progress_steps,
    perc = seq(10, 90, by = 10)
  )
  for (i in 1:reps) {
    if (i %in% progress_df$n) {
      row <- which(progress_df$n == i)
      perc <- progress_df$perc[row]
      incProgress(
        amount = 0.1,
        paste0(perc, "% of simulated samples have been taken."))
    }
    resampled_means <- sample(house_means, size = n, replace = TRUE)
    for (j in 1:n) {
      resampled_means[j] <- resampled_means[j] + mean(rnorm(js[j], 0, sw))
    }
    xbar <- mean(resampled_means)
    se_xbar <- var(resampled_means) / sqrt(n)
    t <- qt((1 + level) / 2, df = n - 1)
    precisions[i] <- t * se_xbar / xbar
  }
  data.frame(x = precisions)
}



density_plot <- function(df) {
  ggplot(df, aes(x = x)) +
    geom_density(fill = "skyblue", color = "black") +
    geom_vline(aes(xintercept = 0.10)) +
    labs(
      x = "simulated precision"
    )
}


# To parse the string and turn it into usable data
parse_js <- function(str) {
  req(str)
  pairs <- str_extract_all(
    str,
    pattern = "\\(\\d+,\\s*\\d+\\)"
  ) %>% 
    unlist()
  pairs %>% 
    map_dfr(function(p) {
      pair <- str_extract_all(p, pattern = "\\d+") %>% 
        unlist() %>% 
        as.numeric()
      list(j = pair[1], n = pair[2])
    })
}


ui <- page_sidebar(
  title = "Precision Simulation",
  sidebar = sidebar(
    textInput(
      "js", "Days at Each House:", 
      value = "(4, 40), (3, 20)", 
      placeholder = "ex. (4, 40), (3, 20)"
    ),
    tableOutput("tab"),
    sliderInput("level", "Level of Confidence:", min = 0.10, max = 0.99, value = .90, step = 0.01),
    numericInput("reps", "Number of Samples:", min = 50, value = 10000),
    actionButton("simulate","Simulate!")
  ),
  card(
    card_header("Density plot of simulated precisions"),
    plotOutput("p")
  ),
  card(
    textOutput("report")
  )
)


server <- function(input, output, session) {
  
  js <- eventReactive(input$js, {
    req(input$js)
    parse_js(input$js)
  })
  
  sim_results <- eventReactive(input$simulate, {
    withProgress(
      message = "Taking samples ...", min = 0, max = 1, value = 0, {
      precision_sim(
        model = mod,
        J = js(),
        reps = input$reps,
        level = input$level
      )
    })
  })
  
  output$p <- renderPlot({
    density_plot(sim_results())
  })
  
  output$tab <- renderTable({
    req(is.data.frame(js()))
    df <- js()
    df$j <- as.integer(df$j)
    df$n <- as.integer(df$n)
    flattened <- integer()
    for (i in 1:nrow(df)) {
      flattened <- c(flattened, rep(df$j[i], times = df$n[i]))
    }
    put_back <- data.frame(
      j = flattened
    ) %>% 
     group_by(j) %>% 
      summarize(n = n())
    names(put_back) <- c("days", "houses")
    put_back$houses <- as.integer(put_back$houses)
    put_back
  })
  
  output$report <- renderText({
    perc <- round(mean(sim_results()$x <= 0.10) * 100, 2)
    glue("{perc}% of simulated precisions are less than 0.10.")
  })
  
}


shinyApp(ui, server)