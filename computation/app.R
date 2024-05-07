library(shiny)
library(bslib)
library(stringr)
library(ggplot2)


###### Helper functions
# probabilty density function for mixed-normal distribution
pop_fun <- function(x, p, mu1, sd1, mu2, sd2) {
  d1 <- p * dnorm(x, mean = mu1, sd = sd1)
  d2 <- (1 - p) * dnorm(x, mean = mu2, sd = sd2)
  d1 + d2
}

# To parse the string and turn it into usable data
parse_js <- function(str) {
  req(str)
  pairs <- str_extract_all(
    str,
    pattern = "\\(\\d+,\\s*\\d+\\)"
  ) %>% 
    unlist()
  js <- numeric()
  print(pairs)
  for (i in 1:length(pairs)) {
    pair <- str_extract_all(pairs[i], pattern = "\\d+") %>% 
      unlist() %>% 
      as.numeric()
    js <- c(js, rep(pair[1], times = pair[2]))
  }
  print(js)
  js
}

# make sample function
make_sample <- function(js, house) {
  n <- length(js)
  mean_h <- numeric(n)
  for (i in 1:n) {
    p <- house$p
    house_type <- sample(c(1, 2), size = 1, prob = c(p, 1 - p)) 
    # randomly assigns which house based on user selected proababilty
    house_mean <- ifelse(
      house_type == 1,
      rnorm(1, mean = house$mu1, sd = house$sd1),
      rnorm(1, mean = house$mu2, sd = house$sd2)
    )
    mean_h[i] <- mean(rnorm(js[i], mean = house_mean, sd = house$sw))
  }
  mean_h
}


# Computes point_estimate, margin, and interval from data
compute_from_sample <- function(data, level, mu) {
  n <- length(data)
  sample_mean <- sum(data) / n
  ss_between <- sum((sample_mean - data)^2)
  multiplier <- qt((1 + level) / 2, df = n - 1)
  margin <- multiplier * sqrt(ss_between / (n * (n - 1)))
  list(
    point_estimate = sample_mean, 
    margin = margin,
    interval = c(
      sample_mean - margin,
      sample_mean + margin
    ),
    t = (sample_mean - mu) / sqrt(ss_between / (n * (n - 1)))
  )
}

# Runs the simulations
simulate <- function(reps, house, js, level) {
  good <- logical(reps)
  ts <- numeric(reps)
  parsed_js <- parse_js(js)
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
    samp <- make_sample(
      js = parsed_js,
      house = house
    )
    mu <- house$p * house$mu1 + (1 - house$p) * house$mu2
    results <- compute_from_sample(
      data = samp,
      level = level,
      mu = mu
    )
    lwr <- results$interval[1]
    upr <- results$interval[2]
    good[i] <- mu >= lwr & mu <= upr
    ts[i] <- results$t
  }
  list(
    good = good,
    t = ts,
    n = length(parsed_js)
  )
}


###### TABS AND SET UP FOR UI
mainCont <- function(){ #sets up the main controls for the ui
  accordion_panel(
    "Main Controls:",
    textInput("js", "Days at Each House:", value = "(4, 8), (3, 7), (5, 1)", placeholder = "ex. (4, 8), (3, 7), (5, 1)"),
    numericInput("sw", "Within-house SD:", min = 0, max = 50, value = 1),
    sliderInput("p", "Proportion of Grp-1 Households:", min = 0, max = 1, value = 0.90, step = 0.01)
  )
}

group1_Cont <- function(){#sets up the controls for group 1
  accordion_panel("Group 1:",
                  numericInput("mu_1","Mean 1:", min = 0, max = 100, value = 5),
                  numericInput("sd_1","Standard Deviation 1:", min = 1, max = 5, value = 1))
  
}

group2_Cont <- function(){ #sets up the controls for group 2
  accordion_panel("Group 2",
                  numericInput("mu_2","Mean 2:", min = 0, max = 100, value = 10),
                  numericInput("sd_2","Standard Deviation 2:", min = 1, max = 5, value = 1))
}

mainOutput <- function(){
  #sets up the main output of graphs
  bslib:::navset_card_underline(
    id = "tabs",
    title = "Visualizations",
    height = "500px",
    ## Panels for the plots----
    tab1(), # sets up first plot for mixed-normal distribution
    tab2() # sets up density curve for t-distrution
  )
}

tab1 <- function(){
  nav_panel("The Population",
            layout_columns(plotOutput("plot1")
            )
  )
}

tab2 <- function(){
  nav_panel("Plot of the t-statistics",
            plotOutput("plot2"),
            htmlOutput("report")
  )
  
}


###### UI FUNCTION STARTS HERE------
ui <- page_sidebar(
  mainOutput(),
  sidebar = sidebar(
    accordion(
      mainCont(),
      accordion(
        group1_Cont(),
        group2_Cont()
      )
    ),
    sliderInput("level", "Level of Confidence:", min = 0.10, max = 0.99, value = .90, step = 0.01),
    numericInput("reps", "Number of Samples:", min = 50, value = 10000),
    actionButton("simulate","Start Simulation")
  ),
)

###### SERVER FUNCTION STARTS HERE------
server <- function(input, output, session) {
  
  observeEvent(input$simulate, {
    nav_select("tabs", "Plot of the t-statistics")
  })
  
  sim_results <- eventReactive(input$simulate, {
    #simulates the means of means for second graph
    withProgress(message = "Taking samples ...", min = 0, max = 1, value = 0, {
      simulate(
        reps = input$reps,
        house =  list(
          p = input$p,
          mu1 = input$mu_1,
          sd1 = input$sd_1,
          mu2 = input$mu_2,
          sd2 = input$sd_2,
          sw = input$sw),
        js = input$js,
        level = input$level
      )
    })
  })
  
  output$report <- renderText({
    perc <- round(mean(sim_results()$good) * 100, 2)
    num_good <- sum(sim_results()$good)
    numb <- isolate(input$reps)
    glue::glue(
      '<table class="table table-bordered">
        <thead>
          <tr>
            <th scope="col">Total Intervals</th>
            <th scope="col">Intervals Covering the Mean</th>
            <th scope="col">Coverage Rate</th>
          </tr>
        <tbody>
          <tr>
            <td>{numb}</td>
            <td>{num_good}</td>
            <td>{perc}%</td>
          </tr>
        </tbody>
      </table>
      '
    )
  })
  
  
  output$plot2 <- renderPlot({
    req(length(input$js >= 2))
    data <- data.frame(t = sim_results()$t)
    df <- sim_results()$n - 1
    ggplot(data, aes(x = t)) +
      geom_density(fill = "skyblue", color = "black") +
      stat_function(
        fun = dt,
        n = 101,
        args = list(df = df),
        color = "red"
      )
  })
  
  output$plot1 <- renderPlot({
    #renders the first plot of household mean-per capita usage
    req(input$mu_1, input$sd_1, input$mu_2, input$sd_2)
    xlims <- c(
      lower = min(input$mu_1 - 3 * input$sd_1, input$mu_2 - 3 * input$sd_2),
      upper = max(input$mu_1 + 3 *input$sd_1, input$mu_2 + 3 * input$sd_2)
    )
    ggplot(data.frame(x = xlims), aes(x = x)) +
      stat_function(
        fun = pop_fun, 
        n = 101, 
        args = list(
          mu1 = input$mu_1, 
          sd1 = input$sd_1,
          mu2 = input$mu_2,
          sd2 = input$sd_2,
          p = input$p
        )) + 
      labs(
        x = "household mean per-capita usage",
        y = NULL,
        title = paste0(
          "The mean of the population is ",
          round(input$p * input$mu_1 + (1 - input$p) * input$mu_2, 2)
        )
      )
  })
  
}


shinyApp(ui, server)