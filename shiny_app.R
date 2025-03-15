library(shiny)
options(shiny.mathjax.url = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js")
library(dplyr)
library(tidyr)
library(ggplot2)

ggplot2::theme_set(theme_bw())

stocks <- list(
  "Apple" = "AAPL",
  "Exxon Mobil" = "XOM",
  "UnitedHealthCare" = "UNH",
  "JP Morgan" = "JPM",
  "Costco" = "COST",
  "Microsoft" = "MSFT",
  "Amazon" = "AMZN",
  "Berkshire Hathaway" = "BRK-B",
  "Alphabet (Class A)" = "GOOGL",
  "NVIDIA" = "NVDA",
  "Tesla" = "TSLA",
  "Meta Platforms" = "META",
  "Eli Lilly" = "LLY",
  "Visa" = "V",
  "Procter & Gamble" = "PG",
  "Johnson & Johnson" = "JNJ",
  "Mastercard" = "MA",
  "Home Depot" = "HD",
  "Walmart" = "WMT",
  "Bank of America" = "BAC",
  "Chevron" = "CVX",
  "Coca-Cola" = "KO",
  "PepsiCo" = "PEP"
)

# Define UI -------------------------------------------------------------------
ui <- fluidPage(
  br(),
  withMathJax(),
  sidebarLayout(
    sidebarPanel(
      # h3("Nutzenmaximierung"),
      tabsetPanel(
        tabPanel("Portfolio",
          width = "400px",
          br(),
          p("Portfolio zusammenstellen"),
          dateRangeInput(
            inputId = "date_range",
            label = "Date Range",
            startview = "year",
            start = "2020-01-01",
            min = "2010-01-01",
            max = format(Sys.Date() + 1, "%Y-%m-%d")
          ),
          selectInput(
            "select_portfolio",
            "Select options below:",
            stocks,
            selected = list("AAPL", "XOM"),
            multiple = TRUE
          ),
          actionButton("reset", "Reset")
        ),
        tabPanel(
          "More",
          br(),
          p("Additional settings for the VAR plot"),
          numericInput(
            "var_alpha",
            "VAR α",
            value = 0.01,
            min = 0.001,
            max = 0.1
          ),
          checkboxInput("adjusted_scaling_checkbox", "Adjusted Scaling", TRUE),
          numericInput(
            "pseudo_log_sigma",
            "Pseudo-Log σ",
            value = 0.001,
            min = 0.0001,
            max = 0.1
          )
        )
      )
    ),
    mainPanel(
      align = "center",
      tabsetPanel(
        type = "tabs",
        tabPanel("Portfolio Value", plotOutput(outputId = "returnsPlot")),
        tabPanel("Daily Returns", plotOutput(outputId = "dailyReturnsPlot")),
        tabPanel("Distribution", plotOutput(outputId = "returnsDistribution")),
        tabPanel("VAR Plot", plotOutput(outputId = "varPlot")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

# Define server logic ---------------------------------------------------------
server <- function(input, output) {
  # stock_returns_complete <- read.csv("./stock_returns.csv") %>%
  #   mutate(date = as.Date(date))

  stock_returns_complete <- read.csv(url("https://raw.githubusercontent.com/EinMaulwurf/risk/refs/heads/main/stock_returns.csv")) %>%
    mutate(date = as.Date(date))

  # Reactive: Filter stock returns to portfolio and date range
  stock_returns <- reactive({
    req(input$select_portfolio, input$date_range)
    stock_returns_complete %>%
      filter(symbol %in% input$select_portfolio) %>%
      filter(date >= input$date_range[[1]], date <= input$date_range[[2]])
  })

  # Reactive: Create weights dataframe (updates when selection changes)
  weights_df <- reactive({
    req(input$select_portfolio)
    data.frame(
      symbol = input$select_portfolio,
      weight = rep(1 / length(input$select_portfolio), length(input$select_portfolio))
    )
  })

  # Reactive: Calculate portfolio returns (updates when returns/weights change)
  portfolio_returns <- reactive({
    req(stock_returns(), weights_df())
    stock_returns() %>%
      left_join(weights_df(), by = "symbol") %>%
      group_by(date) %>%
      summarise(return = weighted.mean(return, weight))
  })

  portfolio_mean <- reactive({
    mean(portfolio_returns()$return)
  })

  portfolio_var <- reactive({
    var(portfolio_returns()$return)
  })

  # Outputs that depend on portfolio_returns
  output$returnsPlot <- renderPlot(
    {
      portfolio_returns() %>%
        mutate(cum_return = cumprod(1 + return)) %>%
        ggplot(aes(x = date, y = cum_return)) +
        geom_line()
    },
    res = 100
  )

  output$dailyReturnsPlot <- renderPlot(
    {
      portfolio_returns() %>%
        ggplot(aes(x = date, y = return)) +
        geom_col()
    },
    res = 100
  )

  output$returnsDistribution <- renderPlot(
    {
      portfolio_returns() %>%
        ggplot() +
        geom_density(aes(x = return)) +
        stat_function(
          fun = dnorm, args = list(mean = portfolio_mean(), sd = sqrt(portfolio_var())),
          linetype = "dashed"
        )
    },
    res = 100
  )

  output$varPlot <- renderPlot(
    {
      empirical_var_alpha <- quantile(portfolio_returns()$return, probs = input$var_alpha, names = FALSE, type = 3)
      normal_var_alpha <- qnorm(input$var_alpha, mean = portfolio_mean(), sd = sqrt(portfolio_var()))

      p <- portfolio_returns() %>%
        ggplot(aes(x = return)) +
        stat_ecdf() +
        stat_function(
          fun = pnorm, args = list(mean = portfolio_mean(), sd = sqrt(portfolio_var())),
          linetype = "dashed"
        ) +
        # Add lines showing empirical VaR values
        annotate(
          aes(x = empirical_var_alpha, xend = empirical_var_alpha, y = 0, yend = input$var_alpha), 
          color = "magenta") +
        geom_segment(
          aes(x = empirical_var_alpha, xend = min(portfolio_returns()$return), y = input$var_alpha, yend = input$var_alpha), 
          color = "magenta") +
        # Add lines showing normal distribution VaR
        geom_segment(
          aes(x = normal_var_alpha, xend = normal_var_alpha, y = 0, yend = input$var_alpha), 
          color = "magenta", linetype = "dashed") +
        geom_segment(
          aes(x = normal_var_alpha, xend = min(portfolio_returns()$return), y = input$var_alpha, yend = input$var_alpha), 
          color = "magenta", linetype = "dashed") +
        # Scale options and transformation
        scale_y_continuous(
          limits = c(0, 0.2),
          transform = scales::pseudo_log_trans(sigma = .0001),
          breaks = c(0, 0.001, 0.01, 0.05, 0.25, 0.5, 1)
        ) +
        scale_x_continuous(breaks = seq(-0.125, 0.1, by = 0.025), limits = c(-0.125, 0.025))

      p
    },
    res = 100
  )

  output$table <- renderTable({
    portfolio_returns() %>%
      slice_head(n = 10)
  })

  observeEvent(input$reset, {
    updateSelectInput(inputId = "select_portfolio", selected = list("AAPL", "XOM"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
