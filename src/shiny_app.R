library(shiny)
library(bslib)
#options(shiny.mathjax.url = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js")
library(dplyr)
#library(tidyr)
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
      tabsetPanel(
        tabPanel("Portfolio",
          width = "400px",
          br(),
          p("Choose portfolio"),
          dateRangeInput(
            inputId = "date_range",
            label = "Date Range",
            startview = "year",
            start = "2020-01-01",
            end = "2024-12-31",
            min = "2000-01-01",
            max = "2024-12-31"
          ),
          selectInput(
            "select_portfolio",
            "Select options below:",
            stocks,
            selected = list("AAPL", "XOM"),
            multiple = TRUE
          ),
          actionButton("reset_portfolio", "Reset")
        ),
        tabPanel(
          "More",
          br(),
          p("Additional settings for the VAR plot"),
          numericInput(
            "var_alpha",
            "VAR (1-α)",
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
          ),
          actionButton("reset_plot", "Reset")
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
        tabPanel(
          "Values",
          br(),
          fluidRow(
            column(
              6,
              value_box(
                title = "Empirical Value-at-Risk",
                value = uiOutput("var_empirical_output"),
                theme = "bg-gradient-blue-purple",
              )
            ),
            column(
              6,
              value_box(
                title = "Parametric Value-at-Risk",
                value = uiOutput("var_normal_output"),
                theme = "bg-gradient-indigo-blue",
              )
            )
          ),
          fluidRow(
            column(
              6,
              value_box(
                title = "Empirical Expected-Shortfall",
                value = uiOutput("es_empirical_output"),
                theme = "bg-gradient-blue-purple",
              )
            ),
            column(
              6,
              value_box(
                title = "Parametric Expected-Shortfall",
                value = uiOutput("es_normal_output"),
                theme = "bg-gradient-indigo-blue",
              )
            ),
            column(
              12,
              value_box(
                title = "Maximum drawdown",
                value = uiOutput("max_drawdown_output"),
                theme = "bg-gradient-indigo-blue",
              )
            )
          )
        )
      )
    )
  )
)

# Define server logic ---------------------------------------------------------
server <- function(input, output) {
  # stock_returns_complete <- read.csv("./stock_returns.csv") %>%
  #   mutate(date = as.Date(date))

  stock_returns_complete <- read.csv(url("https://raw.githubusercontent.com/EinMaulwurf/risk/refs/heads/main/assets/stock_returns.csv")) %>%
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
        geom_col(width = 2)
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
          geom = "segment",
          x = empirical_var_alpha, xend = empirical_var_alpha, y = 0, yend = input$var_alpha,
          color = "magenta"
        ) +
        annotate(
          geom = "segment",
          x = empirical_var_alpha, xend = min(portfolio_returns()$return), y = input$var_alpha, yend = input$var_alpha,
          color = "magenta"
        ) +
        # Add lines showing normal distribution VaR
        annotate(
          geom = "segment",
          x = normal_var_alpha, xend = normal_var_alpha, y = 0, yend = input$var_alpha,
          color = "magenta", linetype = "dashed"
        ) +
        annotate(
          geom = "segment",
          x = normal_var_alpha, xend = min(portfolio_returns()$return), y = input$var_alpha, yend = input$var_alpha,
          color = "magenta", linetype = "dashed"
        )

      # Scale options and transformation
      if (input$adjusted_scaling_checkbox) {
        p <- p + scale_y_continuous(
          transform = scales::pseudo_log_trans(sigma = input$pseudo_log_sigma),
          breaks = c(0, 0.001, 0.01, 0.05, 0.25, 0.5, 1)
        ) +
          scale_x_continuous(
            #breaks = seq(-0.2, 0.1, by = 0.025), 
            limits = c(min(portfolio_returns()$return), quantile(portfolio_returns()$return, 0.95)))
      }

      p
    },
    res = 100
  )

  # output$table <- renderTable({
  #   portfolio_returns() %>%
  #     slice_head(n = 10)
  # })

  output$var_empirical_output <- renderText({
    empirical_var_alpha <- quantile(portfolio_returns()$return, probs = input$var_alpha, names = FALSE)
    paste0(round(empirical_var_alpha * 100, digits = 2), "%")
  })

  output$var_normal_output <- renderText({
    normal_var_alpha <- qnorm(input$var_alpha, mean = portfolio_mean(), sd = sqrt(portfolio_var()))
    paste0(round(normal_var_alpha * 100, digits = 2), "%")
  })

  output$es_empirical_output <- renderText({
    empirical_es_alpha <- portfolio_returns() %>%
      filter(return < quantile(return, probs = input$var_alpha)) %>%
      pull(return) %>%
      mean()
    paste0(round(empirical_es_alpha * 100, digits = 2), "%")
  })

  output$es_normal_output <- renderText({
    normal_es_alpha <- portfolio_mean() - sqrt(portfolio_var()) * dnorm(qnorm(input$var_alpha, 0, 1)) / input$var_alpha
    paste0(round(normal_es_alpha * 100, digits = 2), "%")
  })
  
  output$max_drawdown_output <- renderText({
    max_drawdown <- min(portfolio_returns()$return)
    paste0(round(max_drawdown * 100, digits = 2), "%")
  })

  observeEvent(input$reset_portfolio, {
    updateSelectInput(inputId = "select_portfolio", selected = list("AAPL", "XOM"))
  })

  observeEvent(input$reset_plot, {
    updateNumericInput(inputId = "var_alpha", value = 0.01)
    updateNumericInput(inputId = "pseudo_log_sigma", value = 0.001)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
