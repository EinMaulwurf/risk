library(shiny)
options(shiny.mathjax.url = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js")
library(tidyverse)

stocks <- list(
  "Apple" = "AAPL",
  "Exxon Mobil" = "XOM",
  "UnitedHealthCare" = "UNH",
  "JP Morgan" = "JPM",
  "Costco" = "COST",
  "Microsoft" = "MSFT",
  "Amazon" = "AMZN",
  "Berkshire Hathaway" = "BRK.B",
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
            min = "2000-01-01",
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
          "Erweitert",
          br(),
          p("Lorem Ipsum")
        )
      )
    ),
    mainPanel(
      align = "center",
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", plotOutput(outputId = "returnsPlot")),
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)

# Define server logic ---------------------------------------------------------
server <- function(input, output) {
  # Reactive: Get stock data (updates when date/selection changes)
  stock_data <- reactive({
    req(input$select_portfolio, input$date_range)
    tidyquant::tq_get(
      input$select_portfolio,
      from = input$date_range[[1]],
      to = input$date_range[[2]]
    )
  })

  # Reactive: Calculate individual stock returns
  stock_returns <- reactive({
    req(stock_data())
    stock_data() %>%
      select(symbol, date, adjusted) %>%
      group_by(symbol) %>%
      mutate(return = (adjusted - lag(adjusted)) / lag(adjusted)) %>%
      slice_tail(n = -1) %>%
      ungroup()
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

  # Outputs that depend on portfolio_returns
  output$returnsPlot <- renderPlot(
    {
      portfolio_returns() %>%
        mutate(cum_return = cumprod(1 + return)) %>%
        ggplot(aes(x = date, y = cum_return)) +
        geom_line() +
        labs(title = "Portfolio Cumulative Returns")
    },
    res = 100
  )

  output$table <- renderTable({
    portfolio_returns()
  })

  observeEvent(input$reset, {
    updateSelectInput(inputId = "select_portfolio", selected = list("AAPL", "XOM"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
