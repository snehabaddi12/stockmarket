# Install and load required packages
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")
#install.packages("plotly")
library(shiny)
library(shinydashboard)
library(DT)  # For interactive tables
library(plotly)  # For interactive plots

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Stock Market Analysis Dashboard"),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Close Price Analysis", tabName = "close_price", icon = icon("line-chart")),
      menuItem("Returns Analysis", tabName = "returns", icon = icon("chart-line")),
      menuItem("Portfolio Analysis", tabName = "portfolio", icon = icon("briefcase")),
      menuItem("Performance Metrics", tabName = "metrics", icon = icon("table"))
    )
  ),
  
  # Main Panel
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(plotlyOutput("stock_prices_plot"), width = 12,
                    title = "Stock Prices Over Time"),
                box(plotlyOutput("macro_indicators_plot"), width = 12,
                    title = "Macroeconomic Indicators"),
                box(plotlyOutput("sector_index_plot"), width = 12,
                    title = "Sector Index")
              )
      ),
      
      # Close Price Analysis Tab
      tabItem(tabName = "close_price",
              fluidRow(
                # Dropdown to select stock
                box(selectInput("stock_select_close", "Select Stock:",
                                choices = c("AAPL", "AMZN", "MSFT", "NVDA")),
                    width = 12),
                # Actual vs Predicted Plot
                box(plotlyOutput("close_prediction_plot"), width = 12),
                # Feature Importance
                box(plotOutput("close_importance_plot"), width = 12)
              )
      ),
      
      # Returns Analysis Tab
      tabItem(tabName = "returns",
              fluidRow(
                box(selectInput("stock_select_returns", "Select Stock:",
                                choices = c("AAPL", "AMZN", "MSFT", "NVDA")),
                    width = 12),
                box(plotlyOutput("returns_prediction_plot"), width = 12),
                box(plotOutput("returns_importance_plot"), width = 12)
              )
      ),
      
      # Portfolio Analysis Tab
      tabItem(tabName = "portfolio",
              fluidRow(
                box(plotlyOutput("portfolio_close_plot"), width = 6,
                    title = "Portfolio Close Price Performance"),
                box(plotlyOutput("portfolio_returns_plot"), width = 6,
                    title = "Portfolio Returns Performance"),
              #  box(plotOutput("performance_comparison_plot"), width = 12,
              #      title = "Performance Metrics Comparison")
              )
      ),
      
      # Metrics Tab
      tabItem(tabName = "metrics",
              fluidRow(
                box(DT::dataTableOutput("metrics_table"), width = 12,
                    title = "Performance Metrics")
              )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  # Overview Plots
  output$stock_prices_plot <- renderPlotly({
    # Your existing stock prices plot converted to plotly
    gg <- ggplot(merged_data_df, aes(x = Date)) +
      geom_line(aes(y = AAPL_Close, color = "Apple")) +
      geom_line(aes(y = AMZN_Close, color = "Amazon")) +
      geom_line(aes(y = MSFT_Close, color = "Microsoft")) +
      geom_line(aes(y = NVDA_Close, color = "Nvidia"))
    ggplotly(gg)
  })
  
  output$macro_indicators_plot <- renderPlotly({
    # Your existing macro_indicators plot converted to plotly
    gg <- ggplot(merged_data_df, aes(x = Date)) +
      geom_line(aes(y = GDP_Norm, color = "GDP")) +
      geom_line(aes(y = Inflation_Norm, color = "Inflation")) +
      geom_line(aes(y = Interest_Rates_Norm, color = "Interest Rates")) 
      ggplotly(gg)
  })
  
  output$sector_index_plot <- renderPlotly({
    # Your existing sector_index plot converted to plotly
    gg <- ggplot(merged_data_df, aes(x = Date)) +
      geom_line(aes(y = SP500_Close, color = "S&P500")) +
      geom_line(aes(y = NASDAQ_Close, color = "NASDAQ")) 
      ggplotly(gg)
  })
  
  # The close price prediction plot:
  
  output$close_prediction_plot <- renderPlotly({
    stock <- input$stock_select_close
    
    # Get the length of predictions for the selected stock
    n_pred <- length(results[[stock]]$predictions)
    
    # Create new plot data using the correct number of dates
    plot_data <- data.frame(
      Date = tail(test_data$Date, n_pred),
      Actual = results[[stock]]$actual,
      Predicted = results[[stock]]$predictions
    )
    
    # Create plotly plot
    plot_ly(plot_data, x = ~Date) %>%
      add_lines(y = ~Actual, name = "Actual", 
                line = list(color = "blue")) %>%
      add_lines(y = ~Predicted, name = "Predicted", 
                line = list(color = "red", dash = "dash")) %>%
      layout(title = paste(stock, "Close Price: Actual vs Predicted"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Close Price"),
             legend = list(orientation = 'h'))
  })
  
  # Returns Predictions
  output$returns_prediction_plot <- renderPlotly({
    stock <- input$stock_select_returns
    ggplotly(results[[stock]]$plot)
  })
  
  # Portfolio Analysis
  output$portfolio_close_plot <- renderPlotly({
    ggplotly(portfolio_results$portfolio_plot)
  })
  
  output$portfolio_returns_plot <- renderPlotly({
    ggplotly(returns_portfolio$plot)
  })
  
  # Metrics Table
  output$metrics_table <- DT::renderDataTable({
    # Combine metrics from both models
    metrics_df <- data.frame(
      Stock = stock_symbols,
      Close_RMSE = sapply(results, function(x) x$metrics$RMSE),
      Close_MAE = sapply(results, function(x) x$metrics$MAE),
      Returns_Direction_Accuracy = sapply(results, function(x) x$metrics$Direction_Accuracy)
    )
    DT::datatable(metrics_df, options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui, server)