# ui.R
# Defines the user interface of the Shiny application.
library(DT)
library(shiny)
library(shinydashboard) # Ensure shinydashboard is loaded for dashboardPage, etc.
library(lubridate) # Ensure lubridate is loaded for date functions if not already

ui <- dashboardPage(
  dashboardHeader(title = "Performance Reporting"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu", # <--- CRITICAL FIX: Added ID for conditionalPanel to work
      menuItem("Traditional Fund Performance", tabName = "traditional_fund_perf", icon = icon("chart-line")),
      menuItem("Hedge Fund Performance", tabName = "hedge_fund_perf", icon = icon("chart-pie")),
      menuItem("Portfolio Blending", tabName = "portfolio_blending", icon = icon("calculator")),
      menuItem("Risk Analysis", tabName = "risk_analysis", icon = icon("exclamation-triangle")), # New tab for risk
      menuItem("Benchmark Analysis", tabName = "benchmark_analysis", icon = icon("chart-bar")), # Updated tab
      menuItem("Sharpe Ratio Analysis", tabName = "sharpe_ratio_analysis", icon = icon("chart-area")), # New tab for Sharpe Ratio
      menuItem("Retirement Ruin Probability", tabName = "retirement_planning", icon = icon("piggy-bank")) # Renamed tab
    ),
    
    # Inputs for Traditional Fund Performance Tab
    conditionalPanel(
      condition = "input.sidebar_menu == 'traditional_fund_perf'",
      selectInput("selected_fund",
                  "Select Fund:",
                  choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0), # Robust choices
                  selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[1] else NULL),
      
      dateRangeInput("date_range_trad",
                     "Select Date Range:",
                     start = if(exists("funds_xts") && NROW(funds_xts) > 0) min(index(funds_xts)) else Sys.Date() - 365,
                     end = if(exists("funds_xts") && NROW(funds_xts) > 0) max(index(funds_xts)) else Sys.Date(),
                     min = if(exists("funds_xts") && NROW(funds_xts) > 0) min(index(funds_xts)) else Sys.Date() - (365*10),
                     max = if(exists("funds_xts") && NROW(funds_xts) > 0) max(index(funds_xts)) else Sys.Date())
    ),
    
    # Inputs for Hedge Fund Performance Tab
    conditionalPanel(
      condition = "input.sidebar_menu == 'hedge_fund_perf'",
      selectInput("selected_hedge_fund",
                  "Select Hedge Fund:",
                  choices = if(exists("hna_fund_returns_set") && NCOL(hna_fund_returns_set) > 0) colnames(hna_fund_returns_set) else character(0), # Robust choices
                  selected = if(exists("hna_fund_returns_set") && NCOL(hna_fund_returns_set) > 0) colnames(hna_fund_returns_set)[1] else NULL),
      
      dateRangeInput("date_range_hedge",
                     "Select Date Range:",
                     start = if(exists("hna_fund_returns_set") && NROW(hna_fund_returns_set) > 0) min(index(hna_fund_returns_set)) else Sys.Date() - 365,
                     end = if(exists("hna_fund_returns_set") && NROW(hna_fund_returns_set) > 0) max(index(hna_fund_returns_set)) else Sys.Date(),
                     min = if(exists("hna_fund_returns_set") && NROW(hna_fund_returns_set) > 0) min(index(hna_fund_returns_set)) else Sys.Date() - (365*10),
                     max = if(exists("hna_fund_returns_set") && NROW(hna_fund_returns_set) > 0) max(index(hna_fund_returns_set)) else Sys.Date())
    ),
    
    # Inputs for Portfolio Blending Tab
    conditionalPanel(
      condition = "input.sidebar_menu == 'portfolio_blending'",
      h4("Portfolio Construction"),
      selectInput("blend_fund1", "Select Fund 1:", choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0), selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[1] else NULL),
      numericInput("blend_weight1", "Weight for Fund 1 (e.60):", value = 0.60, min = 0, max = 1, step = 0.01),
      
      selectInput("blend_fund2", "Select Fund 2:", choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0), selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[2] else NULL),
      numericInput("blend_weight2", "Weight for Fund 2 (e.40):", value = 0.40, min = 0, max = 1, step = 0.01),
      
      selectInput("rebalance_freq", "Rebalancing Frequency:",
                  choices = c("years", "quarters", "months", "none"),
                  selected = "years"),
      
      h4("Select Benchmarks for Comparison"),
      selectInput("benchmark_asisa", "Select ASISA Benchmark:", choices = if(exists("Indices") && NCOL(Indices) > 0) colnames(Indices) else character(0),
                  selected = if(exists("Indices") && NCOL(Indices) > 0) "(ASISA) South African MA High Equity" else NULL),
      selectInput("benchmark_cpi", "Select CPI Target:", choices = if(exists("Indices") && NCOL(Indices) > 0) colnames(Indices) else character(0),
                  selected = if(exists("Indices") && NCOL(Indices) > 0) "CPI + 4%" else NULL)
    ),
    
    # Inputs for Risk Analysis Tab
    conditionalPanel(
      condition = "input.sidebar_menu == 'risk_analysis'",
      h4("Risk Analysis Settings"),
      selectInput("risk_portfolio_type", "Select Portfolio Type:",
                  choices = c("Traditional Fund", "Blended Portfolio"),
                  selected = "Traditional Fund"),
      
      conditionalPanel(
        condition = "input.risk_portfolio_type == 'Traditional Fund'",
        selectInput("risk_selected_trad_fund", "Select Traditional Fund:",
                    choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0),
                    selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[1] else NULL)
      ),
      
      conditionalPanel(
        condition = "input.risk_portfolio_type == 'Blended Portfolio'",
        selectInput("risk_blend_fund1", "Select Fund 1 for Blended Portfolio:",
                    choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0),
                    selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[1] else NULL),
        selectInput("risk_blend_fund2", "Select Fund 2 for Blended Portfolio:",
                    choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0),
                    selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[2] else NULL)
      ),
      
      h4("Select Benchmarks & CPI for Comparison"),
      selectInput("risk_benchmark_select", "Select ASISA Benchmark:",
                  choices = if(exists("Indices") && NCOL(Indices) > 0) colnames(Indices) else character(0),
                  selected = if(exists("Indices") && NCOL(Indices) > 0) "(ASISA) South African MA High Equity" else NULL),
      selectInput("risk_cpi_select", "Select CPI Target:",
                  choices = if(exists("Indices") && NCOL(Indices) > 0) colnames(Indices) else character(0),
                  selected = if(exists("Indices") && NCOL(Indices) > 0) "CPI + 4%" else NULL),
      
      numericInput("horizon_years_input", "Horizon (Years):", value = 5, min = 1, max = 30, step = 1),
      numericInput("n_sim_input", "Number of Simulations:", value = 100000, min = 1000, max = 1000000, step = 1000)
    ),
    
    # Inputs for Benchmark Analysis Tab (NEW)
    # Inputs for Benchmark Analysis Tab (NEW)
    conditionalPanel(
      condition = "input.sidebar_menu == 'benchmark_analysis'",
      h4("Benchmark Analysis Settings"),
      selectInput("benchmark_analysis_portfolio_type", "Select Portfolio Type:",
                  choices = c("Traditional Fund"),  # Removed "Blended Portfolio"
                  selected = "Traditional Fund"),
      
      conditionalPanel(
        condition = "input.benchmark_analysis_portfolio_type == 'Traditional Fund'",
        selectInput("benchmark_analysis_selected_trad_fund", "Select Traditional Fund:",
                    choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0),
                    selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[1] else NULL)
      ),
      
      selectInput("benchmark_analysis_benchmarks", "Select Benchmarks (Multi-select):",
                  choices = if(exists("Indices") && NCOL(Indices) > 0) colnames(Indices) else character(0),
                  multiple = TRUE,
                  selected = if(exists("Indices") && NCOL(Indices) > 0) c("(ASISA) South African MA High Equity", "MSCI World NR USD") else NULL),
      
      dateRangeInput("date_range_benchmark",
                     "Select Date Range:",
                     start = if(exists("funds_xts") && NROW(funds_xts) > 0) min(index(funds_xts)) else Sys.Date() - 365,
                     end = if(exists("funds_xts") && NROW(funds_xts) > 0) max(index(funds_xts)) else Sys.Date(),
                     min = if(exists("funds_xts") && NROW(funds_xts) > 0) min(index(funds_xts)) else Sys.Date() - (365*10),
                     max = if(exists("funds_xts") && NROW(funds_xts) > 0) max(index(funds_xts)) else Sys.Date())
    ),
    
    # Inputs for Sharpe Ratio Analysis Tab
    conditionalPanel(
      condition = "input.sidebar_menu == 'sharpe_ratio_analysis'",
      h4("Sharpe Ratio Settings"),
      selectInput("sharpe_fund_selection", "Select Fund/Portfolio for Sharpe Ratio:",
                  choices = c("Traditional Fund", "Blended Portfolio"), # Changed "Selected Traditional Fund" to "Traditional Fund" for clarity
                  selected = "Traditional Fund"),
      
      # Conditional dropdown for Traditional Fund selection
      conditionalPanel(
        condition = "input.sharpe_fund_selection == 'Traditional Fund'",
        selectInput("sharpe_selected_trad_fund", "Select Specific Traditional Fund:",
                    choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0),
                    selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[1] else NULL)
      ),
      
      # Conditional dropdowns for Blended Portfolio selection in Sharpe tab
      conditionalPanel(
        condition = "input.sharpe_fund_selection == 'Blended Portfolio'",
        selectInput("sharpe_blend_fund1", "Select Fund 1 for Blended Portfolio:",
                    choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0),
                    selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[1] else NULL),
        selectInput("sharpe_blend_fund2", "Select Fund 2 for Blended Portfolio:",
                    choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0),
                    selected = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts)[2] else NULL)
      ),
      
      numericInput("sharpe_rolling_window_years", "Rolling Window (Years):", value = 2, min = 1, max = 10, step = 1)
    ),
    
    # Inputs for Retirement Ruin Probability Tab
    conditionalPanel(
      condition = "input.sidebar_menu == 'retirement_planning'", # This is the tab we are keeping
      h4("Retirement Simulation Settings"),
      selectInput("retirement_fund", "Select Fund:",
                  choices = if(exists("funds_xts") && NCOL(funds_xts) > 0) colnames(funds_xts) else character(0)),
      numericInput("retirement_principal", "Initial Principal:", value = 100000, min = 0),
      numericInput("retirement_years", "Time Horizon (Years):", value = 35, min = 1),
      numericInput("retirement_sims", "Number of Simulations:", value = 10000, min = 1000),
      sliderInput("retirement_inflation", "Annual Inflation Adjustment:",
                  min = 0, max = 0.1, value = 0.05, step = 0.01),
      actionButton("run_retirement_sim", "Run Simulation", class = "btn-primary")
    )
    
  ),
  dashboardBody(
    tabItems(
      # First tab: Traditional Fund Performance
      tabItem(tabName = "traditional_fund_perf",
              h2("Selected Traditional Fund Performance"),
              fluidRow(
                column(width = 4, # Column for stacked TER and Alpha
                       box(title = "Total Expense Ratio (TER)", status = "primary", solidHeader = TRUE,
                           width = 12, verbatimTextOutput("fund_ter_trad")),
                       box(title = "Alpha (vs. MSCI World)", status = "primary", solidHeader = TRUE,
                           width = 12, verbatimTextOutput("fund_alpha_trad"))
                ),
                box(title = "Key Performance Metrics", status = "primary", solidHeader = TRUE,
                    width = 8, tableOutput("trad_fund_summary_metrics_table"))
              ),
              fluidRow(
                box(title = "Cumulative Returns Plot", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("cumulative_returns_plot_trad"))
              ),
              fluidRow(
                box(title = "Annual Returns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("trad_fund_annual_returns_plot"))
              )
      ),
      
      # Second tab: Hedge Fund Performance
      tabItem(tabName = "hedge_fund_perf",
              h2("Selected Hedge Fund Performance"),
              fluidRow(
                column(width = 6, # Column for stacked Sortino and Calmar
                       box(title = "Sortino Ratio", status = "primary", solidHeader = TRUE,
                           width = 12, verbatimTextOutput("hedge_fund_sortino_ratio")),
                       box(title = "Calmar Ratio", status = "primary", solidHeader = TRUE,
                           width = 12, verbatimTextOutput("hedge_fund_calmar_ratio"))
                ),
                box(title = "Key Performance Metrics (Hedge Fund)", status = "primary", solidHeader = TRUE,
                    width = 6, tableOutput("hedge_fund_summary_metrics_table")) # New table for hedge fund summary
              ),
              fluidRow(
                box(title = "Hedge Fund Cumulative Returns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("hedge_fund_cumulative_plot"))
              ),
              fluidRow(
                box(title = "Hedge Fund Annual Returns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("hedge_fund_annual_plot"))
              ),
              fluidRow(
                box(title = "Hedge Fund Historical Drawdowns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("hedge_fund_drawdown_plot")) # New plot for hedge fund drawdowns
              )
      ),
      
      # Third tab: Portfolio Blending
      tabItem(tabName = "portfolio_blending",
              h2("Custom Blended Portfolio Performance"),
              fluidRow(
                box(title = "Blended Portfolio Cumulative Returns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("blended_cumulative_plot"))
              ),
              fluidRow(
                box(title = "Blended Portfolio Annual Returns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("blended_annual_plot"))
              ),
              fluidRow( # New row for portfolio optimization results
                box(title = "Max Sharpe Ratio Portfolio Weights", status = "primary", solidHeader = TRUE,
                    width = 6, tableOutput("max_sharpe_weights_table")),
                box(title = "Minimum Volatility Portfolio Weights", status = "primary", solidHeader = TRUE,
                    width = 6, tableOutput("min_vol_weights_table"))
              ),
              fluidRow(
                box(title = "Weight Sum Check", status = "warning", solidHeader = TRUE,
                    width = 12, uiOutput("weight_sum_message"))
              )
      ),
      
      # New tab: Risk Analysis
      tabItem(tabName = "risk_analysis",
              h2("Risk Analysis"),
              fluidRow(
                box(title = "Max Drawdown Probability Density", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("max_drawdown_density_plot"))
              ),
              fluidRow(
                box(title = "Max Drawdown Probability Table", status = "primary", solidHeader = TRUE,
                    width = 12, tableOutput("max_drawdown_prob_table"))
              ),
              fluidRow(
                box(title = "Historical Drawdowns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("historical_drawdowns_plot"))
              ),
              fluidRow(
                box(title = "1-Year Rolling Returns", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("rolling_returns_1yr_plot"))
              ),
              fluidRow(
                box(title = textOutput("rolling_returns_horizon_title"), status = "primary", solidHeader = TRUE, # Dynamic title
                    width = 12, plotOutput("rolling_returns_horizon_plot"))
              )
      ),
      
      # Benchmark Analysis tab (NEW CONTENT)
      tabItem(tabName = "benchmark_analysis",
              h2("Benchmark Analysis"),
              fluidRow(
                box(title = "Cumulative Performance vs. Benchmarks", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("benchmark_cumulative_plot"))
              ),
              fluidRow(
                box(title = "Annual Performance vs. Benchmarks", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("benchmark_annual_plot"))
              ),
              fluidRow(
                box(title = "Rolling 1-Year Performance vs. Benchmarks", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("benchmark_rolling_plot"))
              ),
              fluidRow(
                box(title = "Performance Summary vs. Benchmarks", status = "primary", solidHeader = TRUE,
                    width = 12, tableOutput("benchmark_summary_table"))
              ),
              fluidRow(
                box(title = "Tracking Error and Information Ratio", status = "primary", solidHeader = TRUE,
                    width = 12, tableOutput("benchmark_tracking_error_table"))
              )
      ),
      
      # New tab: Sharpe Ratio Analysis
      tabItem(tabName = "sharpe_ratio_analysis",
              h2("Sharpe Ratio Analysis"),
              fluidRow(
                box(title = textOutput("sharpe_expanding_title"), status = "primary", solidHeader = TRUE, # Dynamic title
                    width = 12, plotOutput("sharpe_expanding_plot")),
                box(title = textOutput("sharpe_rolling_title"), status = "primary", solidHeader = TRUE, # Dynamic title
                    width = 12, plotOutput("sharpe_rolling_plot"))
              )
      ),
      
      # Consolidated Retirement Planning tab
      tabItem(tabName = "retirement_planning",
              h2("Retirement Ruin Probability Analysis"), # Updated title
              fluidRow(
                box(title = "Ruin Probability Simulation", status = "primary", solidHeader = TRUE,
                    width = 12, plotOutput("ruin_probability_plot") %>% withSpinner()), # Added spinner
              ),
              fluidRow(
                box(title = "Simulation Results", status = "primary", solidHeader = TRUE,
                    width = 12, tableOutput("ruin_probability_table"))
              )
      )
    )
  )
)



