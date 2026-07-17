# Defines the server-side logic of the Shiny application.
# The 'funds_xts', 'Indices', 'CPI', 'TER', 'hna_fund_returns_set', 'HNA_comp', 'hf_comp_subset',
# 'categories_hna', 'type_hna' objects are already loaded into the global environment by global.R.
# server.R
# Defines the server-side logic of the Shiny application.

library(shiny)
library(PerformanceAnalytics)
library(xts)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)
library(MKinfer) # For rmaxdd
library(zoo) # For rollapply
library(timeSeries) # For as.timeSeries
library(SharpeR) # New library for Sharpe Ratio calculations
library(lubridate) # Ensure lubridate is loaded for date operations
library(doParallel) # Already loaded in global.R
library(foreach)    # Already loaded in global.R
library(xts)        # Already loaded in global.R
library(PerformanceAnalytics) #
# Load required libraries
library(ggplot2)    # For plotting
library(lubridate)  # For date manipulation
library(dplyr)      # For data manipulation (though not used directly in this approach)
library(reshape2)   # For reshaping data


server <- function(input, output, session) {
  print("Server function started.") # Debug print
  
  # --- Helper Function: simulate_withdrawal (Client's Robust Version) ---
  # This function simulates portfolio values and calculates ruin probabilities
  # based on historical returns, initial principal, withdrawal rates, and inflation.
  simulate_withdrawal <- function(returns, n_months_ahead, principal, n_sim, escalation_rate, seq_withdrawal_perc = c(0.04, 0.05, 0.06, 0.07, 0.08)) {
    # Ensure necessary libraries are loaded within the function for parallel processing
    # (though they are also loaded in global.R, it's good practice for standalone functions)
    # library(doParallel) # Already loaded in global.R
    # library(foreach)    # Already loaded in global.R
    # library(xts)        # Already loaded in global.R
    # library(PerformanceAnalytics) # Already loaded in global.R
    
    # Set up parallel backend
    numCores <- detectCores() - 1
    if (numCores < 1) numCores <- 1 # Ensure at least one core
    cl <- makeCluster(numCores)
    registerDoParallel(cl)
    
    # Estimate parameters from historical returns (annualized mean and standard deviation)
    # Convert monthly returns to annualized for mean and sd estimation
    # The client's script uses raw mean/sd of monthly returns directly for rnorm,
    # which implies monthly mean and sd are used for monthly simulations.
    # Let's stick to their original approach for consistency with their logic.
    ret_target <- mean(returns, na.rm = TRUE)
    vol_target <- sd(returns, na.rm = TRUE)
    
    # Handle cases where volatility might be zero or NA (e.g., constant returns)
    if (is.na(vol_target) || vol_target == 0) {
      vol_target <- 1e-9 # Small non-zero value to prevent errors
    }
    
    # Simulate return series and store them in a matrix
    # Each column is one simulation path
    sim_out <- replicate(n_sim, rnorm(n_months_ahead, mean = ret_target, sd = vol_target))
    colnames(sim_out) <- paste0("sim_", 1:n_sim)
    # Convert to xts object with sequential dates for clarity, though not strictly needed for calculation
    sim_out <- as.xts(sim_out, order.by = seq(as.Date(Sys.Date()), by = "months", length.out = nrow(sim_out)))
    
    # Sequence of months (years) for which to calculate probabilities
    seq_months <- seq(from = 12, to = n_months_ahead, by = 12)
    
    # Parallelized loop for each withdrawal percentage
    results_list <- foreach(p = 1:length(seq_withdrawal_perc), .packages = c("xts")) %dopar% {
      start_withdrawal_perc <- seq_withdrawal_perc[p]
      
      # Initialize matrices for storing month-end balances and withdrawal percentages for this withdrawal rate
      month_end_bal_sims <- matrix(0, nrow = nrow(sim_out), ncol = ncol(sim_out))
      withdrawal_perc_sims <- matrix(0, nrow = nrow(sim_out), ncol = ncol(sim_out))
      
      # Simulate month-end balances and withdrawals for each simulation path
      for (l in 1:ncol(sim_out)) { # Loop through each simulation (path)
        month_start <- numeric(nrow(sim_out))
        withdrawal <- numeric(nrow(sim_out))
        month_end <- numeric(nrow(sim_out))
        final_withdrawal_perc <- numeric(nrow(sim_out))
        
        for (i in 1:nrow(sim_out)) { # Loop through each month in the simulation path
          if (i == 1) {
            # Initialize the first month's balance and withdrawal
            month_start[i] <- principal
            withdrawal[i] <- ((month_start[i] * start_withdrawal_perc) / 12)
            final_withdrawal_perc[i] <- withdrawal[i] / month_start[i]
          } else {
            # For subsequent months, start with the end balance of the previous month
            month_start[i] <- month_end[i - 1]
          }
          
          # Handle withdrawal adjustment every 12 months based on escalation_rate (inflation)
          # The client's logic applies the escalation at the start of the year (i.e., month 1, 13, 25, etc.)
          if (i > 1 && (i - 1) %% 12 == 0) { # If it's the start of a new year (e.g., month 13, 25, etc.)
            withdrawal[i] <- withdrawal[i - 1] * (1 + escalation_rate)
          } else if (i > 1) {
            withdrawal[i] <- withdrawal[i - 1] # Carry forward previous month's withdrawal
          }
          
          # Check for ruin *before* calculating withdrawal percentage if month_start is very low
          if (month_start[i] <= 0) {
            month_end[i] <- 0
            final_withdrawal_perc[i] <- NA # Cannot calculate meaningful percentage if already ruined
          } else {
            # Calculate the withdrawal percentage (only if not ruined)
            final_withdrawal_perc[i] <- withdrawal[i] / month_start[i]
            
            # Update month-end balance after return and withdrawal
            month_end[i] <- (month_start[i] * (1 + sim_out[i, l])) - withdrawal[i]
            
            # Check if balance goes negative (ruin condition)
            if (month_end[i] < 0) {
              month_end[i] <- 0 # Set balance to zero if it goes negative to simulate ruin
              # No need to break here, the loop will naturally continue to the end of the horizon
              # but subsequent month_start values will be 0, leading to continued ruin.
            }
          }
        }
        
        # Store results for this simulation path
        month_end_bal_sims[, l] <- month_end
        withdrawal_perc_sims[, l] <- final_withdrawal_perc
      }
      
      # Calculate ruin probabilities and withdrawal success rates for this withdrawal percentage
      consistency_row <- numeric(length(seq_months))
      withdrawal_row <- numeric(length(seq_months))
      
      for (j in 1:length(seq_months)) {
        temp_bal <- month_end_bal_sims[seq_months[j], ]
        
        # Calculate ruin probability (percentage of simulations that went to zero)
        consistency_row[j] <- (length(which(temp_bal <= 0)) / length(temp_bal)) * 100 # Use <=0 for ruin
      }
      
      list(consistency_row = consistency_row, withdrawal_row = withdrawal_row)
    }
    
    # Stop the parallel backend
    stopCluster(cl)
    
    # Extract results into matrices
    ruin_mat <- matrix(0, nrow = length(seq_withdrawal_perc), ncol = length(seq_months))
    withdrawal_mat <- matrix(0, nrow = length(seq_withdrawal_perc), ncol = length(seq_months))
    
    for (p in 1:length(seq_withdrawal_perc)) {
      ruin_mat[p, ] <- results_list[[p]]$consistency_row
      withdrawal_mat[p, ] <- results_list[[p]]$withdrawal_row # Corrected: Removed extra ']]'
    }
    
    # Prepare data frame for plotting
    years <- seq(from = 1, to = (n_months_ahead / 12), by = 1)
    df_plot <- cbind(years, t(ruin_mat))
    df_plot <- data.frame(df_plot)
    colnames(df_plot) <- c(
      "Years",
      "WR4", # Renamed for easier access in ggplot
      "WR5",
      "WR6",
      "WR7",
      "WR8"
    )
    
    # Return the results
    list(ruin_mat = df_plot, withdrawal_mat = withdrawal_mat)
  }
  
  
  # --- Helper Function: Performance Appraisal (Existing, modularized) ---
  # This helper is used for Traditional Fund and Portfolio Blending tabs
  performance_appraisal <- function(returns_xts_input) {
    
    if (!inherits(returns_xts_input, "xts")) {
      warning("Input to performance_appraisal is not an xts object. Attempting conversion.")
      returns_xts_input <- tryCatch(as.xts(returns_xts_input), error = function(e) {
        message("Failed to convert input to xts: ", e$message)
        return(NULL)
      })
      if (is.null(returns_xts_input)) {
        return(list(cumRetPlot = NULL, yearly_ret_plot = NULL))
      }
    }
    
    if (is.null(returns_xts_input) || NROW(returns_xts_input) == 0 || NCOL(returns_xts_input) == 0) {
      return(list(cumRetPlot = NULL, yearly_ret_plot = NULL))
    }
    
    valid_cols <- colSums(is.na(returns_xts_input)) < NROW(returns_xts_input) & colSums(returns_xts_input != 0, na.rm = TRUE) > 0
    returns_xts_input <- returns_xts_input[, valid_cols, drop = FALSE]
    
    if (NCOL(returns_xts_input) == 0) {
      return(list(cumRetPlot = NULL, yearly_ret_plot = NULL))
    }
    
    returns_xts_clean <- na.omit(returns_xts_input)
    
    if (NROW(returns_xts_clean) == 0) {
      return(list(cumRetPlot = NULL, yearly_ret_plot = NULL))
    }
    
    returns_df <- data.frame(
      Date = time(returns_xts_clean),
      coredata(returns_xts_clean)
    )
    
    color_palette <- colorRampPalette(c("blue", "red", "orange", "black", "purple", "green", "brown", "grey"))(ncol(returns_xts_clean))
    colnames(returns_df) <- c("Date", colnames(returns_xts_clean))
    
    cumulative_returns_df <- data.frame(
      Date = time(returns_xts_clean),
      coredata(cumprod(1 + returns_xts_clean) - 1)
    )
    colnames(cumulative_returns_df) <- c("Date", colnames(returns_xts_clean))
    
    cumulative_data <- melt(cumulative_returns_df, id.vars = "Date")
    colnames(cumulative_data) <- c("Date", "Series", "Cumulative_Return")
    cumulative_data$Date <- as.Date(cumulative_data$Date)
    
    cumRetPlot <- ggplot(cumulative_data, aes(x = Date, y = Cumulative_Return * 100, color = Series)) +
      geom_line(size = 1) +
      scale_color_manual(values = color_palette) +
      labs(title = "Cumulative Returns",
           x = "",
           y = "Cumulative Return (%)") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.text.x = element_text(size = 20, margin = ggplot2::margin(t = 10), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20, margin = ggplot2::margin(r = 10)),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    yearly_returns <- returns_df %>%
      mutate(Year = as.integer(year(Date))) %>%
      group_by(Year) %>%
      summarize(across(where(is.numeric), ~ prod(1 + .x, na.rm = TRUE) - 1))
    
    yearly_data <- melt(yearly_returns, id.vars = "Year")
    colnames(yearly_data) <- c("Year", "Series", "Yearly_Return")
    yearly_data$Year <- as.integer(yearly_data$Year)
    
    yearly_ret_plot <- ggplot(yearly_data, aes(x = factor(Year), y = Yearly_Return * 100, fill = Series)) +
      geom_hline(yintercept = 0, linetype = 'dashed') +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
      scale_fill_manual(values = color_palette) +
      labs(title = "Annual Returns",
           y = "Annual Return (%)",
           x = "Year") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    return(list(cumRetPlot = cumRetPlot, yearly_ret_plot = yearly_ret_plot))
  }
  
  # --- Traditional Fund Performance Tab Logic (Existing) ---
  
  filtered_trad_fund_data <- reactive({
    req(input$selected_fund, input$date_range_trad)
    
    fund_data <- funds_xts[, input$selected_fund, drop = FALSE]
    
    start_date <- input$date_range_trad[1]
    end_date <- input$date_range_trad[2]
    
    fund_data_filtered <- fund_data[paste0(start_date, "::", end_date)]
    
    return(fund_data_filtered)
  })
  
  output$fund_ter_trad <- renderText({
    req(input$selected_fund)
    
    ter_value <- TER[input$selected_fund]
    
    if (is.na(ter_value) || !is.numeric(ter_value)) {
      return("N/A")
    }
    
    paste0(round(ter_value * 100, 2), "%")
  })
  
  # Reactive for Alpha Calculation
  fund_alpha_trad <- reactive({
    req(input$selected_fund, input$date_range_trad)
    fund_data <- filtered_trad_fund_data()
    
    # Ensure fund data is available and has enough observations
    if (is.null(fund_data) || NROW(fund_data) < 12) { # Need at least 12 months for meaningful alpha
      return("N/A")
    }
    
    # Select MSCI World NR USD as the benchmark for Alpha calculation
    benchmark_data <- Indices[, "MSCI World NR USD", drop = FALSE]
    
    # Align fund and benchmark data by date
    aligned_data <- na.omit(cbind(fund_data, benchmark_data))
    
    # Ensure aligned data has enough observations and non-zero standard deviation for regression
    if (NROW(aligned_data) < 12 || sd(aligned_data[,1]) == 0 || sd(aligned_data[,2]) == 0) {
      return("N/A")
    }
    
    # Alpha calculation (annualized)
    # CAPM.alpha requires Ra (asset returns), Rb (benchmark returns), and Rf (risk-free rate)
    # Using CPI as risk-free rate, aligned to the same period as fund and benchmark
    aligned_cpi <- CPI[index(aligned_data)]
    if (NROW(aligned_cpi) == 0 || all(is.na(aligned_cpi))) {
      rf_rate <- 0 # Default to 0 if no valid CPI data
      warning("No valid CPI data available for Alpha calculation. Using 0 as risk-free rate.")
    } else {
      rf_rate <- aligned_cpi
    }
    
    # Ensure rf_rate is an xts object or numeric vector of the same length as aligned_data
    if (inherits(rf_rate, "xts")) {
      rf_rate_aligned <- rf_rate[index(aligned_data)]
    } else {
      rf_rate_aligned <- rep(rf_rate, NROW(aligned_data)) # If it's a single mean value
    }
    
    # Calculate alpha
    alpha_value <- CAPM.alpha(Ra = aligned_data[,1], Rb = aligned_data[,2], Rf = rf_rate_aligned, scale = 12) * 100
    
    if (is.na(alpha_value) || !is.numeric(alpha_value)) {
      return("N/A")
    }
    
    paste0(round(alpha_value, 2), "%")
  })
  
  # Render output for Alpha
  output$fund_alpha_trad <- renderText({
    print("fund_alpha_trad renderText started.") # Debug print
    fund_alpha_trad()
  })
  
  
  # Reactive for Traditional Fund Summary Metrics
  trad_fund_summary_metrics <- reactive({
    print("trad_fund_summary_metrics reactive started.") # Debug print
    data_for_metrics <- filtered_trad_fund_data()
    
    if (is.null(data_for_metrics) || NROW(data_for_metrics) == 0 || NCOL(data_for_metrics) == 0) {
      return(data.frame(Metric = "No data available", Value = "N/A"))
    }
    
    # Align CPI (risk-free rate) with the fund data
    aligned_cpi <- CPI[index(data_for_metrics)]
    
    # Ensure aligned_cpi is not empty and has valid data for Sharpe Ratio calculation
    if (NROW(aligned_cpi) == 0 || all(is.na(aligned_cpi))) {
      rf_rate <- 0 # Default to 0 if no valid CPI data
      warning("No valid CPI data available for Sharpe Ratio calculation. Using 0 as risk-free rate.")
    } else {
      rf_rate <- mean(aligned_cpi, na.rm = TRUE)
    }
    
    # Calculate metrics
    annualized_return <- Return.annualized(data_for_metrics, scale = 12) * 100
    annualized_stddev <- StdDev.annualized(data_for_metrics, scale = 12) * 100
    sharpe_ratio <- SharpeRatio.annualized(data_for_metrics, Rf = rf_rate, scale = 12)
    max_drawdown <- maxDrawdown(data_for_metrics) * 100
    
    # Create a data frame for display
    metrics_df <- data.frame(
      Metric = c("Annualized Return (%)", "Annualized Volatility (%)", "Sharpe Ratio (vs. CPI)", "Max Drawdown (%)"),
      Value = c(
        sprintf("%.2f", annualized_return),
        sprintf("%.2f", annualized_stddev),
        sprintf("%.2f", sharpe_ratio),
        sprintf("%.2f", max_drawdown)
      )
    )
    return(metrics_df)
  })
  
  # Render table for Traditional Fund Summary Metrics
  output$trad_fund_summary_metrics_table <- renderTable({
    print("trad_fund_summary_metrics_table renderTable started.") # Debug print
    trad_fund_summary_metrics()
  }, rownames = FALSE, digits = 2)
  
  
  output$cumulative_returns_plot_trad <- renderPlot({
    data_to_plot <- filtered_trad_fund_data()
    
    if (is.null(data_to_plot) || NROW(data_to_plot) == 0 || NCOL(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for selected fund/period", size = 6))
    }
    
    plots <- performance_appraisal(data_to_plot)
    plots$cumRetPlot
  })
  
  # Render Annual Returns Plot for Traditional Fund Tab
  output$trad_fund_annual_returns_plot <- renderPlot({
    data_to_plot <- filtered_trad_fund_data()
    
    if (is.null(data_to_plot) || NROW(data_to_plot) == 0 || NCOL(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for selected fund/period", size = 6))
    }
    
    plots <- performance_appraisal(data_to_plot)
    plots$yearly_ret_plot
  })
  
  
  # --- Hedge Fund Performance Tab Logic (Existing + New Features) ---
  
  filtered_hedge_fund_data <- reactive({
    req(input$selected_hedge_fund, input$date_range_hedge)
    
    hedge_fund_data <- hna_fund_returns_set[, input$selected_hedge_fund, drop = FALSE]
    
    start_date <- input$date_range_hedge[1]
    end_date <- input$date_range_hedge[2]
    
    hedge_fund_data_filtered <- hedge_fund_data[paste0(start_date, "::", end_date)]
    
    return(hedge_fund_data_filtered)
  })
  
  # NEW: Reactive for Hedge Fund Summary Metrics
  hedge_fund_summary_metrics <- reactive({
    data_for_metrics <- filtered_hedge_fund_data()
    
    if (is.null(data_for_metrics) || NROW(data_for_metrics) == 0 || NCOL(data_for_metrics) == 0) {
      return(data.frame(Metric = "No data available", Value = "N/A"))
    }
    
    # Align CPI (risk-free rate) with the fund data for Sharpe/Sortino/Calmar
    aligned_cpi <- CPI[index(data_for_metrics)]
    if (NROW(aligned_cpi) == 0 || all(is.na(aligned_cpi))) {
      rf_rate_xts <- xts(rep(0, NROW(data_for_metrics)), order.by = index(data_for_metrics))
      warning("No valid CPI data available for Hedge Fund metrics. Using 0 as risk-free rate.")
    } else {
      rf_rate_xts <- aligned_cpi
    }
    
    # Calculate metrics
    annualized_return <- Return.annualized(data_for_metrics, scale = 12) * 100
    annualized_stddev <- StdDev.annualized(data_for_metrics, scale = 12) * 100
    sharpe_ratio <- SharpeRatio.annualized(data_for_metrics, Rf = mean(rf_rate_xts, na.rm = TRUE), scale = 12) # Use mean for SharpeR
    max_drawdown <- maxDrawdown(data_for_metrics) * 100
    
    # Create a data frame for display
    metrics_df <- data.frame(
      Metric = c("Annualized Return (%)", "Annualized Volatility (%)", "Sharpe Ratio (vs. CPI)", "Max Drawdown (%)"),
      Value = c(
        sprintf("%.2f", annualized_return),
        sprintf("%.2f", annualized_stddev),
        sprintf("%.2f", sharpe_ratio),
        sprintf("%.2f", max_drawdown)
      )
    )
    return(metrics_df)
  })
  
  # NEW: Render table for Hedge Fund Summary Metrics
  output$hedge_fund_summary_metrics_table <- renderTable({
    hedge_fund_summary_metrics()
  }, rownames = FALSE, digits = 2)
  
  # NEW: Reactive for Sortino Ratio
  hedge_fund_sortino_ratio <- reactive({
    req(input$selected_hedge_fund, input$date_range_hedge)
    fund_data <- filtered_hedge_fund_data()
    
    if (is.null(fund_data) || NROW(fund_data) < 12) {
      return("N/A")
    }
    
    aligned_cpi <- CPI[index(fund_data)]
    if (NROW(aligned_cpi) == 0 || all(is.na(aligned_cpi))) {
      rf_rate <- 0
      warning("No valid CPI data available for Sortino Ratio. Using 0 as risk-free rate.")
    } else {
      rf_rate <- mean(aligned_cpi, na.rm = TRUE)
    }
    
    sortino_val <- SortinoRatio(R = fund_data, FUN = "StdDev.annualized", MAR = rf_rate, scale = 12)
    
    if (is.na(sortino_val) || !is.numeric(sortino_val)) {
      return("N/A")
    }
    paste0(round(sortino_val, 2))
  })
  
  # NEW: Render output for Sortino Ratio
  output$hedge_fund_sortino_ratio <- renderText({
    hedge_fund_sortino_ratio()
  })
  
  # NEW: Reactive for Calmar Ratio
  hedge_fund_calmar_ratio <- reactive({
    req(input$selected_hedge_fund, input$date_range_hedge)
    fund_data <- filtered_hedge_fund_data()
    
    if (is.null(fund_data) || NROW(fund_data) < 12) {
      return("N/A")
    }
    
    calmar_val <- CalmarRatio(R = fund_data, scale = 12)
    
    if (is.na(calmar_val) || !is.numeric(calmar_val)) {
      return("N/A")
    }
    paste0(round(calmar_val, 2))
  })
  
  # NEW: Render output for Calmar Ratio
  output$hedge_fund_calmar_ratio <- renderText({
    hedge_fund_calmar_ratio()
  })
  
  output$hedge_fund_cumulative_plot <- renderPlot({
    data_to_plot <- filtered_hedge_fund_data()
    
    if (is.null(data_to_plot) || NROW(data_to_plot) == 0 || NCOL(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for selected hedge fund/period", size = 6))
    }
    
    plots <- performance_appraisal(data_to_plot)
    plots$cumRetPlot
  })
  
  output$hedge_fund_annual_plot <- renderPlot({
    data_to_plot <- filtered_hedge_fund_data()
    
    if (is.null(data_to_plot) || NROW(data_to_plot) == 0 || NCOL(data_to_plot) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for selected hedge fund/period", size = 6))
    }
    
    plots <- performance_appraisal(data_to_plot)
    plots$yearly_ret_plot
  })
  
  # NEW: Render Hedge Fund Historical Drawdowns Plot
  output$hedge_fund_drawdown_plot <- renderPlot({
    returns_xts_input <- filtered_hedge_fund_data()
    
    if (is.null(returns_xts_input) || NROW(returns_xts_input) == 0 || NCOL(returns_xts_input) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for historical drawdowns.", size = 6))
    }
    
    drawdowns_ts <- as.timeSeries(returns_xts_input)
    
    if (NCOL(drawdowns_ts) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data for historical drawdowns.", size = 6))
    }
    
    colnames(drawdowns_ts) <- colnames(returns_xts_input)
    
    drawdowns_xts_result <- drawdowns(drawdowns_ts)
    
    drawdowns_df <- data.frame(Date = index(drawdowns_xts_result), coredata(drawdowns_xts_result))
    drawdowns_df$Date <- as.Date(drawdowns_df$Date) # Ensure Date is Date type
    colnames(drawdowns_df) <- c("Date", colnames(drawdowns_xts_result))
    drawdowns_long <- melt(drawdowns_df, id.vars = "Date", variable.name = "Series", value.name = "Drawdown")
    
    drawdowns_long$Series <- gsub("\\.", " ", drawdowns_long$Series)
    
    color_palette <- colorRampPalette(c("blue", "red", "orange", "black", "purple", "green", "brown", "grey"))(length(unique(drawdowns_long$Series)))
    
    ggplot(drawdowns_long, aes(x = Date, y = Drawdown*100, color = Series)) +
      geom_line(size = 1) +
      labs(title = "Historical Drawdowns",
           x = "",
           y = "Max Drawdown (%)") +
      scale_color_manual(values = color_palette) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = expansion(mult = c(0.01, 0.01))) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.text.x = element_text(size = 12, margin = ggplot2::margin(t = 10), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  
  # --- Portfolio Blending Tab Logic (Existing + New Features) ---
  
  blended_portfolio_returns <- reactive({
    req(input$blend_fund1, input$blend_fund2, input$blend_weight1, input$blend_weight2, input$rebalance_freq)
    
    if (input$blend_fund1 == input$blend_fund2) {
      return(NULL)
    }
    
    fund1_data <- funds_xts[, input$blend_fund1, drop = FALSE]
    fund2_data <- funds_xts[, input$blend_fund2, drop = FALSE]
    
    combined_funds <- na.omit(cbind(fund1_data, fund2_data))
    
    if (NROW(combined_funds) == 0 || NCOL(combined_funds) < 2) {
      return(NULL)
    }
    
    if (input$blend_fund1 == input$blend_fund2) {
      colnames(combined_funds) <- c(paste0(input$blend_fund1, "_Fund1"), paste0(input$blend_fund2, "_Fund2"))
    } else {
      colnames(combined_funds) <- c(input$blend_fund1, input$blend_fund2)
    }
    
    weights <- c(input$blend_weight1, input$blend_weight2)
    
    if (abs(sum(weights) - 1) > 0.001) {
      return(NULL)
    }
    
    blended_returns <- Return.portfolio(
      R = combined_funds,
      weights = weights,
      rebalance_on = input$rebalance_freq
    )
    
    colnames(blended_returns) <- "Blended Portfolio"
    
    return(blended_returns)
  })
  
  # NEW: Reactive for Max Sharpe Ratio Portfolio Weights
  max_sharpe_weights <- reactive({
    req(input$blend_fund1, input$blend_fund2)
    
    if (input$blend_fund1 == input$blend_fund2) {
      return(data.frame(Fund = c(input$blend_fund1, input$blend_fund2), Weight = c("N/A", "N/A")))
    }
    
    fund1_data <- funds_xts[, input$blend_fund1, drop = FALSE]
    fund2_data <- funds_xts[, input$blend_fund2, drop = FALSE]
    
    combined_funds <- na.omit(cbind(fund1_data, fund2_data))
    
    if (NROW(combined_funds) < 12 || NCOL(combined_funds) < 2) {
      return(data.frame(Fund = c(input$blend_fund1, input$blend_fund2), Weight = c("N/A", "N/A")))
    }
    
    # Align CPI (risk-free rate) with the fund data
    aligned_cpi <- CPI[index(combined_funds)]
    if (NROW(aligned_cpi) == 0 || all(is.na(aligned_cpi))) {
      rf_rate <- 0 # Default to 0 if no valid CPI data
    } else {
      rf_rate <- mean(aligned_cpi, na.rm = TRUE)
    }
    
    # Calculate annualized mean returns and standard deviations
    mean_ret1 <- mean(fund1_data, na.rm = TRUE) * 12 # Annualize monthly mean
    mean_ret2 <- mean(fund2_data, na.rm = TRUE) * 12
    sd1 <- sd(fund1_data, na.rm = TRUE) * sqrt(12) # Annualize monthly std dev
    sd2 <- sd(fund2_data, na.rm = TRUE) * sqrt(12)
    correlation <- cor(fund1_data, fund2_data, use = "pairwise.complete.obs")[1,1]
    
    # Handle cases where std dev might be zero or correlation is NA
    if (is.na(sd1) || sd1 == 0) sd1 <- 1e-9
    if (is.na(sd2) || sd2 == 0) sd2 <- 1e-9
    if (is.na(correlation)) correlation <- 0 # Assume no correlation if not calculable
    
    # Define function to minimize (negative Sharpe Ratio)
    neg_sharpe_ratio <- function(w1) {
      w2 <- 1 - w1
      if (w1 < 0 || w1 > 1) return(Inf) # Enforce 0-1 weights
      
      port_return <- w1 * mean_ret1 + w2 * mean_ret2
      port_volatility <- sqrt(w1^2 * sd1^2 + w2^2 * sd2^2 + 2 * w1 * w2 * correlation * sd1 * sd2)
      
      if (port_volatility == 0) return(Inf) # Avoid division by zero
      -(port_return - rf_rate) / port_volatility
    }
    
    # Optimize for w1 within the [0, 1] interval
    optim_result <- optimize(f = neg_sharpe_ratio, interval = c(0, 1))
    w_sharpe1 <- optim_result$minimum
    w_sharpe2 <- 1 - w_sharpe1
    
    # Format weights
    weights_df <- data.frame(
      Fund = c(input$blend_fund1, input$blend_fund2),
      Weight = c(sprintf("%.2f%%", w_sharpe1 * 100), sprintf("%.2f%%", w_sharpe2 * 100))
    )
    return(weights_df)
  })
  
  # NEW: Render Max Sharpe Ratio Weights Table
  output$max_sharpe_weights_table <- renderTable({
    max_sharpe_weights()
  }, rownames = FALSE, align = 'l')
  
  # NEW: Reactive for Minimum Volatility Portfolio Weights
  min_vol_weights <- reactive({
    req(input$blend_fund1, input$blend_fund2)
    
    if (input$blend_fund1 == input$blend_fund2) {
      return(data.frame(Fund = c(input$blend_fund1, input$blend_fund2), Weight = c("N/A", "N/A")))
    }
    
    fund1_data <- funds_xts[, input$blend_fund1, drop = FALSE]
    fund2_data <- funds_xts[, input$blend_fund2, drop = FALSE]
    
    combined_funds <- na.omit(cbind(fund1_data, fund2_data))
    
    if (NROW(combined_funds) < 12 || NCOL(combined_funds) < 2) {
      return(data.frame(Fund = c(input$blend_fund1, input$blend_fund2), Weight = c("N/A", "N/A")))
    }
    
    sd1 <- sd(fund1_data, na.rm = TRUE) * sqrt(12)
    sd2 <- sd(fund2_data, na.rm = TRUE) * sqrt(12)
    correlation <- cor(fund1_data, fund2_data, use = "pairwise.complete.obs")[1,1]
    
    # Handle cases where std dev might be zero or correlation is NA
    if (is.na(sd1) || sd1 == 0) sd1 <- 1e-9
    if (is.na(sd2) || sd2 == 0) sd2 <- 1e-9
    if (is.na(correlation)) correlation <- 0
    
    # Calculate minimum variance weight for w1
    # Formula: w1 = (sd2^2 - correlation * sd1 * sd2) / (sd1^2 + sd2^2 - 2 * correlation * sd1 * sd2)
    denominator <- sd1^2 + sd2^2 - 2 * correlation * sd1 * sd2
    
    if (denominator <= 1e-9) { # Handle near-zero or negative denominator (e.g., perfect correlation)
      # In case of perfect correlation, min vol is 100% in the less volatile asset
      if (sd1 <= sd2) {
        w_min_vol1 <- 1
      } else {
        w_min_vol1 <- 0
      }
    } else {
      w_min_vol1 <- (sd2^2 - correlation * sd1 * sd2) / denominator
    }
    
    # Constrain weights to be between 0 and 1
    w_min_vol1 <- max(0, min(1, w_min_vol1))
    w_min_vol2 <- 1 - w_min_vol1
    
    # Format weights
    weights_df <- data.frame(
      Fund = c(input$blend_fund1, input$blend_fund2),
      Weight = c(sprintf("%.2f%%", w_min_vol1 * 100), sprintf("%.2f%%", w_min_vol2 * 100))
    )
    return(weights_df)
  })
  
  # NEW: Render Minimum Volatility Weights Table
  output$min_vol_weights_table <- renderTable({
    min_vol_weights()
  }, rownames = FALSE, align = 'l')
  
  
  comparison_returns_xts <- reactive({
    req(blended_portfolio_returns())
    req(input$benchmark_asisa, input$benchmark_cpi)
    
    blend <- blended_portfolio_returns()
    
    ASISA_val <- Indices[, input$benchmark_asisa, drop = FALSE]
    CPI_target_val <- Indices[, input$benchmark_cpi, drop = FALSE]
    
    returns_compare <- na.omit(cbind(
      blend,
      ASISA_val,
      CPI_target_val
    ))
    
    if (NCOL(returns_compare) == 0) {
      return(NULL)
    }
    
    final_compare_names <- c(
      "Blended Portfolio",
      input$benchmark_asisa,
      input$benchmark_cpi
    )
    colnames(returns_compare) <- make.unique(final_compare_names, sep = ".")
    
    return(returns_compare)
  })
  
  output$weight_sum_message <- renderUI({
    weights_sum <- input$blend_weight1 + input$blend_weight2
    if (input$blend_fund1 == input$blend_fund2) {
      tags$p("Error: Please select two different funds for blending.", style = "color: red; font-weight: bold;")
    } else if (abs(weights_sum - 1) > 0.001) {
      tags$p("Warning: Weights do not sum to 1. Please adjust weights.", style = "color: red; font-weight: bold;")
    } else {
      return(NULL)
    }
  })
  
  output$blended_cumulative_plot <- renderPlot({
    data_to_plot <- comparison_returns_xts()
    
    if (is.null(data_to_plot) || NROW(data_to_plot) == 0 || NCOL(data_to_plot) == 0) {
      if (input$blend_fund1 == input$blend_fund2) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Please select two different funds for blending.", size = 6, color = "red"))
      } else if (abs(input$blend_weight1 + input$blend_weight2 - 1) > 0.001) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Please ensure weights sum to 1 for blending.", size = 6, color = "red"))
      }
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for blended portfolio/benchmarks.", size = 6))
    }
    
    plots <- performance_appraisal(data_to_plot)
    plots$cumRetPlot
  })
  
  output$blended_annual_plot <- renderPlot({
    data_to_plot <- comparison_returns_xts()
    
    if (is.null(data_to_plot) || NROW(data_to_plot) == 0 || NCOL(data_to_plot) == 0) {
      if (input$blend_fund1 == input$blend_fund2) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Please select two different funds for blending.", size = 6, color = "red"))
      } else if (abs(input$blend_weight1 + input$blend_weight2 - 1) > 0.001) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Please ensure weights sum to 1 for blending.", size = 6, color = "red"))
      }
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for blended portfolio/benchmarks.", size = 6))
    }
    
    plots <- performance_appraisal(data_to_plot)
    plots$yearly_ret_plot
  })
  
  # --- New Risk Analysis Tab Logic ---
  
  returns_for_risk_analysis <- reactive({
    print("returns_for_risk_analysis reactive started.") # Debug print
    req(input$risk_portfolio_type)
    print(paste("risk_portfolio_type:", input$risk_portfolio_type)) # Debug print
    
    portfolio_returns <- NULL
    portfolio_name <- ""
    
    if (input$risk_portfolio_type == "Traditional Fund") {
      req(input$risk_selected_trad_fund)
      portfolio_returns <- funds_xts[, input$risk_selected_trad_fund, drop = FALSE]
      portfolio_name <- input$risk_selected_trad_fund
    } else if (input$risk_portfolio_type == "Blended Portfolio") {
      req(input$risk_blend_fund1, input$risk_blend_fund2)
      if (input$risk_blend_fund1 == input$risk_blend_fund2) {
        return(list(returns = NULL, name = "Error: Select different funds for blended portfolio in Risk Analysis."))
      }
      fund1_data <- funds_xts[, input$risk_blend_fund1, drop = FALSE]
      fund2_data <- funds_xts[, input$risk_blend_fund2, drop = FALSE]
      
      combined_funds <- na.omit(cbind(fund1_data, fund2_data))
      if (NROW(combined_funds) == 0 || NCOL(combined_funds) < 2) {
        return(list(returns = NULL, name = "Not enough data for custom blended portfolio in Risk Analysis."))
      }
      
      portfolio_returns <- Return.portfolio(
        R = combined_funds,
        weights = c(0.5, 0.5),
        rebalance_on = "months"
      )
      portfolio_name <- paste0("Blended (", input$risk_blend_fund1, " & ", input$risk_blend_fund2, ")")
      colnames(portfolio_returns) <- portfolio_name
    }
    
    if (is.null(portfolio_returns)) {
      if (exists("name", where = portfolio_returns) && grepl("Error:|Not enough data:", portfolio_returns$name)) {
        return(portfolio_returns)
      }
      return(NULL)
    }
    
    benchmarks_and_cpi <- NULL
    selected_indices_names <- c()
    
    if (!is.null(input$risk_benchmark_select) && input$risk_benchmark_select != "") {
      benchmarks_and_cpi <- cbind(benchmarks_and_cpi, Indices[, input$risk_benchmark_select, drop = FALSE])
      selected_indices_names <- c(selected_indices_names, input$risk_benchmark_select)
    }
    if (!is.null(input$risk_cpi_select) && input$risk_cpi_select != "") {
      benchmarks_and_cpi <- cbind(benchmarks_and_cpi, Indices[, input$risk_cpi_select, drop = FALSE])
      selected_indices_names <- c(selected_indices_names, input$risk_cpi_select)
    }
    
    if (!is.null(benchmarks_and_cpi)) {
      all_returns_for_risk <- na.omit(cbind(portfolio_returns, benchmarks_and_cpi))
      final_risk_names <- c(portfolio_name, selected_indices_names)
    } else {
      all_returns_for_risk <- na.omit(portfolio_returns)
      final_risk_names <- portfolio_name
    }
    
    colnames(all_returns_for_risk) <- make.unique(final_risk_names, sep = ".")
    
    valid_risk_cols <- sapply(1:ncol(all_returns_for_risk), function(i) {
      col_data <- as.numeric(all_returns_for_risk[, i])
      sd_val <- sd(col_data, na.rm = TRUE)
      !is.na(sd_val) && sd_val > 1e-9
    })
    all_returns_for_risk <- all_returns_for_risk[, valid_risk_cols, drop = FALSE]
    
    if (NCOL(all_returns_for_risk) == 0) {
      return(NULL)
    }
    
    return(all_returns_for_risk)
  })
  
  # Reactive for GBM Simulated Drawdowns
  dd_simulated_reactive <- reactive({
    req(input$horizon_years_input, input$n_sim_input)
    returns_sim_data <- returns_for_risk_analysis()
    
    if (is.null(returns_sim_data) || (is.list(returns_sim_data) && !is.null(returns_sim_data$name) && grepl("Error:|Not enough data:", returns_sim_data$name))) {
      return(NULL)
    }
    
    returns_sim <- returns_sim_data
    
    horizon_months <- input$horizon_years_input * 12
    n_sim <- input$n_sim_input
    
    if (NCOL(returns_sim) == 0) {
      return(NULL)
    }
    
    # Define gbm_simulated_risk function locally for this reactive
    gbm_simulated_risk <- function(returns, horizon, n_sim) {
      n_cols <- ncol(returns)
      dd_risk <- matrix(NA, ncol = n_cols, nrow = n_sim)
      
      for (i in 1:n_cols) {
        current_returns_col <- as.numeric(returns[, i])
        col_mean <- mean(current_returns_col, na.rm = TRUE)
        col_sd <- sd(current_returns_col, na.rm = TRUE)
        
        if (is.na(col_sd) || col_sd == 0 || is.infinite(col_sd)) {
          dd_risk[, i] <- 0
        } else {
          # Calls the rmaxdd function defined in global.R
          dd_risk[, i] <- rmaxdd(n_sim, mean = col_mean * 100, sd = col_sd * 100, horizon)
        }
      }
      
      colnames(dd_risk) <- colnames(returns)
      return(data.frame(dd_risk))
    }
    
    dd_simulated <- gbm_simulated_risk(returns_sim, horizon = horizon_months, n_sim = n_sim)
    colnames(dd_simulated) <- gsub("\\.", " ", make.names(colnames(dd_simulated)))
    return(dd_simulated)
  })
  
  # Reactive for Max Drawdowns (historical)
  max_dd_reactive <- reactive({
    returns_sim_data <- returns_for_risk_analysis()
    if (is.null(returns_sim_data) || (is.list(returns_sim_data) && !is.null(returns_sim_data$name) && grepl("Error:|Not enough data:", returns_sim_data$name))) {
      return(numeric(0))
    }
    returns_sim <- returns_sim_data
    
    if (NCOL(returns_sim) == 0) {
      return(numeric(0))
    }
    
    sapply(colnames(returns_sim), function(col) {
      maxDrawdown(na.omit(as.numeric(returns_sim[, col]))) * 100
    })
  })
  
  # Reactive for Max Drawdown Probability Density Plot
  output$max_drawdown_density_plot <- renderPlot({
    dd_simulated <- dd_simulated_reactive()
    max_dd <- max_dd_reactive()
    n_sim <- input$n_sim_input
    
    returns_sim_status <- returns_for_risk_analysis()
    if (is.null(dd_simulated) || NCOL(dd_simulated) == 0 || NROW(dd_simulated) == 0) {
      if (!is.null(returns_sim_status) && is.list(returns_sim_status) && !is.null(returns_sim_status$name) && grepl("Error:|Not enough data:", returns_sim_status$name)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = returns_sim_status$name, size = 6, color = "red"))
      }
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid simulated drawdown data for plotting.", size = 6))
    }
    
    unique_sim_cols <- colnames(dd_simulated)
    df_dd <- data.frame(value = unlist(dd_simulated),
                        group = factor(rep(unique_sim_cols, each = n_sim),
                                       levels = unique_sim_cols))
    
    color_palette <- colorRampPalette(c("blue", "red", "orange", "black", "purple", "green", "brown", "grey"))(length(unique_sim_cols))
    
    dd_prob_plot <- ggplot(df_dd, aes(x = value, fill = group)) +
      coord_cartesian(xlim = c(0, max(df_dd$value) * 0.5)) +
      geom_density(alpha = 0.5, position = "identity", aes(y = after_stat(density))) +
      scale_fill_manual(values = color_palette, name = "Portfolio") +
      labs(title = paste0("Max Drawdown Probability Density \n(", input$horizon_years_input, " Year Horizon)"),
           x = "Max Drawdown (%)", y = "Density") +
      theme_classic() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        legend.position = "bottom",
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 22, hjust = 0),
        plot.margin = ggplot2::margin(20, 20, 20, 20)
      )
    
    if (length(max_dd) > 0 && length(color_palette) >= length(max_dd)) {
      for (i in seq_along(max_dd)) {
        dd_prob_plot <- dd_prob_plot +
          geom_vline(xintercept = max_dd[i], linetype = "dashed", size = 1.1, color = color_palette[i])
      }
    }
    return(dd_prob_plot)
  })
  
  # Reactive for Risk Probability Analysis Table
  dd_prob_results_reactive <- reactive({
    dd_simulated <- dd_simulated_reactive()
    
    returns_sim_status <- returns_for_risk_analysis()
    if (is.null(dd_simulated) || NCOL(dd_simulated) == 0 || NROW(dd_simulated) == 0) {
      if (!is.null(returns_sim_status) && is.list(returns_sim_status) && !is.null(returns_sim_status$name) && grepl("Error:|Not enough data:", returns_sim_status$name)) {
        return(data.frame(Message = returns_sim_status$name))
      }
      return(data.frame(Message = "No simulated drawdown data available."))
    }
    
    unique_sim_cols <- colnames(dd_simulated)
    
    range <- seq(from = 0, to = 100, by = 5)
    dd_prob_results <- list()
    
    for (i in 1:ncol(dd_simulated)) {
      frequencies <- numeric(length(range)-1)
      medians <- numeric(length(range)-1)
      
      for (j in 1:(length(range)-1)) {
        values_within_range <- dd_simulated[, i][dd_simulated[, i] > range[j] & dd_simulated[, i] <= range[j+1]]
        
        medians[j] <- if(length(values_within_range) > 0) median(values_within_range, na.rm = TRUE) else NA
        frequencies[j] <- length(values_within_range)
      }
      
      sum_freq <- sum(frequencies)
      probabilities <- if(sum_freq > 0) frequencies / sum_freq else rep(0, length(frequencies))
      probabilities <- round(probabilities * 100, 4)
      
      col_name <- unique_sim_cols[i]
      dd_prob_results[[col_name]] <- data.frame(maxDD_range = paste0(format(range[-length(range)], scientific = FALSE), " - ", format(range[-1], scientific = FALSE)),
                                                frequency = frequencies,
                                                median = medians,
                                                probability = probabilities)
    }
    
    combined_results_df <- bind_rows(dd_prob_results, .id = "Portfolio")
    return(combined_results_df)
  })
  
  output$max_drawdown_prob_table <- renderTable({
    req(dd_prob_results_reactive())
    dd_prob_results_reactive()
  }, rownames = FALSE)
  
  # Reactive for Historical Drawdowns Plot
  # Reactive for Historical Drawdowns Plot
  output$historical_drawdowns_plot <- renderPlot({
    print("historical_drawdowns_plot renderPlot started.") # Debug print
    returns_xts_input_data <- returns_for_risk_analysis()
    req(input$horizon_years_input) # Ensure horizon is available
    
    if (is.null(returns_xts_input_data) || (is.list(returns_xts_input_data) && !is.null(returns_xts_input_data$name) && grepl("Error:|Not enough data:", returns_xts_input_data$name))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = returns_xts_input_data$name, size = 6, color = "red"))
    }
    returns_xts_input <- returns_xts_input_data
    
    # Remove CPI series for drawdown plot as per client's original script
    returns_xts_input_clean <- na.omit(returns_xts_input[, !grepl("CPI", colnames(returns_xts_input)), drop = FALSE])
    if (NROW(returns_xts_input_clean) == 0 || NCOL(returns_xts_input_clean) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid data for historical drawdowns.", size = 6))
    }
    
    # Ensure that the 'Date' column in drawdowns_df is a Date or POSIXct object
    drawdowns_ts <- as.timeSeries(returns_xts_input_clean)
    
    # Convert the Date column to POSIXct in drawdowns_df
    # If you need to create drawdowns_df, it should be derived from the drawdowns_ts
    drawdowns_xts_result <- drawdowns(drawdowns_ts)
    
    # Create the data frame from the time series result
    drawdowns_df <- data.frame(Date = index(drawdowns_xts_result), coredata(drawdowns_xts_result))
    drawdowns_df$Date <- as.Date(drawdowns_df$Date) # Ensure Date is Date type (POSIXct)
    colnames(drawdowns_df) <- c("Date", colnames(drawdowns_xts_result))  # Set proper column names
    
    # Now proceed with the filtering and plotting
    drawdowns_long <- melt(drawdowns_df, id.vars = "Date", variable.name = "Series", value.name = "Drawdown")
    
    drawdowns_long$Series <- gsub("\\.", " ", drawdowns_long$Series)
    
    # Validate input$horizon_years_input and convert to numeric
    horizon_years <- as.numeric(input$horizon_years_input)
    if (is.na(horizon_years) || horizon_years <= 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Invalid Horizon Years input.", size = 6, color = "red"))
    }
    
    # Filter data based on Horizon (Years)
    # Ensure Date column is explicitly Date type before filtering
    drawdowns_long$Date <- as.Date(drawdowns_long$Date) # Redundant but safe check
    
    end_date_filter <- max(drawdowns_long$Date, na.rm = TRUE)
    start_date_filter <- end_date_filter - lubridate::years(horizon_years)
    
    # Adjust the start date if it's earlier than the first date in the dataset
    if (start_date_filter < min(drawdowns_long$Date, na.rm = TRUE)) {
      start_date_filter <- min(drawdowns_long$Date, na.rm = TRUE)
      print("Adjusted start date filter to the first date in the dataset.")
    }
    
    # Get the indices of dates within the range
    date_indices <- which(drawdowns_long$Date >= start_date_filter & drawdowns_long$Date <= end_date_filter)
    
    # Subset the data based on these indices
    drawdowns_long_filtered <- drawdowns_long[date_indices, ]
    
    # If no data is available after filtering
    if (nrow(drawdowns_long_filtered) == 0) {
      print("No data available after filtering.")
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data for the selected horizon.", size = 6))
    }
    
    # If data is available, proceed with the plotting
    color_palette <- colorRampPalette(c("blue", "red", "orange", "black", "purple", "green", "brown", "grey"))(length(unique(drawdowns_long_filtered$Series)))
    
    # Adjust x-axis to ensure the years are correctly displayed
    drawdownPlot <- ggplot(drawdowns_long_filtered, aes(x = Date, y = Drawdown * 100, color = Series)) +
      geom_line(size = 1) +
      labs(title = paste0("Historical Drawdowns (Last ", horizon_years, " Years)"), # Dynamic title
           x = "Year",
           y = "Max Drawdown (%)") +
      scale_color_manual(values = color_palette) +
      scale_x_date(
        date_labels = "%Y",              # Format to display only year
        date_breaks = "1 year",          # Breaks every year
        expand = expansion(mult = c(0.01, 0.01)) # Ensure there’s some space around the axis
      ) + 
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.text.x = element_text(size = 12, margin = ggplot2::margin(t = 10), angle = 45, hjust = 1), # Adjusted size for better fit
        axis.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    return(drawdownPlot)
  })

  # Reactive for 1-Year Rolling Returns Plot
  output$rolling_returns_1yr_plot <- renderPlot({
    returns_rolled_data <- returns_for_risk_analysis()
    
    if (is.null(returns_rolled_data) || (is.list(returns_rolled_data) && !is.null(returns_rolled_data$name) && grepl("Error:|Not enough data:", returns_rolled_data$name))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = returns_rolled_data$name, size = 6, color = "red"))
    }
    returns_rolled <- returns_rolled_data
    
    # Remove CPI series for rolling returns plot as per client's original script
    returns_rolled_clean <- na.omit(returns_rolled[, !grepl("CPI", colnames(returns_rolled)), drop = FALSE])
    window_size <- 12 # 1-year rolling
    
    if (NROW(returns_rolled_clean) < window_size || NCOL(returns_rolled_clean) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste0("Not enough data for 1-Year Rolling Returns (requires at least ", window_size, " months)."), size = 6))
    }
    
    rolling_returns_list <- lapply(1:ncol(returns_rolled_clean), function(i) {
      single_series_xts <- returns_rolled_clean[, i, drop = FALSE]
      rollapply(single_series_xts, width = window_size, FUN = Return.cumulative, align = 'right', fill = NA)
    })
    
    rolling_returns <- do.call(cbind, rolling_returns_list)
    colnames(rolling_returns) <- colnames(returns_rolled_clean)
    
    rolling_returns_df <- data.frame(Date = index(rolling_returns), coredata(rolling_returns))
    colnames(rolling_returns_df) <- c("Date", colnames(rolling_returns))
    
    data_melted <- reshape2::melt(rolling_returns_df, id.vars = "Date")
    colnames(data_melted) <- c("Date", "Series", "Rolling_Return")
    data_melted <- na.exclude(data_melted)
    
    data_melted$Series <- gsub("\\.", " ", data_melted$Series)
    
    if(NROW(data_melted) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid rolling returns data after calculation.", size = 6))
    }
    
    # Ensure portfolio_data uses the first column name from returns_rolled_clean (the selected portfolio)
    portfolio_data <- data_melted[data_melted$Series == gsub("\\.", " ", make.names(colnames(returns_rolled_clean)[1])), ]
    other_data <- data_melted[data_melted$Series != gsub("\\.", " ", make.names(colnames(returns_rolled_clean)[1])), ]
    
    color_palette <- colorRampPalette(c("blue", "red", "orange", "black", "purple", "green", "brown", "grey"))(ncol(returns_rolled_clean))
    
    rollingRetPlot1 <- ggplot() +
      geom_line(data = other_data, aes(x = Date, y = Rolling_Return * 100, color = Series), size = 1) +
      geom_line(data = portfolio_data, aes(x = Date, y = Rolling_Return * 100, color = Series), size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
      labs(title = "1 Year Rolling Returns",
           x = "",
           y = "Cumulative Return (%)") +
      scale_color_manual(values = color_palette) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = expansion(mult = c(0.01, 0.01))) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.text.x = element_text(size = 20, margin = ggplot2::margin(t = 10), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    return(rollingRetPlot1)
  })
  
  # Reactive for Horizon Year Rolling Returns Plot
  output$rolling_returns_horizon_plot <- renderPlot({
    req(input$horizon_years_input)
    returns_rolled_data <- returns_for_risk_analysis()
    
    if (is.null(returns_rolled_data) || (is.list(returns_rolled_data) && !is.null(returns_rolled_data$name) && grepl("Error:|Not enough data:", returns_rolled_data$name))) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = returns_rolled_data$name, size = 6, color = "red"))
    }
    returns_rolled <- returns_rolled_data
    
    horizon_months <- input$horizon_years_input * 12
    
    # Remove CPI series for rolling returns plot as per client's original script
    returns_rolled_clean <- na.omit(returns_rolled[, !grepl("CPI", colnames(returns_rolled)), drop = FALSE])
    
    if (NROW(returns_rolled_clean) < horizon_months || NCOL(returns_rolled_clean) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = paste0("Not enough data for ", input$horizon_years_input, "-Year Rolling Returns (requires at least ", horizon_months, " months)."), size = 6))
    }
    
    rolling_returns_list <- lapply(1:ncol(returns_rolled_clean), function(i) {
      single_series_xts <- returns_rolled_clean[, i, drop = FALSE]
      rollapply(single_series_xts, width = horizon_months, FUN = function(x) Return.annualized(x, scale = 12), align = 'right', fill = NA)
    })
    
    rolling_returns <- do.call(cbind, rolling_returns_list)
    colnames(rolling_returns) <- colnames(returns_rolled_clean)
    
    rolling_returns_df <- data.frame(Date = index(rolling_returns), coredata(rolling_returns))
    colnames(rolling_returns_df) <- c("Date", colnames(rolling_returns))
    
    data_melted <- reshape2::melt(rolling_returns_df, id.vars = "Date")
    colnames(data_melted) <- c("Date", "Series", "Rolling_Return")
    data_melted <- na.exclude(data_melted)
    
    data_melted$Series <- gsub("\\.", " ", data_melted$Series)
    
    if(NROW(data_melted) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No valid rolling returns data after calculation.", size = 6))
    }
    
    # Ensure portfolio_data uses the first column name from returns_rolled_clean (the selected portfolio)
    portfolio_data <- data_melted[data_melted$Series == gsub("\\.", " ", make.names(colnames(returns_rolled_clean)[1])), ]
    other_data <- data_melted[data_melted$Series != gsub("\\.", " ", make.names(colnames(returns_rolled_clean)[1])), ]
    
    color_palette <- colorRampPalette(c("blue", "red", "orange", "black", "purple", "green", "brown", "grey"))(ncol(returns_rolled_clean))
    
    rollingRetPlot2 <- ggplot() +
      geom_line(data = other_data, aes(x = Date, y = Rolling_Return * 100, color = Series), size = 1) +
      geom_line(data = portfolio_data, aes(x = Date, y = Rolling_Return * 100, color = Series), size = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
      labs(title = paste0(input$horizon_years_input, " Year Rolling Returns"),
           x = "",
           y = "Annualised Return (%)") +
      scale_color_manual(values = color_palette) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.text.x = element_text(size = 20, margin = ggplot2::margin(t = 10), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    return(rollingRetPlot2)
  })
  
  # Dynamic title for Horizon Year Rolling Returns
  output$rolling_returns_horizon_title <- renderText({
    paste0(input$horizon_years_input, " Year Rolling Returns")
  })
  
  # --- Sharpe Ratio Analysis Tab Logic (REVERTED TO PREVIOUS WORKING VERSION - UNTOUCHED) ---
  
  # Helper function for Sharpe Ratio calculation
  calculate_sharpe_ratios <- function(returns_xts, rf_xts, rolling_window_years) {
    if (is.null(returns_xts) || NROW(returns_xts) == 0 || NCOL(returns_xts) == 0) {
      return(list(expanding_df = NULL, rolling_df = NULL))
    }
    
    # Ensure returns and rf are aligned and clean
    aligned_data <- na.omit(cbind(returns_xts, rf_xts))
    if (NROW(aligned_data) == 0) {
      return(list(expanding_df = NULL, rolling_df = NULL))
    }
    
    returns_clean <- aligned_data[, 1, drop = FALSE] # Assuming the first column is the portfolio/fund
    rf_clean <- aligned_data[, 2, drop = FALSE] # Assuming the second column is the risk-free rate (CPI)
    
    # Expanding Window Sharpe Ratio
    n <- length(returns_clean)
    sr_expanding <- rep(NA, n)
    ci_lower_68_expanding <- rep(NA, n)
    ci_upper_68_expanding <- rep(NA, n)
    ci_lower_95_expanding <- rep(NA, n)
    ci_upper_95_expanding <- rep(NA, n)
    
    window_size_min <- 12 # Minimum 1 year for SharpeR to calculate meaningfully
    
    for (i in window_size_min:n) {
      current_returns <- returns_clean[1:i]
      current_rf <- rf_clean[1:i]
      
      # Check for sufficient non-NA data in current window and non-zero standard deviation
      if (length(na.exclude(current_returns)) > 1 && sd(na.exclude(current_returns)) > 0) {
        sharpe <- as.sr(x = current_returns, c0 = mean(current_rf))
        sr_expanding[i] <- sharpe$sr
        ci_lower_68_expanding[i] <- confint(sharpe, level = 0.68)[,1]
        ci_upper_68_expanding[i] <- confint(sharpe, level = 0.68)[,2]
        ci_lower_95_expanding[i] <- confint(sharpe, level = 0.95)[,1]
        ci_upper_95_expanding[i] <- confint(sharpe, level = 0.95)[,2]
      }
    }
    
    expanding_df <- data.frame(
      Date = time(returns_clean),
      SR = sr_expanding,
      CI_lower_68 = ci_lower_68_expanding,
      CI_upper_68 = ci_upper_68_expanding,
      CI_lower_95 = ci_lower_95_expanding,
      CI_upper_95 = ci_upper_95_expanding
    )
    expanding_df <- expanding_df[complete.cases(expanding_df), ]
    
    # Rolling Window Sharpe Ratio
    rolling_window_months <- rolling_window_years * 12
    sr_rolling <- rep(NA, n)
    ci_lower_68_rolling <- rep(NA, n)
    ci_upper_68_rolling <- rep(NA, n)
    ci_lower_95_rolling <- rep(NA, n)
    ci_upper_95_rolling <- rep(NA, n)
    
    if (n >= rolling_window_months) {
      for (i in rolling_window_months:n) {
        current_returns <- returns_clean[(i - rolling_window_months + 1):i]
        current_rf <- rf_clean[(i - rolling_window_months + 1):i]
        
        if (length(na.exclude(current_returns)) > 1 && sd(na.exclude(current_returns)) > 0) {
          sharpe <- as.sr(x = current_returns, c0 = mean(current_rf))
          sr_rolling[i] <- sharpe$sr
          ci_lower_68_rolling[i] <- confint(sharpe, level = 0.68)[,1]
          ci_upper_68_rolling[i] <- confint(sharpe, level = 0.68)[,2]
          ci_lower_95_rolling[i] <- confint(sharpe, level = 0.95)[,1]
          ci_upper_95_rolling[i] <- confint(sharpe, level = 0.95)[,2]
        }
      }
    }
    
    rolling_df <- data.frame(
      Date = time(returns_clean),
      SR = sr_rolling,
      CI_lower_68 = ci_lower_68_rolling,
      CI_upper_68 = ci_upper_68_rolling,
      CI_lower_95 = ci_lower_95_rolling,
      CI_upper_95 = ci_upper_95_rolling
    )
    rolling_df <- rolling_df[complete.cases(rolling_df), ]
    
    return(list(expanding_df = expanding_df, rolling_df = rolling_df))
  }
  
  
  # Reactive for selected fund/portfolio for Sharpe Ratio analysis
  sharpe_analysis_data <- reactive({
    req(input$sharpe_fund_selection)
    
    returns_to_analyze <- NULL
    portfolio_display_name <- ""
    
    if (input$sharpe_fund_selection == "Traditional Fund") {
      req(input$sharpe_selected_trad_fund)
      returns_to_analyze <- funds_xts[, input$sharpe_selected_trad_fund, drop = FALSE]
      portfolio_display_name <- input$sharpe_selected_trad_fund
    } else if (input$sharpe_fund_selection == "Blended Portfolio") {
      req(input$sharpe_blend_fund1, input$sharpe_blend_fund2)
      
      if (input$sharpe_blend_fund1 == input$sharpe_blend_fund2) {
        return(list(returns = NULL, rf = NULL, name = "Error: Select different funds for blended portfolio in Sharpe Analysis."))
      }
      
      fund1_data <- funds_xts[, input$sharpe_blend_fund1, drop = FALSE]
      fund2_data <- funds_xts[, input$sharpe_blend_fund2, drop = FALSE]
      
      combined_funds <- na.omit(cbind(fund1_data, fund2_data))
      
      if (NROW(combined_funds) == 0 || NCOL(combined_funds) < 2) {
        return(list(returns = NULL, rf = NULL, name = "Not enough data for blended portfolio in Sharpe Analysis."))
      }
      
      blended_returns_for_sharpe <- Return.portfolio(
        R = combined_funds,
        weights = c(0.5, 0.5),
        rebalance_on = "months"
      )
      colnames(blended_returns_for_sharpe) <- "Blended Portfolio"
      
      returns_to_analyze <- blended_returns_for_sharpe
      portfolio_display_name <- paste0("Blended Portfolio (", input$sharpe_blend_fund1, " & ", input$sharpe_blend_fund2, ")")
    }
    
    rf_data <- if (!is.null(returns_to_analyze) && NROW(returns_to_analyze) > 0) {
      CPI[time(returns_to_analyze)]
    } else {
      NULL
    }
    
    list(returns = returns_to_analyze, rf = rf_data, name = portfolio_display_name)
  })
  
  # Reactive for Sharpe Ratio calculation results
  sharpe_results <- reactive({
    req(sharpe_analysis_data(), input$sharpe_rolling_window_years)
    
    data_list <- sharpe_analysis_data()
    if (is.null(data_list$returns) || is.null(data_list$rf)) {
      return(list(expanding_df = NULL, rolling_df = NULL))
    }
    calculate_sharpe_ratios(data_list$returns, data_list$rf, input$sharpe_rolling_window_years)
  })
  
  # Dynamic title for Expanding Window Sharpe Ratio
  output$sharpe_expanding_title <- renderText({
    req(sharpe_analysis_data())
    sharpe_data <- sharpe_analysis_data()
    if (grepl("Error:", sharpe_data$name) || grepl("Not enough data:", sharpe_data$name)) {
      return(sharpe_data$name)
    }
    paste("Expanding Window Sharpe Ratio (Rf = CPI):", sharpe_data$name)
  })
  
  # Render Expanding Window Sharpe Plot
  output$sharpe_expanding_plot <- renderPlot({
    results <- sharpe_results()
    if (is.null(results$expanding_df) || NROW(results$expanding_df) == 0) {
      sharpe_data <- sharpe_analysis_data()
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = sharpe_data$name, size = 6, color = "red"))
    }
    df <- results$expanding_df
    
    ggplot(df, aes(x = Date)) +
      geom_hline(yintercept = 0, linetype = "dashed")+
      geom_line(aes(y = SR), color = "black", size = 1) +
      geom_ribbon(aes(ymin = CI_lower_95, ymax = CI_upper_95), fill = "red", alpha = 0.2) +
      geom_ribbon(aes(ymin = CI_lower_68, ymax = CI_upper_68), fill = "orange", alpha = 0.2) +
      labs(
        x = "",
        y = "Sharpe Ratio (Rf = CPI)"
      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = expansion(mult = c(0.01, 0.01))) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # Dynamic title for Rolling Window Sharpe Ratio
  output$sharpe_rolling_title <- renderText({
    req(sharpe_analysis_data())
    sharpe_data <- sharpe_analysis_data()
    if (grepl("Error:", sharpe_data$name) || grepl("Not enough data:", sharpe_data$name)) {
      return(sharpe_data$name)
    }
    paste0("Rolling Window Sharpe Ratio (", input$sharpe_rolling_window_years, " Years) (Rf = CPI): ", sharpe_data$name)
  })
  
  # Render Rolling Window Sharpe Plot
  output$sharpe_rolling_plot <- renderPlot({
    results <- sharpe_results()
    if (is.null(results$rolling_df) || NROW(results$rolling_df) == 0) {
      sharpe_data <- sharpe_analysis_data()
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = sharpe_data$name, size = 6, color = "red"))
    }
    df <- results$rolling_df
    
    ggplot(df, aes(x = Date)) +
      geom_hline(yintercept = 0, linetype = "dashed")+
      geom_line(aes(y = SR), color = "black", size = 1) +
      geom_ribbon(aes(ymin = CI_lower_95, ymax = CI_upper_95), fill = "red", alpha = 0.2) +
      geom_ribbon(aes(ymin = CI_lower_68, ymax = CI_upper_68), fill = "orange", alpha = 0.2) +
      labs(
        x = "",
        y = "Sharpe Ratio (Rf = CPI)"
      ) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = expansion(mult = c(0.01, 0.01))) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # Reactive value to store simulation results for Retirement Ruin Probability tab
  retirement_sim_results <- reactiveVal(NULL)
  
  # Observe retirement simulation button for Retirement Ruin Probability tab
  observeEvent(input$run_retirement_sim, {
    req(input$retirement_fund)
    
    # Assign a unique ID to the notification
    id <- showNotification("Running retirement simulation...", duration = NULL)
    
    tryCatch({
      fund_data <- funds_xts[, input$retirement_fund, drop = FALSE]
      
      # Additional validation
      if(nrow(fund_data) < 12) stop("Selected fund has insufficient data (need ≥12 months)")
      if(all(fund_data == 0)) stop("Selected fund has no return variation")
      
      # Run simulation using the robust simulate_withdrawal function
      result <- simulate_withdrawal(
        returns = fund_data, # Pass xts object directly
        n_months_ahead = input$retirement_years * 12,
        principal = input$retirement_principal,
        n_sim = input$retirement_sims,
        escalation_rate = input$retirement_inflation # Corrected typo
      )
      
      if(is.null(result) || any(is.na(result$ruin_mat))) stop("Simulation produced invalid or NULL results.")
      
      retirement_sim_results(result$ruin_mat) # Store the data frame part of the result
      
    }, error = function(e) {
      showNotification(paste("Simulation error:", e->message), type = "error", duration = 10, id = id)
    }, finally = {
      removeNotification(id = id)
    })
  })
  
  # Render ruin probability plot for Retirement Ruin Probability tab
  output$ruin_probability_plot <- renderPlot({
    df <- retirement_sim_results()
    req(df)
    
    final_values <- data.frame(
      years = max(df$Years),
      perc_4 = df[nrow(df), "WR4"],
      perc_5 = df[nrow(df), "WR5"],
      perc_6 = df[nrow(df), "WR6"],
      perc_7 = df[nrow(df), "WR7"],
      perc_8 = df[nrow(df), "WR8"]
    )
    
    ggplot(df, aes(x = Years)) +
      ylim(0, 100) +
      geom_line(aes(y = WR4, color = "4% Initial Withdrawal Rate"), linewidth = 0.8) +
      geom_line(aes(y = WR5, color = "5% Initial Withdrawal Rate"), linewidth = 0.8) +
      geom_line(aes(y = WR6, color = "6% Initial Withdrawal Rate"), linewidth = 0.8) +
      geom_line(aes(y = WR7, color = "7% Initial Withdrawal Rate"), linewidth = 0.8) +
      geom_line(aes(y = WR8, color = "8% Initial Withdrawal Rate"), linewidth = 0.8) +
      geom_text(data = final_values, aes(x = years, y = perc_4, label = round(perc_4, 1)),
                color = "green", vjust = -0.5, hjust = -0.1, size = 5) +
      geom_text(data = final_values, aes(x = years, y = perc_5, label = round(perc_5, 1)),
                color = "blue", vjust = -0.5, hjust = -0.1, size = 5) +
      geom_text(data = final_values, aes(x = years, y = perc_6, label = round(perc_6, 1)),
                color = "black", vjust = -0.5, hjust = -0.1, size = 5) +
      geom_text(data = final_values, aes(x = years, y = perc_7, label = round(perc_7, 1)),
                color = "orange", vjust = -0.5, hjust = -0.1, size = 5) +
      geom_text(data = final_values, aes(x = years, y = perc_8, label = round(perc_8, 1)),
                color = "red", vjust = -0.5, hjust = -0.1, size = 5) +
      scale_color_manual(
        name = "Withdrawal Rate",
        values = c(
          "4% Initial Withdrawal Rate" = "green",
          "5% Initial Withdrawal Rate" = "blue",
          "6% Initial Withdrawal Rate" = "black",
          "7% Initial Withdrawal Rate" = "orange",
          "8% Initial Withdrawal Rate" = "red"
        )
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        legend.position = "right"
      ) +
      labs(
        x = paste0("Time (", input$retirement_years, " Years)"), # Dynamic X-axis label
        y = "Probability of Ruin (%)",
        title = paste("Retirement Withdrawal Strategy Analysis -", input$retirement_fund),
        subtitle = paste("Initial Principal:", format(input$retirement_principal, big.mark = ","))
      )
  })
  
  # Render results table for Retirement Ruin Probability tab
  output$ruin_probability_table <- renderTable({
    df <- retirement_sim_results()
    req(df)
    
    # Show final year probabilities for all withdrawal rates (4% to 8%)
    final_df <- data.frame(
      "Withdrawal Rate" = c("4%", "5%", "6%", "7%", "8%"),
      "Ruin Probability" = sprintf("%.1f%%", c(
        tail(df$WR4, 1),
        tail(df$WR5, 1),
        tail(df$WR6, 1),
        tail(df$WR7, 1),
        tail(df$WR8, 1)
      ))
    )
    
    final_df
  }, striped = TRUE, hover = TRUE, digits = 1)
  
  
  # --- Benchmark Analysis Tab Logic (NEW) ---
  
  # Reactive to get selected portfolio and benchmarks for analysis
  benchmark_analysis_data <- reactive({
    req(input$benchmark_analysis_portfolio_type, input$date_range_benchmark)
    
    portfolio_returns <- NULL
    portfolio_name <- ""
    
    # Determine the selected portfolio (Traditional Fund or Blended)
    if (input$benchmark_analysis_portfolio_type == "Traditional Fund") {
      req(input$benchmark_analysis_selected_trad_fund)
      portfolio_returns <- funds_xts[, input$benchmark_analysis_selected_trad_fund, drop = FALSE]
      portfolio_name <- input$benchmark_analysis_selected_trad_fund
    } else if (input$benchmark_analysis_portfolio_type == "Blended Portfolio") {
      req(input$benchmark_analysis_blend_fund1, input$benchmark_analysis_blend_fund2)
      if (input$benchmark_analysis_blend_fund1 == input$benchmark_analysis_blend_fund2) {
        return(list(returns = NULL, name = "Error: Select different funds for blended portfolio in Benchmark Analysis."))
      }
      fund1_data <- funds_xts[, input$benchmark_analysis_blend_fund1, drop = FALSE]
      fund2_data <- funds_xts[, input$benchmark_analysis_blend_fund2, drop = FALSE]
      
      combined_funds <- na.omit(cbind(fund1_data, fund2_data))
      if (NROW(combined_funds) == 0 || NCOL(combined_funds) < 2) {
        return(list(returns = NULL, name = "Not enough data for custom blended portfolio in Benchmark Analysis."))
      }
      
      portfolio_returns <- Return.portfolio(
        R = combined_funds,
        weights = c(0.5, 0.5), # Assuming 50/50 blend for benchmark comparison if weights not provided
        rebalance_on = "months"
      )
      colnames(portfolio_returns) <- portfolio_name
    }
    
    if (is.null(portfolio_returns)) {
      if (exists("name", where = portfolio_returns) && grepl("Error:|Not enough data:", portfolio_returns$name)) {
        return(portfolio_returns)
      }
      return(NULL)
    }
    
    # Get selected benchmarks
    selected_benchmarks <- NULL
    benchmark_names <- c()
    if (!is.null(input$benchmark_analysis_benchmarks) && length(input$benchmark_analysis_benchmarks) > 0) {
      selected_benchmarks <- Indices[, input$benchmark_analysis_benchmarks, drop = FALSE]
      benchmark_names <- input$benchmark_analysis_benchmarks
    }
    
    # Combine portfolio and benchmarks
    all_returns <- portfolio_returns
    all_names <- portfolio_name
    
    if (!is.null(selected_benchmarks)) {
      all_returns <- cbind(all_returns, selected_benchmarks)
      all_names <- c(all_names, benchmark_names)
    }
    
    # Filter by date range
    start_date <- input$date_range_benchmark[1]
    end_date <- input$date_range_benchmark[2]
    all_returns_filtered <- all_returns[paste0(start_date, "::", end_date)]
    
    # Clean names and remove NA rows
    colnames(all_returns_filtered) <- make.unique(all_names, sep = ".")
    all_returns_filtered <- na.omit(all_returns_filtered)
    
    if (NROW(all_returns_filtered) == 0 || NCOL(all_returns_filtered) == 0) {
      return(NULL)
    }
    
    return(all_returns_filtered)
  })
  
  # Reactive for plots data (Cumulative and Annual)
  benchmark_analysis_plots_data <- reactive({
    data_to_plot <- benchmark_analysis_data()
    if (is.null(data_to_plot) || NCOL(data_to_plot) == 0) {
      return(list(cumRetPlot = NULL, yearly_ret_plot = NULL))
    }
    performance_appraisal(data_to_plot)
  })
  
  # Render Cumulative Performance Plot
  output$benchmark_cumulative_plot <- renderPlot({
    plots <- benchmark_analysis_plots_data()
    if (is.null(plots$cumRetPlot)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for selected portfolio/benchmarks/period.", size = 6))
    }
    plots$cumRetPlot
  })
  
  # Render Annual Performance Plot
  output$benchmark_annual_plot <- renderPlot({
    plots <- benchmark_analysis_plots_data()
    if (is.null(plots$yearly_ret_plot)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No data available for selected portfolio/benchmarks/period.", size = 6))
    }
    plots$yearly_ret_plot
  })
  
  # Reactive for Rolling 1-Year Performance
  benchmark_analysis_rolling_returns_data <- reactive({
    returns_data <- benchmark_analysis_data()
    if (is.null(returns_data) || NROW(returns_data) < 12 || NCOL(returns_data) == 0) {
      return(NULL)
    }
    
    window_size <- 12 # 1-year rolling
    
    rolling_returns_list <- lapply(1:ncol(returns_data), function(i) {
      single_series_xts <- returns_data[, i, drop = FALSE]
      rollapply(single_series_xts, width = window_size, FUN = Return.cumulative, align = 'right', fill = NA)
    })
    
    rolling_returns <- do.call(cbind, rolling_returns_list)
    colnames(rolling_returns) <- colnames(returns_data)
    
    rolling_returns_df <- data.frame(Date = index(rolling_returns), coredata(rolling_returns))
    colnames(rolling_returns_df) <- c("Date", colnames(rolling_returns))
    
    data_melted <- reshape2::melt(rolling_returns_df, id.vars = "Date")
    colnames(data_melted) <- c("Date", "Series", "Rolling_Return")
    data_melted <- na.exclude(data_melted)
    
    data_melted$Series <- gsub("\\.", " ", data_melted$Series)
    
    if(NROW(data_melted) == 0) {
      return(NULL)
    }
    return(data_melted)
  })
  
  # Render Rolling 1-Year Performance Plot
  output$benchmark_rolling_plot <- renderPlot({
    data_melted <- benchmark_analysis_rolling_returns_data()
    if (is.null(data_melted) || NROW(data_melted) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Not enough data for Rolling 1-Year Performance (requires at least 12 months).", size = 6))
    }
    
    # Ensure portfolio_data uses the first column name (the selected portfolio)
    portfolio_col_name <- gsub("\\.", " ", make.names(colnames(benchmark_analysis_data())[1]))
    portfolio_data <- data_melted[data_melted$Series == portfolio_col_name, ]
    other_data <- data_melted[data_melted$Series != portfolio_col_name, ]
    
    color_palette <- colorRampPalette(c("blue", "red", "orange", "black", "purple", "green", "brown", "grey"))(length(unique(data_melted$Series)))
    
    ggplot() +
      geom_line(data = other_data, aes(x = Date, y = Rolling_Return * 100, color = Series), size = 1) +
      geom_line(data = portfolio_data, aes(x = Date, y = Rolling_Return * 100, color = Series), size = 1, linetype = "solid") + # Ensure portfolio is prominent
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
      labs(title = "Rolling 1-Year Performance",
           x = "",
           y = "Cumulative Return (%)") +
      scale_color_manual(values = color_palette) +
      scale_x_date(date_labels = "%Y", date_breaks = "2 years", expand = expansion(mult = c(0.01, 0.01))) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 22, hjust = 0, margin = ggplot2::margin(b = 20)),
        plot.margin = ggplot2::margin(20, 20, 20, 20),
        axis.title.x = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.title.y = element_text(size = 22, margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20)),
        axis.text.x = element_text(size = 20, margin = ggplot2::margin(t = 10), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  
  # Reactive for Performance Summary Table
  benchmark_analysis_summary_table_data <- reactive({
    data_for_table <- benchmark_analysis_data()
    if (is.null(data_for_table) || NROW(data_for_table) == 0 || NCOL(data_for_table) == 0) {
      return(data.frame(Message = "No data available for performance summary."))
    }
    
    perf_summary <- table.AnnualizedReturns(data_for_table)
    perf_summary_t <- as.data.frame(t(perf_summary))
    return(perf_summary_t)
  })
  
  # Render Performance Summary Table
  output$benchmark_summary_table <- renderTable({
    req(benchmark_analysis_summary_table_data())
    benchmark_analysis_summary_table_data()
  }, rownames = TRUE, digits = 2) # Adjust digits for better readability
  
  # Reactive for Tracking Error and Information Ratio
  benchmark_analysis_tracking_error_data <- reactive({
    data_for_metrics <- benchmark_analysis_data()
    if (is.null(data_for_metrics) || NCOL(data_for_metrics) < 2) {
      return(data.frame(Message = "Select at least one benchmark to calculate Tracking Error and Information Ratio."))
    }
    
    portfolio_returns <- data_for_metrics[, 1, drop = FALSE] # First column is the portfolio
    benchmark_returns <- data_for_metrics[, 2:ncol(data_for_metrics), drop = FALSE] # Remaining are benchmarks
    
    if (NCOL(benchmark_returns) == 0) {
      return(data.frame(Message = "No benchmarks selected for Tracking Error and Information Ratio."))
    }
    
    results <- data.frame(
      Benchmark = colnames(benchmark_returns),
      TrackingError = NA_real_,
      InformationRatio = NA_real_
    )
    
    for (i in 1:ncol(benchmark_returns)) {
      current_benchmark <- benchmark_returns[, i, drop = FALSE]
      # Align and omit NAs for pairwise comparison
      aligned_pair <- na.omit(cbind(portfolio_returns, current_benchmark))
      
      if (NROW(aligned_pair) > 1) { # Need at least 2 observations for sd/metrics
        results$TrackingError[i] <- TrackingError(Ra = aligned_pair[,1], Rb = aligned_pair[,2]) * 100
        results$InformationRatio[i] <- InformationRatio(Ra = aligned_pair[,1], Rb = aligned_pair[,2])
      }
    }
    
    # Clean up column names for display
    results$Benchmark <- gsub("\\.", " ", results$Benchmark)
    
    return(results)
  })
  
  # Render Tracking Error and Information Ratio Table
  output$benchmark_tracking_error_table <- renderTable({
    req(benchmark_analysis_tracking_error_data())
    benchmark_analysis_tracking_error_data()
  }, rownames = FALSE, digits = 2) # Adjust digits for better readability
  
}
