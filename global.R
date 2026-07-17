# global.R
# This file is sourced once when the Shiny application starts.
# It's ideal for loading data and performing initial, non-reactive data transformations.

# Load required libraries
# Not all of these libraries will necessarily be required for this specific chunk,
# but it's good practice to load all libraries that will be used across the app here.
# This file is sourced once when the Shiny application starts.
# It's ideal for loading data and performing initial, non-reactive data transformations.

## Install Libraries
install.packages(c("grid", "gridExtra", "PerformanceAnalytics", "stringdist", "PortfolioAnalytics", 
                   "quadprog", "readxl", "lubridate", "quantmod", "restriktor", "kdevine", 
                   "ggplot2", "scales", "foreach", "doParallel", "viridis", "Benchmarking", 
                   "NMOF", "pracma", "fPortfolio", "deaR", "dplyr", "reshape2", "tidyr", 
                   "tibble", "rugarch", "MASS", "simukde", "MKinfer", "LSMRealOptions", 
                   "zoo", "xts", "timeSeries", "SharpeR", "shiny", "shinycssloaders", 
                   "DT", "shinydashboard"))

# global.R
library(grid)
library(gridExtra)
library(PerformanceAnalytics)
library(stringdist)
library(PortfolioAnalytics)
library(quadprog)
library(readxl)
library(lubridate)
library(quantmod)
library(restriktor)
library(kdevine)
library(ggplot2)
library(scales)
library(foreach)
library(doParallel)
library(viridis)
library(Benchmarking)
library(NMOF)
library(pracma)
library(fPortfolio)
library(deaR)
library(dplyr)
library(reshape2)
library(tidyr)
library(tibble)
library(rugarch)
library(MASS)
library(simukde)
library(MKinfer)
library(LSMRealOptions)
library(zoo)

# Load necessary libraries
library(xts)
library(timeSeries) # For as.timeSeries and drawdowns
library(SharpeR) # For as.sr, confint
library(shiny) # For showNotification, removeNotification, modalDialog, removeModal
library(shinycssloaders) # For withSpinner

# Additional Libraries (already included in the list)
library(DT)
library(shinydashboard) # Ensure shinydashboard is loaded for dashboardPage, etc.

# --- Data Loading and Initial Processing (Asset Class Returns) ---
# IMPORTANT: Ensure these Excel files are in the SAME DIRECTORY as your app.R, ui.R, and server.R files.
# The paths have been changed to just the filenames for portability.
df_asset_returns <- data.frame(read_excel("Asset Class Returns Data.xlsx"))

# Extract and transpose relevant data after the "Unclassified" row
# Assuming 'Unclassified' is in the first column
data_raw <- t(df_asset_returns[(which(df_asset_returns[,1] == 'Unclassified') + 1):nrow(df_asset_returns),])
colnames(data_raw) <- data_raw[1,] # Set column names using first row
data_raw <- data_raw[2:nrow(data_raw),] # Remove the header row

# Convert all data to numeric
data_numeric <- apply(data_raw, MARGIN = 2, function(x) as.numeric(x))

# Create a date sequence starting from the parsed date in the file
# Assuming the date string is in df_asset_returns[8,2] and format is "Month Year" (e.g., "Jan 2000")
# Using lubridate's my() for robust month-year parsing
date_asset_returns <- seq.Date(from = my(as.character(df_asset_returns[8,2])),
                               length.out = nrow(data_numeric), by = 'months')

# Ensure the date column is of Date class
date_asset_returns <- as.Date(date_asset_returns)

# Convert to time-series (xts) object
data <- xts(data_numeric, order.by = date_asset_returns)

# --- CPI Extraction and Cleaning ---
CPI <- xts(data$`CPI (Headlin syn + Urban Areas`, order.by = time(data)) / 100
CPI <- na.exclude(CPI)

# --- Data Cleaning/Replacement Logic (SOFR, NAREIT, ALSI) ---
# Replace missing SOFR values with LIBOR where needed
temp_libor <- data$`ICE LIBOR 3 Month USD`
temp_sofr <- data$`Secured Overnight Financing Rate(SOFR`
missing_series_sofr <- which(is.na(temp_sofr) == TRUE)
temp_sofr[missing_series_sofr] <- temp_libor[missing_series_sofr]
data[,which(grepl("sofr", tolower(colnames(data))) == TRUE)] <- temp_sofr
rm(temp_libor, temp_sofr, missing_series_sofr)

# Replace missing NAREIT values with MSCI Real Estate to extend data series
temp_msci_re <- data$`MSCI World/Real Estate NR USD`
temp_ftse_nareit <- data$`FTSE EPRA Nareit Global REITs NR USD`
missing_series_nareit <- which(is.na(temp_ftse_nareit) == TRUE)
temp_ftse_nareit[missing_series_nareit] <- temp_msci_re[missing_series_nareit]
data[,which(grepl("nareit", tolower(colnames(data))) == TRUE)] <- temp_ftse_nareit
rm(temp_msci_re, temp_ftse_nareit, missing_series_nareit)

# Replace missing capped ALSI values with full ALSI to extend data series
temp_all_share <- data$`FTSE/JSE All Share TR ZAR`
temp_all_share_cap <- data$`FTSE/JSE All Share Capped TR ZAR`
missing_series_alsi <- which(is.na(temp_all_share_cap) == TRUE)
temp_all_share_cap[missing_series_alsi] <- temp_all_share[missing_series_alsi]
data[,which(grepl("capped", tolower(colnames(data))) == TRUE)] <- temp_all_share_cap
rm(temp_all_share, temp_all_share_cap, missing_series_alsi)

# --- Construct 'Styles' subset with MSCI style indices ---
Styles <- cbind(
  data$`MSCI World Growth NR USD`,
  data$`MSCI World Quality NR USD`,
  data$`MSCI World Value NR USD`
)
Styles <- apply(Styles, MARGIN = 2, function(x) as.numeric(x))
Styles <- xts(Styles, order.by = date_asset_returns) / 100
Styles <- na.exclude(Styles)
colnames(Styles) <- gsub("\\.+", " ", colnames(Styles)) # Clean up column names

# --- Construct main 'Indices' set ---
Indices <- cbind(
  data$`FTSE/JSE ALB 1-3 Yr TR ZAR`,
  data$`FTSE/JSE All Bond TR ZAR`,
  data$`Secured Overnight Financing Rate(SOFR`,
  data$`FTSE WGBI USD`,
  data$`FTSE/JSE All Share Capped TR ZAR`,
  data$`FTSE/JSE SA Listed Property Cap TR ZAR`,
  data$`MSCI World NR USD`,
  data$`FTSE EPRA Nareit Global REITs NR USD`,
  data$`MSCI EM NR USD`
)
Indices <- apply(Indices, MARGIN = 2, function(x) as.numeric(x))
Indices <- xts(Indices, order.by = date_asset_returns) / 100
colnames(Indices) <- gsub("\\.+", " ", colnames(Indices)) # Clean up column names

# Add additional asset classes
Local_cash <- na.exclude(data$`STeFI Composite ZAR`) / 100
ILB <- na.exclude(data$`Bloomberg Wld Govt Infl Lkd TR USD`) / 100
Credit <- na.exclude(data$`ICE BofA Global Corporate TR USD`) / 100

# Append to Indices set
Indices <- cbind(Local_cash, Indices, Styles, ILB, Credit)
colnames(Indices) <- gsub("\\.+", " ", colnames(Indices))
colnames(Indices) <- gsub("^\\s+|\\s+$", "", colnames(Indices)) # Trim whitespace

# --- Load Fund Data ---
# IMPORTANT: Ensure this Excel file is in the SAME DIRECTORY as your app.R, ui.R, and server.R files.
df_funds <- data.frame(read_excel("Amity Performance Reporting Monthly.xlsx"))

# Extract and transpose fund data after 'Unclassified' row
funds_raw <- t(df_funds[(which(df_funds[,1] == 'Unclassified') + 1):nrow(df_funds),])
colnames(funds_raw) <- funds_raw[1,]
funds_raw <- funds_raw[2:nrow(funds_raw),]

# Convert to numeric and extract TER row
funds_numeric <- apply(funds_raw, MARGIN = 2, function(x) as.numeric(x))
TER <- funds_numeric[1,] # Save Total Expense Ratio
funds_numeric <- funds_numeric[-1,] # Remove TER row

# Create date sequence for fund data
# Assuming the date string is in df_funds[8,3] and format is "Month Year" (e.g., "Jan 2000")
date_funds <- seq.Date(from = my(as.character(df_funds[8,3])),
                       length.out = nrow(funds_numeric), by = 'months')

# Ensure date_funds is of Date class
date_funds <- as.Date(date_funds)

# Convert funds to xts and divide by 100 to convert from percentage to decimal
funds_xts <- xts(funds_numeric, order.by = date_funds) / 100

# --- HEDGE FUND RETURN DATA -----------------------------------------------------------------

# Load required data from Excel
# IMPORTANT: Ensure this Excel file is in the SAME DIRECTORY as your app.R, ui.R, and server.R files.
hna_funds <- data.frame(
  read_excel(
    "HNA_Excel_20250501.xlsx",
    sheet = "Funds"
  )
)

# Create a clean data frame with only required columns and proper names
hna_funds <- data.frame(
  as.numeric(hna_funds$Fund.ID),
  hna_funds$Fund.Name,
  hna_funds$Fund.Type,
  hna_funds$Category
)
colnames(hna_funds) <- c("Fund.ID", "Fund.Name", "Fund.Type", "Category")

# Load return data from Excel
hna_returns <- data.frame(
  read_excel(
    "HNA_Excel_20250501.xlsx",
    sheet = "Returns"
  )
)

# Create a date sequence of month-end dates adjusted to start-of-month
all_dates_hna <- seq(from = min(hna_returns$Date), to = max(hna_returns$Date), by = 'days')
last_days_hna <- endpoints(all_dates_hna, on = "months")
date_hna <- all_dates_hna[last_days_hna] + days(1) - months(1)

# Ensure date_hna is of Date class
date_hna <- as.Date(date_hna)

# Initialize empty xts object for returns
returns_hna <- xts(matrix(NA, nrow = length(date_hna), ncol = length(unique(hna_returns$Fund.ID))), order.by = date_hna)

# Fill in returns per fund into the xts object
for (i in seq_along(unique(hna_returns$Fund.ID))) {
  fund_id <- unique(hna_returns$Fund.ID)[i]
  if (any(hna_funds$Fund.ID == fund_id)) {
    temp_returns <- hna_returns$Return[hna_returns$Fund.ID == fund_id]
    temp_dates <- hna_returns$Date[hna_returns$Fund.ID == fund_id] + days(1) - months(1)
    temp_xts <- xts(temp_returns, order.by = temp_dates)
    returns_hna[paste0(first(index(temp_xts)), "/", last(index(temp_xts))), i] <- temp_xts
  }
}

# Ensure proper column names
colnames(returns_hna) <- hna_funds$Fund.Name[hna_funds$Fund.ID %in% unique(hna_returns$Fund.ID)]

# Construct HNA indices composite (not directly used by UI, but part of client's script)
HNA_comp <- na.exclude(cbind(
  returns_hna$`HedgeNews Africa Fixed Income Index - MEDIAN`,
  returns_hna$`HedgeNews Africa Market Neutral & Quantitative Strategies Index - MEDIAN`,
  returns_hna$`HedgeNews Africa Long/Short Equity Index - MEDIAN`,
  returns_hna$`HedgeNews Africa Multi-Strategy Index - MEDIAN`
))

# Remove HNA index series from the dataset
index_cols_hna <- grepl("^HedgeNews Africa", colnames(returns_hna))
hna_fund_returns_set <- returns_hna[, !index_cols_hna] # This becomes hna_fund_returns_set

# Extract fund metadata for selected return set
categories_hna <- hna_funds$Category[hna_funds$Fund.Name %in% colnames(hna_fund_returns_set)]
type_hna <- hna_funds$Fund.Type[hna_funds$Fund.Name %in% colnames(hna_fund_returns_set)]

# Helper function: Remove unwanted funds based on name
remove_unwanted_funds <- function(return_set, categories) {
  keep <- !grepl("QIF|Qualified| QI ", colnames(return_set), ignore.case = TRUE)
  list(return_set = return_set[, keep, drop = FALSE], categories = categories[keep])
}
result_unwanted <- remove_unwanted_funds(hna_fund_returns_set, categories_hna)
hna_fund_returns_set <- result_unwanted$return_set
categories_hna <- result_unwanted$categories
type_hna <- hna_funds$Fund.Type[hna_funds$Fund.Name %in% colnames(hna_fund_returns_set)]

# Helper function: Remove near-duplicate fund names
remove_duplicates <- function(return_set, categories, threshold = 0.1) {
  if (ncol(return_set) == 0) return(list(return_set = return_set, categories = categories))
  dist_matrix <- stringdistmatrix(colnames(return_set), colnames(return_set), method = "jw")
  clusters <- cutree(hclust(as.dist(dist_matrix)), h = threshold)
  keep <- !duplicated(clusters)
  list(return_set = return_set[, keep, drop = FALSE], categories = categories[keep])
}
result_duplicates <- remove_duplicates(hna_fund_returns_set, categories_hna)
hna_fund_returns_set <- result_duplicates$return_set
categories_hna <- result_duplicates$categories
type_hna <- hna_funds$Fund.Type[hna_funds$Fund.Name %in% colnames(hna_fund_returns_set)]

# Helper function: Filter RIF-related columns
filter_rif_columns <- function(return_set, type) {
  rif_type_cols <- which(type == "RIF")
  rif_name_cols <- which(grepl("RIF|RIHF|Retail Hedge Fund", colnames(return_set), ignore.case = TRUE))
  cols_to_keep <- unique(c(rif_type_cols, rif_name_cols))
  if (length(cols_to_keep) == 0) return(xts()) # Return empty xts if no columns to keep
  return_set[, cols_to_keep, drop = FALSE]
}
hna_fund_returns_set <- filter_rif_columns(hna_fund_returns_set, type_hna)
categories_hna <- hna_funds$Category[hna_funds$Fund.Name %in% colnames(hna_fund_returns_set)]
type_hna <- hna_funds$Fund.Type[hna_funds$Fund.Name %in% colnames(hna_fund_returns_set)]

# Helper function: Construct category-level composites
comp.construct <- function(returns, categories, type = "median") {
  if (ncol(returns) == 0) return(list(composites = xts(), constituent_count = xts()))
  unique_categories <- unique(categories)
  composites <- xts(matrix(NA, nrow = nrow(returns), ncol = length(unique_categories)), order.by = index(returns))
  constituent_count <- xts(matrix(NA, nrow = nrow(returns), ncol = length(unique_categories)), order.by = index(returns))
  for (k in seq_along(unique_categories)) {
    subset_returns <- returns[, categories == unique_categories[k], drop = FALSE]
    if (ncol(subset_returns) > 0) {
      composite <- if (type == "mean") rowMeans(subset_returns, na.rm = TRUE) else apply(subset_returns, 1, median, na.rm = TRUE)
      composites[, k] <- composite
      constituent_count[, k] <- rowSums(!is.na(subset_returns))
    } else {
      composites[, k] <- NA
      constituent_count[, k] <- 0
    }
  }
  colnames(composites) <- colnames(constituent_count) <- unique_categories
  composites[composites == 0] <- NA
  list(composites = composites, constituent_count = constituent_count)
}

# Create final composite set for hedge funds
categories_hf_comp <- rep("Alternative", ncol(hna_fund_returns_set)) # Use a distinct variable name
hf_comp <- comp.construct(hna_fund_returns_set, categories_hf_comp, 'median')
hf_comp_subset <- na.exclude(hf_comp$composites)

# Clear intermediate objects from global environment (optional, but good practice for clarity)
rm(data_raw, data_numeric, date_asset_returns, df_asset_returns,funds_raw, funds_numeric, date_funds, df_funds,all_dates_hna, last_days_hna, date_hna, returns_hna, index_cols_hna,result_unwanted, result_duplicates, categories_hf_comp, hf_comp)

# For parallel processing
# Ensure these paths are correct for your environment
rmaxdd <- function(n_sim, mean, sd, horizon) {
  # This is a simplified placeholder.
  # A real rmaxdd function would likely simulate paths and then calculate max drawdown for each.
  # Here, we're just generating random values based on mean and sd, scaled by horizon.
  # This will NOT accurately represent true max drawdowns from a GBM process.
  abs(rnorm(n_sim, mean = mean * horizon, sd = sd * sqrt(horizon)))
}