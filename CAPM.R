# --- Load Required Libraries ---
if (!require(quantmod)) install.packages("quantmod", dependencies = TRUE)
if (!require(TTR)) install.packages("TTR", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

library(quantmod)
library(TTR)
library(ggplot2)
library(dplyr)

# --- Define the Function to Compute CAPM and Plot SML ---
compute_capm_plot_sml_with_test <- function(risky_assets, training_start_date, training_end_date, risk_free_rate) {
  market_ticker <- "^NSEI"
  
  testing_start_date <- as.Date(training_end_date) + 1
  testing_end_date <- testing_start_date + 364
  
  cat("Fetching training data...\n")
  
  tryCatch({
    # Market training data
    market_data <- getSymbols(market_ticker, from = training_start_date, to = training_end_date,
                              warnings = FALSE, auto.assign = FALSE)
    
    # Risky assets training data (fetch individually with auto.assign=FALSE)
    risky_data <- lapply(risky_assets, function(ticker) {
      getSymbols(ticker, from = training_start_date, to = training_end_date,
                 warnings = FALSE, auto.assign = FALSE)
    })
    names(risky_data) <- risky_assets
    
    # Daily returns
    market_returns <- periodReturn(market_data, period = "daily")
    risky_returns_list <- lapply(risky_data, function(x) periodReturn(x, period = "daily"))
    
    # Merge into xts object
    training_returns_xts <- do.call(merge, c(list(market_returns), risky_returns_list))
    
    # Convert to df
    training_returns_df <<- data.frame(date = index(training_returns_xts), coredata(training_returns_xts))
    
    colnames(training_returns_df) <- c("date", market_ticker, risky_assets)
    
  }, error = function(e) {
    cat("Error fetching training data:", e$message, "\n")
    return(NULL)
  })
  
  # --- Calculate Betas ---
  betas <- c()
  for (stock in risky_assets) {
    lm_model <- lm(training_returns_df[[stock]] ~ training_returns_df[[market_ticker]], na.action = na.omit)
    beta <- coef(lm_model)[2]   # slope coefficient
    betas[stock] <- beta
  }
  
  # Risk-free rate + market premium
  daily_risk_free_rate <- (1 + risk_free_rate)^(1/252) - 1
  market_training_returns_mean <- mean(training_returns_df[[market_ticker]], na.rm = TRUE)
  market_risk_premium <- market_training_returns_mean - daily_risk_free_rate
  
  cat("\n----------Fetching Testing Data-------------\n")
  
  # Fetch testing data (individually again)
  risky_data_test <- list()
  for (ticker in risky_assets) {
    tryCatch({
      risky_data_test[[ticker]] <- getSymbols(ticker, from = testing_start_date, to = testing_end_date,
                                              warnings = FALSE, auto.assign = FALSE)
    }, error = function(e) {
      cat("Skipping", ticker, "in testing: No data available.\n")
    })
  }
  
  # --- Calculate Actual Annualized Returns ---
  actual_returns <- c()
  for (stock in names(risky_data_test)) {
    stock_data <- risky_data_test[[stock]]
    if (!is.null(stock_data) && nrow(stock_data) > 1) {
      initial_price <- as.numeric(Cl(stock_data[1, ]))
      final_price <- as.numeric(Cl(stock_data[nrow(stock_data), ]))
      raw_return <- final_price / initial_price - 1
      days_in_period <- as.numeric(testing_end_date - testing_start_date)
      annualized_return <- (1 + raw_return)^(365 / days_in_period) - 1
      actual_returns[stock] <- annualized_return
    }
  }
  
  # Results table
  results_df <- data.frame(
    "Beta" = as.numeric(betas[names(actual_returns)]),
    "ActualAnnualized" = as.numeric(actual_returns)
  )
  rownames(results_df) <- names(actual_returns)
  
  # --- Plot Security Market Line ---
  cat("\nPlotting the Security Market Line\n")
  
  sml_betas <- seq(min(results_df$Beta) - 0.1, max(results_df$Beta) + 0.1, length.out = 100)
  sml_expected_returns_annualized <- (1 + daily_risk_free_rate + sml_betas * market_risk_premium)^252 - 1
  
  sml_data <- data.frame(Beta = sml_betas, ExpectedReturn = sml_expected_returns_annualized)
  
  sml_plot <- ggplot(sml_data, aes(x = Beta, y = ExpectedReturn)) +
    geom_line(color = "red", linetype = "dashed", size = 1) +
    geom_point(data = results_df, aes(x = Beta, y = ActualAnnualized), color = "blue", size = 3) +
    geom_hline(yintercept = risk_free_rate, color = "green", linetype = "solid", size = 1) +
    geom_text(data = results_df, aes(x = Beta, y = ActualAnnualized, label = rownames(results_df)), 
              nudge_x = 0.05, nudge_y = 0.05, check_overlap = TRUE) +
    labs(
      title = paste("SML with Actual Returns (", testing_start_date, " to ", testing_end_date, ")", sep = ""),
      x = "Beta (β) from Training Period",
      y = "Actual Annualized Return from Testing Period"
    ) +
    theme_minimal()
  
  print(sml_plot)
  
  return(results_df)
}

# --- Example Usage ---
risky_assets <- c('RELIANCE.NS', 'TCS.NS', 'INFY.NS', 'HDFCBANK.NS', 
                  'ICICIBANK.NS', 'ITC.NS', 'HINDUNILVR.NS', 
                  'SBIN.NS', 'KOTAKBANK.NS', 'BAJFINANCE.NS')
training_start_date <- '2023-01-01'
training_end_date <- '2024-01-01'
risk_free_rate <- 0.065

results_table <- compute_capm_plot_sml_with_test(risky_assets, training_start_date, training_end_date, risk_free_rate)

if (!is.null(results_table)) {
  cat("\n--- CAPM Results Table ---\n")
  print(results_table)
}

