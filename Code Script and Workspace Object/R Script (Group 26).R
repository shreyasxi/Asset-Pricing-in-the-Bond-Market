### ******** Regressing Excess Returns on MKTB ********

# Load necessary libraries
install.packages("readxl")
library(readxl)

# Read data
file_path <- "C:/Users/Asus/Desktop/Group Project Data.xlsx"
portfolio_returns <- read_excel(file_path, sheet = "Portfolio Returns")
risk_factors <- read_excel(file_path, sheet = "Corporate Bond Risk Factors")

# Extract MKTB and merge with portfolio returns
mktb <- risk_factors$MKTB
data <- cbind(MKTB = mktb, portfolio_returns)

# Extract portfolios (excluding date column)
portfolios <- portfolio_returns[, -1]

# Initialize lists for storing regression results
results <- list()
alphas <- c()
betas <- c()

# Time-series regressions for each portfolio
for (portfolio in colnames(portfolios)) {
  model <- lm(portfolios[[portfolio]] ~ mktb)
  summary_stats <- summary(model)
  
  # Store alpha, beta, and their significance
  alpha <- summary_stats$coefficients[1, 1]  # Intercept (alpha)
  beta <- summary_stats$coefficients[2, 1]   # Coefficient for MKTB (beta)
  alpha_pval <- summary_stats$coefficients[1, 4]  # p-value for alpha
  beta_pval <- summary_stats$coefficients[2, 4]   # p-value for beta
  
  # Store results
  results[[portfolio]] <- list(
    alpha = alpha,
    alpha_pval = alpha_pval,
    beta = beta,
    beta_pval = beta_pval,
    r_squared = summary_stats$r.squared
  )
  
  # Collect for cross-sectional regression
  alphas <- c(alphas, alpha)
  betas <- c(betas, beta)
}

# Create a summary DataFrame
summary_df <- do.call(rbind, lapply(names(results), function(portfolio_name) {
  cbind(
    Portfolio = portfolio_name,
    Alpha = results[[portfolio_name]]$alpha,
    Alpha_P_Value = results[[portfolio_name]]$alpha_pval,
    Beta = results[[portfolio_name]]$beta,
    Beta_P_Value = results[[portfolio_name]]$beta_pval,
    R_Squared = results[[portfolio_name]]$r_squared
  )
}))

summary_df <- as.data.frame(summary_df)

# Print the results for Time Series Regression (MKTB)
print(summary_df)

write.csv(summary_df, "C:/Users/Asus/Desktop/Time Series Results (MKTB).csv", row.names=FALSE)
  
# Count the number of statistically significant alphas
num_significant_alphas <- nrow(significant_alphas)

# Print the results
cat("Total number of statistically significant alphas:", num_significant_alphas, "\n")
cat("Portfolios with statistically significant alphas:\n")
print(significant_alphas)

## For Cross Sectional Regression 

average_returns <- colMeans(portfolios, na.rm = TRUE)  # Average excess returns
cross_sec_model <- lm(average_returns ~ betas)

# Cross-sectional regression summary
cross_sec_summary <- summary(cross_sec_model)

print("Cross-Sectional Regression Summary:")
print(cross_sec_summary)

--------------------------------------------------------------------------------
  
### ******** Regressing Excess Returns on MKTDB ********
  
mktdb <- risk_factors$MKTDB
data2 <- cbind(MKTDB = mktdb, portfolio_returns)

# Create a list to store regression results for each portfolio
results_MKTDB <- list()

# Perform time-series regression for each portfolio against MKTDB
for (portfolio in colnames(portfolios)) {
  # Run the regression: Portfolio Returns ~ MKTDB
  regression <- lm(data2[[portfolio]] ~ data2$MKTDB)
  
  # Extract regression summary statistics
  summary_stats <- summary(regression)
  
  # Extract coefficients, p-values, and R-squared
  alpha <- summary_stats$coefficients[1, 1]  # Intercept (alpha)
  beta <- summary_stats$coefficients[2, 1]   # Coefficient for MKTDB (beta)
  alpha_pval <- summary_stats$coefficients[1, 4]  # p-value for alpha
  beta_pval <- summary_stats$coefficients[2, 4]   # p-value for beta
  
  # Store results
  results_MKTDB[[portfolio]] <- list(
    alpha = alpha,
    alpha_pval = alpha_pval,
    beta = beta,
    beta_pval = beta_pval,
    r_squared = summary_stats$r.squared
  )
}

# Create a summary DataFrame for easy visualization
summary_df_MKTDB <- do.call(rbind, lapply(names(results_MKTDB), function(portfolio_name) {
  cbind(
    Portfolio = portfolio_name,
    Alpha = results_MKTDB[[portfolio_name]]$alpha,
    Alpha_P_Value = results_MKTDB[[portfolio_name]]$alpha_pval,
    Beta = results_MKTDB[[portfolio_name]]$beta,
    Beta_P_Value = results_MKTDB[[portfolio_name]]$beta_pval,
    R_Squared = results_MKTDB[[portfolio_name]]$r_squared
  )
}))

# Convert to a DataFrame and ensure numeric types for analysis
summary_df_MKTDB <- as.data.frame(summary_df_MKTDB)
summary_df_MKTDB$Alpha <- as.numeric(summary_df_MKTDB$Alpha)
summary_df_MKTDB$Alpha_P_Value <- as.numeric(summary_df_MKTDB$Alpha_P_Value)
summary_df_MKTDB$Beta <- as.numeric(summary_df_MKTDB$Beta)
summary_df_MKTDB$Beta_P_Value <- as.numeric(summary_df_MKTDB$Beta_P_Value)
summary_df_MKTDB$R_Squared <- as.numeric(summary_df_MKTDB$R_Squared)

print(summary_df_MKTDB)

write.csv(summary_df_MKTDB, "C:/Users/Asus/Desktop/Time Series Results (MKTDB).csv", row.names=FALSE)

# Checking the significance of alpha 
significant_alpha_mktdb <- summary_df_MKTDB[summary_df_MKTDB$Alpha_P_Value < 0.05, ]

# Print the significant alpha portfolios
print("Significant alpha portfolios:")
nrow(significant_alpha_mktdb)
print(significant_alpha_mktdb)

## Cross sectional analysis for MKTDB

Beta <- summary_df_MKTDB$Beta
average_returns <- colMeans(portfolios, na.rm = TRUE)  # Average excess returns
cross_sec_model_mktdb <- lm(average_returns ~ Beta)

# Cross-sectional regression summary
crosssec_summary_mktdb <- summary(cross_sec_model_mktdb)

print("Cross-Sectional Regression Summary:")
print(crosssec_summary_mktdb)

--------------------------------------------------------------------------------
  
### ******** GLS Estimates ********
  
## GLS Analysis for MKTB
  
library(nlme)

# Create lists to store GLS results
gls_results_mktb <- list()
gls_betas_mktb <- c()

# Time-series GLS regressions for each portfolio with MKTB
for (portfolio in colnames(portfolios)) {
  # Create data frame for GLS
  temp_data <- data.frame(
    returns = portfolios[[portfolio]],
    mktb = mktb
  )
  
  # Fit GLS model
  gls_model <- gls(returns ~ mktb, data = temp_data, 
                   weights = varFixed(~1/mktb^2))  # Using weights inversely proportional to MKTB squared
  
  # Store results
  coeffs <- coef(summary(gls_model))
  gls_results_mktb[[portfolio]] <- list(
    alpha = coeffs[1, 1],  # Intercept
    alpha_se = coeffs[1, 2],  # Standard error for alpha
    alpha_pval = coeffs[1, 4],  # p-value for alpha
    beta = coeffs[2, 1],   # Beta coefficient
    beta_se = coeffs[2, 2],  # Standard error for beta
    beta_pval = coeffs[2, 4]  # p-value for beta
  )
  
  gls_betas_mktb <- c(gls_betas_mktb, coeffs[2, 1])
}

# Create summary dataframe for MKTB GLS results
gls_summary_mktb <- do.call(rbind, lapply(names(gls_results_mktb), function(portfolio_name) {
  data.frame(
    Portfolio = portfolio_name,
    Alpha = gls_results_mktb[[portfolio_name]]$alpha,
    Alpha_SE = gls_results_mktb[[portfolio_name]]$alpha_se,
    Alpha_P_Value = gls_results_mktb[[portfolio_name]]$alpha_pval,
    Beta = gls_results_mktb[[portfolio_name]]$beta,
    Beta_SE = gls_results_mktb[[portfolio_name]]$beta_se,
    Beta_P_Value = gls_results_mktb[[portfolio_name]]$beta_pval
  )
}))

# Print results
cat("\nMKTB GLS Results:\n")
print(gls_summary_mktb)

write.csv(gls_summary_mktb, "C:/Users/Asus/Desktop/GLS Estimates (MKTB).csv", row.names=FALSE)


## GLS Analysis for MKTDB

# Create lists to store GLS results
gls_results_mktdb <- list()
gls_betas_mktdb <- c()

# Time-series GLS regressions for each portfolio with MKTDB
for (portfolio in colnames(portfolios)) {
  # Create data frame for GLS
  temp_data <- data.frame(
    returns = portfolios[[portfolio]],
    mktdb = mktdb
  )
  
  # Fit GLS model
  gls_model <- gls(returns ~ mktdb, data = temp_data, 
                   weights = varFixed(~1/mktdb^2))  # Using weights inversely proportional to MKTDB squared
  
  # Store results
  coeffs <- coef(summary(gls_model))
  gls_results_mktdb[[portfolio]] <- list(
    alpha = coeffs[1, 1],
    alpha_se = coeffs[1, 2],
    alpha_pval = coeffs[1, 4],
    beta = coeffs[2, 1],
    beta_se = coeffs[2, 2],
    beta_pval = coeffs[2, 4]
  )
  
  gls_betas_mktdb <- c(gls_betas_mktdb, coeffs[2, 1])
}

# Create summary dataframe for MKTDB GLS results
gls_summary_mktdb <- do.call(rbind, lapply(names(gls_results_mktdb), function(portfolio_name) {
  data.frame(
    Portfolio = portfolio_name,
    Alpha = gls_results_mktdb[[portfolio_name]]$alpha,
    Alpha_SE = gls_results_mktdb[[portfolio_name]]$alpha_se,
    Alpha_P_Value = gls_results_mktdb[[portfolio_name]]$alpha_pval,
    Beta = gls_results_mktdb[[portfolio_name]]$beta,
    Beta_SE = gls_results_mktdb[[portfolio_name]]$beta_se,
    Beta_P_Value = gls_results_mktdb[[portfolio_name]]$beta_pval
  )
}))

# Print results
cat("\nMKTDB GLS Results:\n")
print(gls_summary_mktdb)

write.csv(gls_summary_mktdb, "C:/Users/Asus/Desktop/GLS Estimates (MKTDB).csv", row.names=FALSE)

# Calculate number of significant alphas for each factor
significant_alphas_mktb <- sum(gls_summary_mktb$Alpha_P_Value < 0.05)
significant_alphas_mktdb <- sum(gls_summary_mktdb$Alpha_P_Value < 0.05)

cat("\nNumber of significant alphas (MKTB):", significant_alphas_mktb)
cat("\nNumber of significant alphas (MKTDB):", significant_alphas_mktdb)

# Cross-sectional GLS regression for MKTB
average_returns <- colMeans(portfolios, na.rm = TRUE)
cross_sec_gls_mktb <- gls(average_returns ~ gls_betas_mktb,
                          weights = varFixed(~1/gls_betas_mktb^2))

print(cross_sec_gls_mktb)

# Cross-sectional GLS regression for MKTDB
cross_sec_gls_mktdb <- gls(average_returns ~ gls_betas_mktdb,
                           weights = varFixed(~1/gls_betas_mktdb^2))

print(cross_sec_gls_mktdb)

-------------------------------------------------------------------------------

### ******** Constructing the Momentum Factor ********
  
install.packages("dplyr")
install.packages("zoo")
library(dplyr)
library(zoo)

# Get dates from portfolio returns
dates <- portfolio_returns[,1]
returns_matrix <- as.matrix(portfolio_returns[,-1])  # Remove date column

# Calculate 12-month rolling returns with 1-month lag
momentum_signals <- apply(returns_matrix, 2, function(x) {
  rollsum(stats::lag(x, 1), k=12, fill=NA, align="right")
})

# Initialize momentum factor vector
mom_factor <- numeric(nrow(returns_matrix))

# Calculate momentum factor for each month
for(t in 13:nrow(returns_matrix)) {  # Start from month 13 due to 12-month rolling window
  current_signals <- momentum_signals[t,]
  
  # Sort into deciles
  decile_ranks <- cut(current_signals, 
                      breaks=quantile(current_signals, 
                                      probs=seq(0,1,0.1), 
                                      na.rm=TRUE),
                      labels=FALSE, 
                      include.lowest=TRUE)
  
  # Calculate returns for winners (decile 10) minus losers (decile 1)
  mom_factor[t] <- mean(returns_matrix[t, which(decile_ranks == 10)], na.rm=TRUE) - 
    mean(returns_matrix[t, which(decile_ranks == 1)], na.rm=TRUE)
}

# Create momentum factor data frame with dates
momentum_df <- data.frame(
  Date = dates,
  MOM = mom_factor
)

# Remove NA periods
momentum_df <- na.omit(momentum_df)

# Print summary statistics
cat("\nMomentum Factor Summary Statistics:\n")
print(summary(momentum_df$MOM))

write.csv(momentum_df, "C:/Users/Asus/Desktop/momentum_factors.csv", row.names=FALSE)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
  
### ******** Multivariate Analysis ********
  
## Multivariate Model 1: MKTB + MKTDB + MOM ###
  
all_data <- data.frame(
portfolios = portfolio_returns[,-1],  # Remove date column
MKTB = mktb,
MKTDB = mktdb,
MOM = momentum_df$MOM
)

# Initialize storage for betas as a data frame with descriptive column names
multi_betas <- data.frame(
  Portfolio = colnames(portfolio_returns[,-1]),  
  Beta_MKTB = numeric(ncol(portfolio_returns[,-1])),  
  Beta_MKTDB = numeric(ncol(portfolio_returns[,-1])),  
  Beta_MOM = numeric(ncol(portfolio_returns[,-1]))
)

# Initialize list for storing results
multi_results <- list()

# Time-series regressions for each portfolio
for (i in 1:ncol(portfolio_returns[,-1])) {
  portfolio_name <- colnames(portfolio_returns)[i + 1]  # Adjust for removed date column
  
  # Multi-factor regression
  multi_model <- lm(portfolio_returns[[i + 1]] ~ MKTB + MKTDB + MOM, data = all_data)
  multi_summary <- summary(multi_model)
  
  # Store results
  multi_results[[portfolio_name]] <- list(
    coefficients = multi_model$coefficients,
    r_squared = multi_summary$r.squared,
    adj_r_squared = multi_summary$adj.r.squared,
    p_values = multi_summary$coefficients[,4],
    alpha = multi_summary$coefficients[1, 1],
    alpha_pval = multi_summary$coefficients[1, 4]
  )
  
  # Store betas for cross-sectional regression
  multi_betas[i, "Beta_MKTB"] <- multi_summary$coefficients["MKTB", 1]
  multi_betas[i, "Beta_MKTDB"] <- multi_summary$coefficients["MKTDB", 1]
  multi_betas[i, "Beta_MOM"] <- multi_summary$coefficients["MOM", 1]
}

# Create summary dataframe
multi_summary_df <- do.call(rbind, lapply(names(multi_results), function(port) {
  data.frame(
    Portfolio = port,
    Alpha = multi_results[[port]]$alpha,
    Alpha_Pval = multi_results[[port]]$alpha_pval,
    Beta_MKTB = multi_results[[port]]$coefficients["MKTB"],
    Beta_MKTDB = multi_results[[port]]$coefficients["MKTDB"],
    Beta_MOM = multi_results[[port]]$coefficients["MOM"],
    R_squared = multi_results[[port]]$r_squared,
    Adj_R_squared = multi_results[[port]]$adj_r_squared
  )
}))

# Print summary dataframe
print(multi_summary_df)

write.csv(multi_summary_df, "C:/Users/Asus/Desktop/Multivariate Regression Output.csv", row.names = FALSE)

# Count significant alphas
sig_alphas_multi <- sum(multi_summary_df$Alpha_Pval < 0.05)
cat("Number of significant alphas:", sig_alphas_multi, "\n\n")

# Cross-sectional regression using named columns
average_returns <- colMeans(portfolio_returns[,-1], na.rm = TRUE)
cross_sec_multi <- lm(average_returns ~ Beta_MKTB + Beta_MKTDB + Beta_MOM, data = multi_betas)
cross_sec_summary_multi <- summary(cross_sec_multi)

cat("\nCross Sectional Regression Summary:\n")
print(cross_sec_summary_multi)

# Test if risk premia are significantly different from zero
cat("Risk Premia Estimates:\n")
print(cross_sec_summary_multi$coefficients)

--------------------------------------------------------------------------------

## Multivariate Model 2: MKTB + MKTDB ##

# Combine all data into a single data frame
data3 <- data.frame(
  portfolios = portfolio_returns[,-1],  # Remove date column
  MKTB = mktb,
  MKTDB = mktdb
)

# Initialize storage for betas as a data frame
betas3 <- data.frame(
  Portfolio = colnames(portfolio_returns[,-1]),  
  Beta_MKTB = numeric(ncol(portfolio_returns[,-1])),  
  Beta_MKTDB = numeric(ncol(portfolio_returns[,-1]))
)

# Initialize list for storing results
results_mktb_mktdb <- list()

# Time-series regressions for each portfolio
for (i in 1:ncol(portfolio_returns[,-1])) {
  portfolio_name <- colnames(portfolio_returns)[i + 1]  # Adjust for removed date column
  
  # Two-factor regression
  model <- lm(portfolio_returns[[i + 1]] ~ MKTB + MKTDB, data = data3)
  model_summary <- summary(model)
  
  # Store results
  results_mktb_mktdb[[portfolio_name]] <- list(
    coefficients = model$coefficients,
    p_values = model_summary$coefficients[,4],
    alpha = model_summary$coefficients[1, 1],
    alpha_pval = model_summary$coefficients[1, 4]
  )
  
  # Store betas for cross-sectional regression with proper labeling
  betas3[i, "Beta_MKTB"] <- model_summary$coefficients["MKTB", 1]
  betas3[i, "Beta_MKTDB"] <- model_summary$coefficients["MKTDB", 1]
}

# Create summary dataframe
summary_mktb_mktdb <- do.call(rbind, lapply(names(results_mktb_mktdb), function(port) {
  data.frame(
    Portfolio = port,
    Alpha = results_mktb_mktdb[[port]]$alpha,
    Alpha_Pval = results_mktb_mktdb[[port]]$alpha_pval,
    Beta_MKTB = results_mktb_mktdb[[port]]$coefficients["MKTB"],
    Beta_MKTDB = results_mktb_mktdb[[port]]$coefficients["MKTDB"]
  )
}))

# Print summary dataframe
print(summary_mktb_mktdb)

# Count significant alphas
sig_alphas_mktb_mktdb <- sum(summary_mktb_mktdb$Alpha_Pval < 0.05)
cat("Number of significant alphas:", sig_alphas_mktb_mktdb, "\n\n")

# Cross-sectional regression using named columns
average_returns <- colMeans(portfolio_returns[,-1], na.rm = TRUE)
cross_sec_mktb_mktdb <- lm(average_returns ~ Beta_MKTB + Beta_MKTDB, data = betas3)
cross_sec_summary_mktb_mktdb <- summary(cross_sec_mktb_mktdb)

# Print cross-sectional regression results
cat("Cross-sectional Regression Results:\n")
print(cross_sec_summary_mktb_mktdb)

--------------------------------------------------------------------------------

## Multivariate Model 3: MKTB + Mom ##
  
# Combine all data into a single dataframe
data4 <- data.frame(
    portfolios = portfolio_returns[,-1],
    MKTB = mktb,
    MOM = momentum_df$MOM
)

# Initialize storage for betas as a data frame with descriptive column names
beta4 <- data.frame(
  Portfolio = colnames(portfolio_returns[,-1]),  # Use portfolio names
  Beta_MKTB = numeric(ncol(portfolio_returns[,-1])),  # Placeholder for MKTB betas
  Beta_MOM = numeric(ncol(portfolio_returns[,-1]))    # Placeholder for MOM betas
)

# Initialize list for storing results
results_mktb_mom <- list()

# Time-series regressions for each portfolio
for (i in 1:ncol(portfolio_returns[,-1])) {
  portfolio_name <- colnames(portfolio_returns)[i + 1]  # Adjust for removed date column
  
# Two-factor regression
model <- lm(portfolio_returns[[i + 1]] ~ MKTB + MOM, data = data4)
model_summary <- summary(model)
  
# Store results
results_mktb_mom[[portfolio_name]] <- list(
coefficients = model$coefficients,
p_values = model_summary$coefficients[,4],
alpha = model_summary$coefficients[1, 1],
alpha_pval = model_summary$coefficients[1, 4]
)
  
# Store betas for cross-sectional regression with proper labeling
beta4[i, "Beta_MKTB"] <- model_summary$coefficients["MKTB", 1]
beta4[i, "Beta_MOM"] <- model_summary$coefficients["MOM", 1]
}  

# Create summary dataframe
summary_mktb_mom <- do.call(rbind, lapply(names(results_mktb_mom), function(port) {
  data.frame(
    Portfolio = port,
    Alpha = results_mktb_mom[[port]]$alpha,
    Alpha_Pval = results_mktb_mom[[port]]$alpha_pval,
    Beta_MKTB = results_mktb_mom[[port]]$coefficients["MKTB"],
    Beta_MOM = results_mktb_mom[[port]]$coefficients["MOM"]
  )
}))

# Print summary dataframe
print(summary_mktb_mom)

# Count significant alphas
sig_alphas_mktb_mom <- sum(summary_mktb_mom$Alpha_Pval < 0.05)
cat("Number of significant alphas:", sig_alphas_mktb_mom, "\n\n")

# Cross-sectional regression using named columns
average_returns <- colMeans(portfolio_returns[,-1], na.rm = TRUE)
cross_sec_mktb_mom <- lm(average_returns ~ Beta_MKTB + Beta_MOM, data = beta4)
cross_sec_summary_mktb_mom <- summary(cross_sec_mktb_mom)

# Print cross-sectional regression results
cat("Cross-sectional Regression Results:\n")
print(cross_sec_summary_mktb_mom)

--------------------------------------------------------------------------------
  
### Multivariate Model 4: MKTDB + MOM ###
  
# Combine all data into a single dataframe
data5 <- data.frame(
    portfolios = portfolio_returns[,-1],  # Remove date column
    MKTDB = mktdb,
    MOM = momentum_df$MOM
)  

# Initialize storage for betas as a data frame with descriptive column names
beta5 <- data.frame(
  Portfolio = colnames(portfolio_returns[,-1]),  # Use portfolio names
  Beta_MKTDB = numeric(ncol(portfolio_returns[,-1])), # Placeholder for MKTDB betas
  Beta_MOM = numeric(ncol(portfolio_returns[,-1]))    # Placeholder for MOM betas
)

# Initialize list for storing results
results_mktdb_mom <- list()

# Time-series regressions for each portfolio
for (i in 1:ncol(portfolio_returns[,-1])) {
  portfolio_name <- colnames(portfolio_returns)[i + 1]  # Adjust for removed date column
  
  # Two-factor regression
  model <- lm(portfolio_returns[[i + 1]] ~ MKTDB + MOM, data = data5)
  model_summary <- summary(model)
  
  # Store results
  results_mktdb_mom[[portfolio_name]] <- list(
    coefficients = model$coefficients,
    p_values = model_summary$coefficients[,4],
    alpha = model_summary$coefficients[1, 1],
    alpha_pval = model_summary$coefficients[1, 4]
  )
  
  # Store betas for cross-sectional regression with proper labeling
  beta5[i, "Beta_MKTDB"] <- model_summary$coefficients["MKTDB", 1]
  beta5[i, "Beta_MOM"] <- model_summary$coefficients["MOM", 1]
}

# Create summary dataframe
summary_mktdb_mom <- do.call(rbind, lapply(names(results_mktdb_mom), function(port) {
  data.frame(
    Portfolio = port,
    Alpha = results_mktdb_mom[[port]]$alpha,
    Alpha_Pval = results_mktdb_mom[[port]]$alpha_pval,
    Beta_MKTDB = results_mktdb_mom[[port]]$coefficients["MKTDB"],
    Beta_MOM = results_mktdb_mom[[port]]$coefficients["MOM"]
  )
}))

# Print summary dataframe
print(summary_mktdb_mom)

# Count significant alphas
sig_alphas_mktdb_mom <- sum(summary_mktdb_mom$Alpha_Pval < 0.05)
cat("Number of significant alphas:", sig_alphas_mktdb_mom, "\n\n")

# Cross-sectional regression using named columns
average_returns <- colMeans(portfolio_returns[,-1], na.rm = TRUE)
cross_sec_mktdb_mom <- lm(average_returns ~ Beta_MKTDB + Beta_MOM, data = beta5)
cross_sec_summary_mktdb_mom <- summary(cross_sec_mktdb_mom)

# Print cross-sectional regression results
cat("Cross-sectional Regression Results:\n")
print(cross_sec_summary_mktdb_mom)

--------------------------------------------------------------------------------
  
### Comparing different combinations of models ###
  
library(gt)

# Data for the table with simplified column names
datatt <- data.frame(
  Model = c("One Factor (MKTB)", "One Factor (MKTDB)", "Multivariate Model 1: MKTB + MKTDB + MOM",
            "Multivariate Model 2: MKTB + MKTDB", "Multivariate Model 3: MKTB + MOM",
            "Multivariate Model 4: MKTDB + MOM"),
  CrossR2 = c(73.04, 85.61, 86.49, 85.9, 85, 86.23),
  AdjR2 = c(72.48, 85.31, 85.61, 85.3, 84.36, 85.65),
  SigAlphas = c(2, 27, 14, 9, 17, 43),
  RiskPremiaSignif = c("Significant", "Significant", "All Significant", 
                       "All Significant", "All Significant", "All Significant"),
  RiskPremiaSigns = c("Positive (MKTB)", "Positive (MKTDB)", "Positive (MKTB, MKTDB), Negative (MOM)",
                      "Positive (MKTB, MKTDB)", "Positive (MKTB), Negative (MOM)",
                      "Positive (MKTDB), Negative (MOM)"),
  Inference = c(
    "Basic single-factor model, the lowest goodness of fit.",
    "Improved explanatory power over MKTB model.",
    "Best fit but MOM introduces residual errors.",
    "Balanced model with lowest pricing errors.",
    "MOM improves explanatory power over single factor MKTB model.",
    "High R² but MOM complicates pricing."
  ),
  stringsAsFactors = FALSE
) 

# Create the table using gt
table <- gt(datatt) %>%
  tab_header(
    title = md("**Model Comparison: Corporate Bond Excess Returns**"),
    subtitle = md("**A detailed evaluation of single-factor and multi-factor models**")
  ) %>%
  cols_label(
    Model = md("**Model Type**"),
    CrossR2 = md("**Cross Sectional R² (%)**"),
    AdjR2 = md("**Adjusted R² (%)**"),
    SigAlphas = md("**Significant Alphas (OLS)**"),
    RiskPremiaSignif = md("**Statistical Significance of Risk Premia**"),
    RiskPremiaSigns = md("**Signs of Risk Premia**"),
    Inference = md("**Inference**")
  ) %>%
  fmt_number(
    columns = c(CrossR2, AdjR2),
    decimals = 2
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Model)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_options(
    table.font.size = px(14),
    data_row.padding = px(10),
    table.width = pct(70)
  )

# Print the table
table

--------------------------------------------------------------------------------
  
### GLS Estimates for multivariate model ###

library(nlme)

# Create lists to store GLS results
gls_results_multivariate <- list()

# Time-series GLS regressions for each portfolio with MKTB, MKTDB, and MOM
for (portfolio in colnames(portfolios)) {
  # Create data frame for GLS
  temp_data <- data.frame(
    returns = portfolios[[portfolio]],
    mktb = mktb,
    mktdb = mktdb,
    mom = MOM
  )
  
  # Fit GLS model
  gls_model <- gls(returns ~ mktb + mktdb + mom, data = temp_data, 
                   weights = varFixed(~1/(mktb^2 + mktdb^2 + mom^2)))  # Combined weights for all factors
  
  # Store results
  coeffs <- coef(summary(gls_model))
  gls_results_multivariate[[portfolio]] <- list(
    alpha = coeffs[1, 1],  # Intercept
    alpha_se = coeffs[1, 2],  # Standard error for alpha
    alpha_pval = coeffs[1, 4],  # p-value for alpha
    beta_mktb = coeffs[2, 1],  # Beta for MKTB
    beta_mktb_se = coeffs[2, 2],  # SE for MKTB beta
    beta_mktb_pval = coeffs[2, 4],  # p-value for MKTB beta
    beta_mktdb = coeffs[3, 1],  # Beta for MKTDB
    beta_mktdb_se = coeffs[3, 2],  # SE for MKTDB beta
    beta_mktdb_pval = coeffs[3, 4],  # p-value for MKTDB beta
    beta_mom = coeffs[4, 1],  # Beta for MOM
    beta_mom_se = coeffs[4, 2],  # SE for MOM beta
    beta_mom_pval = coeffs[4, 4]  # p-value for MOM beta
  )
}

# Create summary dataframe for Multivariate GLS results
gls_summary_multivariate <- do.call(rbind, lapply(names(gls_results_multivariate), function(portfolio_name) {
  data.frame(
    Portfolio = portfolio_name,
    Alpha = gls_results_multivariate[[portfolio_name]]$alpha,
    Alpha_SE = gls_results_multivariate[[portfolio_name]]$alpha_se,
    Alpha_P_Value = gls_results_multivariate[[portfolio_name]]$alpha_pval,
    Beta_MKTB = gls_results_multivariate[[portfolio_name]]$beta_mktb,
    Beta_MKTB_SE = gls_results_multivariate[[portfolio_name]]$beta_mktb_se,
    Beta_MKTB_P_Value = gls_results_multivariate[[portfolio_name]]$beta_mktb_pval,
    Beta_MKTDB = gls_results_multivariate[[portfolio_name]]$beta_mktdb,
    Beta_MKTDB_SE = gls_results_multivariate[[portfolio_name]]$beta_mktdb_se,
    Beta_MKTDB_P_Value = gls_results_multivariate[[portfolio_name]]$beta_mktdb_pval,
    Beta_MOM = gls_results_multivariate[[portfolio_name]]$beta_mom,
    Beta_MOM_SE = gls_results_multivariate[[portfolio_name]]$beta_mom_se,
    Beta_MOM_P_Value = gls_results_multivariate[[portfolio_name]]$beta_mom_pval
  )
}))

# Print results
cat("\nMultivariate GLS Results:\n")
print(gls_summary_multivariate)

write.csv(gls_summary_multivariate, "C:/Users/Asus/Desktop/GLS Estimates (Multivariate).csv", row.names = FALSE)

# Calculate number of significant alphas for the multivariate model
significant_alphas_multivariate <- sum(gls_summary_multivariate$Alpha_P_Value < 0.05)

cat("\nNumber of significant alphas (Multivariate Model):", significant_alphas_multivariate)

--------------------------------------------------------------------------------
  
### ******** BP Test for Heteroskedasticity ********
  
## BP Test for MKTB Factor 
  
install.packages("lmtest")
library(lmtest)

# Perform BP test for each portfolio and store results
bp_test_results <- list()

for (portfolio in colnames(portfolios)) {
  # Run regression model for the portfolio
  model <- lm(portfolios[[portfolio]] ~ mktb)
  
  # Perform the BP test
  bp_test <- bptest(model)
  
  # Store the results in the list
  bp_test_results[[portfolio]] <- list(
    statistic = bp_test$statistic,
    p_value = bp_test$p.value,
    conclusion = ifelse(bp_test$p.value < 0.05, "Heteroskedasticity detected", "No heteroskedasticity")
  )
}

# Create a summary DataFrame for BP test results
bp_summary_df <- do.call(rbind, lapply(names(bp_test_results), function(portfolio_name) {
  cbind(
    Portfolio = portfolio_name,
    BP_Statistic = bp_test_results[[portfolio_name]]$statistic,
    P_Value = bp_test_results[[portfolio_name]]$p_value,
    Conclusion = bp_test_results[[portfolio_name]]$conclusion
  )
}))

# Convert the summary to a data frame
bp_summary_df <- as.data.frame(bp_summary_df)

# Print BP test summary
print("Breusch-Pagan Test Summary:")
print(bp_summary_df)

write.csv(bp_summary_df, "C:/Users/Asus/Desktop/BP Test for MKTB.csv", row.names=FALSE)

# Count portfolios with heteroskedasticity
num_heteroskedastic <- sum(bp_summary_df$Conclusion == "Heteroskedasticity detected")
cat("Number of portfolios with heteroskedasticity (MKTB):", num_heteroskedastic, "\n")

## BP Test For MKTDB ##

# Create a list to store BP test results for each portfolio
bp_test_results_MKTDB <- list()

# Perform BP test for each portfolio regression with MKTDB
for (portfolio in colnames(portfolios)) {
  # Run the regression: Portfolio Returns ~ MKTDB
  regression <- lm(data2[[portfolio]] ~ data2$MKTDB)
  
  # Perform the Breusch-Pagan test for heteroskedasticity
  bp_test <- bptest(regression)
  
  # Store the results
  bp_test_results_MKTDB[[portfolio]] <- list(
    statistic = bp_test$statistic,
    p_value = bp_test$p.value,
    conclusion = ifelse(bp_test$p.value < 0.05, "Heteroskedasticity detected", "No heteroskedasticity")
  )
}

# Create a summary DataFrame for BP test results
bp_summary_df_MKTDB <- do.call(rbind, lapply(names(bp_test_results_MKTDB), function(portfolio_name) {
  cbind(
    Portfolio = portfolio_name,
    BP_Statistic = bp_test_results_MKTDB[[portfolio_name]]$statistic,
    P_Value = bp_test_results_MKTDB[[portfolio_name]]$p_value,
    Conclusion = bp_test_results_MKTDB[[portfolio_name]]$conclusion
  )
}))

# Convert to a DataFrame and ensure numeric types
bp_summary_df_MKTDB <- as.data.frame(bp_summary_df_MKTDB)
bp_summary_df_MKTDB$BP_Statistic <- as.numeric(bp_summary_df_MKTDB$BP_Statistic)
bp_summary_df_MKTDB$P_Value <- as.numeric(bp_summary_df_MKTDB$P_Value)

# Print BP test summary
print("Breusch-Pagan Test Summary for MKTDB Regressions:")
print(bp_summary_df_MKTDB)

write.csv(bp_summary_df_MKTDB, "C:/Users/Asus/Desktop/BP Test for MKTDB.csv", row.names=FALSE)

# OCount portfolios with detected heteroskedasticity
num_heteroskedastic_MKTDB <- sum(bp_summary_df_MKTDB$Conclusion == "Heteroskedasticity detected")
cat("Number of portfolios with heteroskedasticity (MKTDB):", num_heteroskedastic_MKTDB, "\n")

### BP Test for Multivariate Model ###

bp_test_results_multi <- list()

MOM <- momentum_df$MOM

for (portfolio in colnames(portfolios)) {
  # Run multivariate regression model for the portfolio
  model_multi <- lm(portfolios[[portfolio]] ~ mktb + mktdb + MOM)
  
  # Perform the BP test
  bp_test_multi <- bptest(model_multi)
  
  # Store the results in the list
  bp_test_results_multi[[portfolio]] <- list(
    statistic = bp_test_multi$statistic,
    p_value = bp_test_multi$p.value,
    conclusion = ifelse(bp_test_multi$p.value < 0.05, "Heteroskedasticity detected", "No heteroskedasticity")
  )
}

# Create a summary DataFrame for BP test results (multivariate model)
bp_summary_multi_df <- do.call(rbind, lapply(names(bp_test_results_multi), function(portfolio_name) {
  cbind(
    Portfolio = portfolio_name,
    BP_Statistic = bp_test_results_multi[[portfolio_name]]$statistic,
    P_Value = bp_test_results_multi[[portfolio_name]]$p_value,
    Conclusion = bp_test_results_multi[[portfolio_name]]$conclusion
  )
}))

# Convert the summary to a data frame
bp_summary_multi_df <- as.data.frame(bp_summary_multi_df)

# Print BP test summary
print("Breusch-Pagan Test Summary for Multivariate Model:")
print(bp_summary_multi_df)

# Count portfolios with heteroskedasticity

num_heteroskedastic_multi <- sum(bp_summary_multi_df$Conclusion == "Heteroskedasticity detected")
cat("Number of portfolios with heteroskedasticity (multivariate):", num_heteroskedastic_multi, "\n")

--------------------------------------------------------------------------------
  
### ******** GRS Test  ********
  
## GRS Test for MKTB 

library(MASS)  # For matrix computations
library(dplyr) # For clean data manipulation

# Define the GRS Test function

grs_test <- function(portfolio_returns, factor_returns) {
  T <- nrow(portfolio_returns)  # Time periods
  N <- ncol(portfolio_returns)  # Number of portfolios
  K <- ncol(as.matrix(factor_returns))  # Number of factors
  
# Regress each portfolio on factor returns
  
  alphas <- c()
  residuals <- matrix(NA, nrow = T, ncol = N)
  for (i in 1:N) {
    portfolio_column <- as.numeric(portfolio_returns[, i])  # Ensure numeric vector
    factor_matrix <- as.matrix(factor_returns)  # Ensure matrix for regression
    model <- lm(portfolio_column ~ factor_matrix)
    summary_model <- summary(model)
    alphas <- c(alphas, summary_model$coefficients[1, 1])
    residuals[, i] <- model$residuals
  }
  
  # Residual covariance matrix
  Sigma <- cov(residuals)
  
  # Mean and covariance of factor returns
  mean_factors <- colMeans(factor_returns)
  factor_cov <- cov(factor_returns)
  
  # GRS statistic calculation
  Sigma_inv <- solve(Sigma)
  alpha_matrix <- as.matrix(alphas)
  term1 <- T / N * t(alpha_matrix) %*% Sigma_inv %*% alpha_matrix
  term2 <- 1 + t(mean_factors) %*% solve(factor_cov) %*% mean_factors
  GRS_stat <- as.numeric(term1 / term2)
  
  # F-statistic p-value
  df1 <- N
  df2 <- T - N - K
  p_value <- 1 - pf(GRS_stat, df1, df2)
  
  # Return results as a named list
  return(list(
    GRS_stat = GRS_stat,
    p_value = p_value,
    df1 = df1,
    df2 = df2
  ))
}

# and `mktb` is the factor returns matrix for MKTB.
portfolios <- as.data.frame(portfolio_returns[, -1])  # Exclude the date column

# Run the GRS test for MKTB
results_mktb <- grs_test(portfolios, as.matrix(mktb))

# Create a results table for MKTB
results_table_mktb <- data.frame(
  Factor = "MKTB",
  GRS_Statistic = round(results_mktb$GRS_stat, 4),
  P_Value = formatC(results_mktb$p_value, format = "e", digits = 4),
  DF1 = results_mktb$df1,
  DF2 = results_mktb$df2,
  Conclusion = ifelse(results_mktb$p_value < 0.05, 
                      "Reject Null Hypothesis", 
                      "Fail to Reject Null Hypothesis")
)

# Print the results table
print(results_table_mktb, row.names = FALSE)


### GRS Test for MKTDB ###

grs_test <- function(portfolio_returns, factor_returns) {
  T <- nrow(portfolio_returns)  # Time periods
  N <- ncol(portfolio_returns)  # Number of portfolios
  K <- ncol(as.matrix(factor_returns))  # Number of factors
  
# Regress each portfolio on factor returns
  
  alphas <- c()
  residuals <- matrix(NA, nrow = T, ncol = N)
  for (i in 1:N) {
    portfolio_column <- as.numeric(portfolio_returns[, i])  # Ensure numeric vector
    factor_matrix <- as.matrix(factor_returns)  # Ensure matrix for regression
    model <- lm(portfolio_column ~ factor_matrix)
    summary_model <- summary(model)
    alphas <- c(alphas, summary_model$coefficients[1, 1]) 
    residuals[, i] <- model$residuals
  }
  
# Residual covariance matrix
  Sigma <- cov(residuals)
  
# Mean and covariance of factor returns
  
  mean_factors <- colMeans(factor_returns)
  factor_cov <- cov(factor_returns)
  
# GRS statistic calculation
  
  Sigma_inv <- solve(Sigma)
  alpha_matrix <- as.matrix(alphas)
  term1 <- T / N * t(alpha_matrix) %*% Sigma_inv %*% alpha_matrix
  term2 <- 1 + t(mean_factors) %*% solve(factor_cov) %*% mean_factors
  GRS_stat <- as.numeric(term1 / term2)
  
  # F-statistic p-value
  df1 <- N
  df2 <- T - N - K
  p_value <- 1 - pf(GRS_stat, df1, df2)
  
  # Return results
  return(list(
    GRS_stat = GRS_stat,
    p_value = p_value,
    df1 = df1,
    df2 = df2
  ))
}

# Perform GRS test for the MKTDB factor
factor_returns_mktdb <- as.matrix(mktdb)

# Run the GRS test for MKTDB
results_mktdb <- grs_test(as.data.frame(portfolio_returns[, -1]), factor_returns_mktdb)

# Format the results into a table
results_table_mktdb <- data.frame(
  Factor = "MKTDB",
  GRS_Statistic = round(results_mktdb$GRS_stat, 4),
  P_Value = formatC(results_mktdb$p_value, format = "e", digits = 4),
  DF1 = results_mktdb$df1,
  DF2 = results_mktdb$df2,
  Conclusion = ifelse(results_mktdb$p_value < 0.05, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis")
)

# Print the results table
print(results_table_mktdb, row.names = FALSE)

--------------------------------------------------------------------------------

### GRS Test for Various Multivariate Models ###
  
## GRS Test for MKTB + MKTDB + MOM ##

library(MASS)  # For matrix computations

# Define the GRS test function for multivariate regression
grs_test_multivariate <- function(portfolio_returns, factors) {
  T <- nrow(portfolio_returns)  # Number of time periods
  N <- ncol(portfolio_returns)  # Number of portfolios
  K <- ncol(factors)            # Number of factors
  
  # Regress each portfolio on the factors
  alphas <- c()
  residuals <- matrix(NA, nrow = T, ncol = N)
  for (i in 1:N) {
    portfolio_column <- as.numeric(portfolio_returns[, i])  # Ensure numeric vector
    factor_matrix <- as.matrix(factors)  # Ensure matrix for regression
    model <- lm(portfolio_column ~ factor_matrix)
    summary_model <- summary(model)
    alphas <- c(alphas, summary_model$coefficients[1, 1])  # Intercept (alpha)
    residuals[, i] <- model$residuals
  }
  
  # Residual covariance matrix
  Sigma <- cov(residuals)
  
  # Mean and covariance of factors
  mean_factors <- colMeans(factors)
  factor_cov <- cov(factors)
  
  # GRS statistic calculation
  Sigma_inv <- solve(Sigma)
  alpha_matrix <- as.matrix(alphas)
  term1 <- T / N * t(alpha_matrix) %*% Sigma_inv %*% alpha_matrix
  term2 <- 1 + t(mean_factors) %*% solve(factor_cov) %*% mean_factors
  GRS_stat <- as.numeric(term1 / term2)
  
  # F-statistic p-value
  df1 <- N
  df2 <- T - N - K
  p_value <- 1 - pf(GRS_stat, df1, df2)
  
  # Return results
  return(list(
    GRS_stat = GRS_stat,
    p_value = p_value,
    df1 = df1,
    df2 = df2
  ))
}

# Ensure `portfolio_returns` is a numeric matrix
portfolio_returns_numeric <- as.matrix(portfolio_returns[, -1])  # Remove the date column if present
portfolio_returns_numeric <- apply(portfolio_returns_numeric, 2, as.numeric)  # Ensure all columns are numeric

# Combine factors into a single matrix
total_factors <- cbind(mktb, mktdb, MOM)

# Ensure `total_factors` is a numeric matrix
total_factors_matrix <- as.matrix(total_factors)
total_factors_matrix <- apply(total_factors_matrix, 2, as.numeric)  # Ensure all columns are numeric

# Run the multivariate GRS test with corrected data structures
results_multivariate <- grs_test_multivariate(portfolio_returns_numeric, total_factors_matrix)

# Prepare a results table for multivariate regression
grs_multivariate_table <- data.frame(
  Factor = "MKTB + MKTDB + MOM",
  GRS_Statistic = round(results_multivariate$GRS_stat, 4),
  P_Value = formatC(results_multivariate$p_value, format = "e", digits = 4),
  DF1 = results_multivariate$df1,
  DF2 = results_multivariate$df2,
  Conclusion = ifelse(results_multivariate$p_value < 0.05, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis")
)

# Print the results table
print(grs_multivariate_table, row.names = FALSE)


### GRS Test for other multivariate combinations ###

# Create different factor combinations
factors_mktb_mktdb <- cbind(mktb, mktdb)
factors_mktb_mom <- cbind(mktb, MOM)
factors_mktdb_mom <- cbind(mktdb, MOM)

# Run GRS tests for each combination
results_mktb_mktdb <- grs_test_multivariate(portfolio_returns_numeric, factors_mktb_mktdb)
results_mktb_mom <- grs_test_multivariate(portfolio_returns_numeric, factors_mktb_mom)
results_mktdb_mom <- grs_test_multivariate(portfolio_returns_numeric, factors_mktdb_mom)

# Create a comprehensive results table for all combinations
grs_combinations_table <- data.frame(
  Factor_Combination = c("MKTB + MKTDB", "MKTB + MOM", "MKTDB + MOM"),
  GRS_Statistic = round(c(
    results_mktb_mktdb$GRS_stat,
    results_mktb_mom$GRS_stat,
    results_mktdb_mom$GRS_stat
  ), 4),
  P_Value = formatC(c(
    results_mktb_mktdb$p_value,
    results_mktb_mom$p_value,
    results_mktdb_mom$p_value
  ), format = "e", digits = 4),
  DF1 = c(
    results_mktb_mktdb$df1,
    results_mktb_mom$df1,
    results_mktdb_mom$df1
  ),
  DF2 = c(
    results_mktb_mktdb$df2,
    results_mktb_mom$df2,
    results_mktdb_mom$df2
  ),
  Conclusion = ifelse(
    c(
      results_mktb_mktdb$p_value,
      results_mktb_mom$p_value,
      results_mktdb_mom$p_value
    ) < 0.05,
    "Reject Null Hypothesis",
    "Fail to Reject Null Hypothesis"
  )
)

# Print the results table
print(grs_combinations_table, row.names = FALSE)

--------------------------------------------------------------------------------
  
### Code for the plots used in the paper ###
  
# ******* Graph: Heteroskedastic Portfolios *******
  
library(ggplot2)

# Create a simplified dataframe with just portfolio numbers and their status
portfolio_data <- data.frame(
  Portfolio = 1:nrow(bp_summary_df),
  Is_Heteroskedastic = bp_summary_df$P_Value < 0.05,
  P_Value = bp_summary_df$P_Value
)

# Create the simplified portfolio plot
het_plot <- ggplot(portfolio_data, aes(x = Portfolio, y = P_Value)) +
  # Add reference line at 0.05
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.7) +
  # Add points
  geom_point(aes(color = Is_Heteroskedastic), size = 3) +
  # Customize colors
  scale_color_manual(values = c("grey50", "red"),
                     labels = c("Homoskedastic", "Heteroskedastic"),
                     name = "Portfolio Type") +
  # Add labels
  labs(x = "Portfolio Number",
       y = "P-Value",
       title = "Heteroskedasticity Analysis for MKTB Factor Model",
       subtitle = paste0(sum(portfolio_data$Is_Heteroskedastic), 
                         " out of ", nrow(portfolio_data),
                         " portfolios exhibit heteroskedasticity")) +
  # Customize theme
  theme_minimal() +
  theme(
    # Center-align title and subtitle
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5),
    # Bold axis labels
    axis.title = element_text(size = 10, face = "bold"),
    # Bold legend elements
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey95"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  ) +
  # Control axis limits and breaks
  scale_x_continuous(breaks = seq(0, nrow(portfolio_data), by = 10)) +
  scale_y_continuous(limits = c(0, max(portfolio_data$P_Value) * 1.1))

# Save the plot
ggsave("heteroskedasticity_analysis2.pdf", 
       plot = het_plot,
       width = 6.5,  
       height = 4.5,
       units = "in")

# Function to create heteroskedasticity plot
create_het_plot <- function(bp_summary_df, title, num_heteroskedastic, output_filename) {
  # Ensure P_Value is numeric
  bp_summary_df$P_Value <- as.numeric(as.character(bp_summary_df$P_Value))
  
  # Create a simplified dataframe with just portfolio numbers and their status
  portfolio_data <- data.frame(
    Portfolio = 1:nrow(bp_summary_df),
    Is_Heteroskedastic = bp_summary_df$P_Value < 0.05,
    P_Value = bp_summary_df$P_Value
  )
  
  # Create the plot
  het_plot <- ggplot(portfolio_data, aes(x = Portfolio, y = P_Value)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_point(aes(color = Is_Heteroskedastic), size = 3) +
    scale_color_manual(values = c("grey50", "red"),
                       labels = c("Homoskedastic", "Heteroskedastic"),
                       name = "Portfolio Type") +
    labs(x = "Portfolio Number",
         y = "P-Value",
         title = title,
         subtitle = paste0(num_heteroskedastic, 
                           " out of ", nrow(portfolio_data),
                           " portfolios exhibit heteroskedasticity")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5),
      axis.title = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 9, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey95"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) +
    scale_x_continuous(breaks = seq(0, nrow(portfolio_data), by = 10)) +
    scale_y_continuous(limits = c(0, max(portfolio_data$P_Value) * 1.1))
  
  # Save the plot
  ggsave(output_filename, 
         plot = het_plot,
         width = 6.5,
         height = 4.5,
         units = "in")
}

# Create plot for MKTDB model
create_het_plot(
  bp_summary_df_MKTDB,
  "Heteroskedasticity Analysis for MKTDB Factor Model",
  15,
  "heteroskedasticity_analysis_MKTDB.pdf"
)

# Ensure P_Value is numeric in the multivariate dataframe
bp_summary_multi_df$P_Value <- as.numeric(as.character(bp_summary_multi_df$P_Value))

# Create plot for three-factor model
create_het_plot(
  bp_summary_multi_df,
  "Heteroskedasticity Analysis for Three-Factor Model",
  38,
  "heteroskedasticity_analysis_threefactor.pdf"
)

--------------------------------------------------------------------------------
  
### ******* Graph 2: Security Market Line *******
  
library(ggplot2)
library(ggtext)

# Prepare data for plotting
plot_data <- data.frame(
  Betas = betas,
  Average_Returns = average_returns
)

# Extract key statistics from cross-sectional regression
gamma_intercept <- coef(cross_sec_model)[1]
gamma_slope <- coef(cross_sec_model)[2]
r_squared <- summary(cross_sec_model)$r.squared

# Create the plot
ggplot(plot_data, aes(x = Betas, y = Average_Returns)) +
  # Security Market Line (SML)
  geom_abline(
    intercept = gamma_intercept, 
    slope = gamma_slope, 
    color = "red", 
    linetype = "solid", 
    linewidth = 1
  ) +
  
  # Scatter plot of portfolios
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  
  # Annotated text box with regression details
  annotate(
    "richtext", 
    x = min(betas), 
    y = max(average_returns), 
    label = paste(
      "**Regression Estimates:**<br>",
      sprintf("γ₀ (Intercept): %.4f<br>", gamma_intercept),
      sprintf("γ₁ (Slope): %.4f<br>", gamma_slope),
      sprintf("R²: %.4f", r_squared)
    ),
    hjust = 0, 
    vjust = 1,
    fill = "white",
    label.color = "gray",
    size = 4, 
    family = "serif"
  ) +
  
  # Labeling and theme
  labs(
    title = "Security Market Line - MKTB Factor",
    x = "Portfolio Betas (β)",
    y = "Average Portfolio Return"  
  ) +
  
  # Adjust plot limits to make it more compact
  coord_cartesian(
    xlim = c(min(betas), max(betas)),  
    ylim = c(min(average_returns), max(average_returns))
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = 14,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 11, face = "bold", vjust = 1.5),  
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# Save the plot as a PDF
ggsave("MKTB_Factor_Cross_Sectional_Analysis.pdf", 
       width = 7.5,   # Adjusted width
       height = 5.5,  # Adjusted height
       dpi = 300)

## Data for MKTDB for plotting

plot_data_mktdb <- data.frame(
Betas = summary_df_MKTDB$Beta,
Average_Returns = average_returns
)

# Extract key statistics from cross-sectional regression for MKTDB
gamma_intercept_mktdb <- coef(cross_sec_model_mktdb)[1]
gamma_slope_mktdb <- coef(cross_sec_model_mktdb)[2]
r_squared_mktdb <- crosssec_summary_mktdb$r.squared

# Create the plot
ggplot(plot_data_mktdb, aes(x = Betas, y = Average_Returns)) +
  # Security Market Line (SML) for MKTDB
  geom_abline(
    intercept = gamma_intercept_mktdb, 
    slope = gamma_slope_mktdb, 
    color = "red", 
    linetype = "solid", 
    linewidth = 1
  ) +
  
  # Scatter plot of portfolios for MKTDB
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  
  # Annotated text box with regression details for MKTDB
  annotate(
    "richtext", 
    x = min(summary_df_MKTDB$Beta), 
    y = max(average_returns),
    label = paste(
      "Regression Estimates:<br>",
      sprintf("γ₀ (Intercept): %.4f<br>", gamma_intercept_mktdb),
      sprintf("γ₁ (Slope): %.4f<br>", gamma_slope_mktdb),
      sprintf("R²: %.4f", r_squared_mktdb)
    ),
    hjust = 0, 
    vjust = 1,
    fill = "white",
    label.color = "gray",
    size = 4, 
    family = "serif"
  ) +
  
  # Labeling and theme
  labs(
    title = "Security Market Line - MKTDB Factor",
    x = "Portfolio Betas (β)",
    y = "Average Portfolio Return"  
  ) +
  
  # Adjust plot limits to make it more compact
  coord_cartesian(
    xlim = c(min(summary_df_MKTDB$Beta), max(summary_df_MKTDB$Beta)),  
    ylim = c(min(average_returns), max(average_returns))
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = 14,
      margin = margin(b = 10)
    ),
    axis.title = element_text(size = 11, face = "bold", vjust = 1.5),
    axis.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# Save the plot as a PDF
ggsave("MKTDB_Factor_Cross_Sectional_Analysis.pdf", 
       width = 7.5,   # Adjusted width
       height = 5.5,  # Adjusted height
       dpi = 300)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------