# Get the directory of this script
sourceDir <- getSrcDirectory(function(dummy) {dummy})

# Set the working directory to the script's location
setwd(sourceDir)

# Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# List of packages to install
packages <- c("tidyverse", "lfe", "sandwich", "lmtest", "knitr", "stringr", "sf", "dplyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load the packages
invisible(lapply(packages, library, character.only = TRUE))

# Clear console
cat("\014")


# Choose the running variable: "distance" or "distance_1820"
running_variable <- "distance"  # "distance" uses the historical border; "distance_1820" uses the 1820 border throughout

# Load and prepare data
data <- read_csv("database.csv", show_col_types = FALSE)

# Function to prepare data for a specific year and sample size
prepare_data <- function(year, sample_size, dependent_var) {
  data %>%
    filter(!(state %in% c("Kansas Territory", "Nebraska Territory"))) %>%
    mutate(
      log_ruralpopden = log(ruralpopden),
      log_ruralpopden = ifelse(is.infinite(log_ruralpopden) | is.nan(log_ruralpopden), NA, log_ruralpopden),
      log_farmv = log(farmv),
      log_farmv = ifelse(is.infinite(log_farmv) | is.nan(log_farmv), NA, log_farmv)
    ) %>%
    filter(!is.na(slavery_legal),
           !is.na(longitude_miles),
           !is.na(!!sym(running_variable)),
           !is.na(border),
           !is.na(area),
           year == !!year,
           !!sym(running_variable) >= -sample_size & !!sym(running_variable) <= sample_size,
           !is.na(!!sym(dependent_var))) %>%
    mutate(lon_cluster = as.factor(cut(longitude_miles, breaks = 15)))
}

# Function to prepare data for border sample
prepare_data_border <- function(year, dependent_var) {
  data %>%
    filter(!(state %in% c("Kansas Territory", "Nebraska Territory"))) %>%
    mutate(
      log_ruralpopden = log(ruralpopden),
      log_ruralpopden = ifelse(is.infinite(log_ruralpopden) | is.nan(log_ruralpopden), NA, log_ruralpopden),
      log_farmv = log(farmv),
      log_farmv = ifelse(is.infinite(log_farmv) | is.nan(log_farmv), NA, log_farmv)
    ) %>%
    filter(!is.na(slavery_legal),
           !is.na(longitude_miles),
           !is.na(border),
           !is.na(area),
           year == !!year,
           border == 1,
           !is.na(!!sym(dependent_var))) %>%
    mutate(lon_cluster = as.factor(cut(longitude_miles, breaks = 15)))
}

# Function to prepare data for donut sample
prepare_data_donut <- function(year, dependent_var) {
  data %>%
    filter(!(state %in% c("Kansas Territory", "Nebraska Territory"))) %>%
    mutate(
      log_ruralpopden = log(ruralpopden),
      log_ruralpopden = ifelse(is.infinite(log_ruralpopden) | is.nan(log_ruralpopden), NA, log_ruralpopden),
      log_farmv = log(farmv),
      log_farmv = ifelse(is.infinite(log_farmv) | is.nan(log_farmv), NA, log_farmv)
    ) %>%
    filter(!is.na(slavery_legal),
           !is.na(longitude_miles),
           !is.na(!!sym(running_variable)),
           !is.na(border),
           !is.na(area),
           year == !!year,
           abs(!!sym(running_variable)) <= 55,
           border == 0,
           !is.na(!!sym(dependent_var))) %>%
    mutate(lon_cluster = as.factor(cut(longitude_miles, breaks = 15)))
}

# Function to format results with 5-decimal p-values
format_result <- function(coef, se, n, pvalue) {
  significance <- case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01 ~ "**",
    pvalue < 0.05 ~ "*",
    pvalue < 0.1 ~ ".",
    TRUE ~ ""
  )
  
  sprintf("%.3f%s (%.3f) {%.5f} [%d]", coef, significance, se, pvalue, n)
}

# Function to run regression using lfe package and extract results
run_regression_lfe <- function(data, equation = 1, dependent_var) {
  tryCatch({
    if (equation == 1) {
      formula <- as.formula(paste(dependent_var, "~ slavery_legal + longitude_miles + I(longitude_miles^2) + I(longitude_miles^3) | 0 | 0 | lon_cluster"))
    } else if (equation == 2) {
      formula <- as.formula(paste(dependent_var, "~ slavery_legal +", running_variable, "+ longitude_miles + I(", running_variable, "^2) + I(", running_variable, "^3) + I(longitude_miles^2) + I(longitude_miles^3) | 0 | 0 | lon_cluster"))
    } else if (equation == 3) {
      formula <- as.formula(paste(dependent_var, "~ slavery_legal +", running_variable, "+ longitude_miles + slavery_legal:", running_variable, "+ I(", running_variable, "^2) + I(", running_variable, "^3) + I(longitude_miles^2) + I(longitude_miles^3) | 0 | 0 | lon_cluster"))
    }
    
    model <- felm(formula, data = data, weights = data$area)
    
    coef <- summary(model)$coefficients["slavery_legal", "Estimate"]
    se <- summary(model)$coefficients["slavery_legal", "Cluster s.e."]
    pvalue <- summary(model)$coefficients["slavery_legal", "Pr(>|t|)"]
    n <- nobs(model)
    
    format_result(coef, se, n, pvalue)
  }, error = function(e) {
    "Error"
  })
}

# Function to run regression using sandwich and lmtest packages and extract results
run_regression_sandwich <- function(data, equation = 1, dependent_var) {
  tryCatch({
    if (equation == 1) {
      formula <- as.formula(paste(dependent_var, "~ slavery_legal + longitude_miles + I(longitude_miles^2) + I(longitude_miles^3)"))
    } else if (equation == 2) {
      formula <- as.formula(paste(dependent_var, "~ slavery_legal +", running_variable, "+ longitude_miles + I(", running_variable, "^2) + I(", running_variable, "^3) + I(longitude_miles^2) + I(longitude_miles^3)"))
    } else if (equation == 3) {
      formula <- as.formula(paste(dependent_var, "~ slavery_legal +", running_variable, "+ longitude_miles + slavery_legal:", running_variable, "+ I(", running_variable, "^2) + I(", running_variable, "^3) + I(longitude_miles^2) + I(longitude_miles^3)"))
    }
    
    model <- lm(formula, data = data, weights = data$area)
    
    clustered_se <- vcovCL(model, cluster = ~ lon_cluster, type = "HC1")
    model_sandwich <- coeftest(model, vcov = clustered_se)
    
    coef <- model_sandwich["slavery_legal", "Estimate"]
    se <- model_sandwich["slavery_legal", "Std. Error"]
    pvalue <- model_sandwich["slavery_legal", "Pr(>|t|)"]
    n <- nobs(model)
    
    format_result(coef, se, n, pvalue)
  }, error = function(e) {
    "Error"
  })
}

# Function to run full regression and format results
run_full_regression <- function(data, equation, dependent_var) {
  if (equation == 3) {
    formula <- as.formula(paste(dependent_var, "~ slavery_legal +", running_variable, "+ longitude_miles + slavery_legal:", running_variable, "+
      I(", running_variable, "^2) + I(", running_variable, "^3) + I(longitude_miles^2) + I(longitude_miles^3) | 0 | 0 | lon_cluster"))
  } else if (equation == 4) {
    formula <- as.formula(paste(dependent_var, "~ slavery_legal +", running_variable, "+ longitude_miles + slavery_legal:", running_variable, "+
      slavery_legal:longitude_miles + I(", running_variable, "^2) + I(", running_variable, "^3) + I(longitude_miles^2) + I(longitude_miles^3) | 0 | 0 | lon_cluster"))
  }
  
  model <- felm(formula, data = data, weights = data$area)
  
  coef_table <- summary(model)$coefficients
  coef_table <- coef_table[order(match(rownames(coef_table), c("slavery_legal", running_variable, "longitude_miles",
                                                               paste0("slavery_legal:", running_variable), "slavery_legal:longitude_miles",
                                                               paste0("I(", running_variable, "^2)"), paste0("I(", running_variable, "^3)"),
                                                               "I(longitude_miles^2)", "I(longitude_miles^3)"))), ]
  
  adjustments <- setNames(
    c(100, 100, 100, 100, 10000, 10000, 1000000, 1000000),
    c(
      running_variable, "longitude_miles",
      paste0("slavery_legal:", running_variable), "slavery_legal:longitude_miles",
      paste0("I(", running_variable, "^2)"), "I(longitude_miles^2)",
      paste0("I(", running_variable, "^3)"), "I(longitude_miles^3)"
    )
  )
  
  for (var in names(adjustments)) {
    if (var %in% rownames(coef_table)) {
      coef_table[var, "Estimate"] <- coef_table[var, "Estimate"] * adjustments[var]
      coef_table[var, "Cluster s.e."] <- coef_table[var, "Cluster s.e."] * adjustments[var]
    }
  }
  
  formatted_table <- data.frame(
    Coefficient = rownames(coef_table),
    Estimate = sprintf("%.4f", coef_table[, "Estimate"]),
    `Std. Error` = sprintf("%.4f", coef_table[, "Cluster s.e."]),
    `t value` = sprintf("%.4f", coef_table[, "t value"]),
    `Pr(>|t|)` = sprintf("%.5f", coef_table[, "Pr(>|t|)"]),
    Significance = sapply(coef_table[, "Pr(>|t|)"], function(p) {
      if (p < 0.001) "***"
      else if (p < 0.01) "**"
      else if (p < 0.05) "*"
      else if (p < 0.1) "."
      else ""
    })
  )
  
  n <- nobs(model)
  p <- length(coef(model)) - 1
  r_squared <- summary(model)$r.squared
  adjusted_r_squared <- 1 - (1 - r_squared) * (n - 1) / (n - p - 1)
  
  list(
    table = formatted_table,
    observations = n,
    adjusted_r_squared = adjusted_r_squared
  )
}

# Define years and sample sizes
years <- seq(1790, 1860, by = 10)
sample_sizes <- c(300, 450, 600, 900)

# Create results tables for log_ruralpopden
results_lfe_12 <- matrix(nrow = length(years), ncol = length(sample_sizes) + 3)
results_sandwich_12 <- matrix(nrow = length(years), ncol = length(sample_sizes) + 3)
results_lfe_3 <- matrix(nrow = length(years), ncol = length(sample_sizes) + 1)
results_sandwich_3 <- matrix(nrow = length(years), ncol = length(sample_sizes) + 1)

colnames(results_lfe_12) <- colnames(results_sandwich_12) <- c("Year", "Border", "Donut", paste0(sample_sizes, "-mile"))
colnames(results_lfe_3) <- colnames(results_sandwich_3) <- c("Year", paste0(sample_sizes, "-mile"))

# Run regressions for each year and sample size for log_ruralpopden
for (i in seq_along(years)) {
  year <- years[i]
  results_lfe_12[i, 1] <- results_sandwich_12[i, 1] <- results_lfe_3[i, 1] <- results_sandwich_3[i, 1] <- year
  
  border_data <- prepare_data_border(year, "log_ruralpopden")
  results_lfe_12[i, 2] <- run_regression_lfe(border_data, equation = 1, "log_ruralpopden")
  results_sandwich_12[i, 2] <- run_regression_sandwich(border_data, equation = 1, "log_ruralpopden")
  
  donut_data <- prepare_data_donut(year, "log_ruralpopden")
  results_lfe_12[i, 3] <- run_regression_lfe(donut_data, equation = 1, "log_ruralpopden")
  results_sandwich_12[i, 3] <- run_regression_sandwich(donut_data, equation = 1, "log_ruralpopden")
  
  for (j in seq_along(sample_sizes)) {
    sample_size <- sample_sizes[j]
    prepared_data <- prepare_data(year, sample_size, "log_ruralpopden")
    
    results_lfe_12[i, j + 3] <- run_regression_lfe(prepared_data, equation = 2, "log_ruralpopden")
    results_sandwich_12[i, j + 3] <- run_regression_sandwich(prepared_data, equation = 2, "log_ruralpopden")
    
    results_lfe_3[i, j + 1] <- run_regression_lfe(prepared_data, equation = 3, "log_ruralpopden")
    results_sandwich_3[i, j + 1] <- run_regression_sandwich(prepared_data, equation = 3, "log_ruralpopden")
  }
}

# Create results tables for log_farmv (Tables 5-8)
# Create results tables for log_farmv (Tables 5-8)
years_farmv <- c(1850, 1860)
results_lfe_12_farmv <- matrix(nrow = length(years_farmv), ncol = length(sample_sizes) + 3)
results_sandwich_12_farmv <- matrix(nrow = length(years_farmv), ncol = length(sample_sizes) + 3)
results_lfe_3_farmv <- matrix(nrow = length(years_farmv), ncol = length(sample_sizes) + 1)
results_sandwich_3_farmv <- matrix(nrow = length(years_farmv), ncol = length(sample_sizes) + 1)

colnames(results_lfe_12_farmv) <- colnames(results_sandwich_12_farmv) <- c("Year", "Border", "Donut", paste0(sample_sizes, "-mile"))
colnames(results_lfe_3_farmv) <- colnames(results_sandwich_3_farmv) <- c("Year", paste0(sample_sizes, "-mile"))

# Run regressions for 1850 and 1860 with log_farmv as dependent variable
for (i in seq_along(years_farmv)) {
  year <- years_farmv[i]
  results_lfe_12_farmv[i, 1] <- results_sandwich_12_farmv[i, 1] <- results_lfe_3_farmv[i, 1] <- results_sandwich_3_farmv[i, 1] <- year
  
  border_data <- prepare_data_border(year, "log_farmv")
  results_lfe_12_farmv[i, 2] <- run_regression_lfe(border_data, equation = 1, "log_farmv")
  results_sandwich_12_farmv[i, 2] <- run_regression_sandwich(border_data, equation = 1, "log_farmv")
  
  donut_data <- prepare_data_donut(year, "log_farmv")
  results_lfe_12_farmv[i, 3] <- run_regression_lfe(donut_data, equation = 1, "log_farmv")
  results_sandwich_12_farmv[i, 3] <- run_regression_sandwich(donut_data, equation = 1, "log_farmv")
  
  for (j in seq_along(sample_sizes)) {
    sample_size <- sample_sizes[j]
    prepared_data <- prepare_data(year, sample_size, "log_farmv")
    
    results_lfe_12_farmv[i, j + 3] <- run_regression_lfe(prepared_data, equation = 2, "log_farmv")
    results_sandwich_12_farmv[i, j + 3] <- run_regression_sandwich(prepared_data, equation = 2, "log_farmv")
    
    results_lfe_3_farmv[i, j + 1] <- run_regression_lfe(prepared_data, equation = 3, "log_farmv")
    results_sandwich_3_farmv[i, j + 1] <- run_regression_sandwich(prepared_data, equation = 3, "log_farmv")
  }
}

# Function to print formatted table
print_formatted_table <- function(results, title) {
  cat("\n", title, "\n\n")
  
  cat(sprintf("%-6s", "Year"))
  for (col in colnames(results)[-1]) {
    cat(sprintf("%-37s", col))
  }
  cat("\n")
  
  cat(paste(rep("-", 6 + 37 * (ncol(results) - 1)), collapse = ""), "\n")
  
  for (i in 1:nrow(results)) {
    cat(sprintf("%-6s", results[i, 1]))
    for (j in 2:ncol(results)) {
      cat(sprintf("%-37s", results[i, j]))
    }
    cat("\n")
  }
  cat("\n")
}

# Function to print full regression table
print_full_regression_table <- function(results, title) {
  cat("\n", title, "\n\n")
  print(kable(results$table, format = "pipe", align = c("l", "r", "r", "r", "r", "l")))
  cat("\nObservations:", results$observations, "\n")
  cat("Adjusted R-squared:", sprintf("%.4f", results$adjusted_r_squared), "\n")
  cat("\nSignificance levels: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1\n\n")
  cat("Note: Coefficients and standard errors for distance and longitude variables are presented per 100 miles. ",
      "Linear terms are multiplied by 100, quadratic terms by 10,000, and cubic terms by 1,000,000 to maintain correct interpretation.\n")
}

cat("\nPart I: The Replication of Bleakley and Rhode\n")

# Print results tables for log_ruralpopden
print_formatted_table(results_lfe_12, paste("Table 1: Equations 1 and 2, using lfe package with explicit cluster variable (log_ruralpopden) -", running_variable))
print_formatted_table(results_sandwich_12, paste("Table 2: Equations 1 and 2, using sandwich and lmtest packages (log_ruralpopden) -", running_variable))
print_formatted_table(results_lfe_3, paste("Table 3: Equation 3, using lfe package with explicit cluster variable (log_ruralpopden) -", running_variable))
print_formatted_table(results_sandwich_3, paste("Table 4: Equation 3, using sandwich and lmtest packages (log_ruralpopden) -", running_variable))

# Print results tables for log_farmv
print_formatted_table(results_lfe_12_farmv, paste("Table 5: Equations 1 and 2, using lfe package with explicit cluster variable (log_farmv) -", running_variable))
print_formatted_table(results_sandwich_12_farmv, paste("Table 6: Equations 1 and 2, using sandwich and lmtest packages (log_farmv) -", running_variable))
print_formatted_table(results_lfe_3_farmv, paste("Table 7: Equation 3, using lfe package with explicit cluster variable (log_farmv) -", running_variable))
print_formatted_table(results_sandwich_3_farmv, paste("Table 8: Equation 3, using sandwich and lmtest packages (log_farmv) -", running_variable))

cat("Note: Each cell contains: coefficient (standard error) {p-value} [number of observations]\n")
cat("Significance levels: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1\n")

cat("\nPart II: The Addition of the Interaction Term for Slavery’s Legality Multiplied by Longitude\n")
      
# Create full regression tables for log_ruralpopden (Tables 9 and 10)
data_1860_300 <- prepare_data(1860, 300, "log_ruralpopden")

table_9_results <- run_full_regression(data_1860_300, equation = 3, "log_ruralpopden")
print_full_regression_table(table_9_results, paste("Table 9: Full Regression Table - Equation 3, log_ruralpopden (300-mile 1860 sample) -", running_variable))

table_10_results <- run_full_regression(data_1860_300, equation = 4, "log_ruralpopden")
print_full_regression_table(table_10_results, paste("Table 10: Full Regression Table - Equation 4, log_ruralpopden (300-mile 1860 sample) -", running_variable))

# Create full regression tables for log_farmv (Tables 11 and 12)
data_1860_300_farmv <- prepare_data(1860, 300, "log_farmv")

table_11_results <- run_full_regression(data_1860_300_farmv, equation = 3, "log_farmv")
print_full_regression_table(table_11_results, paste("Table 11: Full Regression Table - Equation 3, log_farmv (300-mile 1860 sample) -", running_variable))

table_12_results <- run_full_regression(data_1860_300_farmv, equation = 4, "log_farmv")
print_full_regression_table(table_12_results, paste("Table 12: Full Regression Table - Equation 4, log_farmv (300-mile 1860 sample) -", running_variable))

# Function to run regression for Table 13 and 14
run_regression_table_13_14 <- function(year, sample_size, dependent_var) {
  data <- prepare_data(year, sample_size, dependent_var)
  
  formula <- as.formula(paste(dependent_var, "~ slavery_legal +", running_variable, "+ longitude_miles + slavery_legal:", running_variable, "+
    slavery_legal:longitude_miles + I(", running_variable, "^2) + I(", running_variable, "^3) + I(longitude_miles^2) + I(longitude_miles^3) | 0 | 0 | lon_cluster"))
  
  model <- felm(formula, data = data, weights = data$area)
  
  coef_table <- summary(model)$coefficients
  
  slavery_legal_coef <- coef_table["slavery_legal", "Estimate"]
  slavery_legal_se <- coef_table["slavery_legal", "Cluster s.e."]
  slavery_legal_p <- coef_table["slavery_legal", "Pr(>|t|)"]
  
  interaction_coef <- coef_table["slavery_legal:longitude_miles", "Estimate"] * 100
  interaction_se <- coef_table["slavery_legal:longitude_miles", "Cluster s.e."] * 100
  interaction_p <- coef_table["slavery_legal:longitude_miles", "Pr(>|t|)"]
  
  inflection_point <- -slavery_legal_coef / (interaction_coef / 100)
  
  n <- nobs(model)
  
  c(slavery_legal_coef, slavery_legal_se, slavery_legal_p,
    interaction_coef, interaction_se, interaction_p, inflection_point, n)
}

# Create Table 13 for log_ruralpopden
years_table_13 <- seq(1790, 1860, by = 10)
results_table_13 <- matrix(nrow = length(years_table_13), ncol = 9)
colnames(results_table_13) <- c("Year", "Slavery Legal Coef", "Slavery Legal SE", "Slavery Legal p-value",
                                "Interaction Coef", "Interaction SE", "Interaction p-value", "Inflection Point", "Observations")

for (i in seq_along(years_table_13)) {
  year <- years_table_13[i]
  tryCatch({
    results <- run_regression_table_13_14(year, 300, "log_ruralpopden")
    results_table_13[i, ] <- c(year, results)
  }, error = function(e) {
    cat("Error for year", year, ":", conditionMessage(e), "\n")
    results_table_13[i, ] <- c(year, rep(NA, 8))
  })
}

# Create Table 14 for log_farmv
years_table_14 <- c(1850, 1860)
results_table_14 <- matrix(nrow = length(years_table_14), ncol = 9)
colnames(results_table_14) <- c("Year", "Slavery Legal Coef", "Slavery Legal SE", "Slavery Legal p-value",
                                "Interaction Coef", "Interaction SE", "Interaction p-value", "Inflection Point", "Observations")

for (i in seq_along(years_table_14)) {
  year <- years_table_14[i]
  tryCatch({
    results <- run_regression_table_13_14(year, 300, "log_farmv")
    results_table_14[i, ] <- c(year, results)
  }, error = function(e) {
    cat("Error for year", year, ":", conditionMessage(e), "\n")
    results_table_14[i, ] <- c(year, rep(NA, 8))
  })
}

# Function to print formatted tables 13 and 14
print_formatted_table_13_14 <- function(results, title) {
  cat("\n", title, "\n\n")
  
  cat(sprintf("%-6s %-20s %-20s %-15s %-20s %-20s %-15s %-20s %-12s\n",
              "Year", "Slavery Legal", "SE", "p-value", "Interaction", "SE", "p-value", "Inflection Point", "Observations"))
  
  cat(paste(rep("-", 150), collapse = ""), "\n")
  
  for (i in 1:nrow(results)) {
    cat(sprintf("%-6d %-20.10f %-20.10f %-15.5f %-20.10f %-20.10f %-15.5f %-20.10f %-12d\n",
                results[i, 1], results[i, 2], results[i, 3], results[i, 4],
                results[i, 5], results[i, 6], results[i, 7], results[i, 8], results[i, 9]))
  }
  cat("\n")
  cat("Note: Interaction coefficients and standard errors are presented per 100 miles.\n")
  cat("Inflection Point is the longitude_miles value where slavery's effect flips.\n")
  cat("Significance levels: *** p<0.001, ** p<0.01, * p<0.05, . p<0.1\n")
}

cat("\nPart III: The Inflection Points for Slavery’s Legality Multiplied by Longitude\n")
    
# Print Table 13 and Table 14
print_formatted_table_13_14(results_table_13, paste("Table 13: Equation 4, log_ruralpopden (300-mile sample) -", running_variable))
print_formatted_table_13_14(results_table_14, paste("Table 14: Equation 4, log_farmv (300-mile sample) -", running_variable))

cat("\nPart IV: Creation of GeoPackages\n")

# Load border shapefile
border <- st_read("Data/Border/1820_border/1820_border.shp")

# Print CRS information
print(paste("Border CRS:", st_crs(border)$input))

# Function to find intersection with additional checks
find_intersection <- function(inflection_point, border, year) {
  tryCatch({
    vertical_line <- st_linestring(matrix(c(inflection_point, -1e6, inflection_point, 1e6), ncol=2, byrow=TRUE))
    vertical_line <- st_sfc(vertical_line, crs = st_crs(border))
    
    intersection <- st_intersection(border, vertical_line)
    
    if (length(intersection) > 0) {
      coords <- st_coordinates(intersection)
      cat(paste("Found intersection for year", year, ":", coords[1, c("X", "Y")], "\n"))
      return(coords[1, c("X", "Y")])
    } else {
      warning(paste("No intersection found for year", year, "at inflection point", inflection_point))
      return(c(NA, NA))
    }
  }, error = function(e) {
    warning(paste("Error in find_intersection for year", year, ":", e$message))
    return(c(NA, NA))
  })
}

# Function to process results with additional checks
process_results <- function(results, border) {
  df <- data.frame(
    Year = integer(),
    Inflection_Miles = numeric(),
    Inflection_Meters = numeric(),
    Intersection_X = numeric(),
    Intersection_Y = numeric(),
    Observations = integer(),
    coef = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:nrow(results)) {
    year <- results[i, 1]
    inflection_miles <- results[i, 8]
    inflection_meters <- inflection_miles * 1609.344
    
    cat(paste("Processing year", year, "with inflection point", inflection_miles, "miles (", inflection_meters, "meters)\n"))
    
    border_range <- st_bbox(border)
    if (inflection_meters < border_range["xmin"] || inflection_meters > border_range["xmax"]) {
      warning(paste("Inflection point for year", year, "is outside the range of the border shapefile"))
      intersection <- c(NA, NA)
    } else {
      intersection <- find_intersection(inflection_meters, border, year)
    }
    
    df <- rbind(df, data.frame(
      Year = year,
      Inflection_Miles = inflection_miles,
      Inflection_Meters = inflection_meters,
      Intersection_X = intersection[1],
      Intersection_Y = intersection[2],
      Observations = results[i, 9],
      coef = results[i, 5],
      p_value = results[i, 7]
    ))
  }
  
  return(df)
}

# Process results for log_ruralpopden (Table 13)
ruralpopden_df <- process_results(results_table_13, border)

# Process results for log_farmv (Table 14)
farmv_df <- process_results(results_table_14, border)

# Function to remove rows with NA coordinates and print info
clean_and_report <- function(df, name) {
  na_rows <- sum(is.na(df$Intersection_X) | is.na(df$Intersection_Y))
  if (na_rows > 0) {
    cat(paste0("Removed ", na_rows, " row(s) with NA coordinates from ", name, " dataset.\n"))
    cat("Years with NA coordinates:", paste(df$Year[is.na(df$Intersection_X) | is.na(df$Intersection_Y)], collapse = ", "), "\n")
    df <- df[!is.na(df$Intersection_X) & !is.na(df$Intersection_Y), ]
  }
  return(df)
}

# Clean datasets and report
ruralpopden_df <- clean_and_report(ruralpopden_df, "ruralpopden")
farmv_df <- clean_and_report(farmv_df, "farmv")

# Create directory for saving results if it doesn't exist
inflection_dir <- paste0("Results/Inflection_points_", running_variable)
dir.create(inflection_dir, showWarnings = FALSE, recursive = TRUE)

# Save results as GeoPackage using sf
st_write(st_as_sf(ruralpopden_df, coords = c("Intersection_X", "Intersection_Y"), crs = st_crs(border)),
         file.path(inflection_dir, paste0("ruralpopden_inflection_points_", running_variable, ".gpkg")),
         driver = "GPKG",
         append = FALSE)

st_write(st_as_sf(farmv_df, coords = c("Intersection_X", "Intersection_Y"), crs = st_crs(border)),
         file.path(inflection_dir, paste0("farmv_inflection_points_", running_variable, ".gpkg")),
         driver = "GPKG",
         append = FALSE)

print(paste("GeoPackage files have been created successfully in the", inflection_dir, "directory."))

# Print summary of results
cat("\nSummary of results:\n")
cat("ruralpopden_df:\n")
print(summary(ruralpopden_df))
cat("\nfarmv_df:\n")
print(summary(farmv_df))

# Print the running variable used
cat("\nRunning variable used:", running_variable, "\n")
