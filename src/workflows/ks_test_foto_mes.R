# Kolmogorov-Smirnov Test for Data Drift Detection
# Comparing each foto_mes distribution against 202109 as reference

# Load required libraries
require("data.table")

# Parameters - adjust as needed
PARAM <- list()
PARAM$dataset <- ""  # Will be set based on your dataset
PARAM$reference_month <- 202109  # Reference month for comparison
PARAM$alpha <- 0.05  # Significance level

# Read dataset
cat("Loading dataset...\n")
dataset <- fread("/Users/martinschelp/dm2025b/datasets/datasets_gerencial_competencia_2025.csv.gz")

cat("Dataset loaded. Dimensions:", nrow(dataset), "x", ncol(dataset), "\n")

# Check available foto_mes values
foto_mes_values <- sort(unique(dataset$foto_mes))
cat("Available foto_mes values:", paste(foto_mes_values, collapse = ", "), "\n")

# Verify reference month exists
if (!PARAM$reference_month %in% foto_mes_values) {
  stop("Reference month ", PARAM$reference_month, " not found in dataset!")
}

# Get reference data (202109)
reference_data <- dataset[foto_mes == PARAM$reference_month]
cat("Reference month", PARAM$reference_month, "has", nrow(reference_data), "observations\n")

# Get numeric columns for KS testing (exclude identifiers and categorical variables)
exclude_cols <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]
numeric_cols <- setdiff(numeric_cols, exclude_cols)

cat("Testing", length(numeric_cols), "numeric variables\n")
cat("Variables to test:", paste(head(numeric_cols, 10), collapse = ", "), 
    if(length(numeric_cols) > 10) "..." else "", "\n")

# Function to perform KS test for a single variable between two foto_mes
perform_ks_test <- function(var_name, month1_data, month2_data, month1_name, month2_name) {
  # Remove NA values
  var1 <- month1_data[[var_name]]
  var2 <- month2_data[[var_name]]
  
  var1 <- var1[!is.na(var1)]
  var2 <- var2[!is.na(var2)]
  
  if (length(var1) == 0 || length(var2) == 0) {
    return(list(
      variable = var_name,
      comparison = paste(month1_name, "vs", month2_name),
      n1 = length(var1),
      n2 = length(var2),
      ks_statistic = NA,
      p_value = NA,
      significant = NA,
      interpretation = "Insufficient data"
    ))
  }
  
  # Perform KS test
  ks_result <- ks.test(var1, var2)
  
  return(list(
    variable = var_name,
    comparison = paste(month1_name, "vs", month2_name),
    n1 = length(var1),
    n2 = length(var2),
    ks_statistic = round(ks_result$statistic, 4),
    p_value = round(ks_result$p.value, 6),
    significant = ks_result$p.value < PARAM$alpha,
    interpretation = ifelse(ks_result$p.value < PARAM$alpha, 
                           "Significant difference (data drift detected)", 
                           "No significant difference")
  ))
}

# Perform KS tests for each foto_mes against reference
all_results <- list()
result_count <- 0

cat("\n=== PERFORMING KOLMOGOROV-SMIRNOV TESTS ===\n")

# Test each foto_mes (except reference) against reference month
test_months <- setdiff(foto_mes_values, PARAM$reference_month)

for (test_month in test_months) {
  cat("\nTesting", test_month, "vs", PARAM$reference_month, "\n")
  
  test_data <- dataset[foto_mes == test_month]
  cat("  Sample size:", nrow(test_data), "observations\n")
  
  # Test each numeric variable
  month_results <- list()
  significant_vars <- 0
  
  for (var_name in numeric_cols) {
    result_count <- result_count + 1
    
    ks_result <- perform_ks_test(var_name, test_data, reference_data, 
                                test_month, PARAM$reference_month)
    
    month_results[[var_name]] <- ks_result
    all_results[[result_count]] <- ks_result
    
    if (!is.na(ks_result$significant) && ks_result$significant) {
      significant_vars <- significant_vars + 1
    }
  }
  
  cat("  Variables with significant drift:", significant_vars, "out of", length(numeric_cols), "\n")
}

# Convert results to data.table for analysis
results_dt <- rbindlist(all_results)

# Summary by foto_mes
cat("\n=== SUMMARY BY FOTO_MES ===\n")
summary_by_month <- results_dt[!is.na(significant), .(
  total_variables = .N,
  significant_drift = sum(significant),
  pct_drift = round(100 * sum(significant) / .N, 1),
  min_p_value = min(p_value, na.rm = TRUE),
  max_ks_stat = max(ks_statistic, na.rm = TRUE)
), by = .(foto_mes = sub(" vs.*", "", comparison))]

print(summary_by_month)

# Variables with most drift across months
cat("\n=== VARIABLES WITH MOST FREQUENT DRIFT ===\n")
drift_by_variable <- results_dt[!is.na(significant), .(
  months_tested = .N,
  months_with_drift = sum(significant),
  pct_months_drift = round(100 * sum(significant) / .N, 1),
  avg_ks_statistic = round(mean(ks_statistic, na.rm = TRUE), 4),
  min_p_value = min(p_value, na.rm = TRUE)
), by = variable][order(-pct_months_drift, -avg_ks_statistic)]

print(head(drift_by_variable, 20))

# Most significant cases (lowest p-values)
cat("\n=== MOST SIGNIFICANT DRIFT CASES ===\n")
most_significant <- results_dt[significant == TRUE][order(p_value)][1:20]
print(most_significant[, .(variable, comparison, ks_statistic, p_value)])

# Export results
output_file <- "ks_test_results.csv"
fwrite(results_dt, output_file)
cat("\nDetailed results exported to:", output_file, "\n")

# Summary statistics
total_tests <- nrow(results_dt[!is.na(significant)])
significant_tests <- nrow(results_dt[significant == TRUE])

cat("\n=== OVERALL SUMMARY ===\n")
cat("Total KS tests performed:", total_tests, "\n")
cat("Tests showing significant drift:", significant_tests, "\n")
cat("Percentage of tests with drift:", round(100 * significant_tests / total_tests, 1), "%\n")
cat("Reference month:", PARAM$reference_month, "\n")
cat("Significance level:", PARAM$alpha, "\n")

cat("\nInterpretation:")
cat("\n- KS test detects differences in distributions between months")
cat("\n- Significant results (p <", PARAM$alpha, ") indicate data drift")
cat("\n- Higher KS statistics indicate larger distributional differences\n")
