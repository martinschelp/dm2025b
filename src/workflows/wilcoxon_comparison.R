# Comprehensive Wilcoxon Test Comparison for All Methods
# Data Mining 2025 - Model Comparison Analysis

# Define all method vectors
vBlue <- c(6.435, 6.505, 6.575, 7.506, 7.586, 5.974, 7.336, 6.055, 6.875, 6.845)
vIPC <- c(7.506, 7.316, 6.735, 6.715, 7.055, 6.645, 8.136, 7.836, 6.695, 6.195)
vNinguno <- c(6.885, 7.446, 6.645, 6.365, 6.985, 7.406, 7.356, 6.995, 7.486, 6.605)
vSTD <- c()  # Empty vector - will be skipped
vRankSimple <- c(3.873, 6.225, 6.625, 5.284, 7.045)
vRankCero <- c(7.045, 7.266, 7.125, 7.005, 7.636, 7.255)
vNucleo <- c(6.225, 6.425, 7.065, 7.115, 7.135)

# Create list of all methods (excluding empty STD)
methods <- list(
  "Blue" = vBlue,
  "IPC" = vIPC,
  "Ninguno" = vNinguno,
  "RankSimple" = vRankSimple,
  "RankCero" = vRankCero,
  "Nucleo" = vNucleo
)

# Function to perform Wilcoxon test between two vectors
perform_wilcoxon_test <- function(vec1, vec2, name1, name2) {
  # Check if vectors have same length for paired test
  if (length(vec1) != length(vec2)) {
    # Use unpaired test if lengths differ
    wt <- wilcox.test(vec1, vec2, paired = FALSE)
    test_type <- "unpaired"
  } else {
    # Use paired test if lengths are equal
    wt <- wilcox.test(vec1, vec2, paired = TRUE)
    test_type <- "paired"
  }
  
  pvalue <- wt$p.value
  mean1 <- mean(vec1)
  mean2 <- mean(vec2)
  
  # Determine result
  result <- 0
  interpretation <- "No significant difference"
  
  if (pvalue < 0.05) {
    if (mean1 > mean2) {
      result <- 1
      interpretation <- paste(name1, "significantly better than", name2)
    } else {
      result <- 2
      interpretation <- paste(name2, "significantly better than", name1)
    }
  }
  
  return(list(
    comparison = paste(name1, "vs", name2),
    test_type = test_type,
    n1 = length(vec1),
    n2 = length(vec2),
    mean1 = round(mean1, 3),
    mean2 = round(mean2, 3),
    pvalue = round(pvalue, 4),
    result = result,
    interpretation = interpretation
  ))
}

# Perform all pairwise comparisons
results <- list()
method_names <- names(methods)
comparison_count <- 0

cat("=== WILCOXON TEST RESULTS FOR ALL METHOD PAIRS ===\n\n")

for (i in 1:(length(methods) - 1)) {
  for (j in (i + 1):length(methods)) {
    comparison_count <- comparison_count + 1
    
    name1 <- method_names[i]
    name2 <- method_names[j]
    vec1 <- methods[[i]]
    vec2 <- methods[[j]]
    
    # Perform test
    test_result <- perform_wilcoxon_test(vec1, vec2, name1, name2)
    results[[comparison_count]] <- test_result
    
    # Print individual result
    cat("Comparison", comparison_count, ":", test_result$comparison, "\n")
    cat("  Test type:", test_result$test_type, "\n")
    cat("  Sample sizes: n1 =", test_result$n1, ", n2 =", test_result$n2, "\n")
    cat("  Means:", name1, "=", test_result$mean1, ",", name2, "=", test_result$mean2, "\n")
    cat("  P-value:", test_result$pvalue, "\n")
    cat("  Result:", test_result$interpretation, "\n")
    cat("  ---\n")
  }
}

# Create summary table
cat("\n=== SUMMARY TABLE ===\n")
cat(sprintf("%-20s %-10s %-8s %-8s %-8s %-8s %-10s %s\n", 
            "Comparison", "Test", "n1", "n2", "Mean1", "Mean2", "P-value", "Result"))
cat(paste(rep("-", 90), collapse=""), "\n")

for (result in results) {
  cat(sprintf("%-20s %-10s %-8d %-8d %-8.3f %-8.3f %-10.4f %d\n",
              result$comparison,
              result$test_type,
              result$n1,
              result$n2,
              result$mean1,
              result$mean2,
              result$pvalue,
              result$result))
}

# Summary statistics
cat("\n=== OVERALL SUMMARY ===\n")
cat("Total comparisons performed:", comparison_count, "\n")

significant_results <- sum(sapply(results, function(x) x$result != 0))
cat("Significant differences found:", significant_results, "\n")

paired_tests <- sum(sapply(results, function(x) x$test_type == "paired"))
unpaired_tests <- sum(sapply(results, function(x) x$test_type == "unpaired"))
cat("Paired tests:", paired_tests, "\n")
cat("Unpaired tests:", unpaired_tests, "\n")

# Method performance ranking by mean
method_means <- sapply(methods, mean)
method_ranking <- sort(method_means, decreasing = TRUE)

cat("\n=== METHOD RANKING BY MEAN PERFORMANCE ===\n")
for (i in 1:length(method_ranking)) {
  cat(i, ".", names(method_ranking)[i], ":", round(method_ranking[i], 3), "\n")
}

cat("\nNote: Results with code 0 = no significant difference")
cat("\n      Results with code 1 = first method significantly better")
cat("\n      Results with code 2 = second method significantly better\n")
