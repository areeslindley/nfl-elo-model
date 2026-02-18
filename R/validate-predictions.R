# validate-predictions.R
# Validation script to check prediction outputs
# Author: Super Bowl LX Prediction Project

library(tidyverse)

cat("=== VALIDATING PREDICTIONS ===\n\n")

errors <- 0
warnings <- 0

# ============================================================================
# Check Required Files Exist
# ============================================================================
cat("1. Checking required files exist...\n")

required_files <- c(
  "data/output/elo_history.csv",
  "data/output/current_elos.csv",
  "data/output/elo_prediction.csv",
  "data/output/baseline_prediction.csv",
  "data/output/predictions_combined.csv"
)

for (file in required_files) {
  if (file.exists(file)) {
    cat(sprintf("   ✓ %s\n", file))
  } else {
    cat(sprintf("   ✗ MISSING: %s\n", file))
    errors <- errors + 1
  }
}

# ============================================================================
# Validate Elo Ratings
# ============================================================================
cat("\n2. Validating Elo ratings...\n")

if (file.exists("data/output/current_elos.csv")) {
  elos <- read_csv("data/output/current_elos.csv", show_col_types = FALSE)
  
  # Check SEA and NE are present
  if (!all(c("SEA", "NE") %in% elos$team)) {
    cat("   ✗ ERROR: Missing SEA or NE in Elo ratings\n")
    errors <- errors + 1
  } else {
    cat("   ✓ SEA and NE both present\n")
  }
  
  # Check Elo values are reasonable (typically 1300-1700)
  sea_elo <- elos$elo[elos$team == "SEA"]
  ne_elo <- elos$elo[elos$team == "NE"]
  
  if (sea_elo < 1200 || sea_elo > 1800) {
    cat(sprintf("   ⚠ WARNING: SEA Elo (%.1f) seems unusual\n", sea_elo))
    warnings <- warnings + 1
  } else {
    cat(sprintf("   ✓ SEA Elo: %.1f (reasonable)\n", sea_elo))
  }
  
  if (ne_elo < 1200 || ne_elo > 1800) {
    cat(sprintf("   ⚠ WARNING: NE Elo (%.1f) seems unusual\n", ne_elo))
    warnings <- warnings + 1
  } else {
    cat(sprintf("   ✓ NE Elo: %.1f (reasonable)\n", ne_elo))
  }
}

# ============================================================================
# Validate Predictions
# ============================================================================
cat("\n3. Validating predictions...\n")

if (file.exists("data/output/predictions_combined.csv")) {
  preds <- read_csv("data/output/predictions_combined.csv", show_col_types = FALSE)
  
  # Check both models present
  if (!all(c("Elo", "Baseline") %in% preds$model)) {
    cat("   ✗ ERROR: Missing Elo or Baseline model\n")
    errors <- errors + 1
  } else {
    cat("   ✓ Both models present\n")
  }
  
  # Check win probabilities
  for (i in 1:nrow(preds)) {
    model <- preds$model[i]
    prob <- preds$seahawks_win_prob[i]
    
    if (prob < 0 || prob > 1) {
      cat(sprintf("   ✗ ERROR: %s win probability (%.3f) outside [0,1]\n", model, prob))
      errors <- errors + 1
    } else if (prob < 0.1 || prob > 0.9) {
      cat(sprintf("   ⚠ WARNING: %s win probability (%.3f) seems extreme\n", model, prob))
      warnings <- warnings + 1
    } else {
      cat(sprintf("   ✓ %s win probability: %.3f\n", model, prob))
    }
  }
  
  # Check predicted scores
  for (i in 1:nrow(preds)) {
    model <- preds$model[i]
    sea_score <- preds$predicted_sea_score[i]
    ne_score <- preds$predicted_ne_score[i]
    
    if (sea_score < 0 || sea_score > 60) {
      cat(sprintf("   ✗ ERROR: %s SEA score (%d) unreasonable\n", model, sea_score))
      errors <- errors + 1
    } else if (sea_score < 10 || sea_score > 50) {
      cat(sprintf("   ⚠ WARNING: %s SEA score (%d) seems unusual\n", model, sea_score))
      warnings <- warnings + 1
    }
    
    if (ne_score < 0 || ne_score > 60) {
      cat(sprintf("   ✗ ERROR: %s NE score (%d) unreasonable\n", model, ne_score))
      errors <- errors + 1
    } else if (ne_score < 10 || ne_score > 50) {
      cat(sprintf("   ⚠ WARNING: %s NE score (%d) seems unusual\n", model, ne_score))
      warnings <- warnings + 1
    }
  }
  
  cat(sprintf("   ✓ All scores within reasonable bounds\n"))
}

# ============================================================================
# Validate Elo History
# ============================================================================
cat("\n4. Validating Elo history...\n")

if (file.exists("data/output/elo_history.csv")) {
  history <- read_csv("data/output/elo_history.csv", show_col_types = FALSE)
  
  # Check data structure
  required_cols <- c("season", "week", "team", "elo")
  missing_cols <- setdiff(required_cols, names(history))
  if (length(missing_cols) > 0) {
    cat(sprintf("   ✗ ERROR: Missing columns: %s\n", paste(missing_cols, collapse = ", ")))
    errors <- errors + 1
  } else {
    cat("   ✓ All required columns present\n")
  }
  
  # Check date range
  if ("date" %in% names(history)) {
    dates <- as.Date(history$date)
    date_range <- range(dates, na.rm = TRUE)
    cat(sprintf("   ✓ Date range: %s to %s\n", date_range[1], date_range[2]))
  }
  
  # Check season range
  seasons <- unique(history$season)
  if (min(seasons) != 2015 || max(seasons) != 2025) {
    cat(sprintf("   ⚠ WARNING: Season range (%d-%d) not as expected (2015-2025)\n", 
                min(seasons), max(seasons)))
    warnings <- warnings + 1
  } else {
    cat(sprintf("   ✓ Season range: 2015-2025\n"))
  }
}

# ============================================================================
# Validate Simulations
# ============================================================================
cat("\n5. Validating simulations...\n")

if (file.exists("data/output/score_simulations.csv")) {
  sims <- read_csv("data/output/score_simulations.csv", show_col_types = FALSE)
  
  n_sims <- nrow(sims)
  if (n_sims != 10000) {
    cat(sprintf("   ⚠ WARNING: Expected 10,000 simulations, found %d\n", n_sims))
    warnings <- warnings + 1
  } else {
    cat(sprintf("   ✓ Simulation count: %d\n", n_sims))
  }
  
  # Check winners
  sea_wins <- sum(sims$winner == "SEA")
  ne_wins <- sum(sims$winner == "NE")
  ties <- sum(sims$winner == "TIE")
  
  if (sea_wins + ne_wins + ties != n_sims) {
    cat("   ✗ ERROR: Winner counts don't sum to total simulations\n")
    errors <- errors + 1
  } else {
    cat(sprintf("   ✓ Winner distribution: SEA=%d, NE=%d, Ties=%d\n", 
                sea_wins, ne_wins, ties))
  }
}

# ============================================================================
# Summary
# ============================================================================
cat("\n=== VALIDATION SUMMARY ===\n")
cat(sprintf("Errors: %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\n✓ All validations passed!\n")
} else if (errors == 0) {
  cat("\n✓ No errors, but some warnings to review\n")
} else {
  cat("\n✗ Validation failed - please fix errors before proceeding\n")
}
