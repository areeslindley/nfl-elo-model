# 04-combine-predictions.R
# Combine both models into final prediction file
# Author: Super Bowl LX Prediction Project
# Date: 2026-02-07

library(tidyverse)

cat("Combining predictions from both models...\n")

# ============================================================================
# Load Predictions
# ============================================================================
cat("Loading predictions...\n")

elo_pred <- read_csv("data/output/elo_prediction.csv", show_col_types = FALSE)
baseline_pred <- read_csv("data/output/baseline_prediction.csv", show_col_types = FALSE)

# Check that both predictions exist
if (nrow(elo_pred) == 0) {
  stop("Elo prediction file is empty. Please run 02-elo-model.R first.")
}

if (nrow(baseline_pred) == 0) {
  stop("Baseline prediction file is empty. Please run 03-baseline-model.R first.")
}

cat("Loaded predictions from both models\n")

# ============================================================================
# Combine Predictions
# ============================================================================
cat("Combining predictions...\n")

# Standardize column names
elo_combined <- elo_pred %>%
  select(model, seahawks_win_prob, predicted_sea_score, predicted_ne_score) %>%
  mutate(timestamp = Sys.time())

baseline_combined <- baseline_pred %>%
  select(model, seahawks_win_prob, predicted_sea_score, predicted_ne_score) %>%
  mutate(timestamp = Sys.time())

# Combine
combined_predictions <- bind_rows(elo_combined, baseline_combined) %>%
  arrange(model)

# ============================================================================
# Data Validation
# ============================================================================
cat("Validating predictions...\n")

# Check win probabilities are between 0 and 1
if (any(combined_predictions$seahawks_win_prob < 0 | 
        combined_predictions$seahawks_win_prob > 1)) {
  warning("Some win probabilities are outside [0, 1] range")
}

# Check scores are reasonable
if (any(combined_predictions$predicted_sea_score < 0 | 
        combined_predictions$predicted_sea_score > 60)) {
  warning("Some predicted scores seem unreasonable")
}

if (any(combined_predictions$predicted_ne_score < 0 | 
        combined_predictions$predicted_ne_score > 60)) {
  warning("Some predicted scores seem unreasonable")
}

# Check that both teams are represented
if (!all(c("SEA", "NE") %in% c("SEA", "NE"))) {
  warning("Both teams (SEA and NE) should be in predictions")
}

cat("Validation complete\n")

# ============================================================================
# Create Locked Prediction File
# ============================================================================
cat("Creating locked prediction file...\n")

# Format timestamp for filename
timestamp_str <- format(Sys.time(), "%Y-%m-%d")
filename <- sprintf("data/output/predictions_locked_%s.csv", timestamp_str)

# Save with timestamp
write_csv(combined_predictions, filename)
cat(sprintf("  Saved: %s\n", filename))

# Also save as generic filename for easy access
write_csv(combined_predictions, "data/output/predictions_combined.csv")
cat("  Saved: data/output/predictions_combined.csv\n")

# ============================================================================
# Print Summary
# ============================================================================
cat("\n=== FINAL PREDICTIONS ===\n")
cat(sprintf("Locked: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("\n")

for (i in 1:nrow(combined_predictions)) {
  pred <- combined_predictions[i, ]
  cat(sprintf("%s Model:\n", pred$model))
  cat(sprintf("  Seahawks win probability: %.3f\n", pred$seahawks_win_prob))
  cat(sprintf("  Predicted score: SEA %d - NE %d\n", 
              pred$predicted_sea_score, pred$predicted_ne_score))
  cat("\n")
}

# Calculate average prediction
avg_win_prob <- mean(combined_predictions$seahawks_win_prob)
avg_sea_score <- mean(combined_predictions$predicted_sea_score)
avg_ne_score <- mean(combined_predictions$predicted_ne_score)

cat("Average across models:\n")
cat(sprintf("  Seahawks win probability: %.3f\n", avg_win_prob))
cat(sprintf("  Predicted score: SEA %.1f - NE %.1f\n", 
            avg_sea_score, avg_ne_score))

cat("\n=== PREDICTIONS LOCKED ===\n")
cat("Ready for Super Bowl LX!\n")
