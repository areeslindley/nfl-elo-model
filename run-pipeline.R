# run-pipeline.R
# Master script to run the complete prediction pipeline
# Author: Super Bowl LX Prediction Project
# Date: 2026-02-07

cat("========================================\n")
cat("Super Bowl LX Prediction Pipeline\n")
cat("========================================\n\n")

# Ensure we're in the project root directory
# If running from RStudio, this should already be set
# If running from command line, make sure to run from project root
if (!file.exists("R/01-fetch-data.R")) {
  stop("Please run this script from the project root directory")
}

start_time <- Sys.time()

# Step 1: Fetch data
cat("\n[1/4] Fetching data from nflverse...\n")
cat("----------------------------------------\n")
tryCatch({
  source("R/01-fetch-data.R")
  cat("✓ Step 1 complete\n")
}, error = function(e) {
  cat(sprintf("✗ Step 1 failed: %s\n", e$message))
  stop("Pipeline failed at step 1")
})

# Step 2: Calculate Elo ratings
cat("\n[2/4] Calculating Elo ratings...\n")
cat("----------------------------------------\n")
tryCatch({
  source("R/02-elo-model.R")
  cat("✓ Step 2 complete\n")
}, error = function(e) {
  cat(sprintf("✗ Step 2 failed: %s\n", e$message))
  stop("Pipeline failed at step 2")
})

# Step 3: Run baseline model
cat("\n[3/4] Running baseline historical model...\n")
cat("----------------------------------------\n")
tryCatch({
  source("R/03-baseline-model.R")
  cat("✓ Step 3 complete\n")
}, error = function(e) {
  cat(sprintf("✗ Step 3 failed: %s\n", e$message))
  stop("Pipeline failed at step 3")
})

# Step 4: Combine predictions
cat("\n[4/5] Combining predictions...\n")
cat("----------------------------------------\n")
tryCatch({
  source("R/04-combine-predictions.R")
  cat("✓ Step 4 complete\n")
}, error = function(e) {
  cat(sprintf("✗ Step 4 failed: %s\n", e$message))
  stop("Pipeline failed at step 4")
})

# Step 5: Create visualizations
cat("\n[5/5] Creating visualizations...\n")
cat("----------------------------------------\n")
tryCatch({
  source("R/05-create-visualizations.R")
  cat("✓ Step 5 complete\n")
}, error = function(e) {
  cat(sprintf("✗ Step 5 failed: %s\n", e$message))
  stop("Pipeline failed at step 5")
})

# Validation
cat("\n[Validation] Checking outputs...\n")
cat("----------------------------------------\n")
tryCatch({
  source("R/validate-predictions.R")
}, error = function(e) {
  cat(sprintf("⚠ Validation script error: %s\n", e$message))
})

# Summary
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "secs")

cat("\n========================================\n")
cat("Pipeline Complete!\n")
cat(sprintf("Total time: %.1f seconds\n", as.numeric(elapsed)))
cat("========================================\n")

cat("\nOutput files are in data/output/\n")
cat("Visualizations are in docs/figures/\n")
cat("Run source('R/validate-predictions.R') to verify results.\n")
