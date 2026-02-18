# install-dependencies.R
# Install required R packages for the Super Bowl prediction project

cat("Installing required R packages...\n\n")

# Core packages
packages <- c(
  "tidyverse",
  "ggplot2",
  "readr",
  "dplyr",
  "tidyr",
  "purrr"
)

# NFL data packages
nfl_packages <- c(
  "nflreadr",
  "nflfastR"
)

# Visualization packages
viz_packages <- c(
  "gt",
  "gtExtras",
  "patchwork",
  "scales",
  "viridisLite"
)

# API packages
api_packages <- c(
  "httr",
  "jsonlite"
)

# Quarto (if not already installed)
quarto_packages <- "quarto"

all_packages <- c(packages, viz_packages, api_packages)

cat("Installing core packages...\n")
for (pkg in all_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  Installing %s...\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    cat(sprintf("  ✓ %s already installed\n", pkg))
  }
}

cat("\nInstalling NFL data packages...\n")
cat("  Note: nflfastR requires xgboost, which may need system dependencies\n")
cat("  On macOS: brew install libomp\n\n")

# Try to install nflreadr first (simpler)
if (!require("nflreadr", quietly = TRUE)) {
  cat("  Installing nflreadr...\n")
  install.packages("nflreadr", repos = "https://nflverse.r-universe.dev")
}

# Try nflfastR (may fail if xgboost not available)
if (!require("nflfastR", quietly = TRUE)) {
  cat("  Attempting to install nflfastR...\n")
  cat("  (This may fail if xgboost dependencies are missing)\n")
  tryCatch({
    install.packages("nflfastR", repos = "https://nflverse.r-universe.dev")
  }, error = function(e) {
    cat("  ✗ nflfastR installation failed. You may need to install xgboost first.\n")
    cat("  For macOS: brew install libomp, then install.packages('xgboost')\n")
  })
}

cat("\nChecking Quarto installation...\n")
if (!require("quarto", quietly = TRUE)) {
  cat("  Quarto R package not found. Install Quarto CLI:\n")
  cat("  macOS: brew install quarto\n")
  cat("  Or visit: https://quarto.org/docs/get-started/\n")
} else {
  cat("  ✓ Quarto R package available\n")
}

cat("\n✓ Package installation complete!\n")
cat("\nNext steps:\n")
cat("1. Run: source('R/01-fetch-data.R')\n")
cat("2. Run: source('R/02-elo-model.R')\n")
cat("3. Run: source('R/03-baseline-model.R')\n")
cat("4. Run: source('R/04-combine-predictions.R')\n")
cat("5. Or run everything: source('run-pipeline.R')\n")
