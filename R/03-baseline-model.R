# 03-baseline-model.R
# Historical simulation-based prediction model
# Author: Super Bowl LX Prediction Project
# Date: 2026-02-07

library(tidyverse)
source("R/utils.R")

# Set seed for reproducibility
set.seed(2026)

cat("Starting baseline model calculation...\n")

# ============================================================================
# Load Data
# ============================================================================
cat("Loading data...\n")

# Load historical Super Bowl data
sb_history <- read_csv("data/processed/super_bowl_history.csv", 
                       show_col_types = FALSE)

# Load 2025 team statistics
team_stats <- read_csv("data/processed/team_summary_2025.csv", 
                       show_col_types = FALSE)

# Load all schedules to calculate league averages
schedules <- read_csv("data/processed/schedules_2015_2025.csv", 
                      show_col_types = FALSE)

cat(sprintf("Loaded %d Super Bowl games\n", nrow(sb_history)))
cat(sprintf("Loaded team statistics for %d teams\n", nrow(team_stats)))

# ============================================================================
# Calculate Baseline Scoring Distributions
# ============================================================================
cat("Calculating baseline scoring distributions...\n")

# Focus on recent Super Bowls (last 20 years) for more relevant patterns
recent_sbs <- sb_history %>%
  filter(season >= 2005) %>%
  filter(!is.na(away_score) & !is.na(home_score))

cat(sprintf("Using %d recent Super Bowls for baseline\n", nrow(recent_sbs)))

# Calculate scoring statistics
sb_scoring_stats <- recent_sbs %>%
  summarise(
    mean_away_score = mean(away_score, na.rm = TRUE),
    mean_home_score = mean(home_score, na.rm = TRUE),
    sd_away_score = sd(away_score, na.rm = TRUE),
    sd_home_score = sd(home_score, na.rm = TRUE),
    mean_total = mean(total_points, na.rm = TRUE),
    sd_total = sd(total_points, na.rm = TRUE)
  )

cat("Baseline scoring statistics:\n")
print(sb_scoring_stats)

# ============================================================================
# Calculate 2025 Season Adjustments
# ============================================================================
cat("Calculating 2025 season adjustments...\n")

# Calculate league average points per game for 2025
league_avg_2025 <- schedules %>%
  filter(season == 2025, !is.na(away_score), !is.na(home_score)) %>%
  summarise(
    avg_points_per_game = mean(away_score + home_score, na.rm = TRUE) / 2,
    avg_offense = mean(c(away_score, home_score), na.rm = TRUE)
  )

cat(sprintf("2025 league average points per game: %.2f\n", 
            league_avg_2025$avg_offense))

# Get team statistics
sea_stats <- team_stats %>% filter(team == "SEA")
ne_stats <- team_stats %>% filter(team == "NE")

if (nrow(sea_stats) == 0 || nrow(ne_stats) == 0) {
  stop("Could not find SEA or NE statistics. Please run 01-fetch-data.R first.")
}

# Calculate adjustments relative to league average
sea_offense_adj <- sea_stats$points_for - league_avg_2025$avg_offense
sea_defense_adj <- league_avg_2025$avg_offense - sea_stats$points_against

ne_offense_adj <- ne_stats$points_for - league_avg_2025$avg_offense
ne_defense_adj <- league_avg_2025$avg_offense - ne_stats$points_against

cat(sprintf("SEA offense adjustment: %.2f\n", sea_offense_adj))
cat(sprintf("SEA defense adjustment: %.2f\n", sea_defense_adj))
cat(sprintf("NE offense adjustment: %.2f\n", ne_offense_adj))
cat(sprintf("NE defense adjustment: %.2f\n", ne_defense_adj))

# ============================================================================
# Calculate Expected Scores
# ============================================================================
cat("Calculating expected scores...\n")

# Base expected scores from historical Super Bowls
# For Super Bowl, we'll use average of away and home (no home field)
base_score <- (sb_scoring_stats$mean_away_score + sb_scoring_stats$mean_home_score) / 2

# Adjust for team performance
# SEA expected score = base + SEA offense adj - NE defense adj
sea_expected_score <- base_score + sea_offense_adj - ne_defense_adj
ne_expected_score <- base_score + ne_offense_adj - sea_defense_adj

# Ensure reasonable bounds
sea_expected_score <- max(10, min(45, sea_expected_score))
ne_expected_score <- max(10, min(45, ne_expected_score))

cat(sprintf("Expected SEA score: %.2f\n", sea_expected_score))
cat(sprintf("Expected NE score: %.2f\n", ne_expected_score))

# ============================================================================
# Run Simulations
# ============================================================================
cat("Running 10,000 simulations...\n")

n_sims <- 10000

# Use Poisson distribution for scoring (common in football modeling)
# But adjust based on historical variance
simulations <- map_dfr(1:n_sims, function(i) {
  # Sample scores from adjusted distributions
  # Use Poisson with mean = expected score
  sea_score <- rpois(1, lambda = sea_expected_score)
  ne_score <- rpois(1, lambda = ne_expected_score)
  
  # Determine winner
  if (sea_score > ne_score) {
    winner <- "SEA"
  } else if (ne_score > sea_score) {
    winner <- "NE"
  } else {
    # Tie - very rare, but handle it
    winner <- "TIE"
  }
  
  return(tibble(
    sim_id = i,
    sea_score = sea_score,
    ne_score = ne_score,
    winner = winner
  ))
})

cat("Simulations complete\n")

# ============================================================================
# Calculate Win Probabilities and Statistics
# ============================================================================
cat("Calculating win probabilities...\n")

sea_wins <- sum(simulations$winner == "SEA")
ne_wins <- sum(simulations$winner == "NE")
ties <- sum(simulations$winner == "TIE")

sea_win_prob <- sea_wins / n_sims
ne_win_prob <- ne_wins / n_sims

cat(sprintf("SEA wins: %d (%.3f)\n", sea_wins, sea_win_prob))
cat(sprintf("NE wins: %d (%.3f)\n", ne_wins, ne_win_prob))
cat(sprintf("Ties: %d (%.3f)\n", ties, ties / n_sims))

# Calculate expected scores from simulations
sim_sea_expected <- mean(simulations$sea_score)
sim_ne_expected <- mean(simulations$ne_score)

cat(sprintf("Simulated SEA expected score: %.2f\n", sim_sea_expected))
cat(sprintf("Simulated NE expected score: %.2f\n", sim_ne_expected))

# Calculate score distribution percentiles
sea_score_percentiles <- quantile(simulations$sea_score, 
                                  probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
ne_score_percentiles <- quantile(simulations$ne_score, 
                                 probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

cat("\nScore distribution percentiles:\n")
cat("SEA: ", paste(sea_score_percentiles, collapse = ", "), "\n")
cat("NE: ", paste(ne_score_percentiles, collapse = ", "), "\n")

# ============================================================================
# Create Prediction Output
# ============================================================================
baseline_prediction <- tibble(
  model = "Baseline",
  seahawks_win_prob = sea_win_prob,
  predicted_sea_score = round(sim_sea_expected),
  predicted_ne_score = round(sim_ne_expected),
  sea_expected_lambda = sea_expected_score,
  ne_expected_lambda = ne_expected_score
)

# ============================================================================
# Save Outputs
# ============================================================================
cat("\nSaving outputs...\n")

# Save all simulations
write_csv(simulations, "data/output/score_simulations.csv")
cat("  Saved: data/output/score_simulations.csv\n")

# Save prediction
write_csv(baseline_prediction, "data/output/baseline_prediction.csv")
cat("  Saved: data/output/baseline_prediction.csv\n")

# Save summary statistics
summary_stats <- tibble(
  model = "Baseline",
  sea_win_prob = sea_win_prob,
  ne_win_prob = ne_win_prob,
  sea_expected_score = sim_sea_expected,
  ne_expected_score = sim_ne_expected,
  sea_score_median = median(simulations$sea_score),
  ne_score_median = median(simulations$ne_score),
  sea_score_q05 = sea_score_percentiles["5%"],
  sea_score_q95 = sea_score_percentiles["95%"],
  ne_score_q05 = ne_score_percentiles["5%"],
  ne_score_q95 = ne_score_percentiles["95%"]
)

write_csv(summary_stats, "data/output/baseline_summary.csv")
cat("  Saved: data/output/baseline_summary.csv\n")

cat("\nBaseline model complete!\n")
cat(sprintf("Total simulations: %d\n", n_sims))
cat(sprintf("SEA win probability: %.3f\n", sea_win_prob))
cat(sprintf("NE win probability: %.3f\n", ne_win_prob))
