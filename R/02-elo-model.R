# 02-elo-model.R
# Implement FiveThirtyEight-style Elo ratings for NFL
# Author: Super Bowl LX Prediction Project
# Date: 2026-02-07

library(tidyverse)
source("R/utils.R")

# Set seed for reproducibility
set.seed(2026)

cat("Starting Elo model calculation...\n")

# ============================================================================
# Model Parameters
# ============================================================================
STARTING_ELO <- 1505
K_REGULAR <- 20
K_PLAYOFFS <- 30
HOME_ADVANTAGE <- 65
REGRESSION_FACTOR <- 1/3  # Regress 1/3 toward mean between seasons

# ============================================================================
# Load Data
# ============================================================================
cat("Loading schedules...\n")
schedules <- read_csv("data/processed/schedules_2015_2025.csv", 
                      show_col_types = FALSE)

# Filter for completed games
schedules <- schedules %>%
  filter(!is.na(away_score) & !is.na(home_score)) %>%
  arrange(season, week, gameday)

cat(sprintf("Loaded %d games\n", nrow(schedules)))

# ============================================================================
# Initialize Elo Ratings
# ============================================================================
cat("Initializing Elo ratings...\n")

# Get all unique teams
all_teams <- unique(c(schedules$away_team, schedules$home_team))
all_teams <- all_teams[!is.na(all_teams)]

# Initialize all teams at starting Elo for 2015 season
elo_ratings <- tibble(
  team = all_teams,
  elo = STARTING_ELO
)

# Track Elo history
elo_history <- tibble()

# ============================================================================
# Process Games Season by Season
# ============================================================================

for (season in 2015:2025) {
  cat(sprintf("\nProcessing season %d...\n", season))
  
  # Apply between-season regression (except for first season)
  if (season > 2015) {
    cat("  Applying between-season regression...\n")
    elo_ratings <- elo_ratings %>%
      mutate(elo = regress_elo(elo, STARTING_ELO, REGRESSION_FACTOR))
  }
  
  # Get games for this season
  season_games <- schedules %>%
    filter(season == !!season) %>%
    arrange(week, gameday)
  
  # Track current week for history
  current_week <- 0
  last_week <- 0
  last_date <- NULL
  
  # Process each game
  for (i in 1:nrow(season_games)) {
    game <- season_games[i, ]
    
    # Update week tracking - save ratings when week changes
    current_week <- game$week
    if (is.null(last_date) || current_week != last_week) {
      # Save Elo ratings at end of previous week
      if (!is.null(last_date) && last_week > 0) {
        week_history <- elo_ratings %>%
          mutate(
            season = season,
            week = last_week,
            date = last_date
          )
        elo_history <- bind_rows(elo_history, week_history)
      }
      last_week <- current_week
      last_date <- game$gameday
    } else {
      # Same week, update date if needed
      last_date <- game$gameday
    }
    
    away_team <- game$away_team
    home_team <- game$home_team
    away_score <- game$away_score
    home_score <- game$home_score
    
    # Skip if missing data
    if (is.na(away_team) || is.na(home_team) || 
        is.na(away_score) || is.na(home_score)) {
      next
    }
    
    # Get current Elo ratings
    away_elo <- elo_ratings$elo[elo_ratings$team == away_team]
    home_elo <- elo_ratings$elo[elo_ratings$team == home_team]
    
    # Determine winner and margin
    if (away_score > home_score) {
      winner <- away_team
      loser <- home_team
      margin <- away_score - home_score
      is_home_winner <- FALSE
    } else if (home_score > away_score) {
      winner <- home_team
      loser <- away_team
      margin <- home_score - away_score
      is_home_winner <- TRUE
    } else {
      # Tie - no Elo change
      next
    }
    
    # Determine K-factor (playoffs use higher K)
    is_playoff <- !is.na(game$game_type) && 
                  grepl("POST|WC|DIV|CONF|SB", game$game_type, ignore.case = TRUE)
    k_factor <- ifelse(is_playoff, K_PLAYOFFS, K_REGULAR)
    
    # Update Elo ratings
    winner_elo <- elo_ratings$elo[elo_ratings$team == winner]
    loser_elo <- elo_ratings$elo[elo_ratings$team == loser]
    
    updated <- update_elo(
      winner_elo = winner_elo,
      loser_elo = loser_elo,
      margin = margin,
      k_factor = k_factor,
      home_advantage = HOME_ADVANTAGE,
      is_home_winner = is_home_winner
    )
    
    # Update ratings
    elo_ratings$elo[elo_ratings$team == winner] <- updated$winner_elo
    elo_ratings$elo[elo_ratings$team == loser] <- updated$loser_elo
  }
  
  # Save final ratings for the season (after all games processed)
  if (!is.null(last_date) && last_week > 0) {
    week_history <- elo_ratings %>%
      mutate(
        season = season,
        week = last_week,
        date = last_date
      )
    elo_history <- bind_rows(elo_history, week_history)
  }
  
  cat(sprintf("  Season %d complete. Final ratings range: %.0f - %.0f\n", 
              season, min(elo_ratings$elo), max(elo_ratings$elo)))
}

# ============================================================================
# Calculate Super Bowl Win Probabilities
# ============================================================================
cat("\nCalculating Super Bowl win probabilities...\n")

# Only calculate SB win probability when both teams have made it to the Super Bowl
# For 2025 season, this is week 21+ (after conference championships)
# Before that, we can't know the Super Bowl matchup, so probability should be NA

elo_history <- elo_history %>%
  group_by(season, week, date) %>%
  mutate(
    sea_elo = ifelse(any(team == "SEA"), elo[team == "SEA"], NA),
    ne_elo = ifelse(any(team == "NE"), elo[team == "NE"], NA),
    # Check if this is after the Super Bowl matchup is known (week 21+ for 2025)
    is_sb_week = (season == 2025 & week >= 21) | 
                 (season < 2025 & week >= 21)  # For historical seasons too
  ) %>%
  ungroup() %>%
  mutate(
    # Only calculate head-to-head win probability when Super Bowl matchup is known
    sb_win_prob = case_when(
      # For SEA: calculate win probability vs NE (only when matchup is known)
      team == "SEA" & !is.na(sea_elo) & !is.na(ne_elo) & is_sb_week ~ 
        calculate_sb_prob(sea_elo, ne_elo),
      # For NE: calculate win probability vs SEA (only when matchup is known)
      team == "NE" & !is.na(sea_elo) & !is.na(ne_elo) & is_sb_week ~ 
        calculate_sb_prob(ne_elo, sea_elo),
      # Before Super Bowl matchup is known, set to NA
      TRUE ~ NA_real_
    )
  ) %>%
  select(-sea_elo, -ne_elo, -is_sb_week)

# ============================================================================
# Get Final Elo Ratings (as of Feb 7, 2026)
# ============================================================================
cat("Getting final Elo ratings...\n")

current_elos <- elo_ratings %>%
  filter(team %in% c("SEA", "NE")) %>%
  arrange(team)

cat("\nFinal Elo Ratings:\n")
print(current_elos)

# ============================================================================
# Calculate Super Bowl LX Prediction
# ============================================================================
cat("\nCalculating Super Bowl LX prediction...\n")

sea_elo <- current_elos$elo[current_elos$team == "SEA"]
ne_elo <- current_elos$elo[current_elos$team == "NE"]

# Win probabilities (neutral site)
sea_win_prob <- calculate_sb_prob(sea_elo, ne_elo)
ne_win_prob <- calculate_sb_prob(ne_elo, sea_elo)

cat(sprintf("SEA win probability: %.3f\n", sea_win_prob))
cat(sprintf("NE win probability: %.3f\n", ne_win_prob))

# Expected scores based on Elo difference and historical patterns
# Load historical Super Bowl data for scoring patterns
sb_history <- read_csv("data/processed/super_bowl_history.csv", 
                       show_col_types = FALSE)

# Calculate average scores by era (recent Super Bowls more relevant)
recent_sbs <- sb_history %>%
  filter(season >= 2010) %>%
  summarise(
    avg_away_score = mean(away_score, na.rm = TRUE),
    avg_home_score = mean(home_score, na.rm = TRUE),
    avg_total = mean(total_points, na.rm = TRUE)
  )

# Elo difference affects expected score
elo_diff <- sea_elo - ne_elo

# Base expected total points (from recent Super Bowls)
base_total <- recent_sbs$avg_total[1]

# Adjust based on Elo difference (higher Elo = more points)
# Rough approximation: 1 Elo point ≈ 0.01 points per game
elo_impact <- elo_diff * 0.01

# Expected scores (distribute total based on Elo)
sea_expected <- (base_total / 2) + elo_impact
ne_expected <- (base_total / 2) - elo_impact

# Ensure reasonable bounds
sea_expected <- max(10, min(40, sea_expected))
ne_expected <- max(10, min(40, ne_expected))

cat(sprintf("Expected SEA score: %.1f\n", sea_expected))
cat(sprintf("Expected NE score: %.1f\n", ne_expected))

# Create prediction data frame
elo_prediction <- tibble(
  model = "Elo",
  seahawks_win_prob = sea_win_prob,
  predicted_sea_score = round(sea_expected),
  predicted_ne_score = round(ne_expected),
  sea_elo = sea_elo,
  ne_elo = ne_elo,
  elo_difference = elo_diff
)

# ============================================================================
# Save Outputs
# ============================================================================
cat("\nSaving outputs...\n")

# Save Elo history
write_csv(elo_history, "data/output/elo_history.csv")
cat("  Saved: data/output/elo_history.csv\n")

# Save current Elo ratings
write_csv(current_elos, "data/output/current_elos.csv")
cat("  Saved: data/output/current_elos.csv\n")

# Save Elo prediction
write_csv(elo_prediction, "data/output/elo_prediction.csv")
cat("  Saved: data/output/elo_prediction.csv\n")

cat("\nElo model complete!\n")
cat(sprintf("Total Elo history records: %d\n", nrow(elo_history)))
cat(sprintf("SEA final Elo: %.1f\n", sea_elo))
cat(sprintf("NE final Elo: %.1f\n", ne_elo))
