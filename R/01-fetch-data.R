# 01-fetch-data.R
# Download and prepare all necessary data from nflverse
# Author: Super Bowl LX Prediction Project
# Date: 2026-02-07

library(tidyverse)
library(nflreadr)

# Check if nflfastR is available (optional - only needed for play-by-play EPA)
if (requireNamespace("nflfastR", quietly = TRUE)) {
  library(nflfastR)
  nflfastR_available <- TRUE
} else {
  nflfastR_available <- FALSE
  cat("Note: nflfastR not available. EPA calculations will be skipped.\n")
}

# Set seed for reproducibility
set.seed(2026)

# Create output directories if they don't exist
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

cat("Starting data fetch...\n")

# ============================================================================
# 1. Load NFL schedules from 2015-2025 seasons
# ============================================================================
cat("Loading schedules from 2015-2025...\n")

schedules <- map_dfr(2015:2025, function(season) {
  cat(sprintf("  Loading season %d...\n", season))
  tryCatch({
    sched <- nflreadr::load_schedules(season)
    return(sched)
  }, error = function(e) {
    cat(sprintf("    Warning: Error loading season %d: %s\n", season, e$message))
    return(NULL)
  })
})

# Filter for completed games only
schedules <- schedules %>%
  filter(!is.na(away_score) & !is.na(home_score)) %>%
  arrange(season, week, gameday)

cat(sprintf("Loaded %d games from 2015-2025\n", nrow(schedules)))

# ============================================================================
# 2. Load play-by-play data for 2025 season (for team statistics)
# ============================================================================
cat("Loading 2025 play-by-play data...\n")

pbp_2025 <- NULL
if (nflfastR_available) {
  pbp_2025 <- tryCatch({
    nflfastR::load_pbp(2025)
  }, error = function(e) {
    cat(sprintf("Warning: Could not load 2025 play-by-play: %s\n", e$message))
    return(NULL)
  })
} else {
  cat("Skipping play-by-play data (nflfastR not available)\n")
}

# ============================================================================
# 3. Filter for Seahawks (SEA) and Patriots (NE) games
# ============================================================================
cat("Filtering for SEA and NE games...\n")

sea_ne_games <- schedules %>%
  filter(away_team == "SEA" | home_team == "SEA" | 
         away_team == "NE" | home_team == "NE")

cat(sprintf("Found %d games involving SEA or NE\n", nrow(sea_ne_games)))

# ============================================================================
# 4. Calculate season-level statistics for both teams
# ============================================================================
cat("Calculating team statistics...\n")

calculate_team_stats <- function(team_abbr, season_data, pbp_data = NULL) {
  # Filter games for this team
  team_games <- season_data %>%
    filter(away_team == team_abbr | home_team == team_abbr) %>%
    mutate(
      is_home = home_team == team_abbr,
      team_score = ifelse(is_home, home_score, away_score),
      opponent_score = ifelse(is_home, away_score, home_score),
      opponent = ifelse(is_home, away_team, home_team),
      result = case_when(
        team_score > opponent_score ~ "W",
        team_score < opponent_score ~ "L",
        TRUE ~ "T"
      )
    )
  
  # Basic statistics
  wins <- sum(team_games$result == "W", na.rm = TRUE)
  losses <- sum(team_games$result == "L", na.rm = TRUE)
  ties <- sum(team_games$result == "T", na.rm = TRUE)
  
  # Points per game (offense and defense)
  points_for <- mean(team_games$team_score, na.rm = TRUE)
  points_against <- mean(team_games$opponent_score, na.rm = TRUE)
  point_differential <- points_for - points_against
  
  # Calculate strength of schedule (average opponent win percentage)
  # Get all opponents and their records
  opponents <- unique(team_games$opponent)
  opponent_records <- map_dbl(opponents, function(opp) {
    opp_games <- season_data %>%
      filter(away_team == opp | home_team == opp) %>%
      filter(!is.na(away_score) & !is.na(home_score))
    
    if(nrow(opp_games) == 0) return(0.5)
    
    opp_wins <- sum(
      (opp_games$away_team == opp & opp_games$away_score > opp_games$home_score) |
      (opp_games$home_team == opp & opp_games$home_score > opp_games$away_score),
      na.rm = TRUE
    )
    opp_ties <- sum(
      opp_games$away_score == opp_games$home_score,
      na.rm = TRUE
    )
    
    total_games <- nrow(opp_games)
    win_pct <- (opp_wins + 0.5 * opp_ties) / total_games
    return(win_pct)
  })
  
  sos <- mean(opponent_records, na.rm = TRUE)
  
  # EPA per play (if pbp data available)
  epa_per_play <- NA
  if(!is.null(pbp_data) && nrow(pbp_data) > 0) {
    team_pbp <- pbp_data %>%
      filter(posteam == team_abbr, !is.na(epa))
    
    if(nrow(team_pbp) > 0) {
      epa_per_play <- mean(team_pbp$epa, na.rm = TRUE)
    }
  }
  
  return(tibble(
    team = team_abbr,
    season = unique(season_data$season)[1],
    wins = wins,
    losses = losses,
    ties = ties,
    win_pct = (wins + 0.5 * ties) / (wins + losses + ties),
    points_for = points_for,
    points_against = points_against,
    point_differential = point_differential,
    strength_of_schedule = sos,
    epa_per_play = epa_per_play
  ))
}

# Calculate stats for 2025 season
sea_stats_2025 <- calculate_team_stats("SEA", 
                                       schedules %>% filter(season == 2025),
                                       pbp_2025)
ne_stats_2025 <- calculate_team_stats("NE", 
                                      schedules %>% filter(season == 2025),
                                      pbp_2025)

# Calculate stats for all seasons (for historical context)
sea_stats_all <- map_dfr(2015:2025, function(yr) {
  calculate_team_stats("SEA", schedules %>% filter(season == yr))
})

ne_stats_all <- map_dfr(2015:2025, function(yr) {
  calculate_team_stats("NE", schedules %>% filter(season == yr))
})

cat("Team statistics calculated\n")
print(sea_stats_2025)
print(ne_stats_2025)

# ============================================================================
# 5. Load historical Super Bowl results (1967-2025)
# ============================================================================
cat("Loading historical Super Bowl data...\n")

# Load all schedules and filter for Super Bowl games
all_schedules <- map_dfr(1967:2025, function(season) {
  tryCatch({
    sched <- nflreadr::load_schedules(season)
    return(sched)
  }, error = function(e) {
    return(NULL)
  })
})

super_bowls <- all_schedules %>%
  filter(grepl("Super Bowl", game_type, ignore.case = TRUE) | 
         (week == 21 & season >= 1966)) %>%
  filter(!is.na(away_score) & !is.na(home_score)) %>%
  mutate(
    sb_number = season - 1966,  # SB I was in 1967 season
    total_points = away_score + home_score,
    margin = abs(away_score - home_score)
  ) %>%
  arrange(season)

cat(sprintf("Found %d Super Bowl games\n", nrow(super_bowls)))

# ============================================================================
# 6. Save cleaned data
# ============================================================================
cat("Saving processed data...\n")

# Save full schedules
write_csv(schedules, "data/processed/schedules_2015_2025.csv")
cat("  Saved: data/processed/schedules_2015_2025.csv\n")

# Save SEA/NE games
write_csv(sea_ne_games, "data/processed/sea_ne_games.csv")
cat("  Saved: data/processed/sea_ne_games.csv\n")

# Save team statistics
write_csv(sea_stats_2025, "data/processed/sea_stats_2025.csv")
write_csv(ne_stats_2025, "data/processed/ne_stats_2025.csv")
write_csv(sea_stats_all, "data/processed/sea_stats_all_seasons.csv")
write_csv(ne_stats_all, "data/processed/ne_stats_all_seasons.csv")
cat("  Saved: team statistics files\n")

# Save Super Bowl history
write_csv(super_bowls, "data/processed/super_bowl_history.csv")
cat("  Saved: data/processed/super_bowl_history.csv\n")

# Save summary statistics
summary_stats <- bind_rows(
  sea_stats_2025 %>% mutate(team = "SEA"),
  ne_stats_2025 %>% mutate(team = "NE")
)

write_csv(summary_stats, "data/processed/team_summary_2025.csv")
cat("  Saved: data/processed/team_summary_2025.csv\n")

cat("\nData fetch complete!\n")
cat(sprintf("Total games loaded: %d\n", nrow(schedules)))
cat(sprintf("Super Bowls found: %d\n", nrow(super_bowls)))
cat(sprintf("SEA 2025 record: %d-%d-%d\n", 
            sea_stats_2025$wins, sea_stats_2025$losses, sea_stats_2025$ties))
cat(sprintf("NE 2025 record: %d-%d-%d\n", 
            ne_stats_2025$wins, ne_stats_2025$losses, ne_stats_2025$ties))
