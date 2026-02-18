# utils.R
# Helper functions for Elo calculations and simulations
# Author: Super Bowl LX Prediction Project

# ============================================================================
# Elo Rating Functions
# ============================================================================

#' Update Elo ratings after a game
#' 
#' @param winner_elo Current Elo rating of winning team
#' @param loser_elo Current Elo rating of losing team
#' @param margin Margin of victory (winner_score - loser_score)
#' @param k_factor K-factor for Elo update (20 for regular season, 30 for playoffs)
#' @param home_advantage Elo points for home field advantage (default 65)
#' @param is_home_winner Whether the winner was playing at home
#' @return Named list with new_winner_elo and new_loser_elo
update_elo <- function(winner_elo, loser_elo, margin, k_factor = 20, 
                       home_advantage = 65, is_home_winner = FALSE) {
  
  # Adjust for home field advantage
  if (is_home_winner) {
    adjusted_winner_elo <- winner_elo + home_advantage
    adjusted_loser_elo <- loser_elo
  } else {
    adjusted_winner_elo <- winner_elo
    adjusted_loser_elo <- loser_elo + home_advantage
  }
  
  # Calculate expected win probability
  expected_win <- 1 / (1 + 10^((adjusted_loser_elo - adjusted_winner_elo) / 400))
  
  # Margin of victory multiplier
  mov_multiplier <- log(abs(margin) + 1)
  
  # Calculate Elo change
  elo_change <- k_factor * mov_multiplier * (1 - expected_win)
  
  # Update ratings
  new_winner_elo <- winner_elo + elo_change
  new_loser_elo <- loser_elo - elo_change
  
  return(list(
    winner_elo = new_winner_elo,
    loser_elo = new_loser_elo
  ))
}

#' Calculate win probability from Elo ratings
#' 
#' @param team_elo Elo rating of team
#' @param opponent_elo Elo rating of opponent
#' @param home_advantage Elo points for home field advantage (default 65)
#' @param is_home Whether team is playing at home
#' @return Win probability (0 to 1)
calculate_win_prob <- function(team_elo, opponent_elo, home_advantage = 65, 
                                is_home = FALSE) {
  
  # Adjust for home field
  if (is_home) {
    adjusted_team_elo <- team_elo + home_advantage
    adjusted_opponent_elo <- opponent_elo
  } else {
    adjusted_team_elo <- team_elo
    adjusted_opponent_elo <- opponent_elo + home_advantage
  }
  
  # For neutral site (Super Bowl), no home advantage
  if (is.na(is_home) || is.null(is_home)) {
    adjusted_team_elo <- team_elo
    adjusted_opponent_elo <- opponent_elo
  }
  
  win_prob <- 1 / (1 + 10^((adjusted_opponent_elo - adjusted_team_elo) / 400))
  return(win_prob)
}

#' Calculate Super Bowl win probability from Elo ratings (neutral site)
#' 
#' @param team_elo Elo rating of team
#' @param opponent_elo Elo rating of opponent
#' @return Win probability (0 to 1)
calculate_sb_prob <- function(team_elo, opponent_elo) {
  return(calculate_win_prob(team_elo, opponent_elo, home_advantage = 0, is_home = FALSE))
}

#' Apply between-season regression to Elo ratings
#' 
#' @param previous_elo Elo rating at end of previous season
#' @param mean_elo Mean Elo rating (default 1505)
#' @param regression_factor Fraction to regress toward mean (default 1/3)
#' @return New season starting Elo
regress_elo <- function(previous_elo, mean_elo = 1505, regression_factor = 1/3) {
  return((previous_elo * (1 - regression_factor)) + (mean_elo * regression_factor))
}

# ============================================================================
# Simulation Functions
# ============================================================================

#' Simulate one game score
#' 
#' @param team_mean Expected points for team
#' @param opponent_mean Expected points for opponent
#' @return Named list with team_score and opponent_score
simulate_game <- function(team_mean, opponent_mean) {
  team_score <- rpois(1, lambda = team_mean)
  opponent_score <- rpois(1, lambda = opponent_mean)
  
  return(list(
    team_score = team_score,
    opponent_score = opponent_score
  ))
}

#' Run multiple game simulations
#' 
#' @param team_mean Expected points for team
#' @param opponent_mean Expected points for opponent
#' @param n_sims Number of simulations (default 10000)
#' @return Data frame with simulation results
run_simulations <- function(team_mean, opponent_mean, n_sims = 10000) {
  set.seed(2026)  # For reproducibility
  
  sims <- map_dfr(1:n_sims, function(i) {
    result <- simulate_game(team_mean, opponent_mean)
    return(tibble(
      sim_id = i,
      team_score = result$team_score,
      opponent_score = result$opponent_score,
      winner = ifelse(result$team_score > result$opponent_score, "team",
                     ifelse(result$team_score < result$opponent_score, "opponent", "tie"))
    ))
  })
  
  return(sims)
}

# ============================================================================
# Plotting Functions
# ============================================================================

#' Plot Elo rating evolution over time
#' 
#' @param elo_history Data frame with columns: date, team, elo
#' @param teams Vector of team abbreviations to plot
#' @return ggplot object
plot_elo_evolution <- function(elo_history, teams = c("SEA", "NE")) {
  library(ggplot2)
  
  plot_data <- elo_history %>%
    filter(team %in% teams) %>%
    mutate(date = as.Date(date))
  
  p <- ggplot(plot_data, aes(x = date, y = elo, color = team)) +
    geom_line(size = 1.2) +
    labs(
      title = "Elo Rating Evolution",
      x = "Date",
      y = "Elo Rating",
      color = "Team"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}

#' Plot Super Bowl win probability evolution
#' 
#' @param elo_history Data frame with columns: date, team, sb_win_prob
#' @param teams Vector of team abbreviations to plot
#' @return ggplot object
plot_sb_prob_evolution <- function(elo_history, teams = c("SEA", "NE")) {
  library(ggplot2)
  
  plot_data <- elo_history %>%
    filter(team %in% teams) %>%
    mutate(date = as.Date(date))
  
  p <- ggplot(plot_data, aes(x = date, y = sb_win_prob, color = team)) +
    geom_line(size = 1.2) +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Super Bowl Win Probability Evolution",
      x = "Date",
      y = "Win Probability",
      color = "Team"
    ) +
    ylim(0, 1) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(p)
}
