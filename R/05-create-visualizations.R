# 05-create-visualizations.R
# Generate publication-quality visualizations for R Markdown website
# Author: Super Bowl LX Prediction Project
# Date: 2026-02-07

library(tidyverse)
library(scales)
library(patchwork)
library(viridisLite)  # For plasma color scheme in heatmap

# Check for optional packages
if (requireNamespace("nflplotR", quietly = TRUE)) {
  library(nflplotR)
  nflplotR_available <- TRUE
} else {
  nflplotR_available <- FALSE
}

# Check for local logo files
logo_dir <- "data/logos"
local_logos_available <- dir.exists(logo_dir) && length(list.files(logo_dir, pattern = "\\.png$", ignore.case = TRUE)) > 0

if (local_logos_available) {
  cat("Note: Local team logos found in data/logos/\n")
  # Load png package for reading images
  if (!requireNamespace("png", quietly = TRUE)) {
    install.packages("png", repos = "https://cloud.r-project.org")
  }
  library(png)
} else if (!nflplotR_available) {
  cat("Note: No logos available. Using styled team abbreviations instead.\n")
  cat("  To enable logos, either:\n")
  cat("  1. Download logos: See DOWNLOAD_LOGOS.md\n")
  cat("  2. Install nflplotR: See INSTALL_LOGOS.md\n")
}

if (requireNamespace("plotly", quietly = TRUE)) {
  library(plotly)
  plotly_available <- TRUE
} else {
  plotly_available <- FALSE
  cat("Note: plotly not available. Interactive plots will be skipped.\n")
}

# Create output directory
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

cat("Creating visualizations...\n")

# ============================================================================
# Load Data
# ============================================================================
cat("Loading data...\n")

elo_history <- read_csv("data/output/elo_history.csv", show_col_types = FALSE)
current_elos <- read_csv("data/output/current_elos.csv", show_col_types = FALSE)
elo_prediction <- read_csv("data/output/elo_prediction.csv", show_col_types = FALSE)
score_sims <- read_csv("data/output/score_simulations.csv", show_col_types = FALSE)
baseline_prediction <- read_csv("data/output/baseline_prediction.csv", show_col_types = FALSE)
baseline_summary <- read_csv("data/output/baseline_summary.csv", show_col_types = FALSE)
schedules <- read_csv("data/processed/schedules_2015_2025.csv", show_col_types = FALSE)

# Combine predictions
predictions <- bind_rows(
  elo_prediction %>% select(model, seahawks_win_prob, predicted_sea_score, predicted_ne_score),
  baseline_prediction %>% select(model, seahawks_win_prob, predicted_sea_score, predicted_ne_score)
)

cat("Data loaded successfully\n")

# ============================================================================
# Calculate Team Performance Statistics (2025 Season)
# ============================================================================
cat("Calculating team performance statistics...\n")

calculate_team_performance <- function(schedules) {
  # Filter to 2025 regular season games only
  games_2025 <- schedules %>%
    filter(season == 2025, 
           !is.na(away_score), !is.na(home_score),
           game_type == "REG")  # Regular season only
  
  # Calculate offensive stats (points scored)
  offense_stats <- bind_rows(
    games_2025 %>%
      select(team = away_team, points = away_score, game_id) %>%
      filter(!is.na(team)),
    games_2025 %>%
      select(team = home_team, points = home_score, game_id) %>%
      filter(!is.na(team))
  ) %>%
    group_by(team) %>%
    summarise(
      games = n_distinct(game_id),
      total_points = sum(points, na.rm = TRUE),
      points_per_game = mean(points, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate defensive stats (points allowed)
  defense_stats <- bind_rows(
    games_2025 %>%
      select(team = away_team, points_allowed = home_score, game_id) %>%
      filter(!is.na(team)),
    games_2025 %>%
      select(team = home_team, points_allowed = away_score, game_id) %>%
      filter(!is.na(team))
  ) %>%
    group_by(team) %>%
    summarise(
      points_allowed_per_game = mean(points_allowed, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Combine stats
  team_performance <- offense_stats %>%
    left_join(defense_stats, by = "team") %>%
    mutate(
      point_differential = points_per_game - points_allowed_per_game,
      is_sb_team = team %in% c("SEA", "NE")
    ) %>%
    arrange(desc(points_per_game))
  
  return(team_performance)
}

team_performance <- calculate_team_performance(schedules)

# Calculate league averages
league_avg_offense <- mean(team_performance$points_per_game, na.rm = TRUE)
league_avg_defense <- mean(team_performance$points_allowed_per_game, na.rm = TRUE)

cat(sprintf("League average offense: %.2f PPG\n", league_avg_offense))
cat(sprintf("League average defense: %.2f PPG allowed\n", league_avg_defense))

# Save for reference
write_csv(team_performance, "data/output/team_performance_2025.csv")
cat("  Saved: data/output/team_performance_2025.csv\n")

# ============================================================================
# Theme Setup
# ============================================================================

# Custom theme inspired by FiveThirtyEight
theme_sb <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# ============================================================================
# Figure 1a: Elo Rating Evolution Through Season
# ============================================================================
cat("\nCreating Figure 1a: Elo Rating Evolution...\n")

create_elo_ratings_plot <- function(elo_history) {
  # Filter to 2025 season only
  elo_2025 <- elo_history %>%
    filter(season == 2025) %>%
    mutate(date = as.Date(date))
  
  # Get top 8 teams by final Elo rating (playoff teams)
  playoff_teams <- elo_2025 %>%
    group_by(team) %>%
    filter(week == max(week)) %>%
    ungroup() %>%
    arrange(desc(elo)) %>%
    slice_head(n = 8) %>%
    pull(team)
  
  cat(sprintf("  Top 8 playoff teams: %s\n", paste(playoff_teams, collapse = ", ")))
  
  # Filter to top 8 teams and get one record per team per week
  plot_data <- elo_2025 %>%
    filter(team %in% playoff_teams) %>%
    group_by(team, week) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(
      is_sb_team = team %in% c("SEA", "NE"),
      line_size = if_else(is_sb_team, 2.5, 0.8),
      line_alpha = if_else(is_sb_team, 1.0, 0.35)
    )
  
  # Find playoff start week
  schedules_2025 <- read_csv("data/processed/schedules_2015_2025.csv", show_col_types = FALSE)
  playoff_week <- schedules_2025 %>%
    filter(season == 2025) %>%
    filter(grepl("POST|WC|DIV|CONF|SB", game_type, ignore.case = TRUE)) %>%
    pull(week) %>%
    min(na.rm = TRUE)
  
  if (is.na(playoff_week)) {
    playoff_week <- 18
  }
  
  # Get final week data for logo placement
  final_data <- plot_data %>%
    filter(week == max(week))
  
  # Create plot
  p <- ggplot(plot_data, aes(x = week, y = elo, group = team, color = team)) +
    # Playoff shading
    annotate(
      "rect",
      xmin = playoff_week - 0.5,
      xmax = max(plot_data$week) + 0.5,
      ymin = -Inf,
      ymax = Inf,
      fill = "lightblue",
      alpha = 0.1
    ) +
    # Playoff start line
    geom_vline(
      xintercept = playoff_week - 0.5,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.8
    ) +
    # Lines for all teams
    geom_line(aes(linewidth = line_size, alpha = line_alpha)) +
    # Scales
    scale_linewidth_continuous(range = c(0.8, 2.5), guide = "none") +
    scale_alpha_continuous(range = c(0.35, 1.0), guide = "none") +
    scale_x_continuous(
      breaks = seq(1, max(plot_data$week), 3),
      limits = c(1, max(plot_data$week) + 2),
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      breaks = seq(1400, 1800, 100),
      limits = c(1350, 1800)
    ) +
    labs(
      title = "Elo Rating Evolution - Top 8 Playoff Teams",
      subtitle = "2025 Season | Playoff period shaded",
      x = "Week",
      y = "Elo Rating",
      caption = "Data: nflverse | Thicker lines = Super Bowl participants"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Add team colors
  if (nflplotR_available) {
    p <- p + scale_color_nfl(type = "primary")
    
    # Add logos at endpoints
    p <- p + geom_nfl_logos(
      data = final_data,
      aes(team_abbr = team),
      width = 0.05,
      hjust = 0
    )
    
    # Add text labels for SEA and NE only
    p <- p + geom_text(
      data = final_data %>% filter(team %in% c("SEA", "NE")),
      aes(label = team, x = week + 0.5),
      hjust = 0,
      fontface = "bold",
      size = 4,
      show.legend = FALSE
    )
  } else {
    # Manual colors
    all_teams <- unique(plot_data$team)
    team_colors <- setNames(
      c("#002244", "#C60C30", "#0B162A", "#0085CA", "#FFB612", 
        "#000000", "#4B92DB", "#A71930")[1:length(all_teams)],
      all_teams
    )
    team_colors["SEA"] <- "#002244"
    team_colors["NE"] <- "#C60C30"
    
    p <- p + scale_color_manual(values = team_colors, breaks = all_teams)
    
    # Add text labels for all teams (fallback)
    p <- p + geom_text(
      data = final_data,
      aes(label = team, x = week + 0.5),
      hjust = 0,
      fontface = "bold",
      size = 3.5,
      show.legend = FALSE
    )
  }
  
  # Playoff label inside plot area
  p <- p + annotate(
    "text",
    x = playoff_week + 1,
    y = max(plot_data$elo) * 0.98,
    label = "Playoffs",
    color = "gray30",
    size = 3.5,
    fontface = "italic"
  )
  
  return(p)
}

p1a <- create_elo_ratings_plot(elo_history)

ggsave(
  "docs/figures/elo-ratings-evolution.png",
  p1a,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/elo-ratings-evolution.png\n")

# ============================================================================
# Figure 1b: Super Bowl Win Probability Evolution
# ============================================================================
cat("\nCreating Figure 1b: Super Bowl Win Probability Evolution...\n")

create_sb_probability_plot <- function(elo_history) {
  # Filter to 2025 season only
  elo_2025 <- elo_history %>%
    filter(season == 2025) %>%
    mutate(date = as.Date(date))
  
  # Get top 8 teams by final Elo rating (playoff teams)
  playoff_teams <- elo_2025 %>%
    group_by(team) %>%
    filter(week == max(week)) %>%
    ungroup() %>%
    arrange(desc(elo)) %>%
    slice_head(n = 8) %>%
    pull(team)
  
  cat(sprintf("  Top 8 playoff teams: %s\n", paste(playoff_teams, collapse = ", ")))
  
  # Filter to top 8 teams and get one record per team per week
  # Use sb_win_prob when available (week 21+ for SEA/NE), otherwise NA
  plot_data <- elo_2025 %>%
    filter(team %in% playoff_teams) %>%
    group_by(team, week) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(
      is_sb_team = team %in% c("SEA", "NE"),
      line_size = if_else(is_sb_team, 2.5, 0.8),
      line_alpha = if_else(is_sb_team, 1.0, 0.35)
    )
  
  # Find playoff start week
  schedules_2025 <- read_csv("data/processed/schedules_2015_2025.csv", show_col_types = FALSE)
  playoff_week <- schedules_2025 %>%
    filter(season == 2025) %>%
    filter(grepl("POST|WC|DIV|CONF|SB", game_type, ignore.case = TRUE)) %>%
    pull(week) %>%
    min(na.rm = TRUE)
  
  if (is.na(playoff_week)) {
    playoff_week <- 18
  }
  
  # Get final week data for logo placement
  final_data <- plot_data %>%
    filter(week == max(week))
  
  # Create plot
  p <- ggplot(plot_data, aes(x = week, y = sb_win_prob, group = team, color = team)) +
    # 50% reference line
    geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "gray40",
      alpha = 0.5
    ) +
    # Playoff shading
    annotate(
      "rect",
      xmin = playoff_week - 0.5,
      xmax = max(plot_data$week) + 0.5,
      ymin = -Inf,
      ymax = Inf,
      fill = "lightblue",
      alpha = 0.1
    ) +
    # Playoff start line
    geom_vline(
      xintercept = playoff_week - 0.5,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.8
    ) +
    # Lines for all teams (only show where sb_win_prob is not NA)
    # Note: sb_win_prob is only available for SEA/NE in week 21+
    # So we'll show lines only where data exists
    geom_line(
      data = plot_data %>% filter(!is.na(sb_win_prob)),
      aes(linewidth = line_size, alpha = line_alpha)
    ) +
    # Add points at final week to make it clear where the probabilities are
    geom_point(
      data = plot_data %>% filter(!is.na(sb_win_prob), week == max(week)),
      aes(size = if_else(is_sb_team, 3, 2)),
      alpha = 0.8
    ) +
    scale_size_identity(guide = "none") +
    # Scales
    scale_linewidth_continuous(range = c(0.8, 2.5), guide = "none") +
    scale_alpha_continuous(range = c(0.35, 1.0), guide = "none") +
    scale_x_continuous(
      breaks = seq(1, max(plot_data$week), 3),
      limits = c(1, max(plot_data$week) + 2),
      expand = c(0.01, 0)
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    ) +
    labs(
      title = "Super Bowl LX Win Probability Evolution",
      subtitle = "2025 Season - Top 8 Playoff Teams",
      x = "Week",
      y = "Probability of Winning Super Bowl",
      caption = "Data: nflverse | Elo-based predictions"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  # Add team colors
  if (nflplotR_available) {
    p <- p + scale_color_nfl(type = "primary")
    
    # Add logos at endpoints (only for teams with final probability)
    final_with_prob <- final_data %>% filter(!is.na(sb_win_prob))
    if (nrow(final_with_prob) > 0) {
      p <- p + geom_nfl_logos(
        data = final_with_prob,
        aes(team_abbr = team),
        width = 0.04,
        hjust = 0
      )
    }
    
    # Add text labels for SEA and NE only
    sb_final <- final_data %>% filter(team %in% c("SEA", "NE"), !is.na(sb_win_prob))
    if (nrow(sb_final) > 0) {
      p <- p + geom_text(
        data = sb_final,
        aes(label = team, x = week + 0.5),
        hjust = 0,
        fontface = "bold",
        size = 4,
        show.legend = FALSE
      )
    }
  } else {
    # Manual colors
    all_teams <- unique(plot_data$team)
    team_colors <- setNames(
      c("#002244", "#C60C30", "#0B162A", "#0085CA", "#FFB612", 
        "#000000", "#4B92DB", "#A71930")[1:length(all_teams)],
      all_teams
    )
    team_colors["SEA"] <- "#002244"
    team_colors["NE"] <- "#C60C30"
    
    p <- p + scale_color_manual(values = team_colors, breaks = all_teams)
    
    # Add text labels for teams with probabilities
    final_with_prob <- final_data %>% filter(!is.na(sb_win_prob))
    if (nrow(final_with_prob) > 0) {
      p <- p + geom_text(
        data = final_with_prob,
        aes(label = team, x = week + 0.5),
        hjust = 0,
        fontface = "bold",
        size = 3.5,
        show.legend = FALSE
      )
    }
  }
  
  # Playoff label inside plot area
  p <- p + annotate(
    "text",
    x = playoff_week + 1,
    y = 0.95,
    label = "Playoffs",
    color = "gray30",
    size = 3.5,
    fontface = "italic"
  )
  
  return(p)
}

p1b <- create_sb_probability_plot(elo_history)

ggsave(
  "docs/figures/sb-probability-evolution.png",
  p1b,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/sb-probability-evolution.png\n")

# Save interactive version if plotly available
if (plotly_available) {
  p1b_interactive <- ggplotly(p1b, tooltip = c("team", "week", "sb_win_prob"))
  saveRDS(p1b_interactive, "docs/figures/sb-probability-interactive.rds")
  cat("  Saved: docs/figures/sb-probability-interactive.rds\n")
}

# ============================================================================
# Figure 2: Score Distribution Heatmap
# ============================================================================
cat("\nCreating Figure 2: Score Distribution Heatmap...\n")

create_score_heatmap <- function(score_sims) {
  # Create score combination frequency table
  score_freq <- score_sims %>%
    count(sea_score, ne_score) %>%
    mutate(probability = n / nrow(score_sims))
  
  # Filter to reasonable score range
  score_freq <- score_freq %>%
    filter(sea_score >= 10, sea_score <= 40,
           ne_score >= 10, ne_score <= 40)
  
  # Create heatmap
  p <- ggplot(score_freq, aes(x = sea_score, y = ne_score, fill = probability)) +
    geom_tile(color = "white", linewidth = 0.1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", 
                color = "gray40", linewidth = 0.8) +
    scale_fill_viridis_c(
      option = "plasma",  # Plasma color scheme - shows low values clearly on white background
      direction = 1,  # Normal direction (low = dark, high = bright)
      labels = percent_format(),
      name = "Probability",
      begin = 0.1,  # Start slightly into the scale to avoid pure black
      end = 0.95  # End before pure yellow for better contrast
    ) +
    scale_x_continuous(breaks = seq(10, 40, 5), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(10, 40, 5), expand = c(0, 0)) +
    labs(
      title = "Predicted Score Distribution",
      subtitle = "Probability of each score combination (10,000 simulations)",
      x = "Seattle Seahawks Score",
      y = "New England Patriots Score"
    ) +
    theme_sb() +
    theme(
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  return(p)
}

p2 <- create_score_heatmap(score_sims)

ggsave(
  "docs/figures/score-distribution.png",
  p2,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/score-distribution.png\n")

# ============================================================================
# Figure 3: Win Probability Bars
# ============================================================================
cat("\nCreating Figure 3: Win Probability Comparison...\n")

create_win_prob_bars <- function(predictions) {
  # Reshape data for plotting
  plot_data <- predictions %>%
    mutate(
      ne_win_prob = 1 - seahawks_win_prob
    ) %>%
    select(model, seahawks_win_prob, ne_win_prob) %>%
    pivot_longer(
      cols = c(seahawks_win_prob, ne_win_prob),
      names_to = "team",
      values_to = "probability"
    ) %>%
    mutate(
      team = case_when(
        team == "seahawks_win_prob" ~ "Seattle Seahawks",
        team == "ne_win_prob" ~ "New England Patriots"
      )
    )
  
  p <- ggplot(plot_data, aes(x = model, y = probability, fill = team)) +
    geom_col(position = "stack", width = 0.6) +
    geom_text(
      aes(label = percent(probability, accuracy = 0.1)),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 4.5
    ) +
    scale_y_continuous(
      labels = percent_format(),
      expand = c(0, 0),
      limits = c(0, 1)
    ) +
    scale_fill_manual(
      values = c(
        "Seattle Seahawks" = "#002244",  # Seahawks blue
        "New England Patriots" = "#C60C30"  # Patriots red
      )
    ) +
    labs(
      title = "Win Probability by Model",
      subtitle = "Comparison of Elo and Baseline model predictions",
      x = "Model",
      y = "Win Probability"
    ) +
    theme_sb() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      panel.grid.major.x = element_blank()
    ) +
    coord_flip()
  
  return(p)
}

p3 <- create_win_prob_bars(predictions)

ggsave(
  "docs/figures/win-probability-comparison.png",
  p3,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/win-probability-comparison.png\n")

# ============================================================================
# Figure 4: Score Prediction Comparison
# ============================================================================
cat("\nCreating Figure 4: Score Prediction Comparison...\n")

create_score_comparison <- function(predictions, baseline_summary) {
  # Prepare data with error bars
  plot_data <- predictions %>%
    left_join(
      baseline_summary %>%
        select(model, sea_score_q05, sea_score_q95, ne_score_q05, ne_score_q95),
      by = "model"
    ) %>%
    mutate(
      sea_score_lower = ifelse(model == "Baseline", sea_score_q05, predicted_sea_score - 3),
      sea_score_upper = ifelse(model == "Baseline", sea_score_q95, predicted_sea_score + 3),
      ne_score_lower = ifelse(model == "Baseline", ne_score_q05, predicted_ne_score - 3),
      ne_score_upper = ifelse(model == "Baseline", ne_score_q95, predicted_ne_score + 3)
    ) %>%
    select(model, predicted_sea_score, predicted_ne_score,
           sea_score_lower, sea_score_upper, ne_score_lower, ne_score_upper) %>%
    pivot_longer(
      cols = c(predicted_sea_score, predicted_ne_score),
      names_to = "team",
      values_to = "score"
    ) %>%
    mutate(
      team = case_when(
        team == "predicted_sea_score" ~ "Seattle Seahawks",
        team == "predicted_ne_score" ~ "New England Patriots"
      ),
      score_lower = ifelse(team == "Seattle Seahawks", sea_score_lower, ne_score_lower),
      score_upper = ifelse(team == "Seattle Seahawks", sea_score_upper, ne_score_upper)
    ) %>%
    select(model, team, score, score_lower, score_upper)
  
  p <- ggplot(plot_data, aes(x = model, y = score, fill = team)) +
    geom_col(position = "dodge", width = 0.6, alpha = 0.8) +
    geom_errorbar(
      aes(ymin = score_lower, ymax = score_upper),
      position = position_dodge(width = 0.6),
      width = 0.2,
      linewidth = 0.8,
      color = "gray30"
    ) +
    geom_text(
      aes(label = round(score)),
      position = position_dodge(width = 0.6),
      vjust = -0.5,
      fontface = "bold",
      size = 4
    ) +
    scale_fill_manual(
      values = c(
        "Seattle Seahawks" = "#002244",
        "New England Patriots" = "#C60C30"
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = "Predicted Scores by Model",
      subtitle = "With 90% confidence intervals (Baseline model) or ±3 points (Elo model)",
      x = "Model",
      y = "Predicted Score",
      fill = "Team"
    ) +
    theme_sb() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      panel.grid.major.x = element_blank()
    )
  
  return(p)
}

p4 <- create_score_comparison(predictions, baseline_summary)

ggsave(
  "docs/figures/score-comparison.png",
  p4,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/score-comparison.png\n")

# ============================================================================
# Figure 5: Social Media Hero Image
# ============================================================================
cat("\nCreating Figure 5: Social Media Hero Image...\n")

create_hero_image <- function(predictions, elo_prediction) {
  # Get average predictions
  avg_pred <- predictions %>%
    summarise(
      sea_win_prob = mean(seahawks_win_prob),
      sea_score = round(mean(predicted_sea_score)),
      ne_score = round(mean(predicted_ne_score))
    )
  
  # Create a clean, professional hero image
  p <- ggplot() +
    # Background
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, 
             fill = "white") +
    # Title
    annotate("text", x = 0.5, y = 0.85, 
             label = "SUPER BOWL LX",
             size = 14, fontface = "bold", hjust = 0.5, color = "#002244") +
    # Subtitle
    annotate("text", x = 0.5, y = 0.75,
             label = "Seattle Seahawks vs New England Patriots",
             size = 9, hjust = 0.5, color = "gray30") +
    # Date
    annotate("text", x = 0.5, y = 0.68,
             label = "February 8, 2026",
             size = 7, hjust = 0.5, color = "gray40", fontface = "italic") +
    # SEA section
    annotate("rect", xmin = 0.1, xmax = 0.45, ymin = 0.25, ymax = 0.55,
             fill = "#002244", alpha = 0.1, color = "#002244", linewidth = 2) +
    annotate("text", x = 0.275, y = 0.5,
             label = "SEATTLE SEAHAWKS",
             size = 8, fontface = "bold", hjust = 0.5, color = "#002244") +
    annotate("text", x = 0.275, y = 0.4,
             label = paste0(percent(avg_pred$sea_win_prob, accuracy = 0.1), " win probability"),
             size = 6, hjust = 0.5, color = "gray20") +
    annotate("text", x = 0.275, y = 0.3,
             label = paste0("Predicted: ", avg_pred$sea_score, " points"),
             size = 6, hjust = 0.5, color = "gray20", fontface = "bold") +
    # NE section
    annotate("rect", xmin = 0.55, xmax = 0.9, ymin = 0.25, ymax = 0.55,
             fill = "#C60C30", alpha = 0.1, color = "#C60C30", linewidth = 2) +
    annotate("text", x = 0.725, y = 0.5,
             label = "NEW ENGLAND PATRIOTS",
             size = 8, fontface = "bold", hjust = 0.5, color = "#C60C30") +
    annotate("text", x = 0.725, y = 0.4,
             label = paste0(percent(1 - avg_pred$sea_win_prob, accuracy = 0.1), " win probability"),
             size = 6, hjust = 0.5, color = "gray20") +
    annotate("text", x = 0.725, y = 0.3,
             label = paste0("Predicted: ", avg_pred$ne_score, " points"),
             size = 6, hjust = 0.5, color = "gray20", fontface = "bold") +
    # Footer
    annotate("text", x = 0.5, y = 0.1,
             label = "Predictions locked: February 7, 2026",
             size = 5, color = "gray50", hjust = 0.5, fontface = "italic") +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA))
  
  return(p)
}

p5 <- create_hero_image(predictions, elo_prediction)

ggsave(
  "docs/figures/predictions-hero.png",
  p5,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/predictions-hero.png\n")

# ============================================================================
# Figure 6: Points Scored Distribution
# ============================================================================
cat("\nCreating Figure 6: Offense Comparison...\n")

create_offense_comparison <- function(team_performance, league_avg) {
  # Order teams by points per game
  plot_data <- team_performance %>%
    arrange(points_per_game) %>%
    mutate(
      team = fct_reorder(team, points_per_game),
      fill_color = case_when(
        team == "SEA" ~ "SEA",
        team == "NE" ~ "NE",
        TRUE ~ "Other"
      )
    )
  
  p <- ggplot(plot_data, aes(x = points_per_game, y = team, fill = fill_color)) +
    geom_col(width = 0.7) +
    geom_vline(
      xintercept = league_avg,
      linetype = "dashed",
      color = "gray40",
      linewidth = 1
    ) +
    annotate("text", 
             x = league_avg, 
             y = length(unique(plot_data$team)) + 1,
             label = "League Average",
             hjust = -0.1,
             color = "gray40",
             size = 3.5) +
    scale_fill_manual(
      values = c(
        "SEA" = "#002244",
        "NE" = "#C60C30",
        "Other" = "gray70"
      ),
      guide = "none"
    ) +
    labs(
      title = "Points Per Game - 2025 Season",
      subtitle = "Seahawks and Patriots offensive performance",
      x = "Points Per Game",
      y = NULL
    ) +
    theme_sb() +
    theme(
      axis.text.y = element_text(size = 9),
      panel.grid.major.y = element_blank()
    )
  
  # Add team logos if available
  if (nflplotR_available) {
    sb_teams <- plot_data %>% filter(team %in% c("SEA", "NE"))
    p <- p + geom_nfl_logos(
      data = sb_teams,
      aes(team_abbr = team),
      width = 0.03,
      alpha = 0.9
    )
  }
  
  return(p)
}

p6 <- create_offense_comparison(team_performance, league_avg_offense)

ggsave(
  "docs/figures/offense-comparison.png",
  p6,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/offense-comparison.png\n")

# ============================================================================
# Figure 7: Defensive Performance
# ============================================================================
cat("\nCreating Figure 7: Defense Comparison...\n")

create_defense_comparison <- function(team_performance, league_avg) {
  # Order teams by points allowed (lower is better)
  plot_data <- team_performance %>%
    arrange(points_allowed_per_game) %>%
    mutate(
      team = fct_reorder(team, -points_allowed_per_game),  # Reverse for better display
      fill_color = case_when(
        team == "SEA" ~ "SEA",
        team == "NE" ~ "NE",
        TRUE ~ "Other"
      )
    )
  
  p <- ggplot(plot_data, aes(x = points_allowed_per_game, y = team, fill = fill_color)) +
    geom_col(width = 0.7) +
    geom_vline(
      xintercept = league_avg,
      linetype = "dashed",
      color = "gray40",
      linewidth = 1
    ) +
    annotate("text", 
             x = league_avg, 
             y = length(unique(plot_data$team)) + 1,
             label = "League Average",
             hjust = -0.1,
             color = "gray40",
             size = 3.5) +
    scale_fill_manual(
      values = c(
        "SEA" = "#002244",
        "NE" = "#C60C30",
        "Other" = "gray70"
      ),
      guide = "none"
    ) +
    labs(
      title = "Points Allowed Per Game - 2025 Season",
      subtitle = "Lower is better - Seahawks and Patriots defensive performance",
      x = "Points Allowed Per Game",
      y = NULL
    ) +
    theme_sb() +
    theme(
      axis.text.y = element_text(size = 9),
      panel.grid.major.y = element_blank()
    )
  
  # Add team logos if available
  if (nflplotR_available) {
    sb_teams <- plot_data %>% filter(team %in% c("SEA", "NE"))
    p <- p + geom_nfl_logos(
      data = sb_teams,
      aes(team_abbr = team),
      width = 0.03,
      alpha = 0.9
    )
  }
  
  return(p)
}

p7 <- create_defense_comparison(team_performance, league_avg_defense)

ggsave(
  "docs/figures/defense-comparison.png",
  p7,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/defense-comparison.png\n")

# ============================================================================
# Figure 8: Point Differential Scatter
# ============================================================================
cat("\nCreating Figure 8: Point Differential Comparison...\n")

create_point_differential_plot <- function(team_performance) {
  plot_data <- team_performance %>%
    mutate(
      is_sb_team = team %in% c("SEA", "NE"),
      # Calculate logo size based on whether it's a SB team
      logo_size = if_else(is_sb_team, 0.14, 0.10),
      logo_alpha = if_else(is_sb_team, 1.0, 0.7)
    )
  
  # Calculate quadrant boundaries for annotations
  x_max <- max(plot_data$points_per_game)+2
  x_min <- min(plot_data$points_per_game)-2
  y_max <- max(plot_data$points_allowed_per_game)+2
  y_min <- min(plot_data$points_allowed_per_game)-2
  
  # Calculate ranges for logo sizing
  x_range <- x_max - x_min
  y_range <- y_max - y_min
  
  p <- ggplot(plot_data, 
              aes(x = points_per_game, y = points_allowed_per_game)) +
    # Add quadrant lines at league average
    geom_vline(
      xintercept = league_avg_offense,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.8,
      alpha = 0.5
    ) +
    geom_hline(
      yintercept = league_avg_defense,
      linetype = "dashed",
      color = "gray40",
      linewidth = 0.8,
      alpha = 0.5
    ) +
    # Add quadrant labels
    annotate("text", 
             x = x_max - 0.5,
             y = y_min - 0.5,
             label = "Elite\n(High O, Low D)",
             hjust = 1,
             vjust = 0,
             color = "gray50",
             size = 3.5,
             fontface = "italic") +
    annotate("text",
             x = x_min + 0.5,
             y = y_min - 0.5,
             label = "Weak Offense\nStrong Defense",
             hjust = 0,
             vjust = 0,
             color = "gray50",
             size = 3.5,
             fontface = "italic") +
    annotate("text",
             x = x_max - 0.5,
             y = y_max - 1.0,
             label = "Strong Offense\nWeak Defense",
             hjust = 1,
             vjust = 1,
             color = "gray50",
             size = 3.5,
             fontface = "italic") +
    annotate("text",
             x = x_min + 0.5,
             y = y_max - 0.5,
             label = "Weak Overall",
             hjust = 0,
             vjust = 1,
             color = "gray50",
             size = 3.5,
             fontface = "italic") +
    scale_y_reverse() +  # Lower defense is better, so reverse y-axis
    labs(
      title = "Offensive vs Defensive Performance",
      subtitle = "2025 Season - Elite teams in top-right quadrant (high offense, low defense allowed)",
      x = "Points Scored Per Game",
      y = "Points Allowed Per Game (lower is better)",
      caption = "Data: nflverse"
    ) +
    theme_sb() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  # Add team logos as markers
  if (nflplotR_available) {
    # Add all team logos positioned at their data points
    # Use dynamic sizing based on data range for better visibility
    logo_width_base <- min(x_range, y_range) * 0.015  # Scale based on data range
    
    # Non-SB teams: smaller, semi-transparent logos
    p <- p + geom_nfl_logos(
      data = plot_data %>% filter(!is_sb_team),
      aes(x = points_per_game, y = points_allowed_per_game, team_abbr = team),
      width = logo_width_base * 1.4,
      alpha = 0.8
    ) +
    # SEA and NE: larger, fully opaque logos
    geom_nfl_logos(
      data = plot_data %>% filter(is_sb_team),
      aes(x = points_per_game, y = points_allowed_per_game, team_abbr = team),
      width = logo_width_base * 2.2,
      alpha = 1.0
    ) +
    # Add text labels next to SEA and NE for clarity
    geom_text(
      data = plot_data %>% filter(is_sb_team) %>%
        mutate(
          # Offset labels slightly to the right to avoid logo overlap
          label_x = points_per_game - 3*(x_range * 0.02)
        ),
      aes(x = label_x, y = points_allowed_per_game, label = team),
      hjust = 0,
      vjust = 0.5,
      fontface = "bold",
      size = 5,
      color = "gray10"
    )
  } else if (local_logos_available) {
    # Use local logo files with png package and grid
    library(grid)
    
    # Map team abbreviations to logo file names
    # Some teams have different abbreviations in data vs logo files
    team_logo_map <- c(
      "LA" = "lar",  # LA Rams
      "WAS" = "wsh",  # Washington
      "LV" = "lv"    # Las Vegas (already correct)
    )
    
    plot_data <- plot_data %>%
      mutate(
        logo_filename = if_else(
          team %in% names(team_logo_map),
          paste0(team_logo_map[team], ".png"),
          paste0(tolower(team), ".png")
        ),
        logo_path = file.path(logo_dir, logo_filename),
        logo_exists = file.exists(logo_path)
      )
    
    # Check for missing logos and report
    missing_logos <- plot_data %>% filter(!logo_exists) %>% pull(team)
    if (length(missing_logos) > 0) {
      cat(sprintf("  Warning: Missing logos for %d teams: %s\n", 
                  length(missing_logos), paste(missing_logos, collapse = ", ")))
      cat("  These teams will use text labels as fallback.\n")
    }
    
    logo_size_base <- min(x_range, y_range) * 0.015
    
    # Separate teams with and without logos
    teams_with_logos <- plot_data %>% filter(logo_exists)
    teams_without_logos <- plot_data %>% filter(!logo_exists)
    
    # Load and add logos using annotation_custom for teams with logos
    logo_layers <- list()
    
    for (i in 1:nrow(teams_with_logos)) {
      team <- teams_with_logos$team[i]
      x_pos <- teams_with_logos$points_per_game[i]
      y_pos <- teams_with_logos$points_allowed_per_game[i]
      is_sb <- teams_with_logos$is_sb_team[i]
      logo_file <- teams_with_logos$logo_path[i]
      
      # Read the PNG image
      img <- tryCatch({
        readPNG(logo_file)
      }, error = function(e) {
        cat(sprintf("  Error loading logo for %s: %s\n", team, e$message))
        NULL
      })
      
      if (!is.null(img)) {
        # Calculate size
        size <- if (is_sb) logo_size_base * 1.8 else logo_size_base * 1.2
        alpha_val <- if (is_sb) 1.0 else 0.7
        
        # Apply alpha to image if needed
        if (alpha_val < 1.0 && length(dim(img)) == 3) {
          if (dim(img)[3] == 4) {
            # Has alpha channel, modify it
            img[,,4] <- img[,,4] * alpha_val
          } else if (dim(img)[3] == 3) {
            # RGB only, add alpha channel
            if (requireNamespace("abind", quietly = TRUE)) {
              alpha_layer <- array(alpha_val, dim = c(dim(img)[1:2], 1))
              img <- abind::abind(img, alpha_layer, along = 3)
            }
          }
        }
        
        # Create raster grob (annotation_custom uses data coordinates)
        g <- rasterGrob(img, interpolate = TRUE)
        
        # Add as annotation using data coordinates
        logo_layers[[length(logo_layers) + 1]] <- annotation_custom(
          g,
          xmin = x_pos - size,
          xmax = x_pos + size,
          ymin = y_pos - size,
          ymax = y_pos + size
        )
      } else {
        # If logo failed to load, add to teams_without_logos for fallback
        teams_without_logos <- bind_rows(
          teams_without_logos,
          teams_with_logos[i, ]
        )
      }
    }
    
    # Add all logo layers to the plot
    for (layer in logo_layers) {
      p <- p + layer
    }
    
    # Add text labels for teams without logos (fallback)
    if (nrow(teams_without_logos) > 0) {
      p <- p + geom_text(
        data = teams_without_logos %>% filter(!is_sb_team),
        aes(x = points_per_game, y = points_allowed_per_game, label = team),
        hjust = 0.5,
        vjust = 0.5,
        fontface = "bold",
        size = 3,
        color = "gray40",
        bg.color = "white",
        bg.r = 0.1
      ) +
      geom_text(
        data = teams_without_logos %>% filter(is_sb_team),
        aes(x = points_per_game, y = points_allowed_per_game, label = team),
        hjust = 0.5,
        vjust = 0.5,
        fontface = "bold",
        size = 4,
        color = if_else(teams_without_logos %>% filter(is_sb_team) %>% pull(team) == "SEA", 
                       "#002244", "#C60C30")
      )
    }
    
    # Add text labels for SEA and NE (always show these)
    p <- p + geom_text(
      data = plot_data %>% filter(is_sb_team) %>%
        mutate(label_x = points_per_game + (x_range * 0.02)),
      aes(x = label_x, y = points_allowed_per_game, label = team),
      hjust = 0,
      vjust = 0.5,
      fontface = "bold",
      size = 5,
      color = "gray10"
    )
    
    # Verify all teams are accounted for
    total_plotted <- length(logo_layers) + nrow(teams_without_logos)
    if (total_plotted < nrow(plot_data)) {
      cat(sprintf("  Warning: Only %d/%d teams plotted. Missing teams may not have data points.\n",
                  total_plotted, nrow(plot_data)))
    } else {
      cat(sprintf("  Successfully plotted all %d teams.\n", nrow(plot_data)))
    }
  } else {
    # Enhanced fallback: styled team abbreviations as "logo-like" markers
    # Use colored circles with team abbreviations that look professional
    logo_size_base <- min(x_range, y_range) * 0.015
    
    p <- p + 
    # Background circles for all teams (white with gray border)
    geom_point(
      data = plot_data %>% filter(!is_sb_team),
      aes(x = points_per_game, y = points_allowed_per_game),
      color = "gray80",
      fill = "white",
      size = logo_size_base * 50,
      alpha = 0.8,
      shape = 21,
      stroke = 1.2
    ) +
    # Background circles for SEA and NE (colored borders, white fill)
    geom_point(
      data = plot_data %>% filter(is_sb_team),
      aes(x = points_per_game, y = points_allowed_per_game, color = team),
      fill = "white",
      size = logo_size_base * 80,
      alpha = 1,
      shape = 21,
      stroke = 3
    ) +
    scale_color_manual(
      values = c("SEA" = "#002244", "NE" = "#C60C30"),
      guide = "none"
    ) +
    # Team abbreviations for all teams (small, gray)
    geom_text(
      data = plot_data %>% filter(!is_sb_team),
      aes(x = points_per_game, y = points_allowed_per_game, label = team),
      hjust = 0.5,
      vjust = 0.5,
      size = logo_size_base * 200,
      fontface = "bold",
      color = "gray30",
      family = "sans"
    ) +
    # Emphasized SEA and NE labels (larger, colored, bold)
    geom_text(
      data = plot_data %>% filter(is_sb_team),
      aes(x = points_per_game, y = points_allowed_per_game, label = team, color = team),
      hjust = 0.5,
      vjust = 0.5,
      fontface = "bold",
      size = logo_size_base * 350,
      family = "sans"
    ) +
    scale_color_manual(
      values = c("SEA" = "#002244", "NE" = "#C60C30"),
      guide = "none"
    )
  }
  
  return(p)
}

p8 <- create_point_differential_plot(team_performance)

ggsave(
  "docs/figures/point-differential-comparison.png",
  p8,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/point-differential-comparison.png\n")

# ============================================================================
# Figure 9: Four-Panel Performance Dashboard
# ============================================================================
cat("\nCreating Figure 9: Performance Dashboard...\n")

create_performance_dashboard <- function(team_performance, league_avg_off, league_avg_def) {
  # Helper function to add logos to bar charts
  add_logos_to_bars <- function(p, plot_data) {
    if (nflplotR_available) {
      # Add logos at the end of each bar
      p <- p + geom_nfl_logos(
        data = plot_data,
        aes(x = max(plot_data[[names(plot_data)[1]]], na.rm = TRUE) * 0.98,
            y = team,
            team_abbr = team),
        width = 0.02,
        alpha = 0.8,
        hjust = 1
      )
    }
    return(p)
  }
  
  # Panel 1: Top 10 Offenses
  top_offense <- team_performance %>%
    arrange(desc(points_per_game)) %>%
    slice_head(n = 10) %>%
    mutate(
      team = fct_reorder(team, points_per_game),
      fill_color = case_when(
        team == "SEA" ~ "SEA",
        team == "NE" ~ "NE",
        TRUE ~ "Other"
      )
    )
  
  p1 <- ggplot(top_offense, aes(x = points_per_game, y = team, fill = fill_color)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("SEA" = "#002244", "NE" = "#C60C30", "Other" = "gray70"), guide = "none") +
    labs(title = "Top 10 Offenses", x = "PPG", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(axis.text.y = element_text(size = 8))
  
  if (nflplotR_available) {
    p1 <- p1 + geom_nfl_logos(
      data = top_offense,
      aes(x = points_per_game * 0.98, y = team, team_abbr = team),
      width = 0.02,
      alpha = 0.8,
      hjust = 1
    )
  }
  
  # Panel 2: Top 10 Defenses (lowest points allowed)
  top_defense <- team_performance %>%
    arrange(points_allowed_per_game) %>%
    slice_head(n = 10) %>%
    mutate(
      team = fct_reorder(team, -points_allowed_per_game),
      fill_color = case_when(
        team == "SEA" ~ "SEA",
        team == "NE" ~ "NE",
        TRUE ~ "Other"
      )
    )
  
  p2 <- ggplot(top_defense, aes(x = points_allowed_per_game, y = team, fill = fill_color)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("SEA" = "#002244", "NE" = "#C60C30", "Other" = "gray70"), guide = "none") +
    labs(title = "Top 10 Defenses", x = "PPG Allowed", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(axis.text.y = element_text(size = 8))
  
  if (nflplotR_available) {
    p2 <- p2 + geom_nfl_logos(
      data = top_defense,
      aes(x = points_allowed_per_game * 0.98, y = team, team_abbr = team),
      width = 0.02,
      alpha = 0.8,
      hjust = 1
    )
  }
  
  # Panel 3: Point Differential
  top_diff <- team_performance %>%
    arrange(desc(point_differential)) %>%
    slice_head(n = 10) %>%
    mutate(
      team = fct_reorder(team, point_differential),
      fill_color = case_when(
        team == "SEA" ~ "SEA",
        team == "NE" ~ "NE",
        TRUE ~ "Other"
      )
    )
  
  p3 <- ggplot(top_diff, aes(x = point_differential, y = team, fill = fill_color)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_fill_manual(values = c("SEA" = "#002244", "NE" = "#C60C30", "Other" = "gray70"), guide = "none") +
    labs(title = "Top 10 Point Differential", x = "Point Diff", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(axis.text.y = element_text(size = 8))
  
  if (nflplotR_available) {
    p3 <- p3 + geom_nfl_logos(
      data = top_diff,
      aes(x = point_differential * 0.98, y = team, team_abbr = team),
      width = 0.02,
      alpha = 0.8,
      hjust = 1
    )
  }
  
  # Panel 4: Combined ranking (offense rank + defense rank)
  combined_rank <- team_performance %>%
    mutate(
      off_rank = rank(-points_per_game),
      def_rank = rank(points_allowed_per_game),
      combined_rank = off_rank + def_rank
    ) %>%
    arrange(combined_rank) %>%
    slice_head(n = 10) %>%
    mutate(
      team = fct_reorder(team, -combined_rank),
      fill_color = case_when(
        team == "SEA" ~ "SEA",
        team == "NE" ~ "NE",
        TRUE ~ "Other"
      )
    )
  
  p4 <- ggplot(combined_rank, aes(x = combined_rank, y = team, fill = fill_color)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("SEA" = "#002244", "NE" = "#C60C30", "Other" = "gray70"), guide = "none") +
    labs(title = "Top 10 Combined Rank", x = "Rank Sum", y = NULL, 
         subtitle = "Lower is better (off rank + def rank)") +
    theme_minimal(base_size = 10) +
    theme(axis.text.y = element_text(size = 8))
  
  if (nflplotR_available) {
    p4 <- p4 + geom_nfl_logos(
      data = combined_rank,
      aes(x = combined_rank * 0.98, y = team, team_abbr = team),
      width = 0.02,
      alpha = 0.8,
      hjust = 1
    )
  }
  
  # Combine with patchwork
  dashboard <- (p1 | p2) / (p3 | p4) +
    plot_annotation(
      title = "2025 Season Performance Dashboard",
      subtitle = "Seahawks and Patriots among league's elite teams"
    ) &
    theme(plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray40"))
  
  return(dashboard)
}

p9 <- create_performance_dashboard(team_performance, league_avg_offense, league_avg_defense)

ggsave(
  "docs/figures/performance-dashboard.png",
  p9,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)
cat("  Saved: docs/figures/performance-dashboard.png\n")

# ============================================================================
# Figure 10: Elo Rankings Through Season (by Week)
# ============================================================================
# NOTE: This plot is now created by the separate script R/05b-create-elo-rankings-plot.R
# Commented out to avoid duplication
# cat("\nCreating Figure 10: Elo Rankings by Week...\n")
# 
# create_elo_rankings_plot <- function(elo_history) {
#   # Filter to 2025 season only
#   elo_2025 <- elo_history %>%
#     filter(season == 2025) %>%
#     mutate(date = as.Date(date))
#   
#   # Get top 8 teams by final Elo rating
#   playoff_teams <- elo_2025 %>%
#     group_by(team) %>%
#     filter(week == max(week)) %>%
#     ungroup() %>%
#     arrange(desc(elo)) %>%
#     slice_head(n = 8) %>%
#     pull(team)
#   
#   cat(sprintf("  Top 8 teams by final Elo: %s\n", paste(playoff_teams, collapse = ", ")))
#   
#   # Filter to top 8 teams and get Elo ratings by week
#   plot_data <- elo_2025 %>%
#     filter(team %in% playoff_teams) %>%
#     # Get one record per team per week (use the latest date for each week)
#     group_by(team, week) %>%
#     filter(date == max(date)) %>%
#     ungroup() %>%
#     mutate(
#       is_sb_team = team %in% c("SEA", "NE"),
#       line_size = if_else(is_sb_team, 2, 0.8),
#       line_alpha = if_else(is_sb_team, 1, 0.5)
#     )
#   
#   # Find when playoffs begin (first playoff week)
#   schedules_2025 <- read_csv("data/processed/schedules_2015_2025.csv", show_col_types = FALSE)
#   playoff_week <- schedules_2025 %>%
#     filter(season == 2025) %>%
#     filter(grepl("POST|WC|DIV|CONF|SB", game_type, ignore.case = TRUE)) %>%
#     pull(week) %>%
#     min(na.rm = TRUE)
#   
#   if (is.na(playoff_week)) {
#     playoff_week <- 18  # Default to week 18 if not found
#   }
#   
#   cat(sprintf("  Playoffs begin at week %d\n", playoff_week))
#   
#   # Create the plot
#   p <- ggplot(plot_data, aes(x = week, y = elo, color = team, group = team)) +
#     # Shade playoff region
#     annotate("rect",
#              xmin = playoff_week - 0.5,
#              xmax = max(plot_data$week) + 0.5,
#              ymin = -Inf,
#              ymax = Inf,
#              alpha = 0.15,
#              fill = "blue") +
#     # Add dotted vertical line for playoff start
#     geom_vline(xintercept = playoff_week,
#                linetype = "dashed",
#                color = "blue",
#                linewidth = 1,
#                alpha = 0.7) +
#     # Add lines for all teams
#     geom_line(
#       data = plot_data %>% filter(!is_sb_team),
#       aes(linewidth = line_size, alpha = line_alpha),
#       lineend = "round"
#     ) +
#     # Emphasize SEA and NE lines
#     geom_line(
#       data = plot_data %>% filter(is_sb_team),
#       aes(linewidth = line_size, alpha = line_alpha),
#       lineend = "round"
#     ) +
#     scale_size_identity() +
#     scale_alpha_identity() +
#     labs(
#       title = "Elo Rating Rankings - Top 8 Playoff Teams",
#       subtitle = "2025 Season | Playoff period shaded",
#       x = "Week",
#       y = "Elo Rating",
#       caption = "Dashed line indicates start of playoffs"
#     ) +
#     theme_sb() +
#     theme(
#       axis.text.x = element_text(size = 11),
#       panel.grid.minor.x = element_blank(),
#       legend.position = "none"  # Remove legend
#     )
#   
#   # Add team colors
#   if (nflplotR_available) {
#     p <- p + scale_color_nfl(type = "primary")
#   } else {
#     # Manual colors
#     all_teams <- unique(plot_data$team)
#     team_colors <- setNames(
#       c("#002244", "#C60C30", "#0B162A", "#0085CA", "#FFB612", 
#         "#000000", "#4B92DB", "#A71930")[1:length(all_teams)],
#       all_teams
#     )
#     team_colors["SEA"] <- "#002244"
#     team_colors["NE"] <- "#C60C30"
#     
#     p <- p + scale_color_manual(
#       values = team_colors,
#       breaks = all_teams
#     )
#   }
#   
#   # Get data for logo placement
#   max_week <- max(plot_data$week)
#   min_week <- min(plot_data$week)
#   
#   # Left side: Get first week data for non-SB teams
#   left_label_data <- plot_data %>%
#     filter(week == min_week, !is_sb_team) %>%
#     arrange(desc(elo)) %>%
#     mutate(
#       label_x = min_week - 1.5,  # Position to the left
#       label_y = elo
#     )
#   
#   # Right side: Get final week data for SEA and NE
#   right_label_data <- plot_data %>%
#     filter(week == max_week, is_sb_team) %>%
#     arrange(desc(elo)) %>%
#     mutate(
#       label_x = max_week + 1.5,  # Position to the right
#       label_y = elo
#     )
#   
#   # Add logos on LEFT for non-SB teams
#   if (nflplotR_available && local_logos_available) {
#     # Use nflplotR for logos
#     if (nrow(left_label_data) > 0) {
#       p <- p + geom_nfl_logos(
#         data = left_label_data,
#         aes(x = label_x, y = label_y, team_abbr = team),
#         width = 0.4,
#         alpha = 0.8,
#         hjust = 1  # Right-align (logo on left side of point)
#       )
#     }
#     
#     # Add logos on RIGHT for SEA and NE
#     if (nrow(right_label_data) > 0) {
#       p <- p + geom_nfl_logos(
#         data = right_label_data,
#         aes(x = label_x, y = label_y, team_abbr = team),
#         width = 0.5,
#         alpha = 0.9,
#         hjust = 0  # Left-align (logo on right side of point)
#       )
#     }
#   } else if (local_logos_available) {
#     # Use local logos with grid
#     library(grid)
#     
#     # Map team abbreviations to logo file names
#     team_logo_map <- c(
#       "LA" = "lar",
#       "WAS" = "wsh",
#       "LV" = "lv"
#     )
#     
#     # Add logos on LEFT for non-SB teams
#     for (i in 1:nrow(left_label_data)) {
#       team <- left_label_data$team[i]
#       logo_filename <- if_else(
#         team %in% names(team_logo_map),
#         paste0(team_logo_map[team], ".png"),
#         paste0(tolower(team), ".png")
#       )
#       logo_file <- file.path(logo_dir, logo_filename)
#       
#       if (file.exists(logo_file)) {
#         img <- tryCatch({
#           readPNG(logo_file)
#         }, error = function(e) NULL)
#         
#         if (!is.null(img)) {
#           g <- rasterGrob(img, interpolate = TRUE)
#           logo_size <- 0.35
#           p <- p + annotation_custom(
#             g,
#             xmin = left_label_data$label_x[i] - logo_size,
#             xmax = left_label_data$label_x[i] + logo_size,
#             ymin = left_label_data$label_y[i] - logo_size,
#             ymax = left_label_data$label_y[i] + logo_size
#           )
#         }
#       }
#     }
#     
#     # Add logos on RIGHT for SEA and NE
#     for (i in 1:nrow(right_label_data)) {
#       team <- right_label_data$team[i]
#       logo_file <- file.path(logo_dir, paste0(tolower(team), ".png"))
#       
#       if (file.exists(logo_file)) {
#         img <- tryCatch({
#           readPNG(logo_file)
#         }, error = function(e) NULL)
#         
#         if (!is.null(img)) {
#           g <- rasterGrob(img, interpolate = TRUE)
#           logo_size <- 0.4
#           p <- p + annotation_custom(
#             g,
#             xmin = right_label_data$label_x[i] - logo_size,
#             xmax = right_label_data$label_x[i] + logo_size,
#             ymin = right_label_data$label_y[i] - logo_size,
#             ymax = right_label_data$label_y[i] + logo_size
#           )
#         }
#       }
#     }
#   } else {
#     # Fallback: text labels
#     if (nrow(left_label_data) > 0) {
#       p <- p + geom_text(
#         data = left_label_data,
#         aes(x = label_x, y = label_y, label = team, color = team),
#         hjust = 1,
#         vjust = 0.5,
#         fontface = "bold",
#         size = 3
#       )
#     }
#     
#     if (nrow(right_label_data) > 0) {
#       p <- p + geom_text(
#         data = right_label_data,
#         aes(x = label_x, y = label_y, label = team, color = team),
#         hjust = 0,
#         vjust = 0.5,
#         fontface = "bold",
#         size = 4
#       )
#     }
#   }
#   
#   # Add playoff label
#   p <- p + annotate("text",
#                     x = playoff_week,
#                     y = max(plot_data$elo) * 0.98,
#                     label = "Playoffs Begin",
#                     hjust = -0.1,
#                     color = "blue",
#                     size = 3.5,
#                     fontface = "italic",
#                     angle = 90)
#   
#   # Expand x-axis to accommodate labels (space on both left and right)
#   max_week_plot <- max(plot_data$week)
#   min_week_plot <- min(plot_data$week)
#   p <- p + scale_x_continuous(
#     breaks = seq(1, max_week_plot, by = 2),
#     minor_breaks = NULL,
#     limits = c(min_week_plot - 3, max_week_plot + 3),
#     expand = expansion(mult = c(0.1, 0.1))  # Space on both sides for logos
#   )
#   
#   return(p)
# }

# NOTE: Elo rankings plot is now created by separate script R/05b-create-elo-rankings-plot.R
# p10 <- create_elo_rankings_plot(elo_history)
# 
# ggsave(
#   "docs/figures/elo-rankings-by-week.png",
#   p10,
#   width = 12,
#   height = 7,
#   dpi = 300,
#   bg = "white"
# )
# cat("  Saved: docs/figures/elo-rankings-by-week.png\n")

# ============================================================================
# Testing and Validation
# ============================================================================
cat("\n=== VALIDATION ===\n")

expected_files <- c(
  "docs/figures/elo-ratings-evolution.png",
  "docs/figures/sb-probability-evolution.png",
  "docs/figures/score-distribution.png",
  "docs/figures/win-probability-comparison.png",
  "docs/figures/score-comparison.png",
  "docs/figures/predictions-hero.png",
  "docs/figures/offense-comparison.png",
  "docs/figures/defense-comparison.png",
  "docs/figures/point-differential-comparison.png",
  "docs/figures/performance-dashboard.png"
  # "docs/figures/elo-rankings-by-week.png"  # Created by R/05b-create-elo-rankings-plot.R
)

all_created <- TRUE
for (file in expected_files) {
  if (file.exists(file)) {
    file_info <- file.info(file)
    cat(sprintf("✓ %s (%.1f KB)\n", basename(file), file_info$size / 1024))
  } else {
    cat(sprintf("✗ MISSING: %s\n", basename(file)))
    all_created <- FALSE
  }
}

if (plotly_available && file.exists("docs/figures/sb-probability-interactive.rds")) {
  cat("✓ sb-probability-interactive.rds\n")
}

cat("\n=== FIGURE PREVIEWS ===\n")
cat("All figures created successfully!\n")
cat("Check docs/figures/ for output files.\n")
cat("\nTo preview plots in RStudio, run:\n")
cat("  print(p1a) # Elo ratings evolution\n")
cat("  print(p1b) # Super Bowl probability evolution\n")
cat("  print(p2)  # Score heatmap\n")
cat("  print(p3)  # Win probability bars\n")
cat("  print(p4)  # Score comparison\n")
cat("  print(p5)  # Hero image\n")
cat("  print(p6)  # Offense comparison\n")
cat("  print(p7)  # Defense comparison\n")
cat("  print(p8)  # Point differential\n")
cat("  print(p9)  # Performance dashboard\n")
# cat("  print(p10) # Elo rankings by week\n")  # Created by R/05b-create-elo-rankings-plot.R

if (all_created) {
  cat("\n✓ All visualizations created successfully!\n")
} else {
  cat("\n⚠ Some files may be missing. Check errors above.\n")
}
