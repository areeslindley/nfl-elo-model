# 05b-create-elo-rankings-plot.R
# Standalone script to create Elo Rankings by Week plot
# Author: Super Bowl LX Prediction Project
# Date: 2026-02-07

library(tidyverse)
library(scales)

# Check for optional packages
if (requireNamespace("nflplotR", quietly = TRUE)) {
  library(nflplotR)
  nflplotR_available <- TRUE
} else {
  nflplotR_available <- FALSE
  cat("Note: nflplotR not available. Using fallback methods.\n")
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
  library(grid)
} else {
  cat("Note: No local logos found. Using text labels as fallback.\n")
}

# Custom theme
theme_sb <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# Create output directory
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load Data
# ============================================================================
cat("Loading Elo history data...\n")
elo_history <- read_csv("data/output/elo_history.csv", show_col_types = FALSE)

# ============================================================================
# Create Elo Rankings Plot
# ============================================================================
cat("\nCreating Elo Rankings by Week plot...\n")

# Filter to 2025 season only
elo_2025 <- elo_history %>%
  filter(season == 2025) %>%
  mutate(date = as.Date(date))

# Get top 8 teams by final Elo rating
# Note: CHI (Bears) made it to divisional round, so they replace DET in top 8
# Note: SF (49ers) made it to divisional round, so they replace PHI in top 8
top_teams_by_elo <- elo_2025 %>%
  group_by(team) %>%
  filter(week == max(week)) %>%
  ungroup() %>%
  arrange(desc(elo)) %>%
  slice_head(n = 8) %>%
  pull(team)

# Replace DET with CHI and PHI with SF since they made it to divisional round
playoff_teams <- top_teams_by_elo
if ("DET" %in% playoff_teams) {
  playoff_teams <- c(setdiff(playoff_teams, "DET"), "CHI")
}
if ("PHI" %in% playoff_teams) {
  playoff_teams <- c(setdiff(playoff_teams, "PHI"), "SF")
}

# Add PHI (Eagles) and KC (Chiefs) as last year's Super Bowl participants
# PHI won last year's Super Bowl against KC
playoff_teams <- c(playoff_teams, "PHI", "KC")

cat(sprintf("  Teams included: %s\n", paste(playoff_teams, collapse = ", ")))
cat(sprintf("  (Top 8 playoff teams + PHI and KC as last year's Super Bowl participants)\n"))

# Load schedule data to find eliminations
schedules_2025 <- read_csv("data/processed/schedules_2015_2025.csv", show_col_types = FALSE)

# Find elimination weeks for each team from playoff game results
sb_teams <- c("SEA", "NE")

elimination_from_games <- schedules_2025 %>%
  filter(season == 2025) %>%
  filter(grepl("POST|WC|DIV|CONF|CON|SB", game_type, ignore.case = TRUE)) %>%
  mutate(
    winner = if_else(away_score > home_score, away_team, home_team),
    loser = if_else(away_score > home_score, home_team, away_team)
  ) %>%
  select(week, loser) %>%
  distinct() %>%
  rename(team = loser, elimination_week = week) %>%
  # Filter to only our playoff teams
  filter(team %in% playoff_teams)

# Create elimination data for all playoff teams
# Teams that made it to Super Bowl have no elimination week
elimination_data <- tibble(team = playoff_teams) %>%
  left_join(elimination_from_games, by = "team") %>%
  # SEA and NE don't have elimination weeks (they made it to Super Bowl)
  # KC didn't make playoffs, so eliminated at end of regular season (week 18)
  mutate(
    elimination_week = case_when(
      team %in% sb_teams ~ NA_real_,  # Super Bowl teams
      team == "KC" ~ 18,  # Chiefs didn't make playoffs (end of regular season)
      team == "PHI" ~ 19,  # Eagles eliminated in Wild Card (lost to SF)
      TRUE ~ elimination_week
    )
  )

# For any teams still missing elimination weeks, infer from playoff structure
# If a team is in top 8 but not SEA/NE and doesn't have elimination data,
# they must have been eliminated in Conference Championship (week 21)
# since that's the last round before Super Bowl
missing_eliminations <- elimination_data %>%
  filter(is.na(elimination_week) & !team %in% sb_teams)

if (nrow(missing_eliminations) > 0) {
  cat(sprintf("  Note: %d teams missing elimination data: %s\n",
              nrow(missing_eliminations),
              paste(missing_eliminations$team, collapse = ", ")))
  cat("  Inferring elimination at Conference Championship (week 21).\n")
  # Set elimination week to 21 (Conference Championship) for missing teams
  elimination_data <- elimination_data %>%
    mutate(
      elimination_week = if_else(
        is.na(elimination_week) & !team %in% sb_teams,
        21,  # Conference Championship week
        elimination_week
      )
    )
}

# Filter to top 8 teams and get Elo ratings by week
plot_data <- elo_2025 %>%
  filter(team %in% playoff_teams) %>%
  # Get one record per team per week (use the latest date for each week)
  group_by(team, week) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  # Add elimination week
  left_join(elimination_data, by = "team") %>%
  # For SB teams (SEA, NE), no elimination week (they made it to Super Bowl)
  mutate(
    is_sb_team = team %in% c("SEA", "NE"),
    # Stop line at elimination week (or continue to end for SB teams)
    max_week_for_line = if_else(is.na(elimination_week), max(week), elimination_week),
    # Filter out weeks after elimination
    include_in_line = week <= max_week_for_line
  ) %>%
  filter(include_in_line) %>%
  mutate(
    line_size = if_else(is_sb_team, 0.55, 0.5),  # Same thickness for all teams now
    line_alpha = if_else(is_sb_team, 0.7, 0.5),  # Reduced from 1 to 0.75 for SEA/NE
    # Mark elimination points
    is_elimination = !is.na(elimination_week) & week == elimination_week
  )

# Get elimination points for X markers
elimination_points <- plot_data %>%
  filter(is_elimination) %>%
  select(team, week, elo, elimination_week)

cat(sprintf("  Teams eliminated: %s\n", 
            paste(elimination_points %>% pull(team) %>% unique(), collapse = ", ")))

# Find when playoffs begin (first playoff week)
schedules_2025 <- read_csv("data/processed/schedules_2015_2025.csv", show_col_types = FALSE)
playoff_week <- schedules_2025 %>%
  filter(season == 2025) %>%
  filter(grepl("POST|WC|DIV|CONF|SB", game_type, ignore.case = TRUE)) %>%
  pull(week) %>%
  min(na.rm = TRUE)

if (is.na(playoff_week)) {
  playoff_week <- 18  # Default to week 18 if not found
}

cat(sprintf("  Playoffs begin at week %d\n", playoff_week))

# Create the plot
p <- ggplot(plot_data, aes(x = week, y = elo, color = team, group = team)) +
  # Shade playoff region
  annotate("rect",
           xmin = playoff_week - 0.5,
           xmax = max(plot_data$week) + 0.5,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.15,
           fill = "blue") +
  # Add dotted vertical line for playoff start
  geom_vline(xintercept = playoff_week,
             linetype = "dashed",
             color = "blue",
             linewidth = 1,
             alpha = 0.7) +
  # Add lines for all teams (striped/dashed for non-SB teams, solid for SB teams)
  geom_line(
    data = plot_data %>% filter(!is_sb_team),
    aes(linewidth = line_size, alpha = line_alpha),
    linetype = "dotted",  # Striped/dashed for eliminated teams
    lineend = "round"
  ) +
  # Emphasize SEA and NE lines (solid)
  geom_line(
    data = plot_data %>% filter(is_sb_team),
    aes(linewidth = line_size, alpha = line_alpha),
    linetype = "solid",  # Solid for Super Bowl teams
    lineend = "round"
  ) +
  # Add X markers at elimination points
  geom_point(
    data = elimination_points,
    aes(x = week, y = elo),
    shape = 4,  # X marker
    size = 2,
    stroke = 2,
    color = "black",
    alpha = 0.8
  ) +
  scale_size_identity() +
  scale_alpha_identity() +
  labs(
    title = "Elo Rating History - Top 8 Playoff Teams and Last Year's Super Bowl Participants",
    subtitle = "2025-2026 Season",
    x = "Week",
    y = "Elo Rating",
    color = "Team",
    caption = "Data: nflverse"
  ) +
  theme_sb() +
  theme(
    axis.text.x = element_text(size = 11),
    panel.grid.minor.x = element_blank(),
    legend.position = "none",  # Show legend on the right
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.3, "cm")
  )

# Add team colors (matching user specifications)
all_teams <- unique(plot_data$team)
team_colors <- c(
  "SF" = "#AA0000",       # 49ers - red and gold (using red)
  "CHI" = "#0B162A",      # Bears - navy blue and orange (using navy)
  "BUF" = "#00338D",      # Buffalo - royal blue and red (using royal blue)
  "LA" = "#FFA300",       # Rams - dark blue and golden yellow (using golden yellow for visibility)
  "DEN" = "#FB4F14",      # Broncos - orange
  "HOU" = "navyblue",      # Texans - red and navy (using red)
  "SEA" = "#69BE28",      # Seahawks - lime green and navy (using lime green)
  "NE" = "#C60C30",       # Patriots - red and navy and white (using red)
  "PHI" = "#004C54",      # Eagles - green and grey (using green)
  "KC" = "#E31837"        # Chiefs - red and gold (using red)
)

# Ensure all teams have colors (fallback for any missing)
for (team in all_teams) {
  if (!team %in% names(team_colors)) {
    team_colors[team] <- "#666666"  # Gray fallback
  }
}

p <- p + scale_color_manual(
  values = team_colors[all_teams],
  breaks = all_teams
)

# Get data for logo placement
max_week <- max(plot_data$week)
min_week <- min(plot_data$week)

# Left side: Get first week data for non-SB teams
left_label_data <- plot_data %>%
  filter(week == min_week, !is_sb_team) %>%
  arrange(desc(elo)) %>%
  mutate(
    # Default position, with offset for teams that might overlap
    label_x = case_when(
      team %in% c("DEN", "SF") ~ min_week - 3.0,  # Move DEN and SF further left
      TRUE ~ min_week - 1.5  # Standard position for other teams
    ),
    label_y = elo
  )

# Right side: Get final week data for SEA and NE
right_label_data <- plot_data %>%
  filter(week == max_week, is_sb_team) %>%
  arrange(desc(elo)) %>%
  mutate(
    label_x = max_week + 1.5,  # Position to the right
    label_y = elo
  )

cat(sprintf("  Left side logos: %d teams\n", nrow(left_label_data)))
cat(sprintf("  Right side logos: %d teams\n", nrow(right_label_data)))

# Add logos on LEFT for non-SB teams
if (nflplotR_available && local_logos_available) {
  cat("  Using nflplotR for logos...\n")
  # Use nflplotR for logos
  if (nrow(left_label_data) > 0) {
    p <- p + geom_nfl_logos(
      data = left_label_data,
      aes(x = label_x, y = label_y, team_abbr = team),
      width = 0.25,
      alpha = 0.8,
      hjust = 1  # Right-align (logo on left side of point)
    )
    cat("    Added left logos via nflplotR\n")
  }
  
  # Add logos on RIGHT for SEA and NE
  if (nrow(right_label_data) > 0) {
    p <- p + geom_nfl_logos(
      data = right_label_data,
      aes(x = label_x, y = label_y, team_abbr = team),
      width = 0.5,
      alpha = 0.9,
      hjust = 0  # Left-align (logo on right side of point)
    )
    cat("    Added right logos via nflplotR\n")
  }
} else if (local_logos_available) {
  cat("  Using local logo files...\n")
  # Use local logos with grid
  # Map team abbreviations to logo file names
  team_logo_map <- c(
    "LA" = "lar",
    "WAS" = "wsh",
    "LV" = "lv"
  )
  
  # Add logos on LEFT for non-SB teams
  left_logos_added <- 0
  for (i in 1:nrow(left_label_data)) {
    team <- left_label_data$team[i]
    logo_filename <- if_else(
      team %in% names(team_logo_map),
      paste0(team_logo_map[team], ".png"),
      paste0(tolower(team), ".png")
    )
    logo_file <- file.path(logo_dir, logo_filename)
    
    if (file.exists(logo_file)) {
      img <- tryCatch({
        readPNG(logo_file)
      }, error = function(e) {
        cat(sprintf("    Error loading logo for %s: %s\n", team, e$message))
        NULL
      })
      
        if (!is.null(img)) {
          g <- rasterGrob(img, interpolate = TRUE)
          # Logo size in data units (weeks for x, Elo points for y)
          # Use a reasonable size relative to the data range
          x_size <- 1.0  # 1 week width
          y_size <- 30   # 30 Elo points height
          p <- p + annotation_custom(
            g,
            xmin = left_label_data$label_x[i] - x_size,
            xmax = left_label_data$label_x[i] + x_size,
            ymin = left_label_data$label_y[i] - y_size,
            ymax = left_label_data$label_y[i] + y_size
          )
          left_logos_added <- left_logos_added + 1
          cat(sprintf("      Added logo for %s at (%.1f, %.1f)\n", 
                      team, left_label_data$label_x[i], left_label_data$label_y[i]))
        }
    } else {
      cat(sprintf("    Logo file not found: %s\n", logo_file))
    }
  }
  cat(sprintf("    Added %d/%d left logos\n", left_logos_added, nrow(left_label_data)))
  
  # Add logos on RIGHT for SEA and NE
  right_logos_added <- 0
  for (i in 1:nrow(right_label_data)) {
    team <- right_label_data$team[i]
    logo_file <- file.path(logo_dir, paste0(tolower(team), ".png"))
    
    if (file.exists(logo_file)) {
      img <- tryCatch({
        readPNG(logo_file)
      }, error = function(e) {
        cat(sprintf("    Error loading logo for %s: %s\n", team, e$message))
        NULL
      })
      
        if (!is.null(img)) {
          g <- rasterGrob(img, interpolate = TRUE)
          # Logo size in data units (weeks for x, Elo points for y)
          x_size <- 1.2  # 1.2 weeks width
          y_size <- 35   # 35 Elo points height
          p <- p + annotation_custom(
            g,
            xmin = right_label_data$label_x[i] - x_size,
            xmax = right_label_data$label_x[i] + x_size,
            ymin = right_label_data$label_y[i] - y_size,
            ymax = right_label_data$label_y[i] + y_size
          )
          right_logos_added <- right_logos_added + 1
          cat(sprintf("      Added logo for %s at (%.1f, %.1f)\n", 
                      team, right_label_data$label_x[i], right_label_data$label_y[i]))
        }
    } else {
      cat(sprintf("    Logo file not found: %s\n", logo_file))
    }
  }
  cat(sprintf("    Added %d/%d right logos\n", right_logos_added, nrow(right_label_data)))
} else {
  cat("  Using text labels as fallback...\n")
  # Fallback: text labels
  if (nrow(left_label_data) > 0) {
    p <- p + geom_text(
      data = left_label_data,
      aes(x = label_x, y = label_y, label = team, color = team),
      hjust = 1,
      vjust = 0.5,
      fontface = "bold",
      size = 3
    )
    cat("    Added left text labels\n")
  }
  
  if (nrow(right_label_data) > 0) {
    p <- p + geom_text(
      data = right_label_data,
      aes(x = label_x, y = label_y, label = team, color = team),
      hjust = 0,
      vjust = 0.5,
      fontface = "bold",
      size = 4
    )
    cat("    Added right text labels\n")
  }
}

# Add playoff label
p <- p + annotate("text",
                  x = playoff_week - 0.5,
                  y = min(plot_data$elo) + 10,
                  label = "Playoffs Begin",
                  hjust = -0.1,
                  color = "blue",
                  size = 3.5,
                  fontface = "italic",
                  angle = 90)

# Expand x-axis to accommodate labels (space on both left and right)
max_week_plot <- max(plot_data$week)
min_week_plot <- min(plot_data$week)
p <- p + scale_x_continuous(
  breaks = seq(1, max_week_plot, by = 2),
  minor_breaks = NULL,
  limits = c(min_week_plot - 3, max_week_plot + 3),
  expand = expansion(mult = c(0.1, 0.1))  # Space on both sides for logos
)

# Save the plot
ggsave(
  "docs/figures/elo-rankings-by-week.png",
  p,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)
cat("\n✓ Saved: docs/figures/elo-rankings-by-week.png\n")

# Print summary
cat("\n=== SUMMARY ===\n")
cat(sprintf("Plot created with %d teams\n", length(playoff_teams)))
cat(sprintf("X-axis range: %d to %d weeks\n", min_week_plot, max_week_plot))
cat(sprintf("Logo method: %s\n", 
            if (nflplotR_available && local_logos_available) "nflplotR" 
            else if (local_logos_available) "Local files" 
            else "Text labels"))
cat("Done!\n")
