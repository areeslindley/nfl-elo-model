# fetch-betting-odds.R
# Script to fetch real betting odds from The Odds API
# Run this script to download and save odds data
#
# NOTE: The Odds API only provides odds for UPCOMING games.
# Once a game has been played, the odds are removed from the API.
# Historical odds require a paid API plan.
# If the Super Bowl has already been played, you'll need to:
#   1. Use the fallback data (created automatically), OR
#   2. Manually update data/output/betting_odds.csv with the actual pre-game odds

library(tidyverse)
library(httr)
library(jsonlite)

cat("=== Fetching Betting Odds from The Odds API ===\n\n")

# Read API key from .Renviron file
renv_file <- file.path(getwd(), ".Renviron")
api_key <- NULL

if (file.exists(renv_file)) {
  renv_lines <- readLines(renv_file, warn = FALSE)
  key_line <- grep("^ODDS_API_KEY=", renv_lines, value = TRUE)
  if (length(key_line) > 0) {
    api_key <- gsub("^ODDS_API_KEY=", "", key_line)
    api_key <- trimws(api_key)
    # Remove quotes if present
    api_key <- gsub('^["\']|["\']$', "", api_key)
    cat("✓ Found API key in .Renviron\n")
  }
}

# Also check environment variable
if (is.null(api_key) || api_key == "" || api_key == "your_api_key_here") {
  api_key <- Sys.getenv("ODDS_API_KEY")
  if (api_key != "" && api_key != "your_api_key_here") {
    cat("✓ Found API key in environment variable\n")
  }
}

if (is.null(api_key) || api_key == "" || api_key == "your_api_key_here") {
  stop("API key not found. Please set ODDS_API_KEY in .Renviron file or environment variable.")
}

cat("API key (first 10 chars):", substr(api_key, 1, 10), "...\n\n")

# Fetch odds from The Odds API
cat("Fetching NFL odds...\n")
url <- "https://api.the-odds-api.com/v4/sports/americanfootball_nfl/odds"

params <- list(
  apiKey = api_key,
  regions = "us",  # US bookmakers
  markets = "spreads,totals,h2h",  # Spread, totals, and moneyline
  oddsFormat = "american"  # American odds format
)

tryCatch({
  response <- GET(url, query = params, timeout(10))
  
  cat("Response status:", status_code(response), "\n")
  
  if (status_code(response) == 200) {
    response_text <- content(response, "text")
    # Parse JSON without flattening to preserve nested structure
    data <- fromJSON(response_text, flatten = FALSE)
    
    cat("✓ Successfully fetched odds data\n")
    
    # Check if data is empty
    is_empty <- FALSE
    if (is.null(data)) {
      is_empty <- TRUE
    } else if (is.data.frame(data)) {
      is_empty <- (nrow(data) == 0)
    } else if (is.list(data)) {
      is_empty <- (length(data) == 0)
    } else {
      is_empty <- TRUE
    }
    
    if (is_empty) {
      cat("\n⚠ API returned empty data (no upcoming games found)\n")
      cat("This is normal and means:\n")
      cat("  - The Super Bowl game isn't listed by bookmakers yet\n")
      cat("  - OR the NFL season has ended\n")
      cat("  - OR the game is scheduled too far in advance\n\n")
      cat("The script will create a fallback file with example odds.\n")
      cat("When the Super Bowl is posted, re-run this script to get real odds.\n\n")
      cat("Creating fallback betting odds file...\n")
      
      # Create fallback CSV with realistic odds
      fallback_odds <- tibble(
        Bookmaker = c("DraftKings", "FanDuel", "BetMGM", "Caesars", "PointsBet", "Average"),
        `Spread (SEA)` = c("-1.5", "-1.5", "-2.0", "-1.5", "-1.0", "-1.5"),
        `Spread Odds` = c("-110", "-110", "-110", "-110", "-110", "-110"),
        `SEA Moneyline` = c("-120", "-118", "-125", "-120", "-115", "-119.6"),
        `NE Moneyline` = c("+100", "+100", "+105", "+100", "+105", "+102"),
        `Over/Under` = c("47.5", "47.5", "48.0", "47.5", "47.0", "47.5"),
        `O/U Odds` = c("-110", "-110", "-110", "-110", "-110", "-110")
      )
      
      write_csv(fallback_odds, "data/output/betting_odds.csv")
      cat("✓ Created fallback betting odds file: data/output/betting_odds.csv\n")
      cat("  You can manually update this file with real odds when they become available.\n")
    } else {
      # Handle data frame vs list
      if (is.data.frame(data)) {
        num_games <- nrow(data)
        cat("Number of games found:", num_games, "\n\n")
      } else {
        num_games <- length(data)
        cat("Number of games found:", num_games, "\n\n")
      }
      
      # Save raw data
      write_json(data, "data/processed/betting_odds_raw.json", pretty = TRUE)
      cat("✓ Saved raw data to data/processed/betting_odds_raw.json\n")
      
      # Try to find Super Bowl game (Seattle vs New England)
      cat("\nSearching for Super Bowl LX (Seattle vs New England)...\n")
      
      sb_found <- FALSE
      betting_odds_table <- NULL
      
      # Iterate through games - handle both data frame and list
      for (i in 1:num_games) {
        if (is.data.frame(data)) {
            game <- data[i, ]
            home_team <- game$home_team
            away_team <- game$away_team
            # bookmakers is a list column - get first element (which is a data frame)
            game_bookmakers_df <- game$bookmakers[[1]]
          } else {
            game <- data[[i]]
            home_team <- game$home_team
            away_team <- game$away_team
            # If list, bookmakers might be a list or data frame
            if (is.data.frame(game$bookmakers)) {
              game_bookmakers_df <- game$bookmakers
            } else {
              game_bookmakers_df <- game$bookmakers[[1]]
            }
          }
          
          # Check if this is the Super Bowl game
      is_sea_home <- grepl("Seattle|Seahawks|SEA", home_team, ignore.case = TRUE)
        is_sea_away <- grepl("Seattle|Seahawks|SEA", away_team, ignore.case = TRUE)
        is_ne_home <- grepl("New England|Patriots|NE|N\\.E\\.", home_team, ignore.case = TRUE)
        is_ne_away <- grepl("New England|Patriots|NE|N\\.E\\.", away_team, ignore.case = TRUE)
        
        if ((is_sea_home && is_ne_away) || (is_sea_away && is_ne_home)) {
          cat("✓ Found Super Bowl game!\n")
          cat("  Home:", home_team, "\n")
          cat("  Away:", away_team, "\n\n")
          
          sb_found <- TRUE
          
          # Extract bookmaker odds - game_bookmakers_df is a data frame
          if (!is.null(game_bookmakers_df) && nrow(game_bookmakers_df) > 0) {
            bookmaker_list <- list()
            
            for (j in 1:nrow(game_bookmakers_df)) {
              bookmaker <- game_bookmakers_df[j, ]
              bookmaker_name <- bookmaker$title
              # markets is a list column - get first element (which is a data frame)
              markets_df <- bookmaker$markets[[1]]
              
              # Initialize row data
              row_data <- list(
                Bookmaker = bookmaker_name,
                `Spread (SEA)` = NA_character_,
                `Spread Odds` = NA_character_,
                `SEA Moneyline` = NA_character_,
                `NE Moneyline` = NA_character_,
                `Over/Under` = NA_character_,
                `O/U Odds` = NA_character_
              )
              
              # Process each market - iterate through rows of markets data frame
              for (m in 1:nrow(markets_df)) {
                market <- markets_df[m, ]
                market_key <- market$key
                # outcomes is a list column - get first element (which is a data frame)
                outcomes_df <- market$outcomes[[1]]
                
                if (market_key == "spreads") {
                  # Find SEA's spread (negative point means favorite)
                  for (k in 1:nrow(outcomes_df)) {
                    outcome <- outcomes_df[k, ]
                    # Check if this is SEA (negative point or name match)
                    if (grepl("Seattle|Seahawks|SEA", outcome$name, ignore.case = TRUE) || 
                        (is_sea_home && outcome$point < 0) || 
                        (is_sea_away && outcome$point > 0)) {
                      row_data$`Spread (SEA)` <- paste0(ifelse(outcome$point > 0, "+", ""), outcome$point)
                      row_data$`Spread Odds` <- as.character(outcome$price)
                      break
                    }
                  }
                } else if (market_key == "totals") {
                  # Over/under - usually first outcome
                  if (nrow(outcomes_df) > 0) {
                    outcome <- outcomes_df[1, ]
                    row_data$`Over/Under` <- as.character(outcome$point)
                    row_data$`O/U Odds` <- as.character(outcome$price)
                  }
                } else if (market_key == "h2h") {
                  # Moneyline - find SEA and NE
                  for (k in 1:nrow(outcomes_df)) {
                    outcome <- outcomes_df[k, ]
                    if (grepl("Seattle|Seahawks|SEA", outcome$name, ignore.case = TRUE)) {
                      row_data$`SEA Moneyline` <- as.character(outcome$price)
                    } else if (grepl("New England|Patriots|NE|N\\.E\\.", outcome$name, ignore.case = TRUE)) {
                      row_data$`NE Moneyline` <- as.character(outcome$price)
                    }
                  }
                }
              }
              
              bookmaker_list[[length(bookmaker_list) + 1]] <- row_data
            }
            
            if (length(bookmaker_list) > 0) {
              betting_odds_table <- bind_rows(bookmaker_list)
              
              # Calculate averages (excluding any existing "Average" row)
              if (nrow(betting_odds_table) > 0) {
                avg_row <- betting_odds_table %>%
                  filter(!is.na(`Spread (SEA)`) & Bookmaker != "Average") %>%
                  summarise(
                    Bookmaker = "Average",
                    `Spread (SEA)` = paste0(ifelse(mean(as.numeric(gsub("[^0-9.-]", "", `Spread (SEA)`)), na.rm = TRUE) > 0, "+", ""),
                                           round(mean(as.numeric(gsub("[^0-9.-]", "", `Spread (SEA)`)), na.rm = TRUE), 1)),
                    `Spread Odds` = "-110",
                    `SEA Moneyline` = as.character(round(mean(as.numeric(`SEA Moneyline`), na.rm = TRUE), 1)),
                    `NE Moneyline` = as.character(round(mean(as.numeric(`NE Moneyline`), na.rm = TRUE), 1)),
                    `Over/Under` = as.character(round(mean(as.numeric(`Over/Under`), na.rm = TRUE), 1)),
                    `O/U Odds` = "-110"
                  )
                
                betting_odds_table <- bind_rows(betting_odds_table, avg_row)
                
                cat("✓ Processed odds from", nrow(betting_odds_table) - 1, "bookmakers\n\n")
              }
            }
          }
          
          break
        }
      }
      
      if (!sb_found) {
      cat("⚠ Super Bowl game not found in API response\n")
      cat("This is normal if:\n")
      cat("  - The Super Bowl hasn't been posted by bookmakers yet\n")
      cat("  - The game is scheduled too far in advance\n")
      cat("  - The API doesn't list future playoff games\n\n")
      
      if (is.data.frame(data) && nrow(data) > 0) {
        cat("Available games in API:\n")
        for (i in 1:min(5, nrow(data))) {
          home <- ifelse("home_team" %in% names(data), data$home_team[i], "N/A")
          away <- ifelse("away_team" %in% names(data), data$away_team[i], "N/A")
          cat("  -", away, "at", home, "\n")
        }
        if (nrow(data) > 5) cat("  ... and", nrow(data) - 5, "more\n")
      }
      
      cat("\nCreating fallback betting odds file...\n")
      
      # Create fallback CSV with realistic odds
      fallback_odds <- tibble(
        Bookmaker = c("DraftKings", "FanDuel", "BetMGM", "Caesars", "PointsBet", "Average"),
        `Spread (SEA)` = c("-1.5", "-1.5", "-2.0", "-1.5", "-1.0", "-1.5"),
        `Spread Odds` = c("-110", "-110", "-110", "-110", "-110", "-110"),
        `SEA Moneyline` = c("-120", "-118", "-125", "-120", "-115", "-119.6"),
        `NE Moneyline` = c("+100", "+100", "+105", "+100", "+105", "+102"),
        `Over/Under` = c("47.5", "47.5", "48.0", "47.5", "47.0", "47.5"),
        `O/U Odds` = c("-110", "-110", "-110", "-110", "-110", "-110")
      )
      
        write_csv(fallback_odds, "data/output/betting_odds.csv")
        cat("✓ Created fallback betting odds file: data/output/betting_odds.csv\n")
        cat("  You can manually update this file with real odds when they become available.\n")
      } else if (!is.null(betting_odds_table)) {
        # Save processed odds table
        write_csv(betting_odds_table, "data/output/betting_odds.csv")
        cat("✓ Saved betting odds table to data/output/betting_odds.csv\n\n")
        
        # Print preview
        cat("Betting Odds Preview:\n")
        print(betting_odds_table)
      }
    }  # End else block for non-empty data
  } else {
    cat("✗ API request failed\n")
    cat("Status code:", status_code(response), "\n")
    cat("Response:", content(response, "text"), "\n")
  }
  
}, error = function(e) {
  cat("✗ Error fetching odds:\n")
  cat("  ", e$message, "\n")
  cat("\nTroubleshooting:\n")
  cat("  1. Check your API key is correct\n")
  cat("  2. Verify you have remaining API requests (free tier: 500/month)\n")
  cat("  3. Check your internet connection\n")
  cat("  4. The game might not be available yet (check if Super Bowl is listed)\n")
})

cat("\n=== Done ===\n")
