# Super Bowl LX Pre-Game Analysis

A statistical analysis predicting the outcome of Super Bowl LX (Seattle Seahawks vs. New England Patriots) using Elo ratings and baseline simulation models.

## Overview

This project uses two complementary approaches to predict Super Bowl outcomes:

1. **Elo Model**: Dynamic team ratings based on game results and margin of victory
2. **Baseline Model**: Historical simulation using team performance relative to league averages

Both models are implemented in R and generate predictions for win probability, expected scores, and detailed performance metrics.

## Quick Start

### Prerequisites

- R (version 4.0+)
- Required R packages (install automatically with `install-dependencies.R`):
  - `tidyverse`
  - `nflreadr` / `nflfastR`
  - `ggplot2`
  - `gt` / `gtExtras`
  - `patchwork`
  - `quarto` (for rendering the analysis document)

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/nfl-elo-model.git
cd nfl-elo-model
```

2. Install R dependencies:
```r
source("install-dependencies.R")
```

3. Set up environment variables (optional, for betting odds):
```bash
cp .Renviron.example .Renviron
# Edit .Renviron and add your ODDS_API_KEY if you want to fetch real betting odds
```

### Running the Analysis

**Option 1: Run the complete pipeline**
```r
source("run-pipeline.R")
```

**Option 2: Run scripts individually**
```r
# 1. Fetch and process data
source("R/01-fetch-data.R")

# 2. Calculate Elo ratings and predictions
source("R/02-elo-model.R")

# 3. Run baseline simulation model
source("R/03-baseline-model.R")

# 4. Combine predictions
source("R/04-combine-predictions.R")

# 5. Create visualizations
source("R/05-create-visualizations.R")
source("R/05b-create-elo-rankings-plot.R")

# 6. Render Quarto document
quarto::quarto_render("Superbowl-pregame-analysis.qmd")
```

## Project Structure

```
.
├── R/                          # Core analysis scripts
│   ├── 01-fetch-data.R        # Data fetching and processing
│   ├── 02-elo-model.R         # Elo rating calculations
│   ├── 03-baseline-model.R    # Baseline simulation model
│   ├── 04-combine-predictions.R  # Combine model outputs
│   ├── 05-create-visualizations.R # Generate figures
│   ├── 05b-create-elo-rankings-plot.R # Elo rankings plot
│   └── utils.R                # Utility functions
├── Superbowl-pregame-analysis.qmd  # Main analysis document
├── fetch-betting-odds.R        # Optional: Fetch betting odds
├── run-pipeline.R              # Run all scripts in sequence
├── data/                       # Data directories (auto-created)
│   ├── raw/                    # Raw data files
│   ├── processed/              # Processed data files
│   └── output/                 # Model outputs and predictions
└── docs/                       # Generated figures and outputs
```

## Model Methodology

### Elo Model

The Elo model assigns dynamic ratings to teams based on:
- Game outcomes (win/loss)
- Margin of victory (using logarithmic multiplier)
- Home field advantage
- Between-season regression toward the mean

**Key Parameters:**
- K-factor: 20 (controls rating update magnitude)
- Home advantage: 65 Elo points
- Regression factor: 1/3 (between seasons)

**Score Prediction:**
- Uses historical Super Bowl scoring patterns (since 2010)
- Adjusts based on Elo difference: `Elo Impact = (SEA Elo - NE Elo) × 0.01`
- Formula: `Expected Score = (Base Total / 2) ± Elo Impact`

### Baseline Model

The baseline model uses:
- Historical Super Bowl scoring distributions (since 2005)
- Team performance relative to league average (2025 season)
- Poisson simulation (10,000 iterations)

**Score Prediction:**
- Base score from historical Super Bowls
- Adjustments: `Expected Score = Base + Offense Adj - Opponent Defense Adj`
- Simulates games using Poisson distributions

## Outputs

The analysis generates:

1. **Predictions Table**: Win probabilities and expected scores from both models
2. **Score Distribution**: Full distribution of possible outcomes
3. **Performance Metrics**: Expected yards, touchdowns, and other statistics
4. **Betting Market Comparison**: Model predictions vs. bookmaker odds
5. **Elo Rankings Plot**: Season-long Elo evolution for playoff teams

All outputs are saved to `data/output/` and figures to `docs/figures/`.

## Data Sources

- **Game Results**: [nflverse](https://nflverse.nflverse.com/) via `nflreadr`
- **Play-by-Play**: `nflfastR` (2025 season)
- **Betting Odds**: The Odds API (optional, requires API key)

**Elo Model Inspiration:**
This implementation is inspired by [FiveThirtyEight's NFL Elo model](https://fivethirtyeight.com/features/how-we-calculate-nfl-elo-ratings/), using similar principles with customized parameters.

## Rendering the Analysis

To generate the HTML report:

```r
quarto::quarto_render("Superbowl-pregame-analysis.qmd")
```

Or from the command line:
```bash
quarto render Superbowl-pregame-analysis.qmd
```

The rendered HTML will be saved as `Superbowl-pregame-analysis.html`.

## Notes

- **Data Caching**: The scripts cache processed data to avoid re-downloading. Delete files in `data/processed/` to force fresh downloads.
- **Betting Odds**: Historical odds are stored in `data/output/betting_odds.csv`. If the Super Bowl has already been played, you'll need to manually update this file with historical odds (see `fetch-betting-odds.R` for format).
- **Reproducibility**: All scripts use `set.seed()` for reproducible simulations.

## License

[Add your license here]

## Author

[Add your name/contact info here]
