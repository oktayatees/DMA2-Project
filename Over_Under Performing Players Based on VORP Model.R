# Load libraries
library(ggplot2)
library(readr)
library(dplyr)

# Load data
nba_2023 <- read_csv("Source Data/2023-2024 NBA Player Advanced Stats Per Game.csv")
nba_1993 <- read_csv("Source Data/1993-1994 NBA Player Advanced Stats Per Game.csv")

# Function to build and process model for a given dataset
process_vorp_model <- function(df, season_label) {
  df_clean <- df %>%
    filter(!is.na(VORP), !is.na(MP), !is.na(PER), !is.na(WS)) %>%
    mutate(Season = season_label)
  
  model <- lm(VORP ~ MP + PER + WS, data = df_clean)
  
  df_clean %>%
    mutate(
      predicted_vorp = predict(model, .),
      residual = VORP - predicted_vorp
    )
}

# Apply function to both datasets
nba_2023_clean <- process_vorp_model(nba_2023, "2023-24")
nba_1993_clean <- process_vorp_model(nba_1993, "1993-94")

# Combine both
nba_combined <- bind_rows(nba_2023_clean, nba_1993_clean)

# Plot residuals by predicted VORP with Position-based color
ggplot(nba_combined, aes(x = predicted_vorp, y = residual, color = Pos, label = Player)) +
  geom_point(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(data = filter(nba_combined, abs(residual) > 1.5), size = 2.5, hjust = 1) +
  facet_wrap(~Season) +
  labs(title = "Over/Under Performing Players Based on VORP Model",
       subtitle = "Compared Across Seasons and Positions",
       x = "Predicted VORP", y = "Residual (Actual - Predicted)",
       color = "Position") +
  theme_minimal() +
  theme(legend.position = "bottom")
