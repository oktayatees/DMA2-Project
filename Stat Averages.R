library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)

# Load advanced stats
adv_93 <- read_csv("Source Data/1993-1994 NBA Player Advanced Stats Per Game.csv") %>%
  mutate(Season = "1993-94") %>%
  select(Player, Team, Pos, `3PAr`, Season)

adv_24 <- read_csv("Source Data/2023-2024 NBA Player Advanced Stats Per Game.csv") %>%
  mutate(Season = "2023-24") %>%
  select(Player, Team, Pos, `3PAr`, Season)

# Load per-game stats (including G for filtering)
per_93 <- read_csv("Source Data/1993-1994 NBA Player Stats Per Game.csv") %>%
  mutate(Season = "1993-94") %>%
  select(Player, Team, Pos, G, `3PA`, `3P%`, Season) %>%
  filter(G >= 15)

per_24 <- read_csv("Source Data/2023-2024 NBA Player Stats Per Game.csv") %>%
  mutate(Season = "2023-24") %>%
  select(Player, Team, Pos, G, `3PA`, `3P%`, Season) %>%
  filter(G >= 15)

# Combine per-game stats (filtered)
per_all <- bind_rows(per_93, per_24)

# Now join only players who passed the game filter
adv_all <- bind_rows(adv_93, adv_24)

# Merge advanced + per-game stats (only players who played 15+ games included)
nba_all <- inner_join(adv_all, per_all, by = c("Player", "Team", "Pos", "Season"))

# Simplify position to G / F / C groups
nba_all <- nba_all %>%
  mutate(PositionGroup = case_when(
    str_detect(Pos, "PG|SG") ~ "G",
    str_detect(Pos, "SF|PF") ~ "F",
    str_detect(Pos, "C") ~ "C",
    TRUE ~ "Other"
  )) %>%
  filter(PositionGroup %in% c("G", "F", "C"))

# Select features to analyze (only three-point related stats)
features <- c("3PA", "3P%", "3PAr")

# Perform statistical tests
statistical_tests <- nba_all %>%
  select(Season, PositionGroup, all_of(features)) %>%
  group_by(PositionGroup) %>%
  group_map(~ {
    position <- .y$PositionGroup
    data <- .x
    
    # Perform t-test for each feature
    results <- map_dfr(features, function(feature) {
      test_result <- t.test(
        data %>% filter(Season == "2023-24") %>% pull(feature),
        data %>% filter(Season == "1993-94") %>% pull(feature)
      )
      
      tibble(
        PositionGroup = position,
        Stat = feature,
        p_value = test_result$p.value,
        significant = test_result$p.value < 0.05
      )
    })
    
    results
  }) %>%
  bind_rows()

# Calculate means and percentage increases
nba_plot_data <- nba_all %>%
  select(Season, PositionGroup, all_of(features)) %>%
  group_by(Season, PositionGroup) %>%
  summarise(across(all_of(features), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = all_of(features), names_to = "Stat", values_to = "Value") %>%
  pivot_wider(names_from = Season, values_from = Value) %>%
  mutate(
    Percentage_Increase = ((`2023-24` - `1993-94`) / `1993-94`) * 100
  ) %>%
  pivot_longer(
    cols = c(`1993-94`, `2023-24`),
    names_to = "Season",
    values_to = "Value"
  )

# Join statistical test results
nba_plot_data <- nba_plot_data %>%
  left_join(statistical_tests, by = c("PositionGroup", "Stat"))

# Plot
ggplot(nba_plot_data, aes(x = PositionGroup, y = Value, fill = Season)) +
  geom_col(position = "dodge") +
  geom_text(
    data = nba_plot_data %>% filter(Season == "2023-24"),
    aes(label = sprintf("%+.1f%%%s", 
                       Percentage_Increase,
                       ifelse(significant, "*", ""))),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  facet_wrap(~Stat, scales = "free_y") +
  labs(
    title = "NBA Three-Point Statistics by Position (1993-94 vs. 2023-24)",
    subtitle = "Percentage increase shown above 2023-24 bars\n* indicates statistically significant change (p < 0.05)",
    x = "Position",
    y = "Average Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = c("1993-94" = "firebrick", "2023-24" = "steelblue"))

# Print statistical test results
print("\nT-Test Results for Three-Point Statistics:")
print(statistical_tests %>% arrange(PositionGroup, Stat))
