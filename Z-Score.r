# Load required libraries
library(tidyverse)
library(scales)

# Step 1: Load per game and advanced stats separately for each season
pg_93 <- read_csv("Source Data/1993-1994 NBA Player Stats Per Game.csv")
adv_93 <- read_csv("Source Data/1993-1994 NBA Player Advanced Stats Per Game.csv")

pg_23 <- read_csv("Source Data/2023-2024 NBA Player Stats Per Game.csv")
adv_23 <- read_csv("Source Data/2023-2024 NBA Player Advanced Stats Per Game.csv")

# Step 2: Merge per game + advanced for each season separately
nba_93 <- inner_join(pg_93, adv_93, by = c("Player", "Pos"))
nba_23 <- inner_join(pg_23, adv_23, by = c("Player", "Pos"))

# Step 3: Clean and select required variables
select_stats <- function(df) {
  df %>%
    select(Player, Pos,
           AST = AST,    # per-game
           STL = STL,
           TRB = TRB,
           BLK = BLK,
           `3PAr` = `3PAr`, # advanced
           `USG%` = `USG%`,
           TOV = TOV) %>%
    drop_na() %>%
    filter(Pos %in% c("PG", "SG", "SF", "PF", "C"))
}

nba_93_clean <- select_stats(nba_93)
nba_23_clean <- select_stats(nba_23)

# Step 4: Standardize stats within position (z-scores) and compute Positionless Index
calculate_index <- function(df) {
  df %>%
    group_by(Pos) %>%
    mutate(across(c(AST, STL, TRB, BLK, `3PAr`, `USG%`, TOV), ~ scale(.)[,1])) %>%
    ungroup() %>%
    mutate(Positionless_Index = abs(AST) + abs(STL) + abs(`3PAr`) +
             abs(`USG%`) + abs(TOV) +
             abs(TRB) + abs(BLK))
}

nba_93_indexed <- calculate_index(nba_93_clean) %>% mutate(Season = "1993-94")
nba_23_indexed <- calculate_index(nba_23_clean) %>% mutate(Season = "2023-24")

# Step 5: Combine the two processed datasets
nba_index_all <- bind_rows(nba_93_indexed, nba_23_indexed)

# Step 6: Plot Positionless Index distribution
ggplot(nba_index_all, aes(x = Pos, y = Positionless_Index, fill = Season)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#1D428A", "#C8102E")) +
  theme_minimal(base_size = 13) +
  labs(title = "Positionless Index by Position and Season",
       subtitle = "Higher values indicate broader skillsets than positional norms",
       x = "Position", y = "Positionless Index",
       fill = "Season") +
  coord_cartesian(ylim = c(0, NA)) +
  theme(legend.position = "top")

# Desired order: C, PF, SF, SG, PG
position_order <- c("C", "PF", "SF", "SG", "PG")
season_order <- c("1993-94", "2023-24")

# Group, summarize, and arrange
nba_index_all %>%
  group_by(Season, Pos) %>%
  summarise(Mean_Index = round(mean(Positionless_Index, na.rm = TRUE), 3), .groups = "drop") %>%
  mutate(Pos = factor(Pos, levels = position_order),
         Season = factor(Season, levels = season_order)) %>%
  arrange(Pos, Season)
