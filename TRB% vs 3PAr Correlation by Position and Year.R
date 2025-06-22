# Load libraries
library(tidyverse)

# Load the data
adv_93 <- read_csv("Source Data/1993-1994 NBA Player Advanced Stats Per Game.csv")
adv_23 <- read_csv("Source Data/2023-2024 NBA Player Advanced Stats Per Game.csv")
pg_93 <- read_csv("Source Data/1993-1994 NBA Player Stats Per Game.csv")
pg_23 <- read_csv("Source Data/2023-2024 NBA Player Stats Per Game.csv")

# Add season labels
adv_93 <- adv_93 %>% mutate(Season = "1993-94")
adv_23 <- adv_23 %>% mutate(Season = "2023-24")
pg_93 <- pg_93 %>% mutate(Season = "1993-94")
pg_23 <- pg_23 %>% mutate(Season = "2023-24")

# Combine and filter by min games and min minutes per game
pg_all <- bind_rows(pg_93, pg_23) %>%
  filter(G >= 40, MP >= 10)

# Combine advanced stats
adv_all <- bind_rows(adv_93, adv_23)

# Merge filtered stats
combined_all <- left_join(adv_all, pg_all, by = c("Player", "Season", "Pos", "Age"))

# Select relevant columns
analysis_df <- combined_all %>%
  select(Player, Pos, Season, TRB, `3P`) %>%
  drop_na()

# Simplify position
analysis_df <- analysis_df %>%
  mutate(
    PrimaryPos = case_when(
      str_detect(Pos, "C") ~ "C",
      str_detect(Pos, "F") ~ "F",
      str_detect(Pos, "G") ~ "G",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(PrimaryPos %in% c("G", "F", "C"))

# Compute correlation stats per Season and Position
cor_results <- analysis_df %>%
  group_by(Season, PrimaryPos) %>%
  summarize(
    r = cor(TRB, `3P`, use = "complete.obs"),
    p = cor.test(TRB, `3P`)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("r = ", round(r, 2), "\np = ", signif(p, 2))
  )

# Determine label positions for top right corner in each facet
label_positions <- analysis_df %>%
  group_by(Season) %>%
  summarize(
    x = max(TRB, na.rm = TRUE),
    y = max(`3P`, na.rm = TRUE)
  )

# Create vertical spacing for labels with consistent positions across seasons
cor_results <- cor_results %>%
  left_join(label_positions, by = "Season") %>%
  group_by(PrimaryPos) %>%
  mutate(
    y = max(y) - (match(PrimaryPos, c("C", "F", "G")) - 1) * (max(y) * 0.15)  # Stack labels vertically with 15% spacing
  )

# Final plot
ggplot(analysis_df, aes(x = TRB, y = `3P`, color = PrimaryPos)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  geom_text(
    data = cor_results,
    aes(x = x, y = y, label = label, color = PrimaryPos),
    hjust = 1, vjust = 1, inherit.aes = FALSE
  ) +
  facet_wrap(~Season) +
  theme_minimal(base_family = "Arial") +
  theme(plot.background = element_rect(fill = "white", color = NA)) +
  labs(
    title = "TRB vs 3PM by Position and Season",
    x = "Total Rebounds (TRB)",
    y = "3-Point Made",
    color = "Position"
  )

