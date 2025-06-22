# Load libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(webshot2)

# Read data
adv_93 <- read_csv("Source Data/1993-1994 NBA Player Advanced Stats Per Game.csv") %>% mutate(Season = "1993-94")
adv_23 <- read_csv("Source Data/2023-2024 NBA Player Advanced Stats Per Game.csv") %>% mutate(Season = "2023-24")

pg_93 <- read_csv("Source Data/1993-1994 NBA Player Stats Per Game.csv") %>% mutate(Season = "1993-94")
pg_23 <- read_csv("Source Data/2023-2024 NBA Player Stats Per Game.csv") %>% mutate(Season = "2023-24")

# Combine data
combined_93 <- left_join(adv_93, pg_93, by = c("Player", "Season", "Team", "Pos", "Age"))
combined_23 <- left_join(adv_23, pg_23, by = c("Player", "Season", "Team", "Pos", "Age"))
combined_all <- bind_rows(combined_93, combined_23)

# Clean and categorize
analysis_df <- combined_all %>%
  select(Player, Season, Pos, VORP, `3P`) %>%
  mutate(
    PrimaryPos = case_when(
      str_detect(Pos, "C") ~ "C",
      str_detect(Pos, "F") ~ "F",
      str_detect(Pos, "G") ~ "G",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(VORP), !is.na(PrimaryPos), !is.na(`3P`))

# Compute correlation tables
cor_test_93 <- analysis_df %>%
  filter(Season == "1993-94") %>%
  group_by(PrimaryPos) %>%
  summarise(
    r = cor(`3P`, VORP, use = "complete.obs"),
    p_value = cor.test(`3P`, VORP)$p.value,
    .groups = "drop"
  ) %>%
  mutate(Season = "1993-94")

cor_test_23 <- analysis_df %>%
  filter(Season == "2023-24") %>%
  group_by(PrimaryPos) %>%
  summarise(
    r = cor(`3P`, VORP, use = "complete.obs"),
    p_value = cor.test(`3P`, VORP)$p.value,
    .groups = "drop"
  ) %>%
  mutate(Season = "2023-24")

# Combine tables
combined_table <- bind_rows(cor_test_93, cor_test_23) %>%
  select(Season, PrimaryPos, r, p_value) %>%
  arrange(PrimaryPos, Season) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

# Create the gt table as before
gt_table <- combined_table %>%
  gt() %>%
  tab_header(
    title = "Correlation Between 3P and VORP by Position",
    subtitle = "1993-94 vs 2023-24 NBA Seasons"
  ) %>%
  cols_label(
    Season = "Season",
    PrimaryPos = "Position",
    r = "Correlation (r)",
    p_value = "p-value"
  ) %>%
  fmt_number(columns = c(r), decimals = 4) %>%
  fmt_number(columns = c(p_value), decimals = 8) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "gray80", weight = px(1)),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  )

# Save the table as PNG
png("Images/correlation_high_res.png", width = 12, height = 8, units = "in", res = 300)
print(gt_table)
