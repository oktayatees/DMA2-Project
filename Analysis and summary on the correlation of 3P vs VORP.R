# Load required libraries
install.packages("ggrepel")
library(ggrepel)
library(tidyverse)

# Read data
adv_93 <- read_csv("Source Data/1993-1994 NBA Player Advanced Stats Per Game.csv") %>% mutate(Season = "1993-94")
adv_23 <- read_csv("Source Data/2023-2024 NBA Player Advanced Stats Per Game.csv") %>% mutate(Season = "2023-24")

pg_93 <- read_csv("Source Data/1993-1994 NBA Player Stats Per Game.csv") %>% mutate(Season = "1993-94")
pg_23 <- read_csv("Source Data/2023-2024 NBA Player Stats Per Game.csv") %>% mutate(Season = "2023-24")

# Merge advanced + per game stats per season
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
  filter(!is.na(VORP), !is.na(PrimaryPos))

# Correlation analysis
cor_wide <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  summarise(cor = cor(`3P`, VORP, use = "complete.obs"), .groups = "drop") %>%
  pivot_wider(names_from = Season, values_from = cor)

n_counts <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  summarise(n = sum(complete.cases(`3P`, VORP)), .groups = "drop") %>%
  pivot_wider(names_from = Season, values_from = n, names_prefix = "n_")

cor_diff <- cor_wide %>%
  left_join(n_counts, by = "PrimaryPos") %>%
  mutate(
    z_1993 = atanh(`1993-94`),
    z_2023 = atanh(`2023-24`),
    se_diff = sqrt(1 / (`n_1993-94` - 3) + 1 / (`n_2023-24` - 3)),
    z_score = (z_2023 - z_1993) / se_diff,
    p_value = 2 * (1 - pnorm(abs(z_score))),
    cor_diff = `2023-24` - `1993-94`,
    diff_label = sprintf("???r = %.2f\np = %.3f", cor_diff, p_value)
  )

cor_significance <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  summarise(
    r = cor(`3P`, VORP, use = "complete.obs"),
    n = sum(complete.cases(`3P`, VORP)),
    t_stat = r * sqrt(n - 2) / sqrt(1 - r^2),
    df = n - 2,
    p_value = 2 * (1 - pt(abs(t_stat), df)),
    sig_label = ifelse(p_value < 0.0001,
                       sprintf("%s: r = %.2f\np < 0.0001", Season, r),
                       sprintf("%s: r = %.2f\np = %.4f", Season, r, p_value)),
    .groups = "drop"
  )

cor_test_93 <- analysis_df %>%
  filter(Season == "1993-94") %>%
  group_by(PrimaryPos) %>%
  summarise(
    r = cor(`3P`, VORP, use = "complete.obs"),
    p_value = cor.test(`3P`, VORP)$p.value,
    .groups = "drop"
  )

cor_test_23 <- analysis_df %>%
  filter(Season == "2023-24") %>%
  group_by(PrimaryPos) %>%
  summarise(
    r = cor(`3P`, VORP, use = "complete.obs"),
    p_value = cor.test(`3P`, VORP)$p.value,
    .groups = "drop"
  )

# Outliers for labels
outliers <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  mutate(
    vorp_rank = rank(desc(VORP)),
    three_rank = rank(desc(`3P`))
  ) %>%
  filter(vorp_rank <= 2 | three_rank <= 2) %>%
  ungroup()

# Filter density-safe data
density_df <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  filter(sd(`3P`, na.rm = TRUE) > 0 & sd(VORP, na.rm = TRUE) > 0) %>%
  ungroup()

# Final plot
ggplot(analysis_df, aes(x = `3P`, y = VORP, color = Season)) +
  stat_density_2d(data = density_df, aes(fill = ..level..),
                  alpha = 0.2, geom = "polygon", show.legend = FALSE) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_text(data = cor_significance,
            aes(x = max(analysis_df$`3P`, na.rm = TRUE) - 0.3,
                y = max(analysis_df$VORP, na.rm = TRUE) - (0.5 + ifelse(Season == "1993-94", 0, 1.5)),
                label = sig_label, color = Season),
            hjust = 1, vjust = 1, size = 3.5, show.legend = FALSE) +
  geom_text_repel(data = outliers,
                  aes(label = Player),
                  size = 3, max.overlaps = 30, direction = "x",
                  nudge_x = 0.2, segment.color = "gray60", show.legend = FALSE) +
  facet_wrap(~PrimaryPos) +
  theme_minimal() +
  labs(
    title = "Correlation between 3-Pointers Made and VORP by Position",
    subtitle = "1993-94 vs 2023-24 Seasons",
    x = "3-Pointers Made per Game", y = "VORP",
    color = "Season", fill = "Density"
  ) +
  scale_color_manual(values = c("1993-94" = "blue", "2023-24" = "red")) +
  scale_fill_gradient(low = "gray90", high = "gray20") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )

# -----------------------------------
# ???? ADDITIONAL REGRESSION OUTPUTS
# -----------------------------------
cat("\n\n=============================\nDETAILED REGRESSION ANALYSIS\n=============================\n")

for (pos in unique(analysis_df$PrimaryPos)) {
  for (season in unique(analysis_df$Season)) {
    cat("\n------------------------------------\n")
    cat(sprintf("???? %s - %s\n", pos, season))
    
    sub_df <- analysis_df %>% filter(PrimaryPos == pos, Season == season)
    if (nrow(sub_df) < 5 || var(sub_df$`3P`, na.rm = TRUE) == 0 || var(sub_df$VORP, na.rm = TRUE) == 0) {
      cat("??? Not enough variance or data to compute models.\n")
      next
    }
    
    # Linear Model
    lin_model <- lm(VORP ~ `3P`, data = sub_df)
    cat("???? Linear Model Summary:\n")
    print(summary(lin_model))
    
    cor_r <- cor(sub_df$`3P`, sub_df$VORP, use = "complete.obs")
    cat(sprintf("Linear Pearson r = %.4f\n", cor_r))
    
    # Log-Log Model
    sub_df <- sub_df %>% mutate(log_3P = log(`3P` + 0.01), log_VORP = log(VORP + 0.01))
    log_model <- lm(log_VORP ~ log_3P, data = sub_df)
    cat("???? Log-Log Model Summary:\n")
    print(summary(log_model))
    
    cor_log_r <- cor(sub_df$log_3P, sub_df$log_VORP, use = "complete.obs")
    cat(sprintf("Log-Log Pearson r = %.4f\n", cor_log_r))
  }
}

# Merge correlation and p-values into a single table
cor_93 <- cor_significance %>% filter(Season == "1993-94") %>%
  select(PrimaryPos, r_93 = r, p_93 = p_value)

cor_23 <- cor_significance %>% filter(Season == "2023-24") %>%
  select(PrimaryPos, r_23 = r, p_23 = p_value)

# Combine all into one summary table
summary_table <- cor_diff %>%
  select(PrimaryPos, cor_diff, p_value_diff = p_value) %>%
  left_join(cor_93, by = "PrimaryPos") %>%
  left_join(cor_23, by = "PrimaryPos") %>%
  select(PrimaryPos, r_93, p_93, r_23, p_23, cor_diff, p_value_diff)

# Format the numbers nicely
summary_table <- summary_table %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

# Print the summary table
cat("\n==============================\n???? Summary Table\n==============================\n")
print(summary_table)
