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
  select(Player, Season, Pos, VORP, `3P`, `3PA`, `3P%`, `3PAr`) %>%
  mutate(
    PrimaryPos = case_when(
      str_detect(Pos, "C") ~ "C",
      str_detect(Pos, "F") ~ "F",
      str_detect(Pos, "G") ~ "G",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(VORP), !is.na(PrimaryPos))

# Perform ANOVA analysis for each position and 3P stat
anova_results <- analysis_df %>%
  select(Season, PrimaryPos, VORP, `3P`, `3PA`, `3P%`, `3PAr`) %>%
  group_by(PrimaryPos) %>%
  group_map(~ {
    position <- .y$PrimaryPos
    data <- .x
    
    # Perform ANOVA for each 3P stat
    results <- map_dfr(c("3P", "3PA", "3P%", "3PAr"), function(stat) {
      # Create ANOVA model
      formula_str <- paste("VORP ~", paste0("`", stat, "`"))
      model <- aov(as.formula(formula_str), data = data)
      anova_summary <- summary(model)
      
      # Extract F-statistic and p-value
      f_stat <- anova_summary[[1]]$`F value`[1]
      p_value <- anova_summary[[1]]$`Pr(>F)`[1]
      
      tibble(
        PositionGroup = position,
        Stat = stat,
        F_statistic = f_stat,
        p_value = p_value,
        significant = p_value < 0.05
      )
    })
    
    results
  }) %>%
  bind_rows()

# Print ANOVA results
print("ANOVA Results for Three-Point Statistics vs VORP:")
print(anova_results %>% arrange(PositionGroup, Stat))

# --- Linear Regression Summary for 3P vs VORP by Position ---
print("\n--- Linear Regression Summary (3P vs VORP) by Position ---")
analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  group_walk(~ {
    position <- .y$PrimaryPos
    season <- .y$Season
    data_subset <- .x
    
    # Only run if there's enough data and variance
    if (nrow(data_subset) > 1 && sd(data_subset$`3P`, na.rm = TRUE) > 0 && sd(data_subset$VORP, na.rm = TRUE) > 0) {
      model <- lm(VORP ~ `3P`, data = data_subset)
      summary_model <- summary(model)
      
      cat(sprintf("\nPosition: %s, Season: %s\n", position, season))
      print(summary_model)
    } else {
      cat(sprintf("\nPosition: %s, Season: %s - Not enough data for regression.\n", position, season))
    }
  })

# --- Calculate correlation and differences --- #

# Pivot wider to calculate r difference
cor_wide <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  summarise(cor = cor(`3P`, VORP, use = "complete.obs"), .groups = "drop") %>%
  pivot_wider(names_from = Season, values_from = cor)

# Add sample sizes
n_counts <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  summarise(n = sum(complete.cases(`3P`, VORP)), .groups = "drop") %>%
  pivot_wider(names_from = Season, values_from = n, names_prefix = "n_")

# Add difference and p-value using Fisher Z
cor_diff <- cor_wide %>%
  left_join(n_counts, by = "PrimaryPos") %>%
  mutate(
    z_1993 = atanh(`1993-94`),
    z_2023 = atanh(`2023-24`),
    se_diff = sqrt(1 / (`n_1993-94` - 3) + 1 / (`n_2023-24` - 3)),
    z_score = (z_2023 - z_1993) / se_diff,
    p_value = 2 * (1 - pnorm(abs(z_score))),
    cor_diff = `2023-24` - `1993-94`,
    diff_label = sprintf("??r = %.2f\np = %.3f", cor_diff, p_value)
  )

# --- Significance of correlations for both seasons --- #
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

# --- 93-94 Season Correlation and p-value per Position --- #
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

print("\nCorrelation Test Results for 1993-94:")
print(cor_test_93)
print("\nCorrelation Test Results for 2023-24:")
print(cor_test_23)

# Outliers for label highlighting
outliers <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  mutate(
    vorp_rank = rank(desc(VORP)),
    three_rank = rank(desc(`3P`))
  ) %>%
  filter(vorp_rank <= 2 | three_rank <= 2) %>%
  ungroup()

# Handle density safely: filter to valid variance
density_df <- analysis_df %>%
  group_by(PrimaryPos, Season) %>%
  filter(sd(`3P`, na.rm = TRUE) > 0 & sd(VORP, na.rm = TRUE) > 0) %>%
  ungroup()

# --- Final Plot with Adjusted Labels --- #
ggplot(analysis_df, aes(x = `3P`, y = VORP, color = Season)) +
  stat_density_2d(
    data = density_df,
    aes(fill = ..level..),
    alpha = 0.2, geom = "polygon", show.legend = FALSE
  ) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  
  # Correlation and p-value labels for both seasons
  geom_text(data = cor_significance,
            aes(x = max(analysis_df$`3P`, na.rm = TRUE) - 0.3,
                y = max(analysis_df$VORP, na.rm = TRUE) - (0.5 + ifelse(Season == "1993-94", 0, 1.5)),
                label = sig_label,
                color = Season),
            hjust = 1, vjust = 1, size = 3.5, show.legend = FALSE) +
  
  # Label top outliers
  geom_text_repel(data = outliers,
                  aes(label = Player),
                  size = 3,
                  max.overlaps = 30,
                  direction = "x",
                  nudge_x = 0.2,
                  segment.color = "gray60",
                  show.legend = FALSE) +
  
  facet_wrap(~PrimaryPos) +
  theme_minimal() +
  labs(
    title = "Correlation between 3-Pointers Made and VORP by Position",
    subtitle = "1993-94 vs 2023-24 Seasons",
    x = "3-Pointers Made per Game",
    y = "VORP",
    color = "Season",
    fill = "Density"
  ) +
  scale_color_manual(values = c("1993-94" = "blue", "2023-24" = "red")) +
  scale_fill_gradient(low = "gray90", high = "gray20") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold")
  )

