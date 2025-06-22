# Load required packages
library(fmsb)    # For radar charts
library(dplyr)   # For data manipulation
library(scales)  # For data normalization
library(tibble)  # For column_to_rownames

# Define file paths
file_paths <- list(
  adv_2024 = "Source Data\\2023-2024 NBA Player Advanced Stats Per Game.csv",
  per_2024 = "Source Data\\2023-2024 NBA Player Stats Per Game.csv",
  adv_1994 = "Source Data\\1993-1994 NBA Player Advanced Stats Per Game.csv",
  per_1994 = "Source Data\\1993-1994 NBA Player Stats Per Game.csv"
)

# Function to read and preprocess data
read_nba_data <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Clean position column
  data$Pos <- ifelse(grepl("C", data$Pos), "Center",
                     ifelse(grepl("F", data$Pos), "Forward",
                            ifelse(grepl("G", data$Pos), "Guard", NA)))
  
  data <- filter(data, !is.na(Pos))  # Remove unknowns
  return(data)
}

# Read and merge datasets
adv_2024 <- read_nba_data(file_paths$adv_2024)
per_2024 <- read_nba_data(file_paths$per_2024)
adv_1994 <- read_nba_data(file_paths$adv_1994)
per_1994 <- read_nba_data(file_paths$per_1994)

nba_2024 <- merge(per_2024, adv_2024, by = c("Player", "Pos"))
nba_1994 <- merge(per_1994, adv_1994, by = c("Player", "Pos"))

# Function to prepare globally normalized radar data
prepare_combined_radar_data <- function(data_1994, data_2024) {
  get_stats <- function(data) {
    data %>%
      group_by(Pos) %>%
      summarise(
        PTS = mean(PTS, na.rm = TRUE),
        AST = mean(AST, na.rm = TRUE),
        TRB = mean(TRB, na.rm = TRUE),
        STL = mean(STL, na.rm = TRUE),
        BLK = mean(BLK, na.rm = TRUE),
        X3P. = mean(X3P., na.rm = TRUE),
        USG. = mean(USG., na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(Pos = factor(Pos, levels = c("Guard", "Forward", "Center"))) %>%
      arrange(Pos)
  }
  
  stats_1994 <- get_stats(data_1994)
  stats_2024 <- get_stats(data_2024)
  
  # Combine and normalize across both seasons
  combined <- rbind(stats_1994, stats_2024)
  combined_scaled <- combined %>%
    select(-Pos) %>%
    as.data.frame() %>%
    mutate_all(rescale)
  
  # Add position back
  combined_scaled$Pos <- rep(stats_1994$Pos, 2)
  
  # Separate and convert to radar format
  radar_1994 <- combined_scaled[1:3, ] %>% column_to_rownames("Pos")
  radar_2024 <- combined_scaled[4:6, ] %>% column_to_rownames("Pos")
  
  # Add fmsb-required max/min rows
  radar_1994 <- rbind(rep(1, ncol(radar_1994)), rep(0, ncol(radar_1994)), radar_1994)
  radar_2024 <- rbind(rep(1, ncol(radar_2024)), rep(0, ncol(radar_2024)), radar_2024)
  
  return(list(radar_1994 = radar_1994, radar_2024 = radar_2024))
}

# Generate the radar data
radars <- prepare_combined_radar_data(nba_1994, nba_2024)
radar_1994 <- radars$radar_1994
radar_2024 <- radars$radar_2024

# Custom radar chart function
create_radar <- function(data, title) {
  colors <- c("#1f77b4", "#ff7f0e", "#2ca02c") # Blue, Orange, Green
  op <- par(mar = c(1, 1, 2, 1)) # Adjust margins
  
  radarchart(
    data,
    axistype = 1,
    pcol = colors,
    plwd = 2,
    plty = 1,
    cglcol = "gray",
    cglty = 1,
    cglwd = 0.8,
    vlcex = 0.8,
    title = title
  )
  
  legend("topright",
         legend = rownames(data[3:5,]),
         bty = "n",
         pch = 20,
         col = colors,
         text.col = "black",
         cex = 1,
         pt.cex = 1.5)
  
  par(op)
}

# Create plots side by side
par(mfrow = c(1, 2))
create_radar(radar_1994, "NBA Role Archetypes (1993-94)")
create_radar(radar_2024, "NBA Role Archetypes (2023-24)")
