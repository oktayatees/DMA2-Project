# Load required libraries
library(tidyverse)

# File paths
file_paths <- c(
  "Source Data/1993-1994 NBA Player Advanced Stats Per Game.csv",
  "Source Data/2023-2024 NBA Player Advanced Stats Per Game.csv",
  "Source Data/2023-2024 NBA Player Stats Per Game.csv",
  "Source Data/1993-1994 NBA Player Stats Per Game.csv"
)

# Function to count missing values
count_missing_values <- function(file_path) {
  # Read the data
  df <- read.csv(file_path)
  
  # Count total cells in the dataset
  total_cells <- nrow(df) * ncol(df)
  
  # Count missing values
  missing_count <- sum(is.na(df))
  
  # Calculate percentage
  missing_percentage <- (missing_count / total_cells) * 100
  
  # Get file name
  file_name <- basename(file_path)
  
  # Print results
  cat("\nFile:", file_name, "\n")
  cat("Total missing values:", missing_count, "\n")
  cat("Missing value percentage:", round(missing_percentage, 2), "%\n")
  cat("------------------------\n")
}

# Calculate and display missing values for all files
cat("Missing Value Analysis Results:\n")
cat("=============================\n")
lapply(file_paths, count_missing_values) 
