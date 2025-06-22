install.packages("ggplot2")
install.packages("ggrepel")
install.packages("ggalt")
install.packages("dplyr")
install.packages("magrittr")
install.packages("stats")

library(ggplot2)
library(ggrepel)
library(ggalt)
library(dplyr)
library(magrittr)
library(stats)

# Load your PCA data
nba_pca <- read.csv("Source Data/1993-1994 nba_pca_positions.csv")

# Print column names to see what we have
print("Column names in the dataset:")
print(colnames(nba_pca))

# Print first few rows to see the data structure
print("\nFirst few rows of the data:")
print(head(nba_pca))

# Create main clusters for each position using k-means
nba_pca <- nba_pca %>%
  group_by(Position) %>%
  group_modify(~ {
    # Perform k-means clustering with 2 clusters
    km <- kmeans(cbind(.x$PC1, .x$PC2), centers = 2)
    # Add cluster information
    .x$cluster <- km$cluster
    # Calculate cluster centers and distances
    centers <- km$centers
    .x$dist_to_center <- sqrt((.x$PC1 - centers[.x$cluster, 1])^2 + 
                            (.x$PC2 - centers[.x$cluster, 2])^2)
    .x
  }) %>%
  ungroup()

# Label only players far from the origin (outliers)
label_data <- subset(nba_pca, PC1 > 4 | PC2 > 3.5 | PC1 < -3.3 | PC2 < -2)

# Plot
ggplot(nba_pca, aes(x = PC1, y = PC2, color = Position)) +
  # Add position cluster highlighting
  geom_encircle(aes(group = interaction(Position, cluster), 
                    fill = Position), 
                alpha = 0.25,  # Reduced alpha for less visual impact
                expand = 0.01,  # Reduced expand for tighter clusters
                show.legend = FALSE) +
  geom_point(size = 3, alpha = 1) +
  geom_text_repel(data = label_data, aes(label = Player), size = 3) +
  theme_minimal() +
  labs(title = "PCA of NBA Player Stats by Position in 1993-1994",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold")) +
  # Ensure fill colors match the point colors
  scale_fill_manual(values = scales::hue_pal()(length(unique(nba_pca$Position)))) +
  # Add a white background to make overlapping areas less visible
  theme(panel.background = element_rect(fill = "white", color = NA))

